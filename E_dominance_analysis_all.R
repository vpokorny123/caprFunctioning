source('~/Desktop/R_functions/funcs.R')
library(dplyr)
library(tidyr)
library(mice)
library(relaimpo)
library(parallel)
library(pbapply)

local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))
merged_df <- merged_df[merged_df$phenotype_four_groups == 'chr', ]
merged_df$sex<-merged_df$demo_biological_sex
merged_df$loglrgk<-log(merged_df$`Large Reward K (Delay Discounting)`)
merged_df$log_income<-log(merged_df$demo_household_income+1)
merged_df$anxiolytic_lifetime<- factor(ifelse(merged_df$nonbenzo_anx_lifetime |
                                                merged_df$benzodiazepine_lifetime,1,0))
merged_df$anxiolytic_current<- factor(ifelse(merged_df$nonbenzo_anx_current |
                                               merged_df$benzodiazepine_current,1,0))

site_lookup <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, site) %>%
  distinct()

merged_df <- merged_df %>%
  dplyr::select(-site) %>%
  left_join(site_lookup, by = "src_subject_id")

# ---- Settings ----------------------------------------------------------------
dv     <- 'Social Functioning'
n_imp  <- 10
n_perm <- 500
n_boot <- 500

# Null-model covariates
covars <- c("site","age","sex")

#med var names: "antidepressant_current","antipsychotic_current",
#"moodstabilizer_current","stimulant_current","anxiolytic_current"

bl_covar <- "baseline_dv"

cov_tag  <- paste(covars, collapse = "_")   # used in saved file names, e.g. "age_sex_site"
file_tag <- paste0(substr(dv, 1, 3), "_", cov_tag)

# Saved model objects (imputations + settings) so other scripts can run in a fresh session
dir.create("imputations", showWarnings = FALSE)
dir.create("csvs", showWarnings = FALSE)

predictors <- c(
  "Blocking Score (Kamin)",
  "Change in d' (SWS)",
  "Nu Evidence vs. Belief (CH)",
  "Win-Switches (PRL)",
  "Gain vs Loss (Acquisition)",
  "High Effort, High Reward Scores (EEFRT)",
  "Large Reward K (Delay Discounting)", 
  "Hedonic Reactivity (HR)",
  "Finger Tapping Speed (CFT)",
  "Context Sensitivity (Ebbinghaus)",
  "Inverted Face Reports (Mooney Faces)"
)

base_pred <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, all_of(covars), all_of(predictors)) %>%
  mutate(across(all_of(covars) & where(is.character), ~factor(na_if(., ""))),
         across(any_of(intersect("site", covars)), factor)) %>%   # site always a factor, if included
  filter(!if_all(all_of(predictors), is.na))
cat("N in base_pred:", nrow(base_pred), "\n")
for (cv in covars) if (is.factor(base_pred[[cv]])) { cat(cv, ":\n"); print(table(base_pred[[cv]], useNA = "ifany")) }

outcome_bl <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, all_of(dv))

outcome_12m <- merged_df %>%
  filter(visit == "12m") %>%
  dplyr::select(src_subject_id, all_of(dv))

both_timepoints <- merged_df %>%
  filter(visit %in% c("bl", "12m")) %>%
  dplyr::select(src_subject_id, visit, all_of(dv)) %>%
  pivot_wider(names_from = visit, values_from = all_of(dv)) %>%
  filter(!is.na(bl) & !is.na(`12m`))

resid_model <- lm(`12m` ~ bl, data = both_timepoints)
cat(sprintf("\nBaseline → 12m: β = %.3f, R² = %.3f, N = %d\n\n",
            coef(resid_model)["bl"], summary(resid_model)$r.squared, nobs(resid_model)))

outcome_resid <- both_timepoints %>%
  transmute(src_subject_id, resid_change = resid(resid_model))

outcome_change <- outcome_bl %>%
  rename(!!bl_covar := all_of(dv)) %>%
  full_join(outcome_12m, by = "src_subject_id")

run_model <- function(outcome_data, outcome_col, label, covs = covars) {
  f0 <- reformulate(paste0("`", covs, "`"), "outcome")                  # null: covariates only
  f  <- reformulate(paste0("`", c(covs, predictors), "`"), "outcome")   # full: covariates + tasks
  df <- left_join(base_pred, outcome_data, by = "src_subject_id") %>%
    rename(outcome = all_of(outcome_col)) %>%
    dplyr::select(-src_subject_id)
  obs_y <- !is.na(df$outcome)
  cat("N imputed for", label, "model:", nrow(df), "| N in model:", sum(obs_y), "\n")
  
  # mice defaults: pmm for numeric, logreg/polyreg for factor covariates
  imp <- mice(df, m = n_imp, seed = 123, printFlag = FALSE)
  
  data_list <- lapply(seq_len(n_imp), function(i) mice::complete(imp, i)[obs_y, ])
  fit0 <- lapply(data_list, function(d) lm(f0, data = d))
  fit  <- lapply(data_list, function(d) lm(f, data = d))
  pooled_obj <- pool(as.mira(fit))
  
  pooled <- summary(pooled_obj) %>%
    mutate(term = gsub("`", "", as.character(term))) %>%
    filter(term != "(Intercept)") %>%
    mutate(type  = ifelse(term %in% predictors, "task", "covariate"),
           p.fdr = NA_real_)
  is_task <- pooled$type == "task"
  pooled$p.fdr[is_task] <- p.adjust(pooled$p.value[is_task], method = "fdr")
  pooled <- pooled %>%
    arrange(desc(type == "covariate")) %>%
    mutate(across(where(is.numeric), ~round(., 3))) %>%
    dplyr::select(type, term, estimate, std.error, statistic, p.value, p.fdr)
  
  r2_null  <- pool.r.squared(as.mira(fit0))
  r2       <- pool.r.squared(as.mira(fit))
  r2_adj   <- pool.r.squared(as.mira(fit), adjusted = TRUE)
  delta_r2 <- mean(sapply(seq_along(fit), function(i)
    summary(fit[[i]])$r.squared - summary(fit0[[i]])$r.squared))
  
  cat("\n===", label, "===\n")
  cat(sprintf("Null R² = %.3f [%.3f, %.3f]   Full R² = %.3f [%.3f, %.3f]   Adj. R² = %.3f\n",
              r2_null[1], r2_null[2], r2_null[3], r2[1], r2[2], r2[3], r2_adj[1]))
  cat(sprintf("ΔR² = %.3f (significance tested by permutation in run_dominance)\n\n", delta_r2))
  print(pooled, row.names = FALSE)
  cat("\n--- Manuscript Stats ---\n")
  print(pub_ready_stats(pooled_obj))
  
  invisible(list(pooled = pooled, imp = imp, obs_y = obs_y, covars = covs, predictors = predictors, label = label))
}


run_dominance <- function(model_out, save_dir = "perm_results/") {
  covs      <- model_out$covars
  label     <- model_out$label
  f0        <- reformulate(paste0("`", covs, "`"), "outcome")
  f         <- reformulate(paste0("`", c(covs, predictors), "`"), "outcome")
  data_list <- lapply(seq_len(model_out$imp$m), function(i)
    mice::complete(model_out$imp, i)[model_out$obs_y, ])
  n <- nrow(data_list[[1]])
  
  # keep the covariate–outcome relationship intact and only break
  #the link between the tasks and what the
  # covariates leave unexplained
  nulls <- lapply(data_list, function(d) { m <- lm(f0, data = d); list(fit = fitted(m), res = resid(m)) })
  
  obs_all <- sapply(data_list, function(d) {
    m <- lm(f, data = d)
    c(calc.relimp(m, type = "lmg", rela = FALSE, always = covs)@lmg,
      F_change = anova(lm(f0, data = d), m)$F[2])
  })
  relimp_mat <- t(obs_all[rownames(obs_all) != "F_change", , drop = FALSE])
  relimp_avg <- colMeans(relimp_mat)
  obs_F      <- mean(obs_all["F_change", ])
  
  cl <- makeCluster(detectCores() - 1)
  clusterSetRNGStream(cl, 1)                          # reproducible permutations
  clusterExport(cl, c("data_list", "nulls", "f0", "f", "covs", "n"), envir = environment())
  clusterEvalQ(cl, library(relaimpo))
  cat(sprintf("\nRunning %d Freedman–Lane permutations...\n", n_perm))
  perm_all <- do.call(rbind, pblapply(1:n_perm, function(k) {
    idx <- sample(n)                                  # one shuffle, used for every imputation
    rowMeans(sapply(seq_along(data_list), function(i) {
      d <- data_list[[i]]
      d$outcome <- nulls[[i]]$fit + nulls[[i]]$res[idx]
      m <- lm(f, data = d)
      c(calc.relimp(m, type = "lmg", rela = FALSE, always = covs)@lmg,
        F_change = anova(lm(f0, data = d), m)$F[2])
    }))
  }, cl = cl))
  stopCluster(cl)
  
  perm_F   <- perm_all[, "F_change"]
  perm_mat <- perm_all[, colnames(perm_all) != "F_change", drop = FALSE]
  
  dir.create(save_dir, showWarnings = FALSE)
  save(perm_mat, perm_F,
       file = paste0(save_dir, "perm_FL_", gsub(" ", "_", label), "_", cov_tag, ".RData"))
  
  # +1 correction so p is never exactly 0
  p_perm  <- (colSums(sweep(perm_mat, 2, relimp_avg, ">=")) + 1) / (n_perm + 1)
  p_delta <- (sum(perm_F >= obs_F) + 1) / (n_perm + 1)   # permutation F-change test of ΔR²
  
  results <- data.frame(
    Predictor = gsub("`", "", names(relimp_avg)),
    setNames(as.data.frame(round(t(relimp_mat), 3)), paste0("Imp", seq_along(data_list))),
    Mean   = round(relimp_avg, 3),
    SD     = round(apply(relimp_mat, 2, sd), 3),
    Pct_R2 = round(relimp_avg / sum(relimp_avg) * 100, 1),
    p_perm = round(p_perm, 4),
    row.names = NULL
  ) %>% arrange(desc(Mean))
  attr(results, "p_delta") <- p_delta
  
  cat(sprintf("\n=== Dominance Analysis: %s ===\nLMG sum (= ΔR² over covariates): %.3f\n",
              label, sum(relimp_avg)))
  cat(sprintf("Permutation F-change (Freedman–Lane): mean F = %.3f, p = %.4f\n\n",
              obs_F, p_delta))
  print(results, row.names = FALSE)
  invisible(results)
}

# Bootstrap: 95% percentile CIs for each task's dominance weight (and ΔR²)
run_bootstrap <- function(model_out, save_dir = "boot_results/") {
  covs      <- model_out$covars
  label     <- model_out$label
  f         <- reformulate(paste0("`", c(covs, predictors), "`"), "outcome")
  data_list <- lapply(seq_len(model_out$imp$m), function(i)
    mice::complete(model_out$imp, i)[model_out$obs_y, ])
  n <- nrow(data_list[[1]])
  # resample within site so every site level appears in every bootstrap sample
  strata <- if ("site" %in% covs) data_list[[1]]$site else rep(1, n)
  
  cl <- makeCluster(detectCores() - 1)
  clusterSetRNGStream(cl, 2)                          # reproducible resamples
  clusterExport(cl, c("data_list", "f", "covs", "predictors", "strata", "n"), envir = environment())
  clusterEvalQ(cl, library(relaimpo))
  cat(sprintf("\nRunning %d bootstrap resamples...\n", n_boot))
  boot_mat <- do.call(rbind, pblapply(1:n_boot, function(k) {
    # one resample (within site), used for every imputation
    idx <- unlist(lapply(split(seq_len(n), strata),
                         function(i) i[sample.int(length(i), replace = TRUE)]))
    tryCatch(rowMeans(sapply(data_list, function(d)
      calc.relimp(lm(f, data = droplevels(d[idx, ])), type = "lmg", rela = FALSE, always = covs)@lmg)),
      error = function(e) rep(NA_real_, length(predictors)))
  }, cl = cl))
  stopCluster(cl)
  colnames(boot_mat) <- predictors
  cat("Failed bootstrap resamples:", sum(is.na(boot_mat[, 1])), "of", n_boot, "\n")
  
  dir.create(save_dir, showWarnings = FALSE)
  save(boot_mat, file = paste0(save_dir, "boot_", gsub(" ", "_", label), "_", cov_tag, ".RData"))
  
  ci <- apply(boot_mat, 2, quantile, c(.025, .975), na.rm = TRUE)
  out <- data.frame(Predictor = colnames(boot_mat),
                    CI_low = round(ci[1, ], 3), CI_high = round(ci[2, ], 3), row.names = NULL)
  attr(out, "delta_ci") <- quantile(rowSums(boot_mat), c(.025, .975), na.rm = TRUE)
  cat(sprintf("\n=== Bootstrap 95%% CIs: %s ===\nΔR² CI: [%.3f, %.3f]\n\n",
              label, attr(out, "delta_ci")[1], attr(out, "delta_ci")[2]))
  print(out, row.names = FALSE)
  invisible(out)
}

# ---- Tables ------------------------------------------------------------------
# boot_out is optional: pass run_bootstrap() output to add a 95% CI column.
# ΔR² row p-value = Freedman–Lane permutation F-change.
make_model_csv <- function(model_out, dom_out, path, boot_out = NULL) {
  d <- dom_out %>%
    mutate(p_perm_fdr = p.adjust(p_perm, method = "fdr")) %>%
    left_join(model_out$pooled, by = c("Predictor" = "term"))
  
  dom   <- c(d$Mean, sum(d$Mean))
  p     <- c(d$p_perm, attr(dom_out, "p_delta"))
  p_fdr <- d$p_perm_fdr
  sig   <- c(d$p.value, NA)
  sig_fdr <- c(d$p.fdr, NA)
  
  out <- data.frame(
    `Task Predictor`           = c(d$Predictor, "ΔR² (over covariates)"),
    Dominance                  = ifelse(dom < .001, "<.001", as.character(round(dom, 3))),
    `% R2`                     = c(sprintf("%.1f", d$Pct_R2), "100"),
    `P-value`                  = ifelse(p < .001, "<.001", sprintf("%.3f", p)),
    `P-value (FDR)`            = c(ifelse(p_fdr < .001, "<.001", sprintf("%.3f", p_fdr)), ""),
    vanilla_regression_sig     = case_when(sig < .001 ~ "***", sig < .01 ~ "**", sig < .05 ~ "*", TRUE ~ ""),
    vanilla_regression_sig_fdr = case_when(sig_fdr < .001 ~ "***", sig_fdr < .01 ~ "**", sig_fdr < .05 ~ "*", TRUE ~ ""),
    check.names = FALSE
  )
  if (!is.null(boot_out)) {
    b  <- boot_out[match(d$Predictor, boot_out$Predictor), ]
    dc <- attr(boot_out, "delta_ci")
    out <- tibble::add_column(out, `95% CI` = sprintf("[%.3f, %.3f]", c(b$CI_low, dc[1]), c(b$CI_high, dc[2])),
                              .after = "Dominance")
  }
  write.csv(out, file = path, row.names = FALSE)
  cat("Saved:", path, "\n")
}

# ---- Run ---------------------------------------------------------------------
out_bl  <- run_model(outcome_bl, dv, "Baseline Functioning")
saveRDS(out_bl, paste0("imputations/out_baseline_", file_tag, ".rds"))
dom_bl  <- run_dominance(out_bl)
boot_bl <- run_bootstrap(out_bl)
make_model_csv(out_bl, dom_bl, paste0("csvs/table_baseline_", substr(dv, 1, 3), "_cons_", cov_tag, "_FL_cis_permftest.csv"), boot_bl)

out_12m  <- run_model(outcome_12m, dv, "12-Month Functioning")
saveRDS(out_12m, paste0("imputations/out_12m_", file_tag, ".rds"))
dom_12m  <- run_dominance(out_12m)
boot_12m <- run_bootstrap(out_12m)
make_model_csv(out_12m, dom_12m, paste0("csvs/table_12m_", substr(dv, 1, 3), "_cons_", cov_tag, "_FL_cis_permftest.csv"), boot_12m)

out_resid  <- run_model(outcome_resid, "resid_change", "Residualized Change (12m | Baseline)")
saveRDS(out_resid, paste0("imputations/out_resid_", file_tag, ".rds"))
dom_resid  <- run_dominance(out_resid)
boot_resid <- run_bootstrap(out_resid)
make_model_csv(out_resid, dom_resid, paste0("csvs/table_resid_", substr(dv, 1, 3), "_cons_", cov_tag, "_FL_cis_permftest.csv"), boot_resid)

out_change  <- run_model(outcome_change, dv, "12m Functioning (Baseline-Adjusted)", covs = c(covars, bl_covar))
saveRDS(out_change, paste0("imputations/out_change_", file_tag, ".rds"))
dom_change  <- run_dominance(out_change)
boot_change <- run_bootstrap(out_change)
make_model_csv(out_change, dom_change, paste0("csvs/table_change_", substr(dv, 1, 3), "_cons_", cov_tag, "_FL_cis_permftest.csv"), boot_change)