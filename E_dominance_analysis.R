source('~/Desktop/R_functions/funcs.R')
library(dplyr)
library(tidyr)
library(mice)
library(relaimpo)
library(parallel)
library(pbapply)

local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/OLD/do not use CAPR Global Functioning Paper Take 2/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))
merged_df <- merged_df[merged_df$phenotype_four_groups == 'chr', ]

site_lookup <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, site) %>%
  distinct()

merged_df <- merged_df %>%
  dplyr::select(-site) %>%
  left_join(site_lookup, by = "src_subject_id")

dv <- 'Role Functioning'

predictors <- c(
  "Blocking Score (Kamin)",
  "Change in d' (SWS)",
  "Nu Evidence vs. Belief (CH)",
  "Win-Switches (PRL)",
  "Gain vs Loss (Acquisition)",
  "High Effort, High Reward Scores (EEFRT)",
  "Large Reward K (Delayed Discounting)",
  "Hedonic Reactivity (HR)",
  "Finger Tapping Speed (CFT)",
  "Context Sensitivity (Ebbinghaus)",
  "Inverted Face Reports (Mooney Faces)"
)

base_pred <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, site, all_of(predictors)) %>%
  filter(if_any(all_of(predictors), ~!is.na(.)))
cat("N in base_pred:", nrow(base_pred), "\n")

outcome_bl <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, all_of(dv))

outcome_12m <- merged_df %>%
  filter(visit == "12m") %>%
  dplyr::select(src_subject_id, all_of(dv))

both_timepoints <- merged_df %>%
  filter(visit %in% c("bl", "12m")) %>%
  dplyr::select(src_subject_id, site, visit, all_of(dv)) %>%
  pivot_wider(names_from = visit, values_from = all_of(dv)) %>%
  group_by(site) %>%
  mutate(bl_scaled = scale(bl)[, 1], t12m_scaled = scale(`12m`)[, 1]) %>%
  ungroup() %>%
  filter(!is.na(bl_scaled) & !is.na(t12m_scaled))

resid_model <- lm(t12m_scaled ~ bl_scaled, data = both_timepoints)
cat(sprintf("\nBaseline → 12m: β = %.3f, R² = %.3f\n\n",
            coef(resid_model)["bl_scaled"], summary(resid_model)$r.squared))

outcome_resid <- both_timepoints %>%
  mutate(resid_change = resid(resid_model)) %>%
  dplyr::select(src_subject_id, resid_change)

run_model <- function(pred_data, outcome_data, outcome_col, label) {
  if (outcome_col == "resid_change") {
    df <- inner_join(pred_data, outcome_data, by = "src_subject_id") %>%
      rename(outcome = resid_change) %>%
      dplyr::select(-site)
  } else {
    df <- inner_join(pred_data, outcome_data, by = "src_subject_id") %>%
      rename(outcome = all_of(outcome_col)) %>%
      group_by(site) %>%
      mutate(outcome = scale(outcome)[, 1]) %>%
      ungroup() %>%
      dplyr::select(-site)
  }
  cat("N in", label, "model:", nrow(df), "\n")
  imp <- mice(df, m = 5, method = "pmm", seed = 123, printFlag = FALSE)
  f <- as.formula(paste0("outcome ~ `", paste(predictors, collapse = "` + `"), "`"))
  fit <- lapply(1:5, function(i) lm(f, data = mice::complete(imp, i)))
  fit_mira <- as.mira(fit)
  pooled_obj <- pool(fit_mira)
  pooled <- summary(pooled_obj) %>%
    filter(term != "(Intercept)") %>%
    mutate(p.fdr = round(p.adjust(p.value, method = "fdr"), 3),
           across(where(is.numeric), ~round(., 3))) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value, p.fdr)
  r2 <- pool.r.squared(fit_mira)
  r2_adj <- pool.r.squared(fit_mira, adjusted = TRUE)
  f_stats <- sapply(fit, function(m) summary(m)$fstatistic)
  f_pooled <- mean(f_stats[1, ])
  df1 <- f_stats[2, 1]
  df2 <- f_stats[3, 1]
  p_model <- pf(f_pooled, df1, df2, lower.tail = FALSE)
  cat("\n===", label, "===\n")
  cat(sprintf("R² = %.3f [%.3f, %.3f]   Adj. R² = %.3f [%.3f, %.3f]   F(%g, %g) = %.3f, p = %.3f\n\n",
              r2[1], r2[2], r2[3], r2_adj[1], r2_adj[2], r2_adj[3], df1, df2, f_pooled, p_model))
  print(pooled, row.names = FALSE)
  cat("\n--- Manuscript Stats ---\n")
  print(pub_ready_stats(pooled_obj))
  invisible(list(pooled = pooled %>% mutate(model = label), imp = imp,
                 pooled_obj = pooled_obj, r2 = r2, r2_adj = r2_adj,
                 f_pooled = f_pooled, df1 = df1, df2 = df2, p_model = p_model))
}

run_dominance <- function(imp, label, n_perm = 1000, save_dir = "perm_results/") {
  f <- as.formula(paste0("outcome ~ `", paste(predictors, collapse = "` + `"), "`"))
  relimp_list <- lapply(1:5, function(i) {
    calc.relimp(lm(f, data = mice::complete(imp, i)), type = "lmg", rela = FALSE)@lmg
  })
  relimp_mat <- do.call(rbind, relimp_list)
  relimp_avg <- colMeans(relimp_mat)
  relimp_sd <- apply(relimp_mat, 2, sd)
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, varlist = c("imp", "f", "predictors"), envir = environment())
  clusterEvalQ(cl, library(mice))
  clusterEvalQ(cl, library(relaimpo))
  cat(sprintf("\nRunning %d permutations on %d cores...\n", n_perm, n_cores))
  perm_mat <- pblapply(1:n_perm, function(perm_i) {
    avg_lmg <- numeric(length(predictors))
    for (i in 1:5) {
      d <- mice::complete(imp, i)
      d$outcome <- sample(d$outcome)
      avg_lmg <- avg_lmg + calc.relimp(lm(f, data = d), type = "lmg", rela = FALSE)@lmg
    }
    avg_lmg / 5
  }, cl = cl)
  stopCluster(cl)
  perm_mat <- do.call(rbind, perm_mat)
  if (!dir.exists(save_dir)) dir.create(save_dir)
  save_path <- paste0(save_dir, "perm_", gsub(" ", "_", label), ".RData")
  save(perm_mat, file = save_path)
  cat("Saved permutation results to:", save_path, "\n")
  p_perm <- sapply(seq_along(relimp_avg), function(j) {
    mean(perm_mat[, j] >= relimp_avg[j])
  })
  results <- data.frame(
    Predictor = names(relimp_avg),
    Imp1 = round(relimp_mat[1, ], 3),
    Imp2 = round(relimp_mat[2, ], 3),
    Imp3 = round(relimp_mat[3, ], 3),
    Imp4 = round(relimp_mat[4, ], 3),
    Imp5 = round(relimp_mat[5, ], 3),
    Mean = round(relimp_avg, 3),
    SD = round(relimp_sd, 3),
    Pct_R2 = round(relimp_avg / sum(relimp_avg) * 100, 1),
    p_perm = round(p_perm, 3),
    row.names = NULL
  ) %>% arrange(desc(Mean))
  cat(sprintf("\n=== Dominance Analysis: %s ===\n", label))
  cat(sprintf("LMG sum (= total R²): %.3f\n\n", sum(relimp_avg)))
  print(results, row.names = FALSE)
  invisible(results)
}

fmt_dominance <- function(x) ifelse(x < .001, "<.001", as.character(round(x, 3)))

make_model_csv <- function(model_out, dom_out, label, path) {
  header <- data.frame(
    Rank = "",
    Predictor = "Total R²",
    Dominance = fmt_dominance(sum(dom_out$Mean)),
    Pct_R2 = "100", p_perm = "", sig = ""
  )
  sig_df <- model_out$pooled %>%
    dplyr::select(term, p.value) %>%
    mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**",
                           p.value < .05 ~ "*", TRUE ~ ""))
  rows <- dom_out %>%
    left_join(sig_df, by = c("Predictor" = "term")) %>%
    mutate(Rank = as.character(row_number()),
           Mean = fmt_dominance(Mean),
           Pct_R2 = as.character(Pct_R2),
           p_perm = as.character(p_perm)) %>%
    dplyr::select(Rank, Predictor, Dominance = Mean, Pct_R2, p_perm, sig)
  write.csv(bind_rows(rows, header), file = path, row.names = FALSE)
  cat("Saved:", path, "\n")
}

out_bl <- run_model(base_pred, outcome_bl, dv, "Baseline Functioning")
dom_bl <- run_dominance(out_bl$imp, "Baseline Functioning")
make_model_csv(out_bl, dom_bl, "Baseline Functioning", paste0("csvs/table_baseline_", substr(dv, 1, 3), ".csv"))

out_12m <- run_model(base_pred, outcome_12m, dv, "12-Month Functioning")
dom_12m <- run_dominance(out_12m$imp, "12-Month Functioning")
make_model_csv(out_12m, dom_12m, "12-Month Functioning", paste0("csvs/table_12m_", substr(dv, 1, 3), ".csv"))

out_resid <- run_model(base_pred, outcome_resid, "resid_change", "Residualized Change (12m | Baseline)")
dom_resid <- run_dominance(out_resid$imp, "Residualized Change (12m | Baseline)")
make_model_csv(out_resid, dom_resid, "Residualized Change (12m | Baseline)", paste0("csvs/table_resid_", substr(dv, 1, 3), ".csv"))