source('~/Desktop/R_functions/funcs.R')

library(dplyr)
library(tidyr)
library(mice)

local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))

DV   <- 'Role Functioning'
SEED <- 123
MIMP <- 5

PREDICTORS <- c(
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

FORMULA <- as.formula(
  paste0("outcome ~ `", paste(PREDICTORS, collapse = "` + `"), "`")
)

merged_df <- merged_df[merged_df$phenotype_four_groups == 'chr', ]

site_lookup <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, site) %>%
  distinct()

merged_df <- merged_df %>%
  dplyr::select(-site) %>%
  left_join(site_lookup, by = "src_subject_id")

base_pred <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, site, all_of(PREDICTORS)) %>%
  filter(if_any(all_of(PREDICTORS), ~!is.na(.)))
cat("N in base_pred:", nrow(base_pred), "\n")

outcome_bl <- merged_df %>%
  filter(visit == "bl") %>%
  dplyr::select(src_subject_id, all_of(DV))

outcome_12m <- merged_df %>%
  filter(visit == "12m") %>%
  dplyr::select(src_subject_id, all_of(DV))

both_timepoints <- merged_df %>%
  filter(visit %in% c("bl", "12m")) %>%
  dplyr::select(src_subject_id, site, visit, all_of(DV)) %>%
  pivot_wider(names_from = visit, values_from = all_of(DV)) %>%
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


setup_df <- function(pred_data, outcome_data, outcome_col) {
  if (outcome_col == "resid_change") {
    inner_join(pred_data, outcome_data, by = "src_subject_id") %>%
      rename(outcome = resid_change) %>%
      dplyr::select(-site) %>%
      mutate(across(all_of(PREDICTORS), ~scale(.)[, 1]))
  } else {
    inner_join(pred_data, outcome_data, by = "src_subject_id") %>%
      rename(outcome = all_of(outcome_col)) %>%
      group_by(site) %>%
      mutate(outcome = scale(outcome)[, 1]) %>%
      ungroup() %>%
      dplyr::select(-site) %>%
      mutate(across(all_of(PREDICTORS), ~scale(.)[, 1]))
  }
}

# sign Consistency
compute_sign_consistency <- function(data, predictors) {
  n       <- length(predictors)
  weights <- sapply(0:(n-1), function(k) factorial(k) * factorial(n-k-1) / factorial(n))
  
  # precompute all models of size 1 to n and cache coefficients
  all_subsets <- unlist(lapply(1:n, function(k) combn(predictors, k, simplify = FALSE)),
                        recursive = FALSE)
  coef_cache <- list()
  for (subset in all_subsets) {
    key <- paste(sort(subset), collapse = "|")
    f   <- as.formula(paste0("outcome ~ `", paste(subset, collapse = "` + `"), "`"))
    fit <- lm(f, data = data)
    cf  <- coef(fit)[-1]
    names(cf) <- gsub("`", "", names(cf))
    coef_cache[[key]] <- cf
  }
  
  # enumerate base subsets of size 0 to n-1
  base_subsets <- unlist(lapply(0:(n-1), function(k) combn(predictors, k, simplify = FALSE)),
                         recursive = FALSE)
  
  w_pos   <- setNames(numeric(n), predictors)
  w_neg   <- setNames(numeric(n), predictors)
  w_total <- setNames(numeric(n), predictors)
  
  for (subset in base_subsets) {
    k             <- length(subset)
    w             <- weights[k + 1]
    missing_preds <- setdiff(predictors, subset)
    for (pred in missing_preds) {
      key    <- paste(sort(c(subset, pred)), collapse = "|")
      beta_j <- coef_cache[[key]][pred]
      if (is.na(beta_j)) next
      w_total[pred] <- w_total[pred] + w
      if (beta_j > 0) w_pos[pred] <- w_pos[pred] + w
      if (beta_j < 0) w_neg[pred] <- w_neg[pred] + w
    }
  }
  
  setNames((w_pos - w_neg) / w_total, predictors)
}

run_sign_consistency <- function(pred_data, outcome_data, outcome_col, label) {
  df  <- setup_df(pred_data, outcome_data, outcome_col)
  cat("N in", label, "model:", nrow(df), "\n")
  imp <- mice(df, m = MIMP, method = "pmm", seed = SEED, printFlag = FALSE)
  imp_datasets <- lapply(1:MIMP, function(i) mice::complete(imp, i))
  cat(sprintf("Computing sign consistency for: %s\n", label))
  sc_list <- lapply(imp_datasets, function(d) compute_sign_consistency(d, PREDICTORS))
  sc_avg  <- colMeans(do.call(rbind, sc_list))
  results <- data.frame(
    Predictor        = names(sc_avg),
    sign_consistency = round(sc_avg, 3),
    row.names        = NULL
  ) %>% arrange(desc(abs(sign_consistency)))
  cat(sprintf("\n=== Sign Consistency: %s ===\n", label))
  print(results, row.names = FALSE)
  invisible(results)
}




sc_bl    <- run_sign_consistency(base_pred, outcome_bl,    DV,             "Baseline Functioning")
sc_12m   <- run_sign_consistency(base_pred, outcome_12m,   DV,             "12-Month Functioning")
sc_resid <- run_sign_consistency(base_pred, outcome_resid, "resid_change", "Residualized Change (12m | Baseline)")