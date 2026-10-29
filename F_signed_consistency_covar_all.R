
library(dplyr)
library(mice)
dvs        <- c('Social Functioning', 'Role Functioning')
timepoints <- c('baseline', '12m', 'resid', 'change')
cov_tag    <- "site_age_sex"          # must match the main script's covariates
dir.create("csvs", showWarnings = FALSE)

compute_sign_consistency <- function(data, predictors, covars) {
  n       <- length(predictors)
  weights <- sapply(0:(n-1), function(k) factorial(k) * factorial(n-k-1) / factorial(n))
  cov_terms <- if (length(covars)) paste0("`", covars, "` + ", collapse = "") else ""
  all_subsets <- unlist(lapply(1:n, function(k) combn(predictors, k, simplify = FALSE)),
                        recursive = FALSE)
  coef_cache <- list()
  for (subset in all_subsets) {
    key <- paste(sort(subset), collapse = "|")
    f   <- as.formula(paste0("outcome ~ ", cov_terms, "`", paste(subset, collapse = "` + `"), "`"))
    cf  <- coef(lm(f, data = data))
    names(cf) <- gsub("`", "", names(cf))
    coef_cache[[key]] <- cf[subset]
  }
  
  base_subsets <- unlist(lapply(0:(n-1), function(k) combn(predictors, k, simplify = FALSE)),
                         recursive = FALSE)
  
  w_pos   <- setNames(numeric(n), predictors)
  w_neg   <- setNames(numeric(n), predictors)
  w_total <- setNames(numeric(n), predictors)
  
  for (subset in base_subsets) {
    w <- weights[length(subset) + 1]
    for (pred in setdiff(predictors, subset)) {
      beta_j <- coef_cache[[paste(sort(c(subset, pred)), collapse = "|")]][pred]
      if (is.na(beta_j)) next
      w_total[pred] <- w_total[pred] + w
      if (beta_j > 0) w_pos[pred] <- w_pos[pred] + w
      if (beta_j < 0) w_neg[pred] <- w_neg[pred] + w
    }
  }
  
  setNames((w_pos - w_neg) / w_total, predictors)
}

run_sign_consistency <- function(tp, dv) {
  model_out <- readRDS(paste0("imputations/out_", tp, "_", substr(dv, 1, 3), "_", cov_tag, ".rds"))
  imp <- model_out$imp
  imp_datasets <- lapply(seq_len(imp$m), function(i) mice::complete(imp, i)[model_out$obs_y, ])
  cat(sprintf("\nComputing sign consistency for: %s — %s (N = %d, m = %d)\n",
              dv, model_out$label, nrow(imp_datasets[[1]]), imp$m))
  sc_avg <- colMeans(do.call(rbind, lapply(imp_datasets, compute_sign_consistency,
                                           predictors = model_out$predictors,
                                           covars     = model_out$covars)))
  results <- data.frame(
    Predictor        = names(sc_avg),
    sign_consistency = round(sc_avg, 3),
    row.names        = NULL
  ) %>% arrange(desc(abs(sign_consistency)))
  cat(sprintf("\n=== Sign Consistency: %s — %s (covariates: %s) ===\n",
              dv, model_out$label, paste(model_out$covars, collapse = ", ")))
  print(results, row.names = FALSE)
  path <- paste0("csvs/sign_", tp, "_", substr(dv, 1, 3), "_", cov_tag, ".csv")
  write.csv(results, path, row.names = FALSE)
  cat("Saved:", path, "\n")
}


for (dv in dvs) 
  for (tp in timepoints) 
    run_sign_consistency(tp, dv)