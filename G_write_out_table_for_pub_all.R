cov_tag <- "site_age_sex"   # must match `covars` in functioning_dominance.R
cols    <- c("Task Predictor", "Dominance", "95% CI", "% R2", "P-value", "P-value (adj.)", "Sign")

read_tab <- function(tp, outcome) {
  d <- read.csv(paste0("csvs/table_", tp, "_", outcome, "_cons_", cov_tag, "_FL_cis_permftest.csv"),
                check.names = FALSE, colClasses = "character")
  if (!"95% CI" %in% names(d)) d$`95% CI` <- ""  
  names(d)[names(d) == "P-value (FDR)"] <- "P-value (adj.)"
  s <- read.csv(paste0("csvs/sign_", tp, "_", outcome, "_", cov_tag, ".csv"), colClasses = "character")
  d$Sign <- s$sign_consistency[match(d$`Task Predictor`, s$Predictor)]
  d$Sign[is.na(d$Sign)] <- ""
  unname(as.matrix(d[, cols]))
}

for (tp in c("baseline", "12m", "resid", "change")) {
  out <- rbind(c("Role Functioning", rep("", length(cols)), "Social Functioning", rep("", length(cols) - 1)),
               c(cols, "", cols),
               cbind(read_tab(tp, "Rol"), "", read_tab(tp, "Soc")))
  path_out <- paste0("csvs/table_", tp, "_RolSoc_", cov_tag, "_cis.csv")
  write.table(out, path_out, sep = ",", row.names = FALSE, col.names = FALSE)
  cat("Saved:", path_out, "\n")
}