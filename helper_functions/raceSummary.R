raceSummary <- function(demo_clean) {
  
  demo_clean$demo_ethnicity <- ifelse(demo_clean$demo_hispanic == 4, "not_hispanic_or_latino",
                                      ifelse(demo_clean$demo_hispanic == 1, "of_hispanic_or_latino", ""))
  
  race_cols <- names(demo_clean)[grepl("race_", names(demo_clean))]
  
  demo_clean$demo_count <- suppressWarnings(
    rowSums(as.data.frame(sapply(demo_clean[, race_cols], as.numeric)), na.rm = TRUE)
  )
  
  race_vector <- c()
  
  for (i in 1:nrow(demo_clean)) {
    race_vector[i] <- ifelse(
      demo_clean[i,]$demo_count == 1, race_cols[which.max(demo_clean[i, race_cols])],
      ifelse(demo_clean[i,]$demo_count > 1, "more_than_one_race", "unknown_or_not_reported")
    )
  }
  
  demo_clean$demo_race <- race_vector
  
  return(demo_clean)
  
}