# compute_years_of_education(df)
#
# Takes a data frame of survey responses and returns it with a new column
# `years_of_education` containing each participant's estimated years of
# completed education.
#
# HOW IT WORKS
# ------------
# Q50 (demo_education_p) splits everyone into two paths:
#
#   NOT in school (= 0):
#     Years are taken directly from Q51 (demo_occupation_p6), which asked
#     "How far did you get in school (in years)?"
#
#   IN school (= 1):
#     Q52 (demo_edu_p6) identifies the school type, then the current
#     grade/year is mapped to a cumulative years-of-education figure:
#
#       Middle / High school (demo_edu_p6 = 1) via Q56 (demo_edu_p8):
#         6th grade  → 6 yrs   |  9th grade  →  9 yrs
#         7th grade  → 7 yrs   |  10th grade → 10 yrs
#         8th grade  → 8 yrs   |  11th grade → 11 yrs
#                              |  12th grade → 12 yrs
#
#       College / Undergrad (demo_edu_p6 = 2) via Q54 (demo_edu_p2):
#         Freshman  → 13 yrs   |  Junior  → 15 yrs
#         Sophomore → 14 yrs   |  Senior  → 16 yrs
#
#       Graduate school (demo_edu_p6 = 3):
#         Fixed floor of 17 yrs (has at least a bachelor's degree)
#
#   "Other" responses at any branch, or missing values, return NA.
#
# USAGE
# -----
#   df <- compute_years_of_education(df)

compute_years_education <- function(df) {
  
  middle_high_years <- c("1"=6, "2"=7, "3"=8, "4"=9, "6"=10, "7"=11, "8"=12)
  college_years     <- c("1"=13, "2"=14, "3"=15, "4"=16)
  grad_years        <- 17
  
  as_int  <- function(x) suppressWarnings(as.integer(x))
  get_col <- function(name) {
    if (name %in% names(df)) df[[name]] else rep(NA_real_, nrow(df))
  }
  
  in_school   <- as_int(get_col("demo_education_p"))
  yrs_done    <- suppressWarnings(as.numeric(get_col("demo_occupation_p6")))
  school_type <- as_int(get_col("demo_edu_p6"))
  grade_code  <- as.character(as_int(get_col("demo_edu_p8")))
  college_yr  <- as.character(as_int(get_col("demo_edu_p2")))
  
  result <- rep(NA_real_, nrow(df))
  
  for (i in seq_len(nrow(df))) {
    
    if (is.na(in_school[i])) next
    
    if (in_school[i] == 0) {
      result[i] <- yrs_done[i]
      next
    }
    
    if (in_school[i] == 1 && !is.na(school_type[i])) {
      result[i] <- switch(as.character(school_type[i]),
                          "1" = unname(middle_high_years[grade_code[i]]),
                          "2" = unname(college_years[college_yr[i]]),
                          "3" = grad_years,
                          NA_real_
      )
    }
  }
  
  df$years_of_education <- result
  df
}