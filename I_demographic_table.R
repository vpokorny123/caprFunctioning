
source('~/Desktop/R_functions/funcs.R')   # vjp_build_demographics_table
library(data.table)
library(dplyr)
library(tidyr)
library(rstatix)
library(reshape2)

local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))

df <- merged_df[merged_df$phenotype_four_groups %in% 'chr' & merged_df$visit %in% 'bl', ]

# Keep subjects with at least one usable baseline outcome (matches missingness figure, outcome_rule = 'any')
has_social <- !is.na(df$`Social Functioning`)
has_role   <- !is.na(df$`Role Functioning`)
cat("CHR at baseline:", nrow(df), "\n")
print(table(social = has_social, role = has_role))
df <- df[has_social | has_role, ]
cat("With >=1 usable baseline outcome:", nrow(df),
    "| unique IDs:", length(unique(df$src_subject_id)), "\n")
stopifnot(!anyDuplicated(df$src_subject_id))

# ---- Derived variables -------------------------------------------------------
df$anxiolytic_lifetime <- factor(ifelse(df$nonbenzo_anx_lifetime | df$benzodiazepine_lifetime, 1, 0))
df$demo_biological_sex <- factor(df$demo_biological_sex, levels = c('M', 'F'))
df$demo_hispanic<- factor(df$demo_hispanic, levels = c('no', 'yes'))

df$scid_mdd_life_bin  <- factor(ifelse(df$scid_mdd_life == 3, 1, 0))
df$scid_aud_life_bin  <- factor(ifelse(df$scid_aud_life == 3, 1, 0))
df$scid_gad_life_bin  <- factor(ifelse(df$scid_gad_life == 3, 1, 0))
df$scid_adhd_life_bin <- factor(ifelse(df$scid_adhd == 1, 1, 0))

# there's one subject with incorrect age so let's set that to NA
df$demo_age_in_years[df$demo_age_in_years > 40] <- NA
df$site <- factor(df$site)

# set grouping variable name
grouping_var <- 'phenotype_four_groups'

# create demographic variable list: first value is name of the variable, second
# value is the "pretty" name to be printed, third value sets the
# variable as nominal (TRUE) or not (FALSE)
# fourth optional allows you to specify median instead of mean (e.g., for income)
demo_vars <- list(
  c('demo_age_in_years', 'Age', FALSE),
  c('demo_biological_sex', 'Sex', TRUE),
  c('demo_race', 'Race', TRUE),
  c('demo_hispanic', 'Hispanic', TRUE),
  c('years_of_education', 'Education (Years)', FALSE),
  c('demo_household_income', 'Household Income', FALSE, 'median'),
  c('gfs_current', 'Global Functioning: Social', FALSE),
  c('gfr_current', 'Global Functioning: Role', FALSE),
  c('sips_p_total', 'SIPS Positive Total Score', FALSE),
  c('sips_n_total', 'SIPS Negative Total Score', FALSE),
  c('site', '% Site', TRUE),
  c('antidepressant_lifetime', '% Antidepressant', TRUE),
  c('antipsychotic_lifetime', '% Antipsychotic', TRUE),
  c('moodstabilizer_lifetime', '% Mood Stabilizer', TRUE),
  c('stimulant_lifetime', '% Stimulants', TRUE),
  c('anxiolytic_lifetime', '% Anxiolytic', TRUE),
  c('scid_mdd_life_bin', '% Major Depressive Disorder', TRUE),
  c('scid_aud_life_bin', '% Alcohol Use Disorder', TRUE),
  c('scid_gad_life_bin', '% Generalized Anxiety Disorder', TRUE),
  c('scid_adhd_life_bin', '% ADHD', TRUE)
)

demo_df <- vjp_build_demographics_table(df, demo_vars, grouping_var)

write.csv(demo_df, paste0(local_dir, '../csvs/table_raw.csv'))