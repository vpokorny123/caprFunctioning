library(data.table)
library(dplyr)
library(tidyr)
library(rstatix)
library(reshape2)


local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))
merged_df <- merged_df[merged_df$phenotype_four_groups == 'chr' & merged_df$visit =='bl', ]
df <- merged_df

#set grouping variable name
grouping_var = 'phenotype_four_groups'


#there's one subject with incorrect age so let's set that to NA
df$demo_age_in_years[df$demo_age_in_years>40] <- NA
df$demo_hispanic<-factor(df$demo_hispanic,levels = c('yes','no'))

#clean up education variable
df$years
# create demographic variable list: first value is name of the variable, second 
#value is the "pretty" name to be printed, third value sets the 
#variable as nominal (TRUE) or not (FALSE)
#fourth optional allows you to specify median instead of mean (e.g., for income)
demo_vars <- list(
  c('demo_age_in_years', 'Age',FALSE),
  c('demo_biological_sex','Sex',TRUE),
  c('demo_race','Race',TRUE),
  c('demo_hispanic','Hispanic',TRUE),
  c('years_of_education','Education (Years)',FALSE),
  c('demo_household_income','Household Income',FALSE,'median')
  )

demo_df <- vjp_build_demographics_table(df, demo_vars, grouping_var)

write.csv(demo_df, paste0(local_dir,'../csvs/table_raw.csv'))
