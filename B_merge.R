library(wizaRdry)
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'


variables_to_load<-c(
  #tasks (skipping pessiglione and ch because  don't have clean scripts)
  "kamin_model","kamin","prl","sws","eefrt","dd","iaps","nsipr","sips_n","demographics_overview",
  "speed_tap","ebbinghaus","mooney_vjp","ch","glat",  #functioning measures
  "gfs","gfr","bacs","hvlt","sps","wrat",
  #demographics
  "demo", 'sips_p','nda_study_intake', 'sips_scoresheet','cesd'
)
for (j in variables_to_load) {
  load(paste0(local_dir,j,'_clean.RData'))
}

#need to fix race variable
source('./R_scripts/helper_functions/raceSummary.R')
demo_clean <- raceSummary(demo_clean)

#need to fix education variable
source('./R_scripts/helper_functions/compute_years_education.R')
demographics_overview_clean <- compute_years_education(demographics_overview_clean)

#create list of all dataframes to merge and make sure all dataframes are good to be 
# merged
variables_to_merge<-paste0(variables_to_load,'_clean')
dfs_to_merge = NULL
for (j in seq(variables_to_merge)) {
  name <- variables_to_merge[j]
  df<-base::get(name)
  
  # merge gets mad if some src_subjects are factors and others are numeric
  df$src_subject_id<-as.numeric(as.character(df$src_subject_id))
  
  #get rid of columns share between dfs because any discrepancies can and will lead
  #to merging problems
  df <- df %>% 
    ungroup() %>% #had to add ungroup to be able to drop columns that were previously grouped
    dplyr::select(-any_of(c('sex', 'subjectkey', 'interview_age',
                            'interview_date','handedness','int_end', 'int_start',
                            'local_lab_id','sex_other','group_status','intvr1','intvr2',
                            'int_diff','ResponseId','state', 'StartDate','EndDate',
                            'Status','Progress','Duration (in seconds)',
                            'Finished', 'RecordedDate','DistributionChannel',
                            'UserLanguage','candidateId','studyId')))
  #NDA study intake should have the ground truth on phenotype so drop phenotype
  #from any other dataframe
  if (name != 'nda_study_intake_clean'){
    df <- df %>% dplyr::select(-any_of(c('phenotype','phenotype_note','phenotype_dummy',
                                         'nda_study_intake_complete','site')))
  }
  dfs_to_merge[[name]]<-df
  #all_dicts[[name]]<-dict
}
#finally we can merge
merged_df<-meld(dfs_to_merge,by = c("src_subject_id", "visit"))

# check for discrepant variables
discrepant <- merged_df %>% select(ends_with(".x"),ends_with(".y")) 

merged_df<-merged_df %>% relocate(src_subject_id,visit,nda_phenotype) %>% 
  dplyr::rename('phenotype' = 'nda_phenotype')

#check for duplicates per visit
dup <- merged_df %>%
  group_by(src_subject_id, visit) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n>1)

dup

#remove duplicates per visit
merged_df <- merged_df %>%
  group_by(src_subject_id, visit) %>%
  slice(1) %>%
  ungroup()


#remove folks with NAs for phenotype
merged_df <- merged_df[!is.na(merged_df$phenotype_four_groups) ,]

#create extra grouping
merged_df$phenotype_two_groups <- ifelse(merged_df$phenotype_four_groups=='chr', 'chr','con')

#order the visit factor
merged_df$visit <- factor(merged_df$visit, levels = c('bl','12m','24m'))
#merged_df


#-------------------------------------------------------------------------------
#save out
save(merged_df,
     file = paste0(local_dir,'merged_data.RData'))

