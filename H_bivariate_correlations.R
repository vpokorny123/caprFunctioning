library(Hmisc)
library(psych)
local_dir1 <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/OLD/do not use CAPR Global Functioning Paper Take 2/RData/'
local_dir<- paste0(local_dir1)
load(paste0(local_dir,'wide_data_sparse.RData'))
results_table = NULL

chr_only = 1#set to either 1 or 0 
demo_vars = 0 #set to either 1 or 0 
cog_vars = 0 #set to either 1 or 0 
impute = 0 #haven't set this up yet
timepoints = 0

# predictors <- c( "Blocking Score (Kamin)", #positive
#                  "Change in d' (SWS)", #positive
#                  "Nu Evidence vs. Belief (CH)", #positive
#                  "Win-Switches (PRL)", # positive
#                  "Gain vs Loss Avoidance (GLAT)", #negative
#                  "High Effort, High Reward Scores (EEFRT)", #negative
#                  "Large Reward K (Delayed Discounting)",  #negative
#                  "Hedonic Reactivity (HR)", #negative
#                  "Finger Tapping Speed (CFT)", #negative
#                  "Context Sensitivity (Ebbinghaus)", #disorganized
#                  "Inverted Face Reports (Mooney Faces)" #disorganized
#                   )

predictors <- c( "alpha_ml_bl", #positive
                 "Speech Reports (SWS)", #positive
                 "Nu Evidence vs. Belief (CH)", #positive
                 "Win-Switches (PRL)", # positive
                 "Gain vs Loss (Acquisition)", #negative 
                 "High Effort, High Reward Scores (EEFRT)", #negative 
                 "Large Reward K (Delayed Discounting)",  #negative  
                 "Hedonic Reactivity (HR)", #negative
                 "Finger Tapping Speed (CFT)", #negative
                 "Context Sensitivity (Ebbinghaus)", #disorganized        
                 "Inverted Face Reports (Mooney Faces)", #disorganized 
                 "bacs_total_bl",
                 "hvlt_total_score_bl"
)

if (chr_only == 1){
  wide_df<-wide_df[wide_df$phenotype_four_groups_bl=='chr',]
}
if (demo_vars == 1){
  demo_predictors <- 
    c('demo_age_in_years',
      'demo_community_standing',
      'demo_national_standing'
      #'demo_gender_three_groups',
     #'demo_biological_sex', cannot include gender and sex in same model due to multicollinearity
      #'demo_race',
      #'demo_refugee_status'
       )
  predictors <- c(predictors, demo_predictors)
} else if (cog_vars == 1) {
  cog_predictors <- 
    c('bacs_total')
  predictors <- c(predictors, cog_predictors)
} 

dependents<- c("Baseline Social Functioning",
               "Baseline Role Functioning",
               "12m Social Functioning",
               "12m Role Functioning",
               "12m Change Social Functioning",
               "12m Change Role Functioning"
               )
if (timepoints == 0){
  tp_name = ''
  dependents<- c("Baseline Social Functioning",
                 "Baseline Role Functioning",
                 "12m Social Functioning",
                 "12m Role Functioning",
                 "12m Change Social Functioning",
                 "12m Change Role Functioning"
  )
} else {
  tp_name = '_timepoint'
  dependents<- c("Timepoint 1 Social Functioning",
                 "Timepoint 1 Role Functioning",
                 "Timepoint 2 Social Functioning",
                 "Timepoint 2 Role Functioning",
                 "Timepoint Change Social Functioning",
                 "Timepoint Change Role Functioning")
}
#need to look at bivariate correlation matrix of tasks and functioning first
if (chr_only){
  svg_name = 'corrplot_chr_only'
} else {
  svg_name = 'corrplot_all'
}

svg(paste0(local_dir,'../svgs/',svg_name,tp_name,'_rectangle.svg'), pointsize = 12)
vjp_rectangle_corrplot(wide_df[,predictors],wide_df[,dependents])
dev.off()

#now pick out individual correlations for reporting in the manuscript
pub_ready_stats(cor.test(wide_df$`Hedonic Reactivity (HR)` ,
                         wide_df$`Baseline Social Functioning`))
pub_ready_stats(cor.test(wide_df$prl_total_win_switches_bl,
                         wide_df$`Baseline Social Functioning`))
pub_ready_stats(cor.test(wide_df$`Large Reward K (Delayed Discounting)`,
                         wide_df$`Baseline Social Functioning`))
pub_ready_stats(cor.test(wide_df$`Finger Tapping Speed (CFT)`,
                         wide_df$`Baseline Social Functioning`))
pub_ready_stats(cor.test(wide_df$`Nu Evidence vs. Belief (CH)`,
                         wide_df$`Baseline Social Functioning`))


pub_ready_stats(cor.test(wide_df$`Large Reward K (Delayed Discounting)`,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$`Hedonic Reactivity (HR)` ,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$prl_total_win_switches_bl,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$LrgK_bl ,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$meantaps_dominant_bl ,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$`Inverted Face Reports (Mooney Faces)` ,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$`Nu Evidence vs. Belief (CH)` ,
                         wide_df$`Baseline Role Functioning`))
pub_ready_stats(cor.test(wide_df$`Context Sensitivity (Ebbinghaus)` ,
                         wide_df$`Baseline Role Functioning`))


pub_ready_stats(cor.test(wide_df$`Hedonic Reactivity (HR)` ,
                         wide_df$`12m Social Functioning` ))
pub_ready_stats(cor.test(wide_df$`Hedonic Reactivity (HR)` ,
                         wide_df$`12m Role Functioning` ))

pub_ready_stats(cor.test(wide_df$`Nu Evidence vs. Belief (CH)` ,
                         wide_df$`12m Change Social Functioning` ))
pub_ready_stats(cor.test(wide_df$`Inverted Face Reports (Mooney Faces)` ,
                         wide_df$`12m Change Role Functioning` ))
