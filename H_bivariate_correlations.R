library(Hmisc)
library(psych)
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir,'wide_data_sparse.RData'))
results_table = NULL

chr_only = 1#set to either 1 or 0 
demo_vars = 0 #set to either 1 or 0 
cog_vars = 0 #set to either 1 or 0 
impute = 0 #haven't set this up yet
timepoints = 0

predictors <- c(
  "Blocking Score (Kamin)",
  "Change in d' (SWS)",
  "Nu Evidence vs. Belief (CH)",
  "Win-Switches (PRL)",
  "Gain vs Loss (Acquisition)",
  "High Effort, High Reward Scores (EEFRT)",
  "Large Reward K (Delay Discounting)",
  "Hedonic Reactivity (HR)",
  "Finger Tapping Speed (CFT)",
  "Context Sensitivity (Ebbinghaus)",
  "Inverted Face Reports (Mooney Faces)"
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

# ---- residualized change scores ----
# residual of follow-up regressed on baseline; na.exclude keeps rows aligned
# (NA where either timepoint is missing)
resid_change <- function(df, followup, baseline){
  fit <- lm(df[[followup]] ~ df[[baseline]], na.action = na.exclude)
  as.numeric(residuals(fit))
}

if (timepoints == 0){
  tp_name   = ''
  t1        = 'Baseline'
  t2        = '12m'
  chg_label = '12m'
} else {
  tp_name   = '_timepoint'
  t1        = 'Timepoint 1'
  t2        = 'Timepoint 2'
  chg_label = 'Timepoint'
}

domains <- c('Social', 'Role')
for (d in domains){
  wide_df[[paste(chg_label, 'Residualized Change', d, 'Functioning')]] <-
    resid_change(wide_df,
                 followup = paste(t2, d, 'Functioning'),
                 baseline = paste(t1, d, 'Functioning'))
}

dependents <- c(paste(t1, domains, 'Functioning'),
                paste(t2, domains, 'Functioning'),
                paste(chg_label, 'Residualized Change', domains, 'Functioning'))

#need to look at bivariate correlation matrix of tasks and functioning first
if (chr_only){
  svg_name = 'corrplot_chr_only'
} else {
  svg_name = 'corrplot_all'
}

svg(paste0(local_dir,'../svgs/',svg_name,tp_name,'_resid_rectangle.svg'), pointsize = 11)
vjp_rectangle_corrplot(wide_df[,predictors],wide_df[,dependents],adjust = 'none'
                       #print_numbers = TRUE
                       )
dev.off()

# ---- intertask correlations ----
svg(paste0(local_dir,'../svgs/','intertask_cor_matrix.svg'), pointsize = 12)
vjp_rectangle_corrplot(wide_df[predictors], wide_df[predictors], adjust = 'none',
                       type = 'upper', diag = FALSE)
dev.off()

