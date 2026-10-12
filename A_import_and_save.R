#Primary measures: 
#Kamin Blocking (Control and Blocking scores) -> kamin and social_kamin
#Probabilistic Reversal Learning (lose-stay rate and win-switch rate) -> prl 
#Sine Wave Speech (d’ and c indices) -> sws
#Conditioned Hallucinations (prior influence and belief volatility rate indices) ->ch
#Gain vs Loss Avoidance (learning and transfer indices) -> pessiglione
#Effort Expenditure for Rewards (hard index) -> eefrt
#Delay Discounting (large reward trial performance) -> dd
#Hedonic Reactivity (positive index) -> iaps
#Computerized Finger Tapping Task (speed index) -> speed_tap
#Ebbinghaus Illusion (context sensitivity index) -> ebbinghaus 
#Mooney Faces (Upright and Inverted scores) -> mooney
#Global Functioning: Social and Role scores ->  gfr gfs


# THIS NEEDS TO BE RUN IN dataWizardry_back_up Project
library(wizaRdry)


local_dir<- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'

#-------------------------------------------------------------------------------
#I used to run this in a big for loop, but it takes so long to pull data I think it
#will be better to run separately. Esp. since we're going to have to go back and fix
#clean scripts and it will be faster to request only the dfs we need. 
#Plus you kinda have to check if there are any warnings and such.
#so for loop is worse for this case despite how ugly the code looks.

#---------------Let's do tasks first ------------------------
clean('kamin')
save('kamin_clean',
     file = paste0(local_dir,'kamin_clean.RData'))

clean('prl')
save('prl_clean',
     file = paste0(local_dir,'prl_clean.RData'))

clean('sws')
save('sws_clean',
     file = paste0(local_dir,'sws_clean.RData'))

clean('eefrt')
save('eefrt_clean',
     file = paste0(local_dir,'eefrt_clean.RData'))

clean('dd')
save('dd_clean',
     file = paste0(local_dir,'dd_clean.RData'))

clean('iaps')
save('iaps_clean',
     file = paste0(local_dir,'iaps_clean.RData'))

clean('speed_tap')
save('speed_tap_clean',
     file = paste0(local_dir,'speed_tap_clean.RData'))

clean('ebbinghaus')
save('ebbinghaus_clean',
     file = paste0(local_dir,'ebbinghaus_clean.RData'))

clean('mooney_vjp')
save('mooney_vjp_clean',
     file = paste0(local_dir,'mooney_vjp_clean.RData'))

load_dir <- './sites/nu/Data Requests/Externally Processed Data/'
load(paste0(load_dir, 'glat_clean.rda'))
save('glat_clean',
     file = paste0(local_dir,'glat_clean.RData'))
load(paste0(load_dir, 'ch_clean.rda'))
save('ch_clean',
     file = paste0(local_dir,'ch_clean.RData'))
load(paste0(load_dir, 'kamin_model_clean.rda'))
save('kamin_model_clean',
     file = paste0(local_dir,'kamin_model_clean.RData'))


#---------------Next let's get redcap ------------------------

clean('gfs')
save('gfs_clean',
     file = paste0(local_dir,'gfs_clean.RData'))

clean('gfr')
save('gfr_clean',
     file = paste0(local_dir,'gfr_clean.RData'))

clean('bacs')
save('bacs_clean',
     file = paste0(local_dir,'bacs_clean.RData'))

clean('hvlt')
save('hvlt_clean',
     file = paste0(local_dir,'hvlt_clean.RData'))

clean('sps')
save('sps_clean',
     file = paste0(local_dir,'sps_clean.RData'))

clean('wrat')
save('wrat_clean',
     file = paste0(local_dir,'wrat_clean.RData'))

clean('sips_p')
save('sips_p_clean',
     file = paste0(local_dir,'sips_p_clean.RData'))

clean('sips_n')
save('sips_n_clean',
     file = paste0(local_dir,'sips_n_clean.RData'))

clean('nsipr')
save('nsipr_clean',
     file = paste0(local_dir,'nsipr_clean.RData'))

clean('sips_scoresheet')
save('sips_scoresheet_clean',
     file = paste0(local_dir,'sips_scoresheet_clean.RData'))

clean('nda_study_intake')
save('nda_study_intake_clean',
     file = paste0(local_dir,'nda_study_intake_clean.RData'))

#--------------and finally qualtrics -------------------------
clean('demo')
save('demo_clean',
     file = paste0(local_dir,'demo_clean.RData'))

clean('demographics_overview')
save('demographics_overview_clean',
     file = paste0(local_dir,'demographics_overview_clean.RData'))

clean('cesd')
save('cesd_clean',
     file = paste0(local_dir,'cesd_clean.RData'))



