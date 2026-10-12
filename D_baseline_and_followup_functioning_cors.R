
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir,'wide_data_sparse.RData'))
results_table = NULL

wide_df <- wide_df[wide_df$phenotype_four_groups_bl == 'chr', ]



#now pick out individual correlations for reporting in the manuscript
pub_ready_stats(cor.test(wide_df$gfr_current_bl,
                         wide_df$gfr_current_12m))
pub_ready_stats(cor.test(wide_df$gfs_current_bl,
                         wide_df$gfs_current_12m))

pub_ready_stats(cor.test(wide_df$gfr_current_bl,
                         wide_df$gfs_current_bl))
pub_ready_stats(cor.test(wide_df$gfs_current_12m,
                         wide_df$gfr_current_12m))
