
local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'wide_data_sparse.RData'))
library(ggplot2)

site_var <- 'site_bl' 
bl_outcomes  <- c('Baseline Social Functioning', 'Baseline Role Functioning')
outcome_rule <- 'any'

tasks <- c("Blocking Score (Kamin)", "Change in d' (SWS)", "Nu Evidence vs. Belief (CH)",
           "Win-Switches (PRL)", "Gain vs Loss (Acquisition)",
           "High Effort, High Reward Scores (EEFRT)", "Large Reward K (Delay Discounting)",
           "Hedonic Reactivity (HR)", "Finger Tapping Speed (CFT)",
           "Context Sensitivity (Ebbinghaus)", "Inverted Face Reports (Mooney Faces)")

chr <- wide_df[wide_df$phenotype_four_groups_bl %in% 'chr', ]
chr[] <- lapply(chr, function(x) { if (is.character(x)) x[trimws(x) == ''] <- NA; x })   # blank strings count as missing

n_outcomes <- rowSums(!is.na(chr[bl_outcomes]))
keep <- if (outcome_rule == 'all') n_outcomes == length(bl_outcomes) else n_outcomes > 0
df <- chr[keep, ]
cat('CHR N:', nrow(chr), '| with usable baseline outcome(s):', nrow(df),
    '| excluded:', sum(!keep), '\n')

any_task <- rowSums(!is.na(df[tasks])) > 0
df[['Any task data']] <- ifelse(any_task, 1, NA)
cat('Of these, with any task data:', sum(any_task), '| with no task data:', sum(!any_task), '\n')

sections <- list(
  'Functioning'  = c('12m Social Functioning', '12m Role Functioning'),
  'Demographics' = c('demo_age_in_years_bl', 'demo_biological_sex_bl', 'demo_race_bl'),
  'Task indices' = c('Any task data', tasks))
labels <- c(demo_age_in_years_bl = 'Age', demo_biological_sex_bl = 'Sex', demo_race_bl = 'Race',
            `Any task data` = 'Any task data (≥1 task)')

has_site <- !is.null(site_var)
site     <- if (has_site) as.character(df[[site_var]]) else rep('Overall', nrow(df))
site_n   <- table(site)
col_lab  <- c(if (has_site) setNames(sprintf('%s\n(n = %d)', names(site_n), site_n), names(site_n)),
              Overall = sprintf('Overall\n(N = %d)', nrow(df)))

long <- do.call(rbind, lapply(names(sections), function(s) {
  do.call(rbind, lapply(sections[[s]], function(v) {
    pct <- 100 * tapply(is.na(df[[v]]), site, mean)
    if (has_site) pct <- c(pct, Overall = 100 * mean(is.na(df[[v]])))
    data.frame(section = s, variable = ifelse(v %in% names(labels), labels[v], v),
               site = names(pct), pct = as.numeric(pct))
  }))
}))
long$section  <- factor(long$section, levels = names(sections))
long$variable <- factor(long$variable, levels = rev(unique(long$variable)))
long$site     <- factor(long$site, levels = c(sort(setdiff(unique(long$site), 'Overall')), 'Overall'))

sample_note <- sprintf(
  'Missingness is reported as percentage of subjects with usable baseline %s (N = %d of %d CHR).',
  paste(sub('^Baseline ', '', bl_outcomes), collapse = if (outcome_rule == 'all') ' and ' else ' or '),
  nrow(df), nrow(chr))

p <- ggplot(long, aes(site, variable, fill = pct)) +
  geom_tile(colour = 'white', linewidth = 0.6) +
  geom_text(aes(label = sprintf('%.0f%%', pct), colour = pct > 60),
            size = 3, family = 'Times New Roman', show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = 'black', `TRUE` = 'white')) +
  scale_fill_gradient(low = '#F7F7F7', high = '#B2182B', limits = c(0, 100),
                      name = '% missing') +
  scale_x_discrete(labels = col_lab, position = 'top') +
  facet_grid(section ~ ., scales = 'free_y', space = 'free_y', switch = 'y') +
  labs(x = NULL, y = NULL, caption = sample_note) +
  theme_minimal(base_family = 'Times New Roman', base_size = 11) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic', size = 10, colour = 'black'),
        plot.caption.position = 'plot',
        strip.placement = 'outside',
        strip.text.y.left = element_text(angle = 90, face = 'bold'),
        axis.text.y = element_text(colour = 'black'),
        axis.text.x = element_text(colour = 'black'))
if (has_site) p <- p + geom_vline(xintercept = length(site_n) + 0.5, linewidth = 0.8)

ggsave(paste0(local_dir, '../svgs/missingness_tiles_bl_outcome.svg'), p, device = svglite::svglite,
       width = 3.5 + 0.7 * length(col_lab), height = 1.8 + 0.28 * nlevels(long$variable))