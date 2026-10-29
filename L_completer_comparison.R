local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'wide_data_sparse.RData'))
set.seed(1)

df <- wide_df[wide_df$phenotype_four_groups_bl == 'chr', ]

df$completer <- rowSums(!is.na(df[, c('12m Social Functioning', '12m Role Functioning')])) > 0
table(df$completer)

cont_vars <- c('Baseline Social Functioning', 'Baseline Role Functioning',
               'demo_age_in_years_bl',
               "Blocking Score (Kamin)", "Change in d' (SWS)", "Nu Evidence vs. Belief (CH)",
               "Win-Switches (PRL)", "Gain vs Loss (Acquisition)",
               "High Effort, High Reward Scores (EEFRT)", "Large Reward K (Delayed Discounting)",
               "Hedonic Reactivity (HR)", "Finger Tapping Speed (CFT)",
               "Context Sensitivity (Ebbinghaus)", "Inverted Face Reports (Mooney Faces)")
cat_vars  <- c('demo_biological_sex_bl', 'demo_race_bl')
labels    <- c(demo_age_in_years_bl = 'Age (years)', demo_biological_sex_bl = 'Sex',
               demo_race_bl = 'Race')

missing <- setdiff(c(cont_vars, cat_vars), names(df))
if (length(missing)) message('Not in data, skipped: ', paste(missing, collapse = ', '))
cont_vars <- intersect(cont_vars, names(df))
cat_vars  <- intersect(cat_vars, names(df))

cont_res <- do.call(base::rbind, lapply(cont_vars, function(v) {
  x <- na.omit(df[[v]][df$completer]); y <- na.omit(df[[v]][!df$completer])
  n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  data.frame(variable = v, n = paste(n1, '/', n2),
             comp = sprintf('%.2f (%.2f)', mean(x), sd(x)),
             noncomp = sprintf('%.2f (%.2f)', mean(y), sd(y)),
             p = t.test(x, y)$p.value,
             effect = (mean(x) - mean(y)) / sp * (1 - 3 / (4 * (n1 + n2) - 9)),
             stat = 'g')
}))

cat_res <- do.call(base::rbind, lapply(cat_vars, function(v) {
  lvl <- gsub('_', ' ', sub('^demo_race_', '', as.character(df[[v]])))
  tab <- table(droplevels(as.factor(lvl)), df$completer)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  p   <- if (any(chi$expected < 5)) fisher.test(tab, simulate.p.value = TRUE)$p.value else chi$p.value
  pct <- function(col) paste0(tab[, col], ' (', round(100 * tab[, col] / sum(tab[, col])), '%)')
  # header row carries the test; one indented row per level carries n (%)
  rbind(data.frame(variable = v, n = paste(sum(tab[, 'TRUE']), '/', sum(tab[, 'FALSE'])),
                   comp = '', noncomp = '', p = p,
                   effect = unname(sqrt(chi$statistic / (sum(tab) * (min(dim(tab)) - 1)))),
                   stat = 'V'),
        data.frame(variable = paste0('   ', rownames(tab)), n = '',
                   comp = pct('TRUE'), noncomp = pct('FALSE'),
                   p = NA, effect = NA, stat = ''))
}))

results <- base::rbind(cont_res, cat_res)
results$p_fdr <- p.adjust(results$p, 'BH')

fmt_p <- function(p) ifelse(is.na(p), '', ifelse(p < .001, '<.001', sub('^0', '', sprintf('%.3f', p))))
results$effect_txt <- ifelse(is.na(results$effect), '', paste(results$stat, '=', sprintf('%.2f', results$effect)))
results <- data.frame(
  Variable          = ifelse(results$variable %in% names(labels), labels[results$variable], results$variable),
  `n (C / NC)`      = results$n,
  Completers        = results$comp,
  `Non-completers`  = results$noncomp,
  Effect            = results$effect_txt,
  p                 = fmt_p(results$p),
  `p (FDR)`         = fmt_p(results$p_fdr),
  check.names = FALSE)

print(results, row.names = FALSE)

# ---- Excel output ----
library(openxlsx)
is_level <- startsWith(results$Variable, ' ')
results$Variable <- trimws(results$Variable)
first <- 4; last <- first + nrow(results) - 1; nc <- ncol(results)

wb <- createWorkbook()
modifyBaseFont(wb, fontName = 'Times New Roman', fontSize = 11)
addWorksheet(wb, 'Supplemental Table 2', gridLines = FALSE)

writeData(wb, 1, sprintf('Supplemental Table 2. Baseline characteristics of completers (n = %d) and non-completers (n = %d)',
                         sum(df$completer), sum(!df$completer)), startRow = 1)
addStyle(wb, 1, createStyle(fontSize = 13, textDecoration = 'bold'), rows = 1, cols = 1)

writeData(wb, 1, results, startRow = first - 1,
          headerStyle = createStyle(textDecoration = 'bold', halign = 'center',
                                    border = 'TopBottom', borderStyle = 'medium'))
addStyle(wb, 1, createStyle(halign = 'left', textDecoration = 'bold', border = 'TopBottom',
                            borderStyle = 'medium'), rows = first - 1, cols = 1, stack = TRUE)
addStyle(wb, 1, createStyle(halign = 'center'), rows = first:last, cols = 2:nc, gridExpand = TRUE)
addStyle(wb, 1, createStyle(indent = 2, fontColour = '#444444'),
         rows = (first:last)[is_level], cols = 1)
addStyle(wb, 1, createStyle(border = 'bottom', borderStyle = 'medium'),
         rows = last, cols = 1:nc, stack = TRUE)

writeData(wb, 1, paste('Note. Values are M (SD) or n (%). n (C / NC) = completers / non-completers with data.',
                       "g = Hedges' g (completers minus non-completers); V = Cramer's V.",
                       "p from Welch t-tests or chi-square/Fisher's exact tests; FDR = Benjamini-Hochberg."),
          startRow = last + 2)
mergeCells(wb, 1, cols = 1:nc, rows = last + 2)
addStyle(wb, 1, createStyle(fontSize = 9, textDecoration = 'italic', wrapText = TRUE),
         rows = last + 2, cols = 1)
setRowHeights(wb, 1, rows = last + 2, heights = 30)

setColWidths(wb, 1, cols = 1:nc, widths = c(40, 12, 16, 16, 11, 8, 8))
freezePane(wb, 1, firstActiveRow = first)
saveWorkbook(wb, paste0(local_dir, '../Supplemental_Table_2.xlsx'), overwrite = TRUE)