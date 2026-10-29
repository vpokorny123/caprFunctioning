library(dplyr)

local_dir <- '/Users/victorpokorny/Library/CloudStorage/GoogleDrive-vpokorny123@gmail.com/My Drive/CAPR CHR Functioning/RData/'
load(paste0(local_dir, 'merged_data_sparse.RData'))
merged_df <- merged_df[merged_df$phenotype_four_groups == 'chr', ]

predictors <- c(
  "Blocking Score (Kamin)",
  "Change in d' (SWS)",
  "Nu Evidence vs. Belief (CH)",
  "Win-Switches (PRL)",
  "Gain vs Loss (Acquisition)",
  "High Effort, High Reward Scores (EEFRT)",
  "K (Delay Discounting)",
  "Hedonic Reactivity (HR)",
  "Finger Tapping Speed (CFT)",
  "Context Sensitivity (Ebbinghaus)",
  "Inverted Face Reports (Mooney Faces)"
)
functioning <- c("Social Functioning", "Role Functioning")

# Same sample as the main analysis: CHR with at least one baseline task score
bl <- merged_df %>%
  filter(visit == "bl") %>%
  filter(!if_all(all_of(predictors), is.na))
ids <- bl$src_subject_id
m12 <- merged_df %>% filter(visit == "12m", src_subject_id %in% ids)

skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
kurt <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^4) / sd(x)^4 - 3 }  # excess kurtosis

describe <- function(x) data.frame(
  N        = sum(!is.na(x)),
  Mean     = mean(x, na.rm = TRUE),
  SD       = sd(x, na.rm = TRUE),
  Median   = median(x, na.rm = TRUE),
  Min      = min(x, na.rm = TRUE),
  Max      = max(x, na.rm = TRUE),
  Skewness = skew(x),
  Kurtosis = kurt(x)
)

tab <- bind_rows(
  lapply(predictors,  function(v) cbind(Section = "Task index (baseline)", Measure = v, describe(bl[[v]]))),
  lapply(functioning, function(v) cbind(Section = "Functioning", Measure = paste(v, "— baseline"), describe(bl[[v]]))),
  lapply(functioning, function(v) cbind(Section = "Functioning", Measure = paste(v, "— 12 months"), describe(m12[[v]])))
) %>%
  mutate(across(Mean:Kurtosis, ~round(., 2)))

print(tab, row.names = FALSE)
dir.create("csvs", showWarnings = FALSE)
write.csv(tab, "csvs/table_descriptives.csv", row.names = FALSE)
cat("Saved: csvs/table_descriptives.csv\n")

# ---- APA-style Excel table (paste-ready) ----------------------------------------
library(openxlsx)

# Section label rows ("Task indices (baseline)", "Functioning") above their measures
out <- do.call(rbind, lapply(split(tab, factor(tab$Section, levels = unique(tab$Section))), function(d)
  rbind(data.frame(Measure = d$Section[1], N = NA, Mean = NA, SD = NA, Median = NA,
                   Min = NA, Max = NA, Skewness = NA, Kurtosis = NA),
        d[, -1])))
sec_rows <- which(is.na(out$N)) + 1          # +1 for header row
n_rows   <- nrow(out) + 1

wb <- createWorkbook()
addWorksheet(wb, "Descriptives", gridLines = FALSE)
writeData(wb, 1, out, headerStyle = createStyle(textDecoration = "bold", halign = "center",
                                                border = "TopBottom", borderStyle = "thin"))
base <- createStyle(fontName = "Times New Roman", fontSize = 11)
addStyle(wb, 1, base, rows = 1:n_rows, cols = 1:9, gridExpand = TRUE, stack = TRUE)
addStyle(wb, 1, createStyle(halign = "right", numFmt = "0.00"), rows = 2:n_rows, cols = 3:9, gridExpand = TRUE, stack = TRUE)
addStyle(wb, 1, createStyle(halign = "right", numFmt = "0"),    rows = 2:n_rows, cols = 2,   gridExpand = TRUE, stack = TRUE)
addStyle(wb, 1, createStyle(textDecoration = "italic"), rows = sec_rows, cols = 1, stack = TRUE)
addStyle(wb, 1, createStyle(indent = 1), rows = setdiff(2:n_rows, sec_rows), cols = 1, stack = TRUE)
addStyle(wb, 1, createStyle(border = "bottom", borderStyle = "thin"), rows = n_rows, cols = 1:9, stack = TRUE)
setColWidths(wb, 1, cols = 1, widths = 42)
setColWidths(wb, 1, cols = 2:9, widths = 9)
saveWorkbook(wb, "csvs/table_descriptives.xlsx", overwrite = TRUE)
cat("Saved: csvs/table_descriptives.xlsx\n")