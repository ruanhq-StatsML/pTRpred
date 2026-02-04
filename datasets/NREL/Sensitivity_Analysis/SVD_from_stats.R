# =============================================================================
# SVD analysis from battery_cell_SVD_whole_stats.csv only
# Uses current CSV + updated battery cell type categorization (synced with
# threshold_sensitivity.R / battery_categorization.png). Computes all metrics
# and draws all plots.
# =============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Battery category + type per battery_categorization.png (Summary Table) ----
# Category: Commercial (LFP/NMC/LCO), Soteria, Soteria (Control), Lab/Custom, Lab/Generic
assign_battery_category <- function(dataset_name) {
  n <- as.character(dataset_name)
  if (is.na(n) || n == "") return(NA_character_)
  n_lower <- tolower(n)
  if (grepl("soteria-control|sorteria-control|soetria-control", n_lower)) return("Soteria (Control)")
  if (grepl("soteria-scc|sorteria|soetria", n_lower) && !grepl("control", n_lower)) return("Soteria")
  if (grepl("lco-lp", n_lower)) return("Lab/Custom")
  if (grepl("oe-lfp|lfp10ah", n_lower)) return("Commercial (LFP)")
  if (grepl("oe-nmc|oe-10ahr-nmc|oe-10ah.*nmc", n_lower)) return("Commercial (NMC)")
  if (grepl("oe-lco", n_lower)) return("Commercial (LCO)")
  if (grepl("^500mah|^1500mah|^2000mah", n_lower)) return("Lab/Generic")
  NA_character_
}
# Battery type (chemistry) from same table: LFP, NMC, LCO, SO, Rec
assign_battery_type <- function(dataset_name) {
  n <- as.character(dataset_name)
  if (is.na(n) || n == "") return(NA_character_)
  n_lower <- tolower(n)
  if (grepl("soteria-control|sorteria-control|soetria-control", n_lower)) return("SO")       # Soteria (Control) - unspecified
  if (grepl("soteria-scc|sorteria|soetria", n_lower) && !grepl("control", n_lower)) return("SO") # Soteria - unspecified
  if (grepl("lco-lp", n_lower)) return("LCO")     # Lab/Custom LCO
  if (grepl("oe-lfp|lfp10ah", n_lower)) return("LFP")
  if (grepl("oe-nmc|oe-10ahr-nmc|oe-10ah.*nmc", n_lower)) return("NMC")
  if (grepl("oe-lco", n_lower)) return("LCO")
  if (grepl("^500mah|^1500mah|^2000mah", n_lower)) return("Rec")  # Lab/Generic - unspecified
  NA_character_
}

# ---- Safe conclusion assignment ----
safe_conclusion <- function(df, val_col, lab_col, concl_col, label_var = "Label") {
  df[[lab_col]] <- ifelse(df[[val_col]] >= 0.3, 1, 0)
  df[[concl_col]] <- NA_character_
  L <- df[[label_var]]; Lhat <- df[[lab_col]]
  idx_tp <- which((L == Lhat) & (L == 1) & (Lhat != 2))
  idx_tn <- which((L == Lhat) & (L == 0) & (Lhat != 2))
  idx_fn <- which((L != Lhat) & (L == 1) & (Lhat != 2))
  idx_fp <- which((L != Lhat) & (L == 0) & (Lhat != 2))
  if (length(idx_tp) > 0) df[idx_tp, concl_col] <- "TP"
  if (length(idx_tn) > 0) df[idx_tn, concl_col] <- "TN"
  if (length(idx_fn) > 0) df[idx_fn, concl_col] <- "FN"
  if (length(idx_fp) > 0) df[idx_fp, concl_col] <- "FP"
  df
}

# ---- Load data (current battery_cell_SVD_whole_stats.csv) ----
df0 <- read.csv("battery_cell_SVD_whole_stats.csv")
if (!"dataset_name" %in% names(df0) && ncol(df0) >= 2) names(df0)[2] <- "dataset_name"
# Apply battery category and type from battery_categorization.png (Summary Table)
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)
df0$battery <- sapply(df0$dataset_name, assign_battery_type)
label_var <- if ("label" %in% names(df0)) "label" else "Label"

# ---- Compute conclusions for all algorithms ----
# 10_125 (use Label)
df0$label_10_125 <- ifelse(df0$X10_125 >= 0.3, 1, 0)
df0$conclusion_10_125 <- NA_character_
L <- df0[[label_var]]; Lhat <- df0$label_10_125
idx_tp <- which((L == Lhat) & (L == 1) & (Lhat != 2))
idx_tn <- which((L == Lhat) & (L == 0) & (Lhat != 2))
idx_fn <- which((L != Lhat) & (L == 1) & (Lhat != 2))
idx_fp <- which((L != Lhat) & (L == 0) & (Lhat != 2))
if (length(idx_tp) > 0) df0[idx_tp, "conclusion_10_125"] <- "TP"
if (length(idx_tn) > 0) df0[idx_tn, "conclusion_10_125"] <- "TN"
if (length(idx_fn) > 0) df0[idx_fn, "conclusion_10_125"] <- "FN"
if (length(idx_fp) > 0) df0[idx_fp, "conclusion_10_125"] <- "FP"

df0 <- safe_conclusion(df0, "X10_60",  "label_10_60",  "conclusion_10_60")
df0 <- safe_conclusion(df0, "X10_75",  "label_10_75",  "conclusion_10_75")
df0 <- safe_conclusion(df0, "X10_150", "label_10_150", "conclusion_10_150")
df0 <- safe_conclusion(df0, "X40_60",  "label_40_60",  "conclusion_40_60")
df0 <- safe_conclusion(df0, "X40_75",  "label_40_75",  "conclusion_40_75")
df0 <- safe_conclusion(df0, "X40_100", "label_40_100", "conclusion_40_100")
df0 <- safe_conclusion(df0, "X40_125", "label_40_125", "conclusion_40_125")
df0 <- safe_conclusion(df0, "X40_150", "label_40_150", "conclusion_40_150")
df0 <- safe_conclusion(df0, "X30_75",  "label_30_75",  "conclusion_30_75")
df0 <- safe_conclusion(df0, "X30_100", "label_30_100", "conclusion_30_100")
df0 <- safe_conclusion(df0, "X30_125", "label_30_125", "conclusion_30_125")
df0 <- safe_conclusion(df0, "X30_150", "label_30_150", "conclusion_30_150")

# ---- Accuracy summary (overall) ----
conclusion_cols <- grep("^conclusion_[0-9]+_[0-9]+$", names(df0), value = TRUE)
accuracy_list <- lapply(conclusion_cols, function(col) {
  vec <- df0[[col]]
  tp <- sum(vec == "TP", na.rm = TRUE)
  tn <- sum(vec == "TN", na.rm = TRUE)
  fp <- sum(vec == "FP", na.rm = TRUE)
  fn <- sum(vec == "FN", na.rm = TRUE)
  total <- tp + tn + fp + fn
  accuracy <- if (total > 0) (tp + tn) / total else NA_real_
  data.frame(algorithm = col, TP = tp, TN = tn, FP = fp, FN = fn, total = total, accuracy = round(accuracy, 4))
})
accuracy_summary <- bind_rows(accuracy_list)
write.csv(accuracy_summary, "SVD_accuracy_summary.csv", row.names = FALSE)
print("SVD Accuracy summary (overall):")
print(accuracy_summary)

# ---- Accuracy by battery_category ----
accuracy_by_cat <- lapply(conclusion_cols, function(col) {
  df0 %>%
    filter(.data[[col]] %in% c("TP", "TN", "FP", "FN")) %>%
    group_by(battery_category) %>%
    summarize(
      TP = sum(.data[[col]] == "TP", na.rm = TRUE),
      TN = sum(.data[[col]] == "TN", na.rm = TRUE),
      FP = sum(.data[[col]] == "FP", na.rm = TRUE),
      FN = sum(.data[[col]] == "FN", na.rm = TRUE),
      total = n(),
      accuracy = round((TP + TN) / total, 4),
      .groups = "drop"
    ) %>%
    mutate(algorithm = col)
})
accuracy_by_category <- bind_rows(accuracy_by_cat) %>% select(algorithm, battery_category, TP, TN, FP, FN, total, accuracy)
write.csv(accuracy_by_category, "SVD_accuracy_by_category.csv", row.names = FALSE)
print("SVD Accuracy by battery category (first 15 rows):")
print(head(accuracy_by_category, 15))

# ---- Summary by battery (chemistry) -> CSV ----
by_battery_list <- lapply(conclusion_cols, function(col) {
  df0 %>% group_by(battery) %>%
    summarize(
      TP = sum(.data[[col]] == "TP", na.rm = TRUE),
      TN = sum(.data[[col]] == "TN", na.rm = TRUE),
      FP = sum(.data[[col]] == "FP", na.rm = TRUE),
      FN = sum(.data[[col]] == "FN", na.rm = TRUE),
      total = n(),
      .groups = "drop"
    ) %>% mutate(algorithm = col)
})
by_battery <- bind_rows(by_battery_list)
write.csv(by_battery, "SVD_accuracy_by_battery_chemistry.csv", row.names = FALSE)

# ---- Summary by battery_category (positive / negative / unknown counts) ----
summary_cat <- df0 %>%
  group_by(battery_category) %>%
  summarize(
    n_positive = sum(.data[[label_var]] == 1, na.rm = TRUE),
    n_negative = sum(.data[[label_var]] == 0, na.rm = TRUE),
    n_unknown = sum(.data[[label_var]] == 2 | is.na(.data[[label_var]]), na.rm = TRUE),
    n_total = n(),
    .groups = "drop"
  )
write.csv(summary_cat, "SVD_summary_by_battery_category.csv", row.names = FALSE)

# ---- Build long-format data for plots ----
build_vis_df <- function(df, window_lower, label_col) {
  prefix <- paste0("X", window_lower, "_")
  cols <- grep(paste0("^", prefix, "[0-9]+$"), names(df), value = TRUE)
  if (length(cols) == 0) return(NULL)
  thresholds <- as.numeric(sub(prefix, "", cols))
  L <- df[[label_col]]
  out <- lapply(seq_along(cols), function(i) {
    data.frame(
      dataset_name = df$dataset_name,
      Label = L,
      battery_category = df$battery_category,
      battery = df$battery,
      threshold = thresholds[i],
      value = df[[cols[i]]]
    )
  })
  d <- bind_rows(out)
  d$label <- ifelse(d$Label == 1, "Positive", ifelse(d$Label == 2, "Unknown", "Negative"))
  d
}

# ---- Plots: by battery_category (SVD) ----
for (win in c(10, 30, 40)) {
  vis <- build_vis_df(df0, win, label_var)
  if (is.null(vis)) next
  cats <- unique(na.omit(vis$battery_category))
  for (cat in cats) {
    sub <- vis[vis$battery_category == cat, ]
    if (nrow(sub) < 1) next
    sub$threshold <- factor(sub$threshold, levels = sort(unique(sub$threshold)))
    fname <- gsub("[^a-zA-Z0-9]", "_", cat)
    fname <- paste0("SVD_threshold_window", win, "_", fname, ".png")
    png(fname, width = 600, height = 750)
    p <- ggplot(sub, aes(x = threshold, y = value, color = factor(label))) +
      geom_jitter(size = 2.5) +
      scale_color_manual(values = c("Negative" = "orange", "Positive" = "steelblue", "Unknown" = "firebrick"), name = "Label") +
      labs(x = "Maximal Window Size", y = "Maximal Detected Value",
           title = paste0("SVD: Max Detected Value vs Threshold\nWindow ", win, " — ", cat)) +
      geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) +
      theme(plot.title = element_text(size = 22), axis.text = element_text(size = 20),
            axis.title = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 20),
            legend.key.size = unit(1, "cm"))
    print(p)
    dev.off()
  }
}

# ---- Plots: by battery (chemistry) — all types present in data for 10, 30, 40 ----
battery_types <- unique(na.omit(as.character(df0$battery)))
if (length(battery_types) == 0) battery_types <- c("Rec", "LFP", "LCO", "NMC", "SO")
for (win in c(10, 30, 40)) {
  vis <- build_vis_df(df0, win, label_var)
  if (is.null(vis)) next
  for (bt in battery_types) {
    sub <- vis[vis$battery == bt, ]
    if (nrow(sub) < 1) next
    sub$threshold <- factor(sub$threshold, levels = sort(unique(sub$threshold)))
    title_bt <- if (bt == "Rec") "Generic Lab" else bt
    fname <- paste0("SVD_threshold_window", win, "_", bt, ".png")
    png(fname, width = 600, height = 750)
    p <- ggplot(sub, aes(x = threshold, y = value, color = factor(label))) +
      geom_jitter(size = 2.5) +
      scale_color_manual(values = c("Negative" = "orange", "Positive" = "steelblue", "Unknown" = "firebrick"), name = "Label") +
      labs(x = "Maximal Window Size", y = "Maximal Detected Value",
           title = paste0("SVD: Max Detected Value vs Threshold\nWindow ", win, " — ", title_bt)) +
      geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) +
      theme(plot.title = element_text(size = 22), axis.text = element_text(size = 20),
            axis.title = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 20),
            legend.key.size = unit(1, "cm"))
    print(p)
    dev.off()
  }
}

message("Done. Outputs: SVD_accuracy_summary.csv, SVD_accuracy_by_category.csv, SVD_accuracy_by_battery_chemistry.csv, SVD_summary_by_battery_category.csv, and PNG plots.")
