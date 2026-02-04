library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Battery recategorization per Summary Table (battery_categorization.png) ----
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

# Safe conclusion assignment (avoids 0-row and NA subscript errors)
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

if (file.exists("dataset_correspondence_information.xlsx")) {
  battery_cell <- read_xlsx("dataset_correspondence_information.xlsx")
  battery_cell <- battery_cell[, c(1:min(3, ncol(battery_cell)))]
} else if (file.exists("dataset_correspondence.xlsx")) {
  battery_cell <- read_xlsx("dataset_correspondence.xlsx")
  battery_cell <- battery_cell[, c(1:min(3, ncol(battery_cell)))]
}

df0 <- read.csv("battery_class_results.csv")
# Ensure first column is dataset_name (some reads get X...dataset_name)
if (!"dataset_name" %in% names(df0) && ncol(df0) >= 1) names(df0)[1] <- "dataset_name"
df0 <- df0[1:min(118, nrow(df0)), ]
df0 <- df0[!is.na(df0$dataset_name) & as.character(df0$dataset_name) != "", ]
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)

df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()

df0 %>% group_by(battery_category) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()

df1 <- read.csv("10_60_detected_result_whole copy.csv")
df2 <- read.csv("10_75_detected_result_whole copy.csv")
df3 <- read.csv("10_100_detected_result_whole copy.csv")
df4 <- read.csv("10_125_detected_result_whole copy.csv")
df5 <- read.csv("10_150_detected_result_whole copy.csv")
df6 <- read.csv("40_60_detected_result_whole copy.csv")
df7 <- read.csv("40_75_detected_result_whole copy.csv")
df8 <- read.csv("40_100_detected_result_whole copy.csv")
df9 <- read.csv("40_125_detected_result_whole copy.csv")
df10 <- read.csv("40_150_detected_result_whole copy.csv")
df0 <- merge(df0, df1, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df2, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df3, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df4, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df5, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df6, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df7, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df8, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df9, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df10, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
drops <- c("X.y", "X.x")
df0 <- df0[ , !(names(df0) %in% drops)]
# Safe conclusion assignment (avoids 0-row replacement error)
lab_var_10_125 <- if ("label" %in% names(df0)) "label" else "Label"
df0$'label_10_125' <- ifelse(df0$`X10_125` >= 0.3, 1, 0)
df0$'conclusion_10_125' <- NA_character_
L <- df0[[lab_var_10_125]]; Lhat <- df0$label_10_125
idx_tp <- which((L == Lhat) & (L == 1) & (Lhat != 2))
idx_tn <- which((L == Lhat) & (L == 0) & (Lhat != 2))
idx_fn <- which((L != Lhat) & (L == 1) & (Lhat != 2))
idx_fp <- which((L != Lhat) & (L == 0) & (Lhat != 2))
if (length(idx_tp) > 0) df0[idx_tp, "conclusion_10_125"] <- "TP"
if (length(idx_tn) > 0) df0[idx_tn, "conclusion_10_125"] <- "TN"
if (length(idx_fn) > 0) df0[idx_fn, "conclusion_10_125"] <- "FN"
if (length(idx_fp) > 0) df0[idx_fp, "conclusion_10_125"] <- "FP"
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X10_60", "label_10_60", "conclusion_10_60")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X10_75", "label_10_75", "conclusion_10_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X10_150", "label_10_150", "conclusion_10_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_60", "label_40_60", "conclusion_40_60")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_75", "label_40_75", "conclusion_40_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_100", "label_40_100", "conclusion_40_100")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_125", "label_40_125", "conclusion_40_125")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_150", "label_40_150", "conclusion_40_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_150", "label_40_150", "conclusion_40_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df11 <- read.csv("30_75_detected_result_whole copy.csv")
df12 <- read.csv("30_100_detected_result_whole copy.csv")
df13 <- read.csv("30_125_detected_result_whole copy.csv")
df14 <- read.csv("30_150_detected_result_whole copy.csv")
df0 <- merge(df0, df11, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df12, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df13, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df14, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)

df0 <- safe_conclusion(df0, "X30_75", "label_30_75", "conclusion_30_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0 <- safe_conclusion(df0, "X30_100", "label_30_100", "conclusion_30_100")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()


df0 <- safe_conclusion(df0, "X30_125", "label_30_125", "conclusion_30_125")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X30_150", "label_30_150", "conclusion_30_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

# Re-add battery_category after merges (in case it was dropped)
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)

# ---- Accuracy summary statistics for all algorithms ----
conclusion_cols <- grep("^conclusion_[0-9]+_[0-9]+$", names(df0), value = TRUE)
accuracy_list <- lapply(conclusion_cols, function(col) {
  vec <- df0[[col]]
  valid <- vec %in% c("TP", "TN", "FP", "FN")
  tp <- sum(vec == "TP", na.rm = TRUE)
  tn <- sum(vec == "TN", na.rm = TRUE)
  fp <- sum(vec == "FP", na.rm = TRUE)
  fn <- sum(vec == "FN", na.rm = TRUE)
  total <- tp + tn + fp + fn
  accuracy <- if (total > 0) (tp + tn) / total else NA_real_
  data.frame(
    algorithm = col,
    TP = tp, TN = tn, FP = fp, FN = fn,
    total = total,
    accuracy = round(accuracy, 4)
  )
})
accuracy_summary <- bind_rows(accuracy_list)
write.csv(accuracy_summary, "threshold_sensitivity_accuracy_summary.csv", row.names = FALSE)
print("Accuracy summary (overall):")
print(accuracy_summary)

# Accuracy by battery_category
accuracy_by_cat_list <- lapply(conclusion_cols, function(col) {
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
accuracy_by_category <- bind_rows(accuracy_by_cat_list) %>%
  select(algorithm, battery_category, TP, TN, FP, FN, total, accuracy)
write.csv(accuracy_by_category, "threshold_sensitivity_accuracy_by_category.csv", row.names = FALSE)
print("Accuracy by battery category (first few rows):")
print(head(accuracy_by_category, 20))

# ---- Threshold sensitivity plots by battery_category (from df0) ----
# Build long-format data: window_lower 10/30/40 x threshold 60/75/100/125/150
build_vis_df <- function(df, window_lower) {
  prefix <- paste0("X", window_lower, "_")
  cols <- grep(paste0("^", prefix, "[0-9]+$"), names(df), value = TRUE)
  if (length(cols) == 0) return(NULL)
  thresholds <- as.numeric(sub(prefix, "", cols))
  out <- lapply(seq_along(cols), function(i) {
    data.frame(
      dataset_name = df$dataset_name,
      Label = df$Label,
      battery_category = df$battery_category,
      threshold = thresholds[i],
      value = df[[cols[i]]]
    )
  })
  d <- bind_rows(out)
  d$label <- ifelse(d$Label == 1, "Positive", ifelse(d$Label == 2, "Unknown", "Negative"))
  d
}
for (win in c(10, 30, 40)) {
  vis <- build_vis_df(df0, win)
  if (is.null(vis)) next
  cats <- unique(na.omit(vis$battery_category))
  for (cat in cats) {
    sub <- vis[vis$battery_category == cat, ]
    if (nrow(sub) < 1) next
    sub$threshold <- factor(sub$threshold, levels = sort(unique(sub$threshold)))
    fname <- gsub("[^a-zA-Z0-9]", "_", cat)
    fname <- paste0("thresholding_sensitivity_windowlower_", win, "_", fname, ".png")
    png(fname, width = 600, height = 750)
    p <- ggplot(sub, aes(x = threshold, y = value, color = factor(label))) +
      geom_jitter(size = 2.5) +
      scale_color_manual(values = c("Negative" = "orange", "Positive" = "steelblue", "Unknown" = "firebrick"), name = "Label") +
      labs(x = "Maximal Window Size", y = "Maximal Detected Value",
           title = paste0("Maximal Detected Value vs. Threshold\n- Lower Window Size ", win, " ", cat)) +
      geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) +
      theme(plot.title = element_text(size = 22), axis.text = element_text(size = 20),
            axis.title = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 20),
            legend.key.size = unit(1, "cm"))
    print(p)
    dev.off()
  }
}

# Optional: original chemistry-based plots if vis CSVs exist
#policy agent:
#Groundedness, Precision -> does the response directly and accurately answer the user's guide?
#response directly and accurately answer the users' guide?
#Precision, Groundedness.
#score_groundedness and score_precision: 0.75, 0.6
if (file.exists("LCO_vis_start10.csv")) {
df1 <- read.csv("LCO_vis_start10.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_10_LCO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 10 LCO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("LFP_vis_start10.csv")) {
df1 <- read.csv("LFP_vis_start10.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_10_LFP.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 10 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("Rec_vis_start10.csv")) {
df1 <- read.csv("Rec_vis_start10.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_10_Generic.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 10 Generic Lab Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("SO_vis_start10.csv")) {
df1 <- read.csv("SO_vis_start10.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_10_SO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 10 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("NMC_vis_start10.csv")) {
df1 <- read.csv("NMC_vis_start10.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_10_NMC.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 10 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("LCO_vis_start30.csv")) {
df1 <- read.csv("LCO_vis_start30.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_30_LCO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 30 LCO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
    theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("LFP_vis_start30.csv")) {
df1 <- read.csv("LFP_vis_start30.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_30_LFP.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 30 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
    theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("Rec_vis_start30.csv")) {
df1 <- read.csv("Rec_vis_start30.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_30_Generic.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 30 Generic Lab Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
    theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("SO_vis_start30.csv")) {
df1 <- read.csv("SO_vis_start30.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_30_SO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 30 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("NMC_vis_start40.csv")) {
df1 <- read.csv("NMC_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_NMC.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("LCO_vis_start40.csv")) {
df1 <- read.csv("LCO_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_LCO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 LCO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("LFP_vis_start40.csv")) {
df1 <- read.csv("LFP_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_LFP.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("Rec_vis_start40.csv")) {
df1 <- read.csv("Rec_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_Generic.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 Generic Labs Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("SO_vis_start40.csv")) {
df1 <- read.csv("SO_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_SO.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}

if (file.exists("NMC_vis_start40.csv")) {
df1 <- read.csv("NMC_vis_start40.csv")
df1$label <- ifelse(df1$label == 1, "Positive", "Negative")
df1$battery_type = paste(df1$label, df1$battery_type, sep = "_")
png("thresholding_sensitivity_windowlower_40_NMC.png", width = 600, height = 750)
ggplot(df1, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold \n - Lower Window Size 40 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}





# ---- SVD-based procedure (wholeSVD.csv): replicate full analysis ----
#distribution shift attribution (run only if SVD result CSVs exist):
if (file.exists("10_60_detected_result_wholeSVD.csv")) {
library(readxl)
if (file.exists("dataset_correspondence_information.xlsx")) {
  battery_cell <- read_xlsx("dataset_correspondence_information.xlsx")
} else if (file.exists("dataset_correspondence.xlsx")) {
  battery_cell <- read_xlsx("dataset_correspondence.xlsx")
} else {
  battery_cell <- data.frame()
}
if (nrow(battery_cell) > 0) battery_cell <- battery_cell[, c(1:min(3, ncol(battery_cell)))]
df0 <- read.csv("battery_class_results.csv")
if (!"dataset_name" %in% names(df0) && ncol(df0) >= 1) names(df0)[1] <- "dataset_name"
df0 <- df0[1:min(118, nrow(df0)), ]
df0 <- df0[!is.na(df0$dataset_name) & as.character(df0$dataset_name) != "", ]
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()
df0 %>% group_by(battery_category) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()

df1 <- read.csv("10_60_detected_result_wholeSVD.csv")
df2 <- read.csv("10_75_detected_result_wholeSVD.csv")
df3 <- read.csv("10_100_detected_result_wholeSVD.csv")
df4 <- read.csv("10_125_detected_result_wholeSVD.csv")
df5 <- read.csv("10_150_detected_result_wholeSVD.csv")
df6 <- read.csv("30_60_detected_result_wholeSVD.csv")
df7 <- read.csv("30_75_detected_result_wholeSVD.csv")
df8 <- read.csv("30_100_detected_result_wholeSVD.csv")
df9 <- read.csv("30_125_detected_result_wholeSVD.csv")
df10 <- read.csv("30_150_detected_result_wholeSVD.csv")
df11 <- read.csv("40_60_detected_result_wholeSVD.csv")
df12 <- read.csv("40_75_detected_result_wholeSVD.csv")
df13 <- read.csv("40_100_detected_result_wholeSVD.csv")
df14 <- read.csv("40_125_detected_result_wholeSVD.csv")
df15 <- read.csv("40_150_detected_result_wholeSVD.csv")

df0 <- merge(df0, df1, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df2, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df3, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df4, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df5, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df6, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df7, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df8, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df9, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df10, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df11, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df12, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df13, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df14, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
df0 <- merge(df0, df15, by.x = "dataset_name", by.y = "dataset", all.x = TRUE)
drops <- c("X.y", "X.x", "X")
df0 <- df0[ , !(names(df0) %in% drops)]
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)
write.csv(df0, "battery_cell_SVD_whole_stats.csv")
df0 <- read.csv("battery_cell_SVD_whole_stats.csv")
df0$battery_category <- sapply(df0$dataset_name, assign_battery_category)

# Safe conclusion assignment for SVD (use Label)
lab_var_svd <- if ("label" %in% names(df0)) "label" else "Label"
df0$'label_10_125' <- ifelse(df0$`X10_125` >= 0.3, 1, 0)
df0$'conclusion_10_125' <- NA_character_
L <- df0[[lab_var_svd]]; Lhat <- df0$label_10_125
idx_tp <- which((L == Lhat) & (L == 1) & (Lhat != 2))
idx_tn <- which((L == Lhat) & (L == 0) & (Lhat != 2))
idx_fn <- which((L != Lhat) & (L == 1) & (Lhat != 2))
idx_fp <- which((L != Lhat) & (L == 0) & (Lhat != 2))
if (length(idx_tp) > 0) df0[idx_tp, "conclusion_10_125"] <- "TP"
if (length(idx_tn) > 0) df0[idx_tn, "conclusion_10_125"] <- "TN"
if (length(idx_fn) > 0) df0[idx_fn, "conclusion_10_125"] <- "FN"
if (length(idx_fp) > 0) df0[idx_fp, "conclusion_10_125"] <- "FP"
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X10_60", "label_10_60", "conclusion_10_60")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X10_75", "label_10_75", "conclusion_10_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0 <- safe_conclusion(df0, "X10_150", "label_10_150", "conclusion_10_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_60", "label_40_60", "conclusion_40_60")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_75", "label_40_75", "conclusion_40_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_100", "label_40_100", "conclusion_40_100")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_125", "label_40_125", "conclusion_40_125")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_150", "label_40_150", "conclusion_40_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X40_150", "label_40_150", "conclusion_40_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X30_75", "label_30_75", "conclusion_30_75")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0 <- safe_conclusion(df0, "X30_100", "label_30_100", "conclusion_30_100")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()


df0 <- safe_conclusion(df0, "X30_125", "label_30_125", "conclusion_30_125")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0 <- safe_conclusion(df0, "X30_150", "label_30_150", "conclusion_30_150")
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

# ---- SVD: Accuracy summary statistics (replicate main analysis) ----
conclusion_cols_svd <- grep("^conclusion_[0-9]+_[0-9]+$", names(df0), value = TRUE)
accuracy_list_svd <- lapply(conclusion_cols_svd, function(col) {
  vec <- df0[[col]]
  valid <- vec %in% c("TP", "TN", "FP", "FN")
  tp <- sum(vec == "TP", na.rm = TRUE)
  tn <- sum(vec == "TN", na.rm = TRUE)
  fp <- sum(vec == "FP", na.rm = TRUE)
  fn <- sum(vec == "FN", na.rm = TRUE)
  total <- tp + tn + fp + fn
  accuracy <- if (total > 0) (tp + tn) / total else NA_real_
  data.frame(
    algorithm = col,
    TP = tp, TN = tn, FP = fp, FN = fn,
    total = total,
    accuracy = round(accuracy, 4)
  )
})
accuracy_summary_svd <- bind_rows(accuracy_list_svd)
write.csv(accuracy_summary_svd, "threshold_sensitivity_accuracy_summary_SVD.csv", row.names = FALSE)
print("SVD Accuracy summary (overall):")
print(accuracy_summary_svd)

accuracy_by_cat_svd <- lapply(conclusion_cols_svd, function(col) {
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
accuracy_by_category_svd <- bind_rows(accuracy_by_cat_svd) %>%
  select(algorithm, battery_category, TP, TN, FP, FN, total, accuracy)
write.csv(accuracy_by_category_svd, "threshold_sensitivity_accuracy_by_category_SVD.csv", row.names = FALSE)
print("SVD Accuracy by battery category (first few rows):")
print(head(accuracy_by_category_svd, 20))

# ---- SVD: Threshold sensitivity plots by battery_category (replicate main) ----
for (win in c(10, 30, 40)) {
  vis_svd <- build_vis_df(df0, win)
  if (is.null(vis_svd)) next
  cats_svd <- unique(na.omit(vis_svd$battery_category))
  for (cat in cats_svd) {
    sub <- vis_svd[vis_svd$battery_category == cat, ]
    if (nrow(sub) < 1) next
    sub$threshold <- factor(sub$threshold, levels = sort(unique(sub$threshold)))
    fname <- gsub("[^a-zA-Z0-9]", "_", cat)
    fname <- paste0("thresholding_sensitivity_windowlower_", win, "_", fname, "_SVD.png")
    png(fname, width = 600, height = 750)
    p <- ggplot(sub, aes(x = threshold, y = value, color = factor(label))) +
      geom_jitter(size = 2.5) +
      scale_color_manual(values = c("Negative" = "orange", "Positive" = "steelblue", "Unknown" = "firebrick"), name = "Label") +
      labs(x = "Maximal Window Size", y = "Maximal Detected Value",
           title = paste0("Maximal Detected Value vs. Threshold SVD\n- Lower Window Size ", win, " ", cat)) +
      geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) +
      theme(plot.title = element_text(size = 22), axis.text = element_text(size = 20),
            axis.title = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 20),
            legend.key.size = unit(1, "cm"))
    print(p)
    dev.off()
  }
}

#save the thresholding visualization results:
df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "Rec",])
list_label <- df_svd[df_svd$battery == "Rec",]$Label
df_rec_10 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_rec_10$value[1:n_rec] <- df_svd[df_svd$battery == "Rec",]$'X10_60'
df_rec_10$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X10_75'
df_rec_10$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X10_100'
df_rec_10$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X10_125'
df_rec_10$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X10_150'
df_rec_10$label <- ifelse(df_rec_10$label == 1, "Positive", ifelse(df_rec_10$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LFP",])
list_label <- df_svd[df_svd$battery == "LFP",]$Label
df_lfp_10 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lfp_10$value[1:n_rec] <- df_svd[df_svd$battery == "LFP",]$'X10_60'
df_lfp_10$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X10_75'
df_lfp_10$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X10_100'
df_lfp_10$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X10_125'
df_lfp_10$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X10_150'
df_lfp_10$label <- ifelse(df_lfp_10$label == 1, "Positive", ifelse(df_lfp_10$label == 2, "Unknown", "Negative"))

df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LCO",])
list_label <- df_svd[df_svd$battery == "LCO",]$Label
df_lco_10 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lco_10$value[1:n_rec] <- df_svd[df_svd$battery == "LCO",]$'X10_60'
df_lco_10$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X10_75'
df_lco_10$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X10_100'
df_lco_10$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X10_125'
df_lco_10$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X10_150'
df_lco_10$label <- ifelse(df_lco_10$label == 1, "Positive", ifelse(df_lco_10$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "NMC",])
list_label <- df_svd[df_svd$battery == "NMC",]$Label
df_nmc_10 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_nmc_10$value[1:n_rec] <- df_svd[df_svd$battery == "NMC",]$'X10_60'
df_nmc_10$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_75'
df_nmc_10$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_100'
df_nmc_10$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_125'
df_nmc_10$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_150'
df_nmc_10$label <- ifelse(df_nmc_10$label == 1, "Positive", ifelse(df_nmc_10$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "SO",])
list_label <- df_svd[df_svd$battery == "SO",]$Label
df_so_10 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_so_10$value[1:n_rec] <- df_svd[df_svd$battery == "SO",]$'X10_60'
df_so_10$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X10_75'
df_so_10$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X10_100'
df_so_10$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X10_125'
df_so_10$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X10_150'
df_so_10$label <- ifelse(df_so_10$label == 1, "Positive", ifelse(df_so_10$label == 2, "Unknown", "Negative"))

library(ggplot2)
png("thresholding_sensitivity_windowlower_10_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_10, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 10 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()

png("thresholding_sensitivity_windowlower_10_NMC_SVD.png", width = 600, height = 750)
ggplot(df_nmc_10, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 10 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_10_LCO_SVD.png", width = 600, height = 750)
ggplot(df_lco_10, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 10 LCO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_10_Rec_SVD.png", width = 600, height = 750)
ggplot(df_rec_10, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 10 Rec Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()

png("thresholding_sensitivity_windowlower_10_LFP_SVD.png", width = 600, height = 750)
ggplot(df_lfp_10, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 10 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()




###############
df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "Rec",])
list_label <- df_svd[df_svd$battery == "Rec",]$Label
df_rec_30 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_rec_30$value[1:n_rec] <- df_svd[df_svd$battery == "Rec",]$'X30_60'
df_rec_30$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X30_75'
df_rec_30$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X30_100'
df_rec_30$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X30_125'
df_rec_30$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X30_150'
df_rec_30$label <- ifelse(df_rec_30$label == 1, "Positive", ifelse(df_rec_30$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LFP",])
list_label <- df_svd[df_svd$battery == "LFP",]$Label
df_lfp_30 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lfp_30$value[1:n_rec] <- df_svd[df_svd$battery == "LFP",]$'X30_60'
df_lfp_30$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X30_75'
df_lfp_30$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X30_100'
df_lfp_30$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X30_125'
df_lfp_30$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X30_150'
df_lfp_30$label <- ifelse(df_lfp_30$label == 1, "Positive", ifelse(df_lfp_30$label == 2, "Unknown", "Negative"))

df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LCO",])
list_label <- df_svd[df_svd$battery == "LCO",]$Label
df_lco_30 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lco_30$value[1:n_rec] <- df_svd[df_svd$battery == "LCO",]$'X30_60'
df_lco_30$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X30_75'
df_lco_30$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X30_100'
df_lco_30$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X30_125'
df_lco_30$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X30_150'
df_lco_30$label <- ifelse(df_lco_30$label == 1, "Positive", ifelse(df_lco_30$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "NMC",])
list_label <- df_svd[df_svd$battery == "NMC",]$Label
df_nmc_30 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_nmc_30$value[1:n_rec] <- df_svd[df_svd$battery == "NMC",]$'X10_60'
df_nmc_30$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_75'
df_nmc_30$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_100'
df_nmc_30$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_125'
df_nmc_30$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X10_150'
df_nmc_30$label <- ifelse(df_nmc_30$label == 1, "Positive", ifelse(df_nmc_30$label == 2, "Unknown", "Negative"))

df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "SO",])
list_label <- df_svd[df_svd$battery == "SO",]$Label
df_so_30 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_so_30$value[1:n_rec] <- df_svd[df_svd$battery == "SO",]$'X30_60'
df_so_30$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X30_75'
df_so_30$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X30_100'
df_so_30$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X30_125'
df_so_30$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X30_150'
df_so_30$label <- ifelse(df_so_30$label == 1, "Positive", ifelse(df_so_30$label == 2, "Unknown", "Negative"))


library(ggplot2)
png("thresholding_sensitivity_windowlower_30_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_30, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()

png("thresholding_sensitivity_windowlower_30_NMC_SVD.png", width = 600, height = 750)
ggplot(df_nmc_30, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_30_LCO_SVD.png", width = 600, height = 750)
ggplot(df_lco_30, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_30_Generic_SVD.png", width = 600, height = 750)
ggplot(df_rec_30, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 Generic Lab Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_30_LFP_SVD.png", width = 600, height = 750)
ggplot(df_lfp_30, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 22)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()



df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "Rec",])
list_label <- df_svd[df_svd$battery == "Rec",]$Label
df_rec_40 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_rec_40$value[1:n_rec] <- df_svd[df_svd$battery == "Rec",]$'X40_60'
df_rec_40$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X40_75'
df_rec_40$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X40_100'
df_rec_40$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X40_125'
df_rec_40$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "Rec",]$'X40_150'
df_rec_40$label <- ifelse(df_rec_40$label == 1, "Positive", ifelse(df_rec_40$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LFP",])
list_label <- df_svd[df_svd$battery == "LFP",]$Label
df_lfp_40 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lfp_40$value[1:n_rec] <- df_svd[df_svd$battery == "LFP",]$'X40_60'
df_lfp_40$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X40_75'
df_lfp_40$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X40_100'
df_lfp_40$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X40_125'
df_lfp_40$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LFP",]$'X40_150'
df_lfp_40$label <- ifelse(df_lfp_40$label == 1, "Positive", ifelse(df_lfp_40$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "LCO",])
list_label <- df_svd[df_svd$battery == "LCO",]$Label
df_lco_40 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_lco_40$value[1:n_rec] <- df_svd[df_svd$battery == "LCO",]$'X40_60'
df_lco_40$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X40_75'
df_lco_40$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X40_100'
df_lco_40$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X40_125'
df_lco_40$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "LCO",]$'X40_150'
df_lco_40$label <- ifelse(df_lco_40$label == 1, "Positive", ifelse(df_lco_40$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "NMC",])
list_label <- df_svd[df_svd$battery == "NMC",]$Label
df_nmc_40 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_nmc_40$value[1:n_rec] <- df_svd[df_svd$battery == "NMC",]$'X40_60'
df_nmc_40$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X40_75'
df_nmc_40$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X40_100'
df_nmc_40$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X40_125'
df_nmc_40$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "NMC",]$'X40_150'
df_nmc_40$label <- ifelse(df_nmc_40$label == 1, "Positive", ifelse(df_nmc_40$label == 2, "Unknown", "Negative"))


df_svd <- read.csv("battery_cell_SVD_whole_stats.csv")
n_rec <- nrow(df_svd[df_svd$battery == "SO",])
list_label <- df_svd[df_svd$battery == "SO",]$Label
df_so_40 <- data.frame(
	threshold = c(rep(60, n_rec), rep(75, n_rec), rep(100, n_rec),
		rep(125, n_rec), rep(150, n_rec)),
	label = c(rep(list_label, 5)),
	value = numeric(n_rec) * 5
	)
df_so_40$value[1:n_rec] <- df_svd[df_svd$battery == "SO",]$'X40_60'
df_so_40$value[(n_rec+1):(2*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X40_75'
df_so_40$value[(2*n_rec+1):(3*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X40_100'
df_so_40$value[(3*n_rec+1):(4*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X40_125'
df_so_40$value[(4*n_rec+1):(5*n_rec)] <- df_svd[df_svd$battery == "SO",]$'X40_150'
df_so_40$label <- ifelse(df_so_40$label == 1, "Positive", ifelse(df_so_40$label == 2, "Unknown", "Negative"))
library(ggplot2)
png("thresholding_sensitivity_windowlower_40_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_40, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 40 SO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 20)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()

png("thresholding_sensitivity_windowlower_40_NMC_SVD.png", width = 600, height = 750)
ggplot(df_nmc_40, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 40 NMC Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 20)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_40_LCO_SVD.png", width = 600, height = 750)
ggplot(df_lco_40, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 40 LCO Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 20)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_40_Rec_SVD.png", width = 600, height = 750)
ggplot(df_rec_40, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 40 Rec Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 20)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()


png("thresholding_sensitivity_windowlower_40_LFP_SVD.png", width = 600, height = 750)
ggplot(df_lfp_40, aes(x = threshold, y = value, color = factor(label))) +
  geom_jitter(size = 2.5) +
  scale_color_manual(values = c(
    "Negative"="orange",
    "Positive"="steelblue",
    "Unknown"="firebrick" 
    ),
                     name = "Label") +
  labs(
    x = "Maximal Window Size",
    y = "Maximal Detected Value",
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 40 LFP Battery"
  ) +
    geom_hline(yintercept = 0.3, color = "brown", linewidth = 1.5) + 
     theme(plot.title = element_text(size = 20)) + 
   theme(axis.text = element_text(size = 20)) + 
   theme(axis.title = element_text(size = 20)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position='bottom') + 
  theme(legend.title = element_text(size = 20), 
        legend.text = element_text(size = 20)) + 
    theme(legend.title = element_blank()) +   
  theme(legend.key.size = unit(1, 'cm')) 
dev.off()
}  # end if (file.exists("10_60_detected_result_wholeSVD.csv"))
