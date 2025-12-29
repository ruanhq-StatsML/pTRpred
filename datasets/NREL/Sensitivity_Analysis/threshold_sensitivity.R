library(readxl)
battery_cell <- read_xlsx("dataset_correspondence.xlsx")
battery_cell <- battery_cell[,c(1:3)]
df0 <- read.csv("battery_class_results.csv")[1:118,]
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()

df1 <- read.csv("10_60_detected_result_whole.csv")
df2 <- read.csv("10_75_detected_result_whole.csv")
df3 <- read.csv("10_100_detected_result_whole.csv")
df4 <- read.csv("10_125_detected_result_whole.csv")
df5 <- read.csv("10_150_detected_result_whole.csv")
df6 <- read.csv("40_60_detected_result_whole.csv")
df7 <- read.csv("40_75_detected_result_whole.csv")
df8 <- read.csv("40_100_detected_result_whole.csv")
df9 <- read.csv("40_125_detected_result_whole.csv")
df10 <- read.csv("40_150_detected_result_whole.csv")
df0 <- merge(df0, df1, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df2, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df3, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df4, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df5, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df6, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df7, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df8, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df9, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df10, by.x = "dataset_name", by.y = "dataset")
drops <- c("X.y", "X.x")
df0 <- df0[ , !(names(df0) %in% drops)]
df0$'label_10_125' <- ifelse(df0$`X10_125` >= 0.3, 1, 0)
df0$'conclusion_10_125' <- 0 
df0[(df0$label == df0$label_10_125) & (df0$label == 1) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'TP'
df0[(df0$label == df0$label_10_125) & (df0$label == 0) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'TN'
df0[(df0$label != df0$label_10_125) & (df0$label == 1) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'FN'
df0[(df0$label != df0$label_10_125) & (df0$label == 0) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_10_60' <- ifelse(df0$`X10_60` >= 0.3, 1, 0)
df0$'conclusion_10_60' <- 0 
df0[(df0$Label == df0$label_10_60) & (df0$Label == 1) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'TP'
df0[(df0$Label == df0$label_10_60) & (df0$Label == 0) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'TN'
df0[(df0$Label != df0$label_10_60) & (df0$Label == 1) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'FN'
df0[(df0$Label != df0$label_10_60) & (df0$Label == 0) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_10_75' <- ifelse(df0$`X10_75` >= 0.3, 1, 0)
df0$'conclusion_10_75' <- 0 
df0[(df0$Label == df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TP'
df0[(df0$Label == df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_10_150' <- ifelse(df0$`X10_75` >= 0.3, 1, 0)
df0$'conclusion_10_150' <- 0 
df0[(df0$Label == df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TP'
df0[(df0$Label == df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_60' <- ifelse(df0$`X40_60` >= 0.3, 1, 0)
df0$'conclusion_40_60' <- 0 
df0[(df0$Label == df0$label_40_60) & (df0$Label == 1) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'TP'
df0[(df0$Label == df0$label_40_60) & (df0$Label == 0) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'TN'
df0[(df0$Label != df0$label_40_60) & (df0$Label == 1) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'FN'
df0[(df0$Label != df0$label_40_60) & (df0$Label == 0) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_75' <- ifelse(df0$`X40_75` >= 0.3, 1, 0)
df0$'conclusion_40_75' <- 0 
df0[(df0$Label == df0$label_40_75) & (df0$Label == 1) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'TP'
df0[(df0$Label == df0$label_40_75) & (df0$Label == 0) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'TN'
df0[(df0$Label != df0$label_40_75) & (df0$Label == 1) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'FN'
df0[(df0$Label != df0$label_40_75) & (df0$Label == 0) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_100' <- ifelse(df0$`X40_100` >= 0.3, 1, 0)
df0$'conclusion_40_100' <- 0 
df0[(df0$Label == df0$label_40_100) & (df0$Label == 1) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'TP'
df0[(df0$Label == df0$label_40_100) & (df0$Label == 0) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'TN'
df0[(df0$Label != df0$label_40_100) & (df0$Label == 1) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'FN'
df0[(df0$Label != df0$label_40_100) & (df0$Label == 0) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_125' <- ifelse(df0$`X40_125` >= 0.3, 1, 0)
df0$'conclusion_40_125' <- 0 
df0[(df0$Label == df0$label_40_125) & (df0$Label == 1) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'TP'
df0[(df0$Label == df0$label_40_125) & (df0$Label == 0) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'TN'
df0[(df0$Label != df0$label_40_125) & (df0$Label == 1) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'FN'
df0[(df0$Label != df0$label_40_125) & (df0$Label == 0) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_150' <- ifelse(df0$`X40_150` >= 0.3, 1, 0)
df0$'conclusion_40_150' <- 0 
df0[(df0$Label == df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TP'
df0[(df0$Label == df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_150' <- ifelse(df0$`X40_150` >= 0.3, 1, 0)
df0$'conclusion_40_150' <- 0 
df0[(df0$Label == df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TP'
df0[(df0$Label == df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df11 <- read.csv("30_75_detected_result_whole.csv")
df12 <- read.csv("30_100_detected_result_whole.csv")
df13 <- read.csv("30_125_detected_result_whole.csv")
df14 <- read.csv("30_150_detected_result_whole.csv")
df0 <- merge(df0, df11, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df12, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df13, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df14, by.x = "dataset_name", by.y = "dataset")

df0$'label_30_75' <- ifelse(df0$`X30_75` >= 0.3, 1, 0)
df0$'conclusion_30_75' <- 0 
df0[(df0$Label == df0$label_30_75) & (df0$Label == 1) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'TP'
df0[(df0$Label == df0$label_30_75) & (df0$Label == 0) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'TN'
df0[(df0$Label != df0$label_30_75) & (df0$Label == 1) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'FN'
df0[(df0$Label != df0$label_30_75) & (df0$Label == 0) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0$'label_30_100' <- ifelse(df0$`X30_100` >= 0.3, 1, 0)
df0$'conclusion_30_100' <- 0 
df0[(df0$Label == df0$label_30_100) & (df0$Label == 1) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'TP'
df0[(df0$Label == df0$label_30_100) & (df0$Label == 0) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'TN'
df0[(df0$Label != df0$label_30_100) & (df0$Label == 1) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'FN'
df0[(df0$Label != df0$label_30_100) & (df0$Label == 0) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()


df0$'label_30_125' <- ifelse(df0$`X30_125` >= 0.3, 1, 0)
df0$'conclusion_30_125' <- 0 
df0[(df0$Label == df0$label_30_125) & (df0$Label == 1) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'TP'
df0[(df0$Label == df0$label_30_125) & (df0$Label == 0) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'TN'
df0[(df0$Label != df0$label_30_125) & (df0$Label == 1) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'FN'
df0[(df0$Label != df0$label_30_125) & (df0$Label == 0) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_30_150' <- ifelse(df0$`X30_150` >= 0.3, 1, 0)
df0$'conclusion_30_150' <- 0 
df0[(df0$Label == df0$label_30_150) & (df0$Label == 1) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'TP'
df0[(df0$Label == df0$label_30_150) & (df0$Label == 0) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'TN'
df0[(df0$Label != df0$label_30_150) & (df0$Label == 1) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'FN'
df0[(df0$Label != df0$label_30_150) & (df0$Label == 0) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
#policy agent:
#Groundedness, Precision -> does the response directly and accurately answer the user's guide?
#response directly and accurately answer the users' guide?
#Precision, Groundedness.
#score_groundedness and score_precision: 0.75, 0.6






#distribution shift attribution:
library(readxl)
battery_cell <- read_xlsx("dataset_correspondence.xlsx")
battery_cell <- battery_cell[,c(1:3)]
df0 <- read.csv("battery_class_results.csv")[1:118,]
df0 %>% group_by(battery) %>% 
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
library(readxl)
battery_cell <- read_xlsx("dataset_correspondence.xlsx")
battery_cell <- battery_cell[,c(1:3)]
df0 <- read.csv("battery_class_results.csv")[1:118,]
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(Conclusion == "FP", na.rm = TRUE),
count_tp = sum(Conclusion == "TP", na.rm = TRUE),
count_fn = sum(Conclusion == "TN", na.rm = TRUE),
count_tn = sum(Conclusion == "FN", na.rm = TRUE),
total = n(),
proportion = count_fp/total
) %>% ungroup()
df0 <- merge(df0, df1, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df2, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df3, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df4, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df5, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df6, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df7, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df8, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df9, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df10, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df11, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df12, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df13, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df14, by.x = "dataset_name", by.y = "dataset")
df0 <- merge(df0, df15, by.x = "dataset_name", by.y = "dataset")
drops <- c("X.y", "X.x", "X")
df0 <- df0[ , !(names(df0) %in% drops)]
write.csv(df0, "battery_cell_SVD_whole_stats.csv")
df0 <- read.csv("battery_cell_SVD_whole_stats.csv")
df0$'label_10_125' <- ifelse(df0$`X10_125` >= 0.3, 1, 0)
df0$'conclusion_10_125' <- 0 
df0[(df0$label == df0$label_10_125) & (df0$label == 1) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'TP'
df0[(df0$label == df0$label_10_125) & (df0$label == 0) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'TN'
df0[(df0$label != df0$label_10_125) & (df0$label == 1) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'FN'
df0[(df0$label != df0$label_10_125) & (df0$label == 0) & (df0$label_10_125 != 2),]$'conclusion_10_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_10_60' <- ifelse(df0$`X10_60` >= 0.3, 1, 0)
df0$'conclusion_10_60' <- 0 
df0[(df0$Label == df0$label_10_60) & (df0$Label == 1) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'TP'
df0[(df0$Label == df0$label_10_60) & (df0$Label == 0) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'TN'
df0[(df0$Label != df0$label_10_60) & (df0$Label == 1) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'FN'
df0[(df0$Label != df0$label_10_60) & (df0$Label == 0) & (df0$label_10_60 != 2),]$'conclusion_10_60' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_10_75' <- ifelse(df0$`X10_75` >= 0.3, 1, 0)
df0$'conclusion_10_75' <- 0 
df0[(df0$Label == df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TP'
df0[(df0$Label == df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0$'label_10_150' <- ifelse(df0$`X10_75` >= 0.3, 1, 0)
df0$'conclusion_10_150' <- 0 
df0[(df0$Label == df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TP'
df0[(df0$Label == df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'TN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 1) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FN'
df0[(df0$Label != df0$label_10_75) & (df0$Label == 0) & (df0$label_10_75 != 2),]$'conclusion_10_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_10_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_10_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_10_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_10_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_60' <- ifelse(df0$`X40_60` >= 0.3, 1, 0)
df0$'conclusion_40_60' <- 0 
df0[(df0$Label == df0$label_40_60) & (df0$Label == 1) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'TP'
df0[(df0$Label == df0$label_40_60) & (df0$Label == 0) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'TN'
df0[(df0$Label != df0$label_40_60) & (df0$Label == 1) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'FN'
df0[(df0$Label != df0$label_40_60) & (df0$Label == 0) & (df0$label_40_60 != 2),]$'conclusion_40_60' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_60 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_60 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_60 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_60 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_75' <- ifelse(df0$`X40_75` >= 0.3, 1, 0)
df0$'conclusion_40_75' <- 0 
df0[(df0$Label == df0$label_40_75) & (df0$Label == 1) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'TP'
df0[(df0$Label == df0$label_40_75) & (df0$Label == 0) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'TN'
df0[(df0$Label != df0$label_40_75) & (df0$Label == 1) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'FN'
df0[(df0$Label != df0$label_40_75) & (df0$Label == 0) & (df0$label_40_75 != 2),]$'conclusion_40_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_100' <- ifelse(df0$`X40_100` >= 0.3, 1, 0)
df0$'conclusion_40_100' <- 0 
df0[(df0$Label == df0$label_40_100) & (df0$Label == 1) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'TP'
df0[(df0$Label == df0$label_40_100) & (df0$Label == 0) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'TN'
df0[(df0$Label != df0$label_40_100) & (df0$Label == 1) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'FN'
df0[(df0$Label != df0$label_40_100) & (df0$Label == 0) & (df0$label_40_100 != 2),]$'conclusion_40_100' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_125' <- ifelse(df0$`X40_125` >= 0.3, 1, 0)
df0$'conclusion_40_125' <- 0 
df0[(df0$Label == df0$label_40_125) & (df0$Label == 1) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'TP'
df0[(df0$Label == df0$label_40_125) & (df0$Label == 0) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'TN'
df0[(df0$Label != df0$label_40_125) & (df0$Label == 1) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'FN'
df0[(df0$Label != df0$label_40_125) & (df0$Label == 0) & (df0$label_40_125 != 2),]$'conclusion_40_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_150' <- ifelse(df0$`X40_150` >= 0.3, 1, 0)
df0$'conclusion_40_150' <- 0 
df0[(df0$Label == df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TP'
df0[(df0$Label == df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_40_150' <- ifelse(df0$`X40_150` >= 0.3, 1, 0)
df0$'conclusion_40_150' <- 0 
df0[(df0$Label == df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TP'
df0[(df0$Label == df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'TN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 1) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FN'
df0[(df0$Label != df0$label_40_150) & (df0$Label == 0) & (df0$label_40_150 != 2),]$'conclusion_40_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_40_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_40_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_40_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_40_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_30_75' <- ifelse(df0$`X30_75` >= 0.3, 1, 0)
df0$'conclusion_30_75' <- 0 
df0[(df0$Label == df0$label_30_75) & (df0$Label == 1) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'TP'
df0[(df0$Label == df0$label_30_75) & (df0$Label == 0) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'TN'
df0[(df0$Label != df0$label_30_75) & (df0$Label == 1) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'FN'
df0[(df0$Label != df0$label_30_75) & (df0$Label == 0) & (df0$label_30_75 != 2),]$'conclusion_30_75' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_75 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_75 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_75 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_75 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()

df0$'label_30_100' <- ifelse(df0$`X30_100` >= 0.3, 1, 0)
df0$'conclusion_30_100' <- 0 
df0[(df0$Label == df0$label_30_100) & (df0$Label == 1) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'TP'
df0[(df0$Label == df0$label_30_100) & (df0$Label == 0) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'TN'
df0[(df0$Label != df0$label_30_100) & (df0$Label == 1) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'FN'
df0[(df0$Label != df0$label_30_100) & (df0$Label == 0) & (df0$label_30_100 != 2),]$'conclusion_30_100' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_100 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_100 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_100 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_100 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()


df0$'label_30_125' <- ifelse(df0$`X30_125` >= 0.3, 1, 0)
df0$'conclusion_30_125' <- 0 
df0[(df0$Label == df0$label_30_125) & (df0$Label == 1) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'TP'
df0[(df0$Label == df0$label_30_125) & (df0$Label == 0) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'TN'
df0[(df0$Label != df0$label_30_125) & (df0$Label == 1) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'FN'
df0[(df0$Label != df0$label_30_125) & (df0$Label == 0) & (df0$label_30_125 != 2),]$'conclusion_30_125' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_125 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_125 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_125 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_125 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()
df0$'label_30_150' <- ifelse(df0$`X30_150` >= 0.3, 1, 0)
df0$'conclusion_30_150' <- 0 
df0[(df0$Label == df0$label_30_150) & (df0$Label == 1) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'TP'
df0[(df0$Label == df0$label_30_150) & (df0$Label == 0) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'TN'
df0[(df0$Label != df0$label_30_150) & (df0$Label == 1) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'FN'
df0[(df0$Label != df0$label_30_150) & (df0$Label == 0) & (df0$label_30_150 != 2),]$'conclusion_30_150' <- 'FP'
df0 %>% group_by(battery) %>% 
summarize(
count_fp = sum(conclusion_30_150 == "FP", na.rm = TRUE),
count_tp = sum(conclusion_30_150 == "TP", na.rm = TRUE),
count_fn = sum(conclusion_30_150 == "TN", na.rm = TRUE),
count_tn = sum(conclusion_30_150 == "FN", na.rm = TRUE),
total = n(),
proportion = (count_fp+count_fn)/total
) %>% ungroup()


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

library(ggplot2)
png("thresholding_sensitivity_windowlower_10_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_10, aes(x = threshold, y = value, color = factor(label))) +
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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

library(ggplot2)
png("thresholding_sensitivity_windowlower_30_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_30, aes(x = threshold, y = value, color = factor(label))) +
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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


png("thresholding_sensitivity_windowlower_30_Rec_SVD.png", width = 600, height = 750)
ggplot(df_rec_30, aes(x = threshold, y = value, color = factor(label))) +
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
    title = "Maximal Detected Value vs. Threshold SVD Procedure \n - Lower Window Size 30 Rec Battery"
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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

library(ggplot2)
png("thresholding_sensitivity_windowlower_40_SO_SVD.png", width = 600, height = 750)
ggplot(df_so_40, aes(x = threshold, y = value, color = factor(label))) +
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
    "0"="orange",
    "1"="steelblue",
    "2"="firebrick" 
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
