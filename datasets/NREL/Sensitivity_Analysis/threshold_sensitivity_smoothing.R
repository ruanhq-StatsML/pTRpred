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

df_30 <- data.frame(
    dataset_name = rep(5, battery_cell$)
	)
for(i in 1:nrow(battery_cell)){

}

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


