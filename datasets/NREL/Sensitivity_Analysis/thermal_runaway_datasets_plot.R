#Plotting thermal runaway datasets:

plot_detection_overlay <- function(
    df,
    title = "Real-time Detection vs Signal",
    time_col   = "time_ind",
    score_col  = "detected_value_rt",
    signal_col = "signal",
    first_det_col = "first_detection_time",
    score_label  = "Real-Time Detected Value",
    signal_label = "Signal",
    x_label = "Index",
    threshold = NULL,           # optional numeric; draws a horizontal line if provided
    scale_factor = NULL,        # if NULL, auto-compute to overlay signal on left axis
    score_color  = "red",
    signal_color = "black",
    save_path = NULL,           # e.g., "winsing_singularvalue_modelU_s1.png"
    width = 9, height = 7.5, dpi = 300
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. install.packages('ggplot2')")
  }

  df_local <- df
  x  <- df_local[[time_col]]
  sc <- df_local[[score_col]]
  sg <- df_local[[signal_col]]

  # Auto scale the signal onto the left axis range so the two lines are visually comparable.
  # y_left = score; we plot scaled_signal = signal * alpha
  # Right axis shows original signal via sec_axis(~ . / alpha).
  if (is.null(scale_factor)) {
    rng_sc <- range(sc, na.rm = TRUE)
    rng_sg <- range(sg, na.rm = TRUE)
    span_sc <- diff(rng_sc)
    span_sg <- diff(rng_sg)
    if (!is.finite(span_sc) || span_sc == 0 || !is.finite(span_sg) || span_sg == 0) {
      alpha <- 1
    } else {
      alpha <- span_sc / span_sg
    }
  } else {
    alpha <- as.numeric(scale_factor)
    if (!is.finite(alpha) || alpha == 0) alpha <- 1
  }

  df_local$scaled_signal__ <- sg * alpha

  # First detection time (constant across rows in detect_realtime output)
  first_det_time <- NA
  if (first_det_col %in% names(df_local)) {
    uvals <- unique(df_local[[first_det_col]])
    # keep finite values only; supports numeric time, Date, POSIXct (is.finite works)
    uvals <- uvals[is.finite(uvals)]
    if (length(uvals) > 0) first_det_time <- uvals[1]
  }

  ggplot2::ggplot(df_local, ggplot2::aes(x = .data[[time_col]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[score_col]], color = "Detected Value"),
                       linewidth = 1.1, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["scaled_signal__"]], color = "Signal"),
                       linewidth = 0.9, alpha = 0.8, na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      #name = score_label,
      #sec.axis = ggplot2::sec_axis(~ . / alpha, name = signal_label)
      c(0,1)
    ) +
    ggplot2::scale_color_manual(
      name = "Legend",
      values = c("Detected Value" = score_color, "Signal" = signal_color)
    ) +
    ggplot2::labs(
      x = x_label,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.title.y = ggplot2::element_text(color = score_color, size = 16),
      axis.title.y.right = ggplot2::element_text(color = signal_color, size = 16),
      axis.title.x = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = score_color),
      axis.text.y.right = ggplot2::element_text(color = signal_color),
      legend.position = "top"
    ) ->
    p

  # Optional horizontal threshold line (on score axis)
  if (!is.null(threshold) && is.finite(threshold)) {
    p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed",
                                 color = score_color, linewidth = 0.6)
  }

  # Optional vertical line at first detection time
  if (is.finite(first_det_time)) {
    p <- p + ggplot2::geom_vline(xintercept = first_det_time, linetype = "dotted",
                                 color = "steelblue", linewidth = 0.8)
  }

  # Save if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = p, width = width, height = height, dpi = dpi)
  }

  p
}

df1 <- read.csv("mat3_result_df_detect.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - SimulatedTR3", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("mat5_result_df_detect.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - SimulatedTR5", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("mat4_result_df_detect.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - SimulatedTR1", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_0SOC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - LFP 0SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("LMO_40SOC1_T.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - LMO 40SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LMO_20SOC2_T.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - LMO 20SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LMO_20SOC2_T.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - LMO 20SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LMO_80SOC2_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. Temperature - LFP 80SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)



df1 <- read.csv("mat4_result_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. SVD SimulatedTR1", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("mat6_result_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. SVD SimulatedTR4", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("mat1_severity_setting3_result_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. SVD SimulatedTR3", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("ISC_faulty1_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty1 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty1_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty1 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty2_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty2 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty2_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty2 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty3_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty3 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty3_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty3 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty4_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty4 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty4_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty4 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("mat1_severity_setting3_result_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. SimulatedTR2 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty4_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty4 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty4_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty4 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty5_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty5 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty5_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty5 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("ISC_faulty6_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty6 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty6_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty6 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty7_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty7 Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ISC_faulty7_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ISC Faulty7 SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("ESC_100BSOC_df_detect_SVD.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ESC 100SOC SVD", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ESC_100ASOC_df_detect.csv")
plot_detection_overlay(df1[1:3000,], 
title = "Real-Time Detection Value VS. ESC 100SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("ESC_100BSOC_df_detect_SVD.csv")
plot_detection_overlay(df1,
title = "Real-Time Detection Value VS. ESC 100SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ESC_100ASOC_df_detect.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. ESC 100SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ESC_50SOC_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. ESC 50SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ESC_75SOC_df_detect_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. ESC 75SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("ESC_50SOC_df_detect.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. ESC 50SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)




df1 <- read.csv("LFP_Cathode_75SOC_TGADSC.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP Cathode 75SOC Temperature", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_75SOC_ESARC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP ESARC 75SOC SVD", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_75SOC_ESARC.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP ESARC 75SOC", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)


df1 <- read.csv("LCO_100SOC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LCO ESARC 100SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_75SOC_ESARC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP ESARC 75SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_100SOC_ESARC.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP ESARC 100SOC", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LFP_100SOC_ESARC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LFP ESARC 100SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LCO_50SOC_ESARC.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LCO ESARC 50SOC", 
signal_label = "Temperature",
x_label = "Time Index",
threshold = 0.3)

df1 <- read.csv("LCO_50SOC_ESARC_SVD.csv")
plot_detection_overlay(df1, 
title = "Real-Time Detection Value VS. LCO ESARC 50SOC SVD", 
signal_label = "First Singular Value",
x_label = "Time Index",
threshold = 0.3)

########################################################
df1 <- read.csv("batteryCCCV_SC1T_detect.csv")
df1$temperature <- df1$signal
png("batteryCCCV_TR1_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df1$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Start 1e10, End 0.002", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 3)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df1)), df1$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

df1 <- read.csv("batteryCCCV_SC2T_detect.csv")
df1$temperature <- df1$signal
png("batteryCCCV_TR2_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df1$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Start 1e9, End 0.02", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 3)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df1)), df1$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


df1 <- read.csv("batteryCCCV_SC30T_detect.csv")
df1$temperature <- df1$signal
png("batteryCCCV_OC1_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df1$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV ordinary Charging Cycle", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 3)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df1)), df1$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

#Identifying the
df2 <- read.csv("batteryCCCV_SC22T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_OC2_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV ordinary Charging Cycle Start 1e6 End 0.1", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 



#Identifying the
df2 <- read.csv("batteryCCCV_SC25T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR2_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Setting 1", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

#Identifying the
df2 <- read.csv("batteryCCCV_SC26T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR1_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Setting 2", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


#Identifying the
df2 <- read.csv("batteryCCCV_SC27T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR4_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Start 1e6, End 0.015", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 





#Identifying the
df2 <- read.csv("batteryCCCV_SC27T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR5_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal RunawayStart 1e6, End 0.015", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


#Identifying the
df2 <- read.csv("batteryCCCV_SC28T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR6_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Start 1e6, End 0.0475", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 



#Identifying the
df2 <- read.csv("batteryCCCV_SC23T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR3_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway - Start 1e6, End 0.04", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 





#Identifying the
df2 <- read.csv("batteryCCCV_SC28SVD_detect.csv")
png("batteryCCCV_TR1_detected_SVD.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. First Singular Value \n BatteryCCCV Thermal Runaway Start 1e6, End 0.0475", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$signal, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("first singular value", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


#Identifying the
df2 <- read.csv("batteryCCCV_SC23SVD_detect.csv")
png("batteryCCCV_TR3_detected_SVD.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. First Singular Value \n BatteryCCCV Start 1e6, End 0.04", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$signal, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("first singular value", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

#The neural collapse phenomenon mainly focus on the following two aspects:
#n_points <- end_indice - start_indice  + 1

#How to predict the video view counts?
#How to estimate the 99% confidence interval?


########
df5 <- read.csv("batteryCCCV_SC29SVD_detect.csv")
png("batteryCCCV_TR2_detected_SVD.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. First Singular Value \n BatteryCCCV Thermal Runaway Start 1e6, end 0.055", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$signal, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("first singular value", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

########
df2 <- read.csv("batteryCCCV_SC26T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR26_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Start 1e6, end 0.02", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


df2 <- read.csv("batteryCCCV_SC5T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR5_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Start 1e5, end 0.00002", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 


df2 <- read.csv("batteryCCCV_SC5T_detect.csv")
df2$temperature <- df2$signal
png("batteryCCCV_TR5_detected.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. Temperature \n BatteryCCCV Thermal Runaway Setting 5", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$temperature, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("temperature(F)", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 

df5 <- read.csv("batteryCCCV_SC28SVD_detect.csv")
png("batteryCCCV_TR28_detected_SVD.png", width = 1200, height = 600)
par(mai = c(1.2, 1.2, 1.2, 1.2))
plot(df2$detected_value_rt, type = "l", col = "black", 
     main = "Real-Time Detection Value vs. First Singular Value \n BatteryCCCV Thermal Runaway Start 1e6, end 0.0475", 
     lwd = 1.75, xlab = "", ylab = "", cex.lab = 2, cex.axis = 2, cex.main = 2)
par(new = TRUE)
#n_points <- end_indice - start_indice + 1
plot(c(1:nrow(df2)), df2$signal, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "", lwd = 1.75)
axis(side = 4, col.axis = "red", cex.axis = 2)
mtext("detected value", side = 2, col = "black", cex = 2.5, line = 3)
mtext("first singular value", side = 4, col="red", cex = 2.5, line = 3)
mtext("time index(1s)", side = 1, col = "black", cex = 2.5, line = 3)
#detect_indice <- min(which(df_output$detected_value_rt >= 0.3))
#abline(v=(indice-start_indice), col="orange", lty = 4, lwd = 2)
dev.off() 







