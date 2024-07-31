#*******************************************************************************
#*
#*                               Create Figure 4
#*                               (Mosaic plots) 
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2024         
#*******************************************************************************



## Load R packages ----
list.of.packages <- c("plyr", "ggmosaic", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



#' Function to calculate conditional percentages in mosaic plot
#' Source: https://www.kaggle.com/code/dhafer/mosaics-plots-using-ggmosaic
conditional_perc <- function (plot_data) {
  
  get_data <- ggplot_build(plot_data)$data %>% as.data.frame()
  get_data_new <- get_data[order(get_data[, 9], get_data[, 2]), ]
  y_perc <- tapply(get_data_new$.n, factor(get_data_new$fill, levels = unique(get_data_new$fill)), function(x) x/sum(x))
  get_data_new$percentage <- paste0(sprintf("%1.f", unlist(y_perc) * 100), "%")

  return(get_data_new)
}



## Load data ----
# Meta-analysis level
load("./data/Dataset_Meta level.RData")



## Prepare dataset
# Meta-analysis model chosen a priori?
data_meta$Meta_model_choice <- factor(revalue(factor(data_meta$Meta_model_choice), 
                                              c("Unclear"="Other", "Insufficient"="Other", "Not reported"="Other")),
                                      levels = c("Yes", "No", "Other"))

# Choice of meta-analysis model based on I2
data_meta$Meta_hetero <- factor(revalue(factor(data_meta$Meta_hetero), 
                                        c("Unclear"="Other", "Insufficient"="Other", "Not reported"="Other")),
                                levels = c("Yes", "No", "Other"))

# Heterogeneity interpretation was based on I2
data_meta$Interpret_heter <- factor(revalue(factor(data_meta$Interpret_heter), 
                                            c("Unclear"="Other", "Insufficient"="Other", "Not reported"="Other")),
                                    levels = c("Yes", "No", "Other"))

# Heterogeneity interpretation was based on tau2
data_meta$Interpret_tau <- factor(revalue(factor(data_meta$Interpret_tau), 
                                          c("Unclear"="Other", "Not reported"="Other", "Not applicable"="Other")),
                                  levels = c("Yes", "No", "Other"))


## Meta-analysis model chosen a priori AND Heterogeneity interpretation was based on I2
# Prepare dataset
data_plot1 <- data.frame(data_meta$Meta_model_choice, data_meta$Interpret_heter)
colnames(data_plot1) <- c("Model_choice", "Interpret_I2")
data_plot1[is.na(data_plot1)] <- "Other"

# Prepare mosaic plot
plot1 <- 
  ggplot(data_plot1) +
  geom_mosaic(aes(x = product(Model_choice, Interpret_I2), 
                  fill = Model_choice), # Of those chosen the model a priori ...
              offset = 0.033,
              show.legend = TRUE) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(x = "",#x = expression(bold(paste("Heterogeneity interpreted with ", I^{2}))),
       y = "Model chosen a priori?",
       fill = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

# Add conditional percentages
plot1_new <- plot1 +
  geom_text(data = conditional_perc(plot1), 
            aes(x = (xmin + xmax)/2, 
                y = (ymin + ymax)/2, 
                label = percentage))


## Meta-analysis model chosen a priori AND Heterogeneity interpretation was based on tau2
# Prepare dataset
data_plot2 <- data.frame(data_meta$Meta_model_choice, data_meta$Interpret_tau)
colnames(data_plot2) <- c("Model_choice", "Interpret_tau2")
data_plot2[is.na(data_plot2)] <- "Other"

# Prepare mosaic plot
plot2 <- 
  ggplot(data_plot2) +
  geom_mosaic(aes(x = product(Model_choice, Interpret_tau2), 
                  fill = Model_choice), 
              offset = 0.03,
              show.legend = TRUE) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(x = "", #x = expression(bold(paste("Heterogeneity interpreted with ", tau^{2}))),
       y = "", #y = "Model chosen a priori?",
       fill = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

# Add conditional percentages
plot2_new <- plot2 +
  geom_text(data = conditional_perc(plot2), 
            aes(x = (xmin + xmax)/2, 
                y = (ymin + ymax)/2, 
                label = percentage))


## Choice of meta-analysis model based on I2 AND Heterogeneity interpretation was based on I2
# Prepare dataset
data_plot3 <- data.frame(data_meta$Meta_hetero, data_meta$Interpret_heter)
colnames(data_plot3) <- c("Choice_based_I2", "Interpret_I2")
data_plot3[is.na(data_plot3)] <- "Other"

# Prepare mosaic plot
plot3 <- 
  ggplot(data_plot3) +
  geom_mosaic(aes(x = product(Choice_based_I2, Interpret_I2), 
                  fill = Choice_based_I2), 
              offset = 0.04,
              show.legend = TRUE) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(x = expression(bold(paste("Heterogeneity interpreted with ", I^{2}))),
       y = expression(bold(paste("Model chosen based on ", I^{2}))),
       fill = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

# Add conditional percentages
plot3_new <- plot3 +
  geom_text(data = conditional_perc(plot3), 
            aes(x = (xmin + xmax)/2, 
                y = (ymin + ymax)/2, 
                label = percentage))


## Choice of meta-analysis model based on I2 AND Heterogeneity interpretation was based on tau2
# Prepare dataset
data_plot4 <- data.frame(data_meta$Meta_hetero, data_meta$Interpret_tau)
colnames(data_plot4) <- c("Choice_based_I2", "Interpret_tau2")
data_plot4[is.na(data_plot4)] <- "Other"

# Prepare mosaic plot
plot4 <- 
  ggplot(data_plot4) +
  geom_mosaic(aes(x = product(Choice_based_I2, Interpret_tau2), 
                  fill = Choice_based_I2), 
              offset = 0.03,
              show.legend = TRUE) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(x = expression(bold(paste("Heterogeneity interpreted with ", tau^{2}))),
       y = "", #y = expression(bold(paste("Model chosen based on ", I^{2}))),
       fill = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))

# Add conditional percentages
plot4_new <- plot4 +
  geom_text(data = conditional_perc(plot4), 
            aes(x = (xmin + xmax)/2, 
                y = (ymin + ymax)/2, 
                label = percentage))

## Bring together
tiff("./Figures/Figure 4.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 600)
ggarrange(plot1_new, plot2_new, plot3_new, plot4_new,
          nrow = 2,
          ncol = 2,
          labels = c("a)", "b)", "c)", "d)"),
          common.legend = TRUE,
          legend = "none")
dev.off()
