#*******************************************************************************
#*
#*                               Create Figure 4
#*                             (Stacked bar plots) 
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2024         
#*******************************************************************************



## Load R packages ----
list.of.packages <- c("plyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



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

# Calculate % conditionally on 'Model_choice'
data_plot1_new <- 
  data_plot1 %>%
  count() %>%
  group_by(Model_choice) %>%
  mutate(perc = freq / sum(freq))

# Prepare stacked bar plot
plot1 <- 
  ggplot(data_plot1_new,
         aes(x = Model_choice,
             y = perc,
             fill = Interpret_I2)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = Model_choice,
                y = perc,
                group = Interpret_I2,
                label = ifelse(perc > 0.04, paste0(round(perc * 100, 0), "% (", freq,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.0,
            position = "stack",
            colour = "white") +
  labs(x = "Model chosen a priori?",
       y = "Percentage meta-analyses (%)",
       fill = expression(bold(paste("Heterogeneity interpreted with ", I^{2})))) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))
  


## Meta-analysis model chosen a priori AND Heterogeneity interpretation was based on tau2
# Prepare dataset
data_plot2 <- data.frame(data_meta$Meta_model_choice, data_meta$Interpret_tau)
colnames(data_plot2) <- c("Model_choice", "Interpret_tau2")

# Calculate % conditionally on 'Model_choice'
data_plot2_new <- 
  data_plot2 %>%
  count() %>%
  group_by(Model_choice) %>%
  mutate(perc = freq / sum(freq))

# Prepare stacked bar plot
plot2 <- 
  ggplot(data_plot2_new,
         aes(x = Model_choice,
             y = perc,
             fill = Interpret_tau2)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = Model_choice,
                y = perc,
                group = Interpret_tau2,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", freq,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.0,
            position = "stack",
            colour = "white") +
  labs(x = "Model chosen a priori?",
       y = " ",
       fill = expression(bold(paste("Heterogeneity interpreted with ", tau^{2})))) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))



## Choice of meta-analysis model based on I2 AND Heterogeneity interpretation was based on I2
# Prepare dataset
data_plot3 <- data.frame(data_meta$Meta_hetero, data_meta$Interpret_heter)
colnames(data_plot3) <- c("Choice_based_I2", "Interpret_I2")

# Calculate % conditionally on 'Choice_based_I2'
data_plot3_new <- 
  data_plot3 %>%
  count() %>%
  group_by(Choice_based_I2) %>%
  mutate(perc = freq / sum(freq))

# Prepare stacked bar plot
plot3 <- 
  ggplot(data_plot3_new,
         aes(x = Choice_based_I2,
             y = perc,
             fill = Interpret_I2)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = Choice_based_I2,
                y = perc,
                group = Interpret_I2,
                label = ifelse(perc > 0.04, paste0(round(perc * 100, 0), "% (", freq,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.0,
            position = "stack",
            colour = "white") +
  labs(x = expression(bold(paste("Model chosen based on ", I^{2}))),
       y = "Percentage meta-analyses (%)",
       fill = expression(bold(paste("Heterogeneity interpreted with ", I^{2})))) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))



## Choice of meta-analysis model based on I2 AND Heterogeneity interpretation was based on tau2
# Prepare dataset
data_plot4 <- data.frame(data_meta$Meta_hetero, data_meta$Interpret_tau)
colnames(data_plot4) <- c("Choice_based_I2", "Interpret_tau2")

# Calculate % conditionally on 'Choice_based_I2'
data_plot4_new <- 
  data_plot4 %>%
  count() %>%
  group_by(Choice_based_I2) %>%
  mutate(perc = freq / sum(freq))

# Prepare mosaic plot
plot4 <- 
  ggplot(data_plot4_new,
         aes(x = Choice_based_I2,
             y = perc,
             fill = Interpret_tau2)) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(x = Choice_based_I2,
                y = perc,
                group = Interpret_tau2,
                label = ifelse(perc != 0, paste0(round(perc * 100, 0), "% (", freq,")"), " ")),
            hjust = 0.5,
            vjust = 1.0,
            size = 4.0,
            position = "stack",
            colour = "white") +
  labs(x = expression(bold(paste("Model chosen based on ", I^{2}))),
       y = " ",
       fill = expression(bold(paste("Heterogeneity interpreted with ", tau^{2})))) +
  scale_fill_manual(breaks = c("Yes", "No", "Other"),
                    values = c("#1B9E77", "#D95F02", "#7570B3")) +
  scale_y_continuous(labels = scales::label_percent(suffix = " ")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"))



## Bring together those sharing the shame legend
# Heterogeneity interpreted with I2
plots13 <- 
  ggarrange(plot1, plot3,
            nrow = 2,
            labels = c("a)", "c)"),
            common.legend = TRUE,
            legend = "bottom")

# Heterogeneity interpreted with tau2
plots24 <- 
  ggarrange(plot2, plot4,
            nrow = 2,
            labels = c("b)", "d)"),
            common.legend = TRUE,
            legend = "bottom")



## Bring all together
tiff("./Figures/Figure 4.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 600)
ggarrange(plots13, plots24)
dev.off()
