#*******************************************************************************
#*
#*                               Create Figure 3 
#*                      (Box-dot plots on I2 & tau2 values)                      
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2024         
#*******************************************************************************



## Load R packages ----
list.of.packages <- c("plyr", "ggplot2", "reshape2", "scales", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load data ----
# Meta-analysis level
load("./data/Dataset_Meta level.RData")



## Boxplot with jitter points for I2 and tau2 estimates
# Keep only the necessary columns
data_i2tau <- data_meta[, c("i2 %", "i2 ci lb", "i2 ci ub", "tau", "tau ci lb", "tau ci ub")]

# Turn into long
data_i2tau_long0 <- melt(data_i2tau)
data_i2tau_long0$type <- rep(rep(c("Estimate", "Lower bound", "Upper bound"), each = dim(data_i2tau)[1]), 2)
data_i2tau_long0$method <- rep(c("I2", "tau"), each = dim(data_i2tau)[1] * (dim(data_i2tau)[2] / 2))
data_i2tau_long <- data_i2tau_long0[complete.cases(data_i2tau_long0), ]

# Create for I2
fig_i2 <- ggplot(subset(data_i2tau_long, method == "I2"), 
                 aes(x = type, 
                     y = value, 
                     color = type)) +
  geom_boxplot(data = subset(data_i2tau_long, method == "I2" & type == "Estimate"),
               aes(x = type, 
                   y = value + 0.001, 
                   color = type),
               size = 1, 
               outlier.shape = NA,
               inherit.aes = FALSE) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 2.8,
             alpha = .4) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(y = "", 
       x = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  ggtitle(expression(paste(I^{2}, " statistic"))) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold.italic"))

# Create for tau2
fig_tau2 <- ggplot(subset(data_i2tau_long, method == "tau"), 
                   aes(x = type, 
                       y = value + 0.001, 
                       color = type)) +
  geom_boxplot(data = subset(data_i2tau_long, method == "tau" & type == "Estimate"),
               aes(x = type, 
                   y = value + 0.001, 
                   color = type),
               size = 1, 
               outlier.shape = NA,
               inherit.aes = FALSE) +
  geom_point(position = position_jitterdodge(jitter.height = 0.5),
             size = 2.8,
             alpha = .4) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(y = "", 
       x = "") +
  scale_y_continuous(trans = log_trans(),
                     labels = scales::comma) +
  ggtitle(expression(paste(tau^{2}, " statistic"))) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 15, face = "bold.italic"),
        plot.caption = element_text(size = 12, hjust = 0.01)) 

# Bring together
tiff("./Figures/Figure 3.tiff", 
     height = 25, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 600)
ggarrange(fig_i2, fig_tau2)
dev.off()


## Summary statistics
# Number of meta-analyses reporting the estimated I2
dim(subset(data_i2tau_long, type == "Estimate" & method == "I2"))[1] # 308 out of 313

# Summary results for I2
summary(subset(data_i2tau_long, type == "Estimate" & method == "I2")[, 2])

# zero I2
dim(subset(data_i2tau_long, type == "Estimate" & value == 0 & method == "I2"))[1] # 43 out of 308

# 100% I2
dim(subset(data_i2tau_long, type == "Estimate" & value == 100 & method == "I2"))[1] # 3 out of 308

# Number of meta-analysis reporting the estimated tau2
dim(subset(data_i2tau_long, type == "Estimate" & method == "tau"))[1] # 160 out of 278

# Summary results for tau2
summary(subset(data_i2tau_long, type == "Estimate" & method == "tau")[, 2])

# zero tau2
dim(subset(data_i2tau_long, type == "Estimate" & value == 0 & method == "tau"))[1] # 20 out of 160

# Heterogeneity estimators and zero tau2
heter_est_0_tau2 <- factor(data_meta$estimator_typ[which(data_i2tau$tau == 0)], 
                                   levels = c("DerSimonian and Laird", 
                                              "Restricted maximum likelihood", 
                                              "Paule and Mandel", 
                                              "Mantel-Haenszel",
                                              "Not reported"))
table(heter_est_0_tau2)
