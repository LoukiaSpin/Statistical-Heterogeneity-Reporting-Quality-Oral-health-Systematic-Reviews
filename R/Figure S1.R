#*******************************************************************************
#*
#*                        Create Supplementary Figure S1                                                                                                                                 
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2024         
#*******************************************************************************



## Load R packages ----
list.of.packages <- c("dplyr", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load data ----
# Systematic review level
load("./data/Dataset_Review level.RData")



## Bubble plot: Year by Journal ----
# Cross-tabulation frequencies
table_bubble <- data.frame(data_sr$year_sr, data_sr$journal) %>% 
  count(data_sr$year_sr, data_sr$journal) 
colnames(table_bubble) <- c("Year", "Journal", "Freq")

# Frequencies per Journal
table_total <- data.frame(data_sr$year_sr, data_sr$journal) %>% 
  count(data_sr$journal)

# Prepare dataset for ggplot2 
data_bubble <- rbind(data.frame(Year = rep("Total", dim(table_total)[1]), 
                                Journal = table_total[, 1],
                                Freq = table_total[, 2]),
                     table_bubble)

# Add radius as new variable to 'data_bubble'
data_bubble$radius <- sqrt(data_bubble$Freq/pi)

# Create bubble plot
tiff("./Figures/Figure S1.tiff", 
     height = 30, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 600)
ggplot(data_bubble, 
       aes(x = Year, 
           y = factor(Journal, levels = rev(unique(Journal))))) + 
  geom_point(aes(size = radius * 5.3), 
             shape = 21, 
             fill = "white", 
             color = "black",
             stroke = 1.2,
             alpha = 0.5) +
  geom_text(aes(label = Freq), 
            size = 4, #3.8, 
            color="black",
            fontface = "bold") +
  labs(x = "Publication year",
       y = "Journals") + 
  scale_size_identity() +
  theme_classic() +
  theme(panel.grid.major = element_line(linetype = 2, color = "grey"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        legend.position = "none")
dev.off()

