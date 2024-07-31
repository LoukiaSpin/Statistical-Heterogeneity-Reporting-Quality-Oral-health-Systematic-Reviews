#*******************************************************************************
#*
#*                            Descriptive Statistics                                                       
#*                             (Tabulated results)                              
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2023         
#*******************************************************************************



## Load R packages ----
list.of.packages <- c("gtsummary", "plyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)



## Load data ----
# Systematic review level
load("./data/Dataset_Review level.RData")

# Meta-analysis level
load("./data/Dataset_Meta level.RData")

# Summary level
load("./data/Dataset_Pooled level.RData")



## At systematic review level ----
# Create Table
data_sr[, c("no_authors", "continent", "prospero")] %>% 
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75}) ({min}, {max})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(no_authors ~ "continuous",
                continent ~ "categorical",
                prospero ~ "categorical")) %>%
  italicize_levels()



## At meta-analysis level ----
# Estimator of heterogeneity parameter applied: Add 'Mantel-Haenszel as 'Not reported'
data_meta$estimator_typ <- revalue(factor(data_meta$estimator_typ), c("Mantel-Haenszel" = "Not reported"))

# Collect meta-analysis level variables
meta_level <- 
  data_meta[, c("meta_model", "estimator_typ", "tau ci", "heteror_report", "i2 ci", 
                "est_type", "Meta_model_choice", "choice explained", 
                "Meta_hetero", "Interpret_heter", "Interpret_tau")]

# Create Table
meta_level %>% 
  tbl_summary(
    by = meta_model,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    type = list(estimator_typ ~ "categorical",
                `tau ci` ~ "categorical",
                heteror_report ~ "categorical",
                `i2 ci` ~ "categorical",
                est_type ~ "categorical",
                Meta_model_choice ~ "categorical",
                `choice explained` ~ "categorical",
                Meta_hetero ~ "categorical",
                Interpret_heter ~ "categorical",
                Interpret_tau ~ "categorical"),
    digits = list(all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  italicize_levels() 



## At pooled level ----
table(data_pooled$pool_ci_adjust)
