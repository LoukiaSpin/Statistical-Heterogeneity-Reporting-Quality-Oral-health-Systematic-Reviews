#*******************************************************************************
#*
#*                            Descriptive Statistics                                                       
#*                             (Tabulated results)                              
#*                                                       
#* Author: Loukia Spineli
#* Year: July 2024         
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

# Merge "Insufficient" wirh unclear in the following variables in a new 'version' (_new)
data_meta$Meta_model_choice_new <- revalue(factor(data_meta$Meta_model_choice), c("Insufficient" = "Unclear"))
data_meta$choice_explained_new <- revalue(factor(data_meta$`choice explained`), c("Insufficient" = "Unclear"))
data_meta$Meta_hetero_new <- revalue(factor(data_meta$Meta_hetero), c("Insufficient" = "Unclear"))
data_meta$Interpret_heter_new <- revalue(factor(data_meta$Interpret_heter), c("Insufficient" = "Unclear"))

# Collect meta-analysis level variables
meta_level <- 
  data_meta[, c("meta_model", "estimator_typ", "tau ci", "heteror_report", "i2 ci", 
                "est_type", "Meta_model_choice_new", "choice_explained_new", 
                "Meta_hetero_new", "Interpret_heter_new", "Interpret_tau")]

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
                Meta_model_choice_new ~ "categorical",
                Meta_hetero_new ~ "categorical",
                Interpret_heter_new ~ "categorical",
                Interpret_tau ~ "categorical"),
    digits = list(all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  italicize_levels() 

# Restrict to meta-analyses that selected the model a prior
subset(meta_level, Meta_model_choice_new == "Yes")[, c(1, 8)] %>% 
  tbl_summary(
    by = meta_model,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    type = list( choice_explained_new ~ "categorical"),
    digits = list(all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  italicize_levels() 


## At pooled level ----
table(data_pooled$pool_ci_adjust)
