#======================================================================================================
# CHD and beta-carotene with bias risk assessment: All studies
# Last updated: 19/09/2024
# Author: Chin Yang Shapland
#======================================================================================================

### Functions ###

#devtools::install_github("mcguinlu/robvis")
#devtools::install_github("mcguinlu/triangulate")
library(dplyr)
library(readxl)
library(tibble)
library(robvis)
library(magrittr)
#library(triangulate)
library(metafor)

source("R/plot_bias_direction.R")
source("R/helpers.R")
source("R/helpers-metafor.R")

###############################################
### Creating Data and formatting for robviz ###
###############################################

### load effect size and SE ###
load("Res_MetaAll_beta_carotene_AllCHD_v2.Rdata")

### Merge with RoB ###
RoB <- read_excel("raw/beta_carotene_data.xlsx", sheet="main_data")
colnames(RoB)[2]<-"Author"

Metadta_RoB_CHD<-merge(Metadta_all_CHD, RoB, by="Author")

# format to robviz
colnames(Metadta_RoB_CHD)[which(colnames(Metadta_RoB_CHD)=="id")]<-"result_id"
colnames(Metadta_RoB_CHD)[which(colnames(Metadta_RoB_CHD)=="Study")]<-"type"
colnames(Metadta_RoB_CHD)[which(colnames(Metadta_RoB_CHD)=="Author")]<-"study"
colnames(Metadta_RoB_CHD)[which(colnames(Metadta_RoB_CHD)=="logRR")]<-"yi"
Metadta_RoB_CHD["vi"]<-Metadta_RoB_CHD["se_logRR"]^2

Metadta_RoB_CHD$study<-as.character(Metadta_RoB_CHD$study)
Metadta_RoB_CHD$type<-as.character(Metadta_RoB_CHD$type)
Metadta_RoB_CHD$Design<-as.character(Metadta_RoB_CHD$Design)

dat_CHDbias<-as_tibble(Metadta_RoB_CHD)

#check using triangulate
triangulate::tri_dat_check(dat_CHDbias)

save(dat_CHDbias, file="RoB_betaCarotene_CHD_v3.Rdata")

###########################################
### Visualisation of Directions of bias ###
###########################################

load(paste("RoB_betaCarotene_CHD_v3.Rdata", sep=""))

### change domains that doesn't exists in RoB to "None"
dat_CHDbias[is.na(dat_CHDbias)]<-"None"

# convert data to absolute directions of bias/indirectness.
dat_CHDbias_rob <- dat_CHDbias %>%
  tri_to_long() %>%
  tri_absolute_direction() %>%
  tri_to_wide()

#####################################
### Triangulate: correct for bias ###
#####################################

### load data ###
load(paste("RoB_betaCarotene_CHD_v3.Rdata", sep=""))

### Indirectness ###
# Creating all as missing

dat_CHDind<-dat_CHDbias %>% select(result_id,study,type,yi,vi)

for (j in paste0("d",1:3)) {
  #j<-"d1"
  dat_CHDind[,paste0(j,"j")] <- rep("None", nrow(dat_CHDind))
  dat_CHDind[,paste0(j,"t")] <- rep("None", nrow(dat_CHDind))
  dat_CHDind[,paste0(j,"d")] <- rep("None", nrow(dat_CHDind))
}

dat_CHDind <- dat_CHDind %>%
  tri_to_long() %>%
  tri_absolute_direction() %>%
  tri_append_indirect(triangulate::dat_ind_values)

### Bias from above ###

### add critical to prior ###
custom_bias_priors <- triangulate::dat_bias_values %>%
  add_row(domain = 'all',
          j = 'critical',
          bias_m_add = 0.36,
          bias_v_add = 0.2,
          bias_m_prop = 0.12,
          bias_v_prop = 0.064)

dat_CHDbias_prep <- dat_CHDbias %>%
  tri_to_long() %>%
  tri_absolute_direction() %>%
  tri_append_bias(custom_bias_priors)

datCHD_final <- tri_prep_data(dat_CHDbias_prep, dat_CHDind)

### Plot adjusted with robviz
dat_CHDbias_final<-dat_CHDbias_rob %>% right_join(datCHD_final[c("result_id","yi_adj", "vi_adj")], by="result_id")

# png(file = 'Figure1.png', # Save plot as PNG
#     width=850,
#     height=620)
# dat_CHDbias_final %>%
#   rob_direction(ov)
# dev.off()

plot_data <- left_join(dat_CHDbias, datCHD_final[, c("result_id", "yi_adj", "vi_adj")])
tri_plot_bias_direction(plot_data)


