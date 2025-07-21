#-----------------------------------------------------------------------------------------------------------------
# CHD and beta-carotene: extraction of effect estimate and relevance (converting to common metric)
# Last updated: 04/09/2023
#-----------------------------------------------------------------------------------------------------------------

### Functions ###
library(tidyverse)
library("tidyr")
#library(dbplyr)
library(reshape2)
library(data.table)
library(metafor)

##########################
### Creating datapoint ###
##########################
# For observational studies directly from Aune's paper Figure 8
# For RCT studies, directly from Supplementary Figure 18 for total CHD risk and Supplementary Figure 22 for CHD mortality risk

### Diet ###
CHD_diet<-data.frame(Author=c("Osganian", "Klipstein-Grobusch", "Todd", "Pandey"),
                     RR=c(0.83, 0.22, 0.87, 0.84),
                     RR_LC=c(0.72, 0.07, 0.68, 0.66),
                     RR_UC=c(0.96, 0.68, 1.12, 1.09),
                     Study=rep("Obs", 4), Design=rep("Diet", 4), Dose=rep(1, 4))

### Circulatory ###
#Aune's results is per 25 ug/dL
sd_circ<-sqrt(78)*(((0.48-0.46)/0.01863)/3.92)
sd_diet<-sqrt(67)*((4465.5-3383.8)/3.92)
conv_factor<-0.27*(sd_diet/sd_circ)

CHD_circ<-data.frame(Author=c("Karppi", "Koh", "Hak"),
                     RR=c(0.64, 0.95, 0.82),
                     RR_LC=c(0.45, 0.66, 0.63),
                     RR_UC=c(0.93, 1.37, 1.07),
                     Study=rep("Obs", 3), Design=rep("Circ", 3), Dose=rep(25*conv_factor, 3)) %>%
  mutate(RR=exp((5000/Dose)*log(RR)), RR_LC=exp((5000/Dose)*log(RR_LC)),
         RR_UC=exp((5000/Dose)*log(RR_UC)), Dose=Dose^0) # convert RR to per 5000 ug/d

### RCT ###
CHD_rct<-data.frame(Author=c("Tornwall", "Cook", "Hennekens"),
                    RR=c(1.03, 1.00, 0.96),
                    RR_LC=c(0.92, 0.89, 0.85),
                    RR_UC=c(1.16, 1.13, 1.08),
                    Study=rep("RCT", 3), Design=rep("RCT", 3), Dose=c(20000, 25000, 25000)) %>%
  mutate(RR=exp(log(RR)/(Dose/5000)), RR_LC=exp(log(RR_LC)/(Dose/5000)),
         RR_UC=exp(log(RR_UC)/(Dose/5000)), Dose=Dose^0) # convert RR to per 5000 ug/d

### MR ###
# the conversion of per log transformed to per 1 unit of beta-carotene
# is based on Rodr?guez_Barranco2017
# conv_factor is based on µmol/L not µg/L
sd_circ<-sqrt(78)*((((0.48-0.46)*10)/0.01863)/3.92)
sd_diet<-sqrt(67)*((4465.5-3383.8)/3.92)
conv_factor<-0.27*(sd_diet/sd_circ)

CHD_MR<-data.frame(Author=c("CARDIoGRAMplusC4D", "UKBB", "FinnGen"),
                   RR=c(exp((5000/conv_factor)*(log(1.054)/301)),
                        exp((5000/conv_factor)*(log(1.015)/301)),
                        exp((5000/conv_factor)*(log(1.027)/301))),
                   RR_LC=c(exp((5000/conv_factor)*(log(0.948)/301)),
                           exp((5000/conv_factor)*(log(0.925)/301)),
                           exp((5000/conv_factor)*(log(0.840)/301))),
                   RR_UC=c(exp((5000/conv_factor)*(log(1.173)/301)),
                           exp((5000/conv_factor)*(log(1.114)/301)),
                           exp((5000/conv_factor)*(log(1.255)/301))),
                   Study=rep("MR", 3), Design=rep("Circ", 3), Dose=rep(1, 3))


### Combine all ###
Metadta_all_CHD<-CHD_diet %>%
  rbind(CHD_circ,CHD_rct, CHD_MR) %>%
  mutate(logRR=log(RR), se_logRR=(log(RR_UC)-log(RR_LC))/3.92, id=1:13, Outcome="CHD") %>%
  select(Author,Study, id,Dose, logRR, se_logRR, Design, Outcome)

#save(Metadta_all_CHD, file="Res_MetaAll_beta_carotene_AllCHD_v2.Rdata")
