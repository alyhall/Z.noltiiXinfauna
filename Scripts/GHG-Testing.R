######################################
##
##
## Purpose: Stastically analyzing where variation across Mesocosm Treatments, Shade Percentage, and Replicates exist (possibly incorrectly)
##
## Dataset and beginning exploration from GHG_Explore.R by Alyson Hall 2025
##
## Data created: 21-06-2025 - JW
## 
##



##Required Packages & Loading Datasets-----------
library(car)
library(ggpubr)
library(tidyverse)
library(rstatix)
##Dataset Gasflux2 from GHG_Explore.R by Alyson Hall 2025

Gasflux2 <- read.csv("Data/Gasflux2.csv")

Gasflux2 <- Gasflux2 %>% 
  mutate(Replicate = as.factor(Replicate))

##ANOVA Gasflux explained by Mesocosm Treatment-------

#Testing for Homogeneity of Variance 
leveneTest(CO2 ~ Mesocosm_Treatment, data=Gasflux2) # non-equal variance (nev) F(3,7342) = 371.34 p<0.001 :(
leveneTest(CO2flux ~ Mesocosm_Treatment, data=Gasflux2) # nev F(3,7342) = 21.67 p<0.001
leveneTest(CH4~ Mesocosm_Treatment, data=Gasflux2) #nev F(3,7342) = 398.12 p<0.001
leveneTest(CH4flux ~ Mesocosm_Treatment, data=Gasflux2) #nev F(3,7342) = 12.49 p<0.001


Gasflux2 %>%
  welch_anova_test(CO2 ~ Mesocosm_Treatment) %>%
  


##ANOVA, Mean Gasflux explained by Replicate per Mesocosm Treatment------




##Subsets for only High Replicate
HighFD_Gasflux <-
  Gasflux2%>% 
  filter(Mesocosm_Treatment=="HIGH")
  
leveneTest(CO2 ~ Replicate, data=HighFD_Gasflux) #nev F(4,1836)=1634.6 p<0.001 :(
leveneTest(CO2flux ~Replicate, data=HighFD_Gasflux) #nev F(4, 1836) = 3.57 p<0.001 :(
leveneTest(CH4~ Replicate, data=HighFD_Gasflux) #nev F(3,1836) = 380.13 p<0.001



leveneTest(CH4flux ~ Replicate, data=HighFD_Gasflux) #Homogeneity of variance  F(4, 1836) = 1.247 p>0.05

 