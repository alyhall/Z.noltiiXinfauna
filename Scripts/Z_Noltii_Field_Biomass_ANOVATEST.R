#################################

#Z. Noltii Field Biomass Stastical Analysis Testing

#JW June 2025

#################################

##See Z_Noltii_Field_Biomass.R for plots

#################################


##Required Packages for Analysis
#install.packages("ggpubr")
library("ggpubr")
library("car")
library("tidyverse")

###
ABZ <- read_csv("Data/Lab_Core.csv", na = c("","NA"))
ABZ$Month <- forcats::as_factor(ABZ$Month) #forcats::as_factor to ensure Month order is written as in csv file
ABZ$Biomass_Part <- forcats::as_factor(ABZ$Biomass_Part) #Unsure why this is needed but doesn't work without it
ABZ <- ABZ %>% 
  mutate(TSC=(TSC*(100/pi))) %>% #Makes TSC Total Shoot Count/m^2 
  mutate(ID=row_number()) %>% #Ensures if any data is missing a unique identifier (such as sample ID and replicate) subsetted data can identify which data point is which 
  rename("SGP"="Biomass_Part", "BDW"="Bio_Dry_Weight_m2") %>%  #Biomass_Part changed to SGP (sea grass part) and sets BDW as Bio Dryweight / m^2
  select(-Processing_Date, -Sample_Date, -Sampler, -Dry_Weight, -Tin_Weight, -Bio_Dry_Weight, -Date_in_out_of_oven, -Notes)


#ABZ filtered only for Above Ground Biomass

AboveZ <- ABZ %>%
  filter(SGP=="Above")

#ABZ filtered only for Below Ground Biomass
BelowZ <- ABZ %>% 
  filter(SGP=="Below")

AprilRT <- subset(ABZ, ABZ$Month == "April") %>% 
  select(-Replicate, -Sample_ID, -TSC) %>% 
  group_by(SGP)%>%
  mutate(ID=row_number()) %>% 
  pivot_wider(names_from = SGP, values_from = BDW) %>%
  ungroup() %>% 
  select(-"NA") %>% #Due to some values of SGP having NA 
  group_by(Month) %>%
  summarise(Above=mean(Above), Below=mean(Below)) %>% 
  mutate(TotalBDW= Above+Below, ABRat= Above/Below) %>%  #ABRat = A:B 
  select(-Above, -Below) %>% 
  mutate(SampRep = NA)

NoAprilRT <- subset(ABZ, ABZ$Month != "April") %>% 
  select(-TSC, -ID)  %>% 
  pivot_wider( names_from = SGP, values_from = BDW) %>% 
  mutate(SampRep= paste(Sample_ID, Replicate)) %>% 
  mutate(TotalBDW=Above+Below, ABRat= Above/Below) %>% 
  group_by(Month) %>% 
  select(-Sample_ID, -Replicate, -Above, -Below, )

TM_ABRat <- rbind(AprilRT, NoAprilRT)

#### 20/6/2025 only has 2 months of data, but for future analysis using ANOVA to reduce coding in future--JW 

##Initial ANOVA Testing : Response = TSC Explanatory = Month
TSC_aov <- aov(TSC~Month, data=ABZ)
    ##Tests for Normalcy
    ggqqplot(TSC_aov$residuals) 
    ggdensity (TSC_aov$residuals)
    shapiro.test(TSC_aov$residuals) 

    #Homogeneity of Variance Test 
    leveneTest(TSC~Month, data=ABZ) 


##initial ANOVA testing : Response = Above_BDW Explanatory = Month
Above_aov <- aov(BDW ~ Month, data=AboveZ)


    ##Tests for Normalcy
    ggqqplot(Above_aov$residuals) 
    ggdensity (Above_aov$residuals)
    shapiro.test(Above_aov$residuals) 

    #Homogeneity of Variance Test 
    leveneTest(BDW~Month, data=AboveZ) 
    



##Initial ANOVA Testing : Response = Below_BDW, Explanatory = Month
Below_aov <- aov(BDW ~ Month, data= BelowZ)
    ##Tests for Normalcy
    ggqqplot(Below_aov$residuals)
    ggdensity(Below_aov$residuals)
    shapiro.test(Below_aov$residuals)

    ##Homogeneity of Variance Test
    leveneTest(BDW~Month, data=BelowZ)
    
    
##Inital ANOVA Testing Response = Total_BDW, Explanatory = Month 
TotalBDW_aov <- aov(data=TM_ABRat, TotalBDW~Month)
    ##Tests for Normalcy
    ggqqplot(TotalBDW_aov$residuals) 
    ggdensity (TotalBDW_aov$residuals)
    shapiro.test(TotalBDW_aov$residuals) 

    #Homogeneity of Variance Test 
    leveneTest(data=TM_ABRat, TotalBDW~Month) 

##Initial ANOVA Testing Response = ABRat, Explanatory = Month
ABRat_aov <- aov(data=TM_ABRat, ABRat~Month)
    ##Tests for Normalcy
    ggqqplot(ABRat_aov$residuals) 
    ggdensity (ABRat_aov$residuals)
    shapiro.test(ABRat_aov$residuals) 

    #Homogeneity of Variance Test 
    leveneTest(data=TM_ABRat, ABRat~Month)
    
##ANOVA results of Above_aov
summary(Above_aov) #### 06/2025- F(1,9) =36.05, p<0.0001 
    #Post-Hoc Tukey test ####For future use
    TukeyHSD(Above_aov)
    
##ANOVA results of Below_aov
summary(Below_aov)
    #Post-Hoc Tukey test ####For future use
    TukeyHSD(Below_aov)
##ANOVA results of TSC_aov
summary(TSC_aov)
    #Post-Hoc Tukey test ####For future use
    TukeyHSD(TSC_aov)
##ANOVA results of TotalBDW_aov
summary(TotalBDW_aov)
    #Post-Hoc Tukey test ####For future use
    TukeyHSD(TotalBDW_aov)

##ANOVA results of ABRat_aov
summary(ABRat_aov)
    #Post-Hoc Tukey test ####For future use
    TukeyHSD(ABRat_aov)
