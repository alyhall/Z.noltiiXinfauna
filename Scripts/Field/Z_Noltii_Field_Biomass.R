#Title and Blurb#################################

#Z. Noltii Field Biomass

#Created- June 2025
#Last Edited - 7 August 2025

#NOTES:#################################

##!!!!!NOTE: As of 7 August 2025 July weights are not included

#Packages#################################



library("ggpubr")
library("car")
library("tidyverse")

#Loading in Data-------
##Lab_Core.csv from shared Z. Noltii google drive (/Raw Data/Field/Field_Ambient_Seagrass -- Sheet Lab_Cores )
ABZ <- read_csv("Data/Field/Lab_Core.csv", na = c("","NA"))
ABZ$Month <- forcats::as_factor(ABZ$Month) #forcats::as_factor to ensure Month order is written as in csv file
 #Unsure why this is needed but doesn't work without it
ABZ <- ABZ %>% 
  mutate(TSC=(TSC*(100/pi))) %>% #Makes TSC Total Shoot Count/m^2 
  mutate(ID=row_number()) %>% #Ensures if any data is missing a unique identifier (such as sample ID and replicate) subsetted data can identify which data point is which 
  rename("SGP"="Biomass_Part", 
         "BDW"="Bio_Dry_Weight_m2") %>%  #Biomass_Part changed to SGP (sea grass part) and sets BDW as Bio Dryweight / m^2
  select(-Processing_Date,
         -Sample_Date, 
         -Sampler,
         -Dry_Weight, 
         -Tin_Weight, 
         -Bio_Dry_Weight, 
         -Date_in_out_of_oven, 
         -Notes) #Removes other columns

## Just to check if loaded properly
print(ABZ)
#Data Subsetting ----------

##Any Subsetted data with Mean and SE used for plots, otherwise used for ANOVA (subsets for ANOVA marked with **)

##Subsets data to only include "Above" (Leaves) 
##**
AboveZ <- ABZ %>%
  filter(SGP=="Above")

#Calculates mean +- SE per each month for Above

AboveZStats <- AboveZ %>% 
  group_by(Month) %>% 
  summarise(mean= mean(BDW), 
            sd = sd(BDW), 
            n=n(), 
            se=sd/sqrt(n)) %>%
  mutate(ID=1) #Same ID number to identify as group for ggplot2::geom_line() to connect to

##Subsets data to only include "Below" (Roots+Rhizome)
##**
BelowZ <- ABZ %>% 
  filter(SGP=="Below")

# calculates mean +- SE per each month for Below

BelowZStats <- BelowZ %>% 
  group_by(Month) %>% 
  summarise(mean= mean(BDW),
            sd = sd(BDW),
            n=n(),
            se=sd/sqrt(n)) %>% 
  mutate(ID=1) 

##Subsets data to only include TSC and calculates mean+- SE per each month

TSCStats <- ABZ %>% 
  group_by(Month) %>% 
  summarise(mean= mean(TSC, na.rm=TRUE),
            sd = sd(TSC, na.rm=TRUE),
            n=n(),
            se=sd/sqrt(n)) %>%  
  mutate(ID=1)




#Subsets data for only April and calculates A:B ratio as a whole

AprilABRT <- subset(ABZ, ABZ$Month == "April") %>% 
  select(-Replicate,
         -Sample_ID,
         -TSC) %>% 
  group_by(SGP)%>%
  mutate(ID=row_number()) %>% 
  pivot_wider(names_from = SGP,
              values_from = BDW) %>%
  ungroup() %>% 
  select(-"NA") %>% #Due to some values of SGP having NA 
  group_by(Month) %>%
  summarise(Above=mean(Above),
            Below=mean(Below)) %>% 
  mutate(TotalBDW= Above+Below,
         ABRat= Above/Below) %>%  #ABRat = A:B 
  select(-Above, 
         -Below) %>% 
  mutate(SampRep = NA)

##subsets data for only April to calculate mean TOTAL Bio Dry Weight and Above:Below Ratio (No SE due to lack of replicate and sample ID)
AprilRTSTATS <- subset(ABZ, ABZ$Month == "April") %>% 
  select(-Replicate, 
         -Sample_ID, 
         -TSC) %>% 
  group_by(SGP)%>%
  mutate(ID=row_number()) %>% 
  pivot_wider(names_from = SGP,
              values_from = BDW) %>%
  ungroup() %>% 
  select(-"NA") %>% #Due to some values of SGP having NA 
  group_by(Month) %>%
  summarise(Above=mean(Above),
            Below=mean(Below)) %>% 
  mutate(TotalBDW= Above+Below,
         ABRat= Above/Below, 
         ID=1) %>%  #ABRat = A:B 
  select(-Above, 
         -Below) %>%
  mutate(TSE=NA,
         RSE=NA) #TSE and RSE are added for .rbind with sub-data set ABRT
              
##subsets data for months OTHER than April and calculates Total Bio Dry WEight & Above:Below Ratio 

NoAprilRT <- subset(ABZ, ABZ$Month != "April") %>% 
  select(-TSC,
         -ID)  %>% 
  pivot_wider( names_from = SGP,
               values_from = BDW) %>% 
  mutate(SampRep= paste(Sample_ID, Replicate)) %>% 
  mutate(TotalBDW=Above+Below,
         ABRat= Above/Below) %>% 
  group_by(Month) %>% 
  select(-Sample_ID, -Replicate, -Above, -Below, )

##subsets data for months OTHER than April and calculates mean Total Bio Dry WEight +-SE & mean Above:Below Ratio  SE per Month

NoAprilStats <- subset(ABZ, ABZ$Month != "April") %>% 
  select(-TSC, -ID)  %>% 
  pivot_wider( names_from = SGP, values_from = BDW) %>% 
  mutate(SampRep= paste(Sample_ID, Replicate)) %>% 
  mutate(TotalBDW=Above+Below,
         ABRat= Above/Below,) %>% 
  group_by(Month) %>% 
  select(-Sample_ID, -Replicate) %>% 
  summarise(TSE=sd(TotalBDW)/sqrt(n()), 
            RSE=sd(ABRat)/sqrt(n()),
            TotalBDW=mean(TotalBDW),
            ABRat= mean(ABRat)) %>% #ABRat=A:B
  mutate(ID=1)

##Binds Data for Ratio and Total Dry weight back together
##**
ABRat <- rbind(AprilABRT, NoAprilRT)

##Binds data for Ratio and Total dry weight with SE together

ABStats <- rbind(AprilRTSTATS, NoAprilStats)



#PLOTS & ANOVA-------

##ABOVE ---------

## plots Monthly means of Leaf Biomass (gm^-2)
ggplot(AboveZStats, aes(x=Month, y = mean, group=ID))+ #group=ID is how geom_line recognizes datapoints are grouped
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Leaf"~(g~m^"-2")))+
  theme_classic()+
  ylim(0,20) 

##initial ANOVA testing : Response = Above_BDW Explanatory = Month
Above_aov <- aov(BDW ~ Month, data=AboveZ)


##Tests for Normalcy
ggqqplot(Above_aov$residuals) 
ggdensity (Above_aov$residuals)
shapiro.test(Above_aov$residuals) 

#Homogeneity of Variance Test 
leveneTest(BDW~Month, data=AboveZ) 

summary(Above_aov) 

TukeyHSD(Above_aov)


##BELOW-------
## plots Monthly Means of Roots + Rhizome Biomass (gm^2)

ggplot(BelowZStats, aes(x=Month, y = mean, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Rhizome & Roots"~(g~m^"-2")))+
  theme_classic()+
  ylim(0,20) 

##Initial ANOVA Testing : Response = Below_BDW, Explanatory = Month
Below_aov <- aov(BDW ~ Month, data= BelowZ)
##Tests for Normalcy
ggqqplot(Below_aov$residuals)
ggdensity(Below_aov$residuals)
shapiro.test(Below_aov$residuals)

##Homogeneity of Variance Test
leveneTest(BDW~Month, data=BelowZ)
##ANOVA results
summary(Below_aov)
#Post-Hoc Tukey test 
TukeyHSD(Below_aov)

##TOTAL SHOOT COUNT----------

##plots Monthly means of TSC (Shoot m^-2)

ggplot(TSCStats, aes(x=Month, y=mean, group = ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Shoot Density"~("Shoot"~m^"-2")))+
  theme_classic()

##Initial ANOVA Testing : Response = TSC Explanatory = Month
TSC_aov <- aov(TSC~Month, data=ABZ)
##Tests for Normalcy
ggqqplot(TSC_aov$residuals) 
ggdensity (TSC_aov$residuals)
shapiro.test(TSC_aov$residuals) #!!Needs July

#Homogeneity of Variance Test 
leveneTest(TSC~Month, data=ABZ) #!!Needs July

##ANOVA results of TSC_aov
summary(TSC_aov)                
#Post-Hoc Tukey test
TukeyHSD(TSC_aov) 


##TOAL DRY WEIGHT----------

##plots monthly means of TotalBDW +- SE
ggplot(ABStats, aes(x=Month, y=TotalBDW, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = TotalBDW+TSE, ymin=TotalBDW-TSE, width=0.2))+ 
  labs(y=bquote(~"Total Weight"~(g~m^"-2")))+
  theme_classic()

##Inital ANOVA Testing Response = Total_BDW, Explanatory = Month 
TotalBDW_aov <- aov(data=ABRat, TotalBDW~Month)
##Tests for Normalcy
ggqqplot(TotalBDW_aov$residuals) 
ggdensity (TotalBDW_aov$residuals)
shapiro.test(TotalBDW_aov$residuals) 

#Homogeneity of Variance Test 
leveneTest(data=ABRat, TotalBDW~Month) 

##ANOVA results of TotalBDW_aov
summary(TotalBDW_aov)
#Post-Hoc Tukey test 
TukeyHSD(TotalBDW_aov)



##ABOVE:BELOW---------

##plots monthly means of Above:Below ratio +- SE

ggplot(ABStats, aes(x=Month, y=ABRat, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = ABRat+RSE, ymin=ABRat-RSE, width=0.2))+
  labs(y="A:B")+
  theme_classic()

##Initial ANOVA Testing Response = ABRat, Explanatory = Month
ABRat_aov <- aov(data=ABRat, ABRat~Month)
##Tests for Normalcy
ggqqplot(ABRat_aov$residuals) 
ggdensity (ABRat_aov$residuals)
shapiro.test(ABRat_aov$residuals) 

#Homogeneity of Variance Test 
leveneTest(data=ABRat, ABRat~Month)
##ANOVA results of ABRat_aov
summary(ABRat_aov)
#Post-Hoc Tukey test 
TukeyHSD(ABRat_aov)



