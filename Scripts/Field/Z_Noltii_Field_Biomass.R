#Z. Noltii Field Biomass (initial)
#JW June 2025

##Required packages


library(tidyverse)



##Lab_Core.csv from shared Z. Noltii google drive (/Raw Data/Field/Field_Ambient_Seagrass -- Sheet Lab_Cores )
ABZ <- read_csv("Data/Field/Lab_Core.csv", na = c("","NA"))
ABZ$Month <- forcats::as_factor(ABZ$Month) #forcats::as_factor to ensure Month order is written as in csv file
 #Unsure why this is needed but doesn't work without it
ABZ <- ABZ %>% 
  mutate(TSC=(TSC*(100/pi))) %>% #Makes TSC Total Shoot Count/m^2 
  mutate(ID=row_number()) %>% #Ensures if any data is missing a unique identifier (such as sample ID and replicate) subsetted data can identify which data point is which 
  rename("SGP"="Biomass_Part", "BDW"="Bio_Dry_Weight_m2") %>%  #Biomass_Part changed to SGP (sea grass part) and sets BDW as Bio Dryweight / m^2
  select(-Processing_Date, -Sample_Date, -Sampler, -Dry_Weight, -Tin_Weight, -Bio_Dry_Weight, -Date_in_out_of_oven, -Notes)

## Just to check if loaded properly
print(ABZ)

##Subsets data to only include "Above" (Leaves) and calculates mean +- SE per each month
AboveZStats <- ABZ %>% 
  filter(SGP =="Above") %>% 
  group_by(Month) %>% 
  summarise(mean= mean(BDW), sd = sd(BDW), n=n(), se=sd/sqrt(n)) %>%
  mutate(ID=1) #Same ID number to identify as group for ggplot2::geom_line() to connect to



##Subsets data to only include "Below" (Roots+Rhizome) and calculates mean +- SE per each month
BelowZStats <- ABZ %>% 
  filter(SGP =="Below") %>% 
  group_by(Month) %>% 
  summarise(mean= mean(BDW), sd = sd(BDW), n=n(), se=sd/sqrt(n)) %>% 
  mutate(ID=1) #same as Above


##Subsets data to only include TSC and calculates mean+- SE per each month
TSCStats <- ABZ %>% 
  group_by(Month) %>% 
  summarise(mean= mean(TSC, na.rm=TRUE), sd = sd(TSC, na.rm=TRUE), n=n(), se=sd/sqrt(n)) %>%  
  mutate(ID=1)



## plots Monthly means of Leaf Biomass (gm^-2)
ggplot(AboveZStats, aes(x=Month, y = mean, group=ID))+ #group=ID is how geom_line recognizes datapoints are grouped
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Leaf"~(g~m^"-2")))+
  theme_classic()+
  ylim(0,20) #CHANGE AS NEEDED ylim should be set to be equal to Below -JW June 2025

## plots Monthly Means of Roots + Rhizome Biomass (gm^2)
ggplot(BelowZStats, aes(x=Month, y = mean, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Rhizome & Roots"~(g~m^"-2")))+
  theme_classic()+
  ylim(0,20) #CHANGE AS NEEDED ylim should be set to be equal to Above

##plots Monthly means of TSC (Shoot m^-2)

ggplot(TSCStats, aes(x=Month, y=mean, group = ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se, width=0.2))+
  labs(y=bquote(~"Shoot Density"~("Shoot"~m^"-2")))+
  theme_classic()


##subsets data for only April to calculate mean TOTAL Bio Dry Weight and Above:Below Ratio (No SE due to lack of replicate and sample ID)
AprilABRT <- subset(ABZ, ABZ$Month == "April") %>% 
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
  mutate(TSE=NA, RSE=NA) #TSE and RSE are added for .rbind with sub-data set ABRT (see lines 102)

##subsets data for months OTHER than April and calculates mean Total Bio Dry WEight +-SE & mean Above:Below Ratio +- SE per Month
ABRT_1 <- subset(ABZ, ABZ$Month != "April") %>% 
  select(-TSC, -ID)  %>% 
  pivot_wider( names_from = SGP, values_from = BDW) %>% 
  mutate(SampRep= paste(Sample_ID, Replicate)) %>% 
  mutate(TotalBDW=Above+Below, ABRat= Above/Below) %>% 
  group_by(Month) %>% 
  select(-Sample_ID, -Replicate) %>% 
  summarise(TSE=sd(TotalBDW)/sqrt(n()), RSE=sd(ABRat)/sqrt(n()), TotalBDW=mean(TotalBDW), ABRat= mean(ABRat) ) #ABRat=A:B


##Combines AprilABRT and ABRT_1 for monthly means analysis
ABRT <- rbind(ABRT_1, AprilABRT) %>% mutate(ID=1)


##plots monthly means of TotalBDW +- SE
ggplot(ABRT, aes(x=Month, y=TotalBDW, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = TotalBDW+TSE, ymin=TotalBDW-TSE, width=0.2))+ 
  labs(y=bquote(~"Total Weight"~(g~m^"-2")))+
  theme_classic()



##plots monthly means of Above:Below ratio +- SE
ggplot(ABRT, aes(x=Month, y=ABRat, group=ID))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = ABRat+RSE, ymin=ABRat-RSE, width=0.2))+
  labs(y="A:B")+
  theme_classic()




