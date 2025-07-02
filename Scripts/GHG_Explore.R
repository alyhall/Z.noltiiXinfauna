## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Explore GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-06-12
## Date Edited: 2025-06-27
## Lat Edited By: AH
## 
##
## Copyright (c) Alyson Hall, 2025 
## Email: achall@vims.edu
##
##
## Notes:
##
## Licore time was behind real time by 50 s
## Chamber Low 4, 100 shadig didn't get 4 mins
## Found out High 1 Shaded 100% had wrong time input from 11:38 - 11:32 corrected it to 11:42 

## Packages Needed

library(tidyverse)
library(readxl)
library(fuzzyjoin)

## Load in Data ----------------------------

GHG <- read_excel("Data/GHG_2025-06-12.xlsx", 
                  na = "nan", skip = 4) %>%
              {
                colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
                . <- .[-c(1, 2), ]  # Remove both the header row and units row
                . 
                } %>% 
          select(TIME:LASER_T) %>%  # Retain only columns I want
          mutate(TIME = hms::as_hms(TIME),
                 TIME = hms::as_hms(TIME - 50), # Licore was ahead by 50 s
                 CO2 = as.numeric(CO2),
                 CH4 = as.numeric(CH4)) 


# Read in measurements from data sheet

Measure <- read_excel("Data/GHG_2025-06-12.xlsx", sheet = 2) %>% 
                mutate(Start_Time = hms::as_hms(Start_Time),
                       End_Time = hms::as_hms(End_Time),
                       Shade_Per = as.factor(Shade_Per),
                       Replicate = as.factor(Replicate))


Light <- read_excel("Data/Mesocosm_lightfield.xlsx") %>% 
            mutate(Pot_Location = as.numeric(Pot_Location),
                   Water = as.numeric(Water))


# Combine based on run time

Combo <- 
    fuzzy_left_join(
      GHG,
      Measure,
      by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
      match_fun = list(`>=`, `<=`)) 


Gasflux <- 
  Combo %>% 
      mutate(Dur = difftime(Combo$TIME, Combo$Start_Time)) %>% # Create a column for runtime 
      filter(!Dur == "NA") # Remove the NA (between measurements and before machine started)

Gasflux <-
  Gasflux %>% 
      left_join(Light %>% 
                  select(c("Pot_Location", "Water")), 
                  by = c("Current_Location" = "Pot_Location"))


## Calculations ----------------------------------------------

# get flux per s for each treatment

Fluxes <- 
  Gasflux %>%
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
  arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
  # Generating fluxes for gar exchange per s 
  mutate(CO2flux = (as.numeric(CO2) - lag(as.numeric(CO2), n = 1)),
         CH4flux = (as.numeric(CH4) - lag(as.numeric(CH4), n = 1)),
         CO2rate = (CO2flux*60*60)/ (314.159 / 1e4)) 



# This is a little fine detail, maybe instead we do the slope over the whole incubation...

Slope <- Gasflux %>%
  filter(Dur < 241) %>% 
  filter(Dur > 60) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
  summarise(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    .groups = "drop"
  ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2flux = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4flux = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60   # slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  # Now our units are in ppm CO2 per hour per 20 cm2, Our pots have 314.159cm2
  mutate(CO2flux = CO2flux/ (314.159 / 1e4),  
         CH4flux = CH4flux/ (314.159 / 1e4))  %>% 
  unique()
  # Now our units are in ppm CO2 per hour per m2

Slope <- Gasflux2 %>% 
  filter(Dur > 100, Dur < 241) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  summarise(
    CO2_slope = coef(lm(CO2 ~ Dur))[2],   # β₁ for CO₂
    CH4_slope = coef(lm(CH4 ~ Dur))[2],   # β₁ for CH₄
    Volume    = first(Volume),            # keep Volume (or any other static columns)
    .groups   = "drop"
  ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2flux = CO2_slope*60*60,  # slope of CO2 ~ hour
    CH4flux = CH4_slope*60*60   # slope of CH4 ~ hour
  ) %>%
  # Now our units are in ppm CO2 per hour per 20 cm2, Our pots have 314.159cm2
  mutate(CO2flux = CO2flux/ (314.159 / 1e4),  
         CH4flux = CH4flux/ (314.159 / 1e4))  %>% 
  unique()


# Average

Slope %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2flux, color = Shade_Per)) +
  geom_violin() +
  facet_wrap(~Shade_Per, scales = "free_y") +
  theme_bw()

Slope %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CH4flux, color = Shade_Per)) +
  geom_violin() +
  facet_wrap(~Shade_Per, scales = "free_y") +
  theme_bw()

## Data Exploration ---------------------------------

# Example of medium treatments 

Gasflux %>% 
  filter(Dur < 241) %>%
  # filter(Dur > 120) %>%
  filter(Replicate == "3") %>% 
  filter(Mesocosm_Treatment == "CON") %>% 
  filter(Shade_Per == 0) %>% 
  ggplot(aes( x = Dur, y = CO2, color = Shade_Per)) +
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y") +
  theme_bw()

test <-
Gasflux %>% 
  filter(Mesocosm_Treatment == "LOW") %>%
  filter(Replicate == "4")

Gasflux %>% 
  filter(Dur < 241) %>%
  filter(Dur > 120) %>%
  ggplot(aes( x = Mesocosm_Treatment, y = CO2, fill = Shade_Per)) +
  geom_violin() +
  theme_bw()

Gasflux %>% 
  filter(Dur < 241) %>% 
  filter(Dur > 120) %>% 
  ggplot(aes( x = Dur, y = CO2, color = Mesocosm_Treatment)) +
  geom_point() +
  facet_wrap(~Shade_Per, scales = "free_y") +
  theme_bw()

## JW Sandbox--------

#Creates csv from Fluxes dataset with Duration cutoff point based on settling point (See chunk Visualizing Variation 179-219)
#Fluxes %>% 
  #filter(Dur > 60) %>% 
  #mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  #write.csv("Gasflux2.csv") #utilized in GHG-Testing.R


Gasflux2 <- read.csv("Data/Gasflux2.csv") #See Notes below
#manually added different Functional Diversity Metrics, from Mesocosm_t0_Functional_Diversity
#Volume (dm^3 or L) calculations: Air_Height(dm)*r^2*pi 
#Air_Height(dm)= Height_Sediment_Bucket - Height_Sediment_Water
#!!! R = 17.5cm Total H = 39cm V=37.5224 L
#

##Using Ideal Gas Law to estimate micromoles of CO2 and nanomoles of CH4 within closed system
#Pressure Temperature acquired from Cavity Pressure and Temperature changed to Atmo and Kelvin respectively 
# CO2m = micromoles/L of CO2, CH4m = nanomoles of CH4/L

# Small edits by AHall for units
Gasflux2 <- 
  Gasflux2 %>% 
  mutate(Shade_Per= as.factor(Shade_Per), Replicate= as.factor(Replicate)) %>% 
  mutate(Mole= ((Pressure*Volume)/(0.08205*Temperature)), CO2m=(CO2*Mole)/Volume, CH4m=(CH4*Mole)/Volume) %>% 
  #Units of CO2m and CH4m 
  #From Chunk 82
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
  arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
  mutate(CO2flux2 = (as.numeric(CO2m) - lag(as.numeric(CO2m), n = 1)),
         CH4flux2 = (as.numeric(CH4m) - lag(as.numeric(CH4m), n = 1)))  %>%
  # Now putting in units of per hour per m2
  mutate(CO2flux2 = (CO2flux2*60*60) / (314.159 / 1e4),
         CH4flux2 = (CH4flux2*60*60) / (314.159 / 1e4))
  


###-Visualizing Variation-----


#purpose is to find outliers within Treatments


Gasflux2 %>%  #Identifies outliers? among Mesocosm treatments which 
  mutate(sdCO2= sd(CO2m), sdCH4 = sd(CH4m)) %>% 
  group_by(Mesocosm_Treatment, Replicate) %>% 
  mutate(MeanCO2= mean(CO2m), MeanCH4 = mean(CH4m)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Replicate, y=MeanCO2))+
  geom_point()+
  geom_errorbar(aes(ymax=MeanCO2+sdCO2, ymin=MeanCO2-sdCO2))+
  facet_wrap(~Mesocosm_Treatment)
 

  
Gasflux2 %>% 
  mutate(Replicate= as.factor(Replicate), MicroCO2 = (MicroCO2/Volume)) %>% 
  
  ggplot(aes(y=MicroCO2, x=Replicate))+
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)
  


  #Visualizing variation of CO2 Flux per each run to look for a good settling point Approximately 60s
Fluxes %>% 
  ggplot(aes(x=Replicate, y=CO2flux, color=Shade_Per))+  
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)+
  theme_classic() #Variation in change in CO2 flux between each second ranges around 500 ppm without any cutoff point
  
Fluxes %>%  
  filter(Dur>60) %>%
  ggplot(aes(x=Replicate, y=CO2flux, color=Shade_Per))+ 
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)+
  theme_classic()  #at 60s variation of CO2 flux between each second ranges from -3 -5 ppm 

#Visualizing variation of CH4 Flux for each run to look for a good settling point #Ch4 has odd variation Outliers? Con 5 Low 4 
Fluxes %>% 
  ggplot(aes(x=Replicate, y=CH4flux, color=Shade_Per))+  
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)+
  theme_classic()

Fluxes %>% 
  filter(Dur>60) %>% 
  ggplot(aes(x=Replicate, y=CH4flux, color=Shade_Per))+ 
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)+
  theme_classic()


###Par & Epiphytes Exploration-------------
## To explore if previous PAR may have a reason for some variation 



Gasflux2 %>% 
  ggplot(aes(x=as.factor(Current_Location), y=CO2, color=Mesocosm_Treatment))+
  geom_point()

Gasflux2 %>%
  filter(Dur>60) %>% 
  
  ggplot(aes(x=Water, y=CO2, color=Mesocosm_Treatment))+ 
  geom_point()+
  geom_line() #Water = PAR reading within Mesocosm at previous location

Gasflux2 %>%
  filter(Dur>60) %>% 
  ggplot(aes(x=Water, y=CH4m, color=Mesocosm_Treatment))+  
  geom_point()+
  geom_line()

Gasflux %>%  #attempted to see if EPIPHYTES could have caused variation in CO2
  filter(Dur>60 ) %>% 
  mutate(Epiphytes=Epi+GA) %>%  #Adding Epiphyes and gree algae on wall together
  ggplot(aes(x=Replicate, y=CO2, colour = Epiphytes))+
  geom_point()+
  facet_wrap(~Mesocosm_Treatment)
  
Gasflux %>% 
  filter(Dur>60) %>% 
  mutate(Epiphytes=Epi+GA) %>% 
  ggplot(aes(x=Epiphytes, y=CO2, colour = Mesocosm_Treatment))+
  geom_point()


  



Fluxes %>% ## 0% shaded run, viewing variation in CO2Flux ### No connection to PAR?
  filter(Dur>60) %>% 
  filter(Shade_Per == "0") %>% 
  ggplot(aes(x=Replicate, y=CO2flux, color=Water))+ 
  scale_color_gradient(low = "red", high = "green")+
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment, scales= "free_y")

Fluxes %>% #100% shaded run Flux in CO2? # no connection?
  filter(Dur>60) %>% 
  filter(Shade_Per == "100") %>% 
  ggplot(aes(x=Replicate, y=CO2flux, color=Water))+ 
  scale_color_gradient(low = "red", high = "green")+
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)


Fluxes %>% ## 0% shaded run, viewing variation in CH4Flux ### No connection to PAR?
  filter(Dur>60) %>% 
  filter(Shade_Per == "0") %>% 
  ggplot(aes(x=Replicate, y=CH4flux, color=Water))+ 
  scale_color_gradient(low = "red", high = "green")+
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)


Fluxes %>% #100% shaded run CH4 flux
  filter(Dur>60) %>% 
  filter(Shade_Per == "100") %>% 
  ggplot(aes(x=Replicate, y=CH4flux, color=Water))+ 
  scale_color_gradient(low = "red", high = "green")+
  geom_boxplot()+
  facet_wrap(~Mesocosm_Treatment)


### Vitalization of CO2 flux over time for each Different Treatment and Replicate-------

##High Functional Diversity
#CO2
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "HIGH") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()


Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "HIGH") %>% 
  ggplot(aes(x=Dur, y=CO2flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()

#CH4
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "HIGH") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()

Gasflux2 %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "HIGH") %>% 
  ggplot(aes(x=Dur, y=CH4flux2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

##Medium CO2
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()

Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CO2flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

#Medium CH4
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CH4flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

##Low CO2
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "LOW") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()

Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "LOW") %>% 
  ggplot(aes(x=Dur, y=CO2flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()
#Low CH4
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "MED") %>% 
  ggplot(aes(x=Dur, y=CH4flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()

##Control CO2
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate,scales = "free_y")+
  theme_classic()
Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>% 
  ggplot(aes(x=Dur, y=CO2flux, color=Shade_Per))+
  geom_point() +0
  facet_wrap(~Replicate)+
  theme_classic()
##Control CH4
Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")+
  theme_classic()
Fluxes %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>% 
  ggplot(aes(x=Dur, y=CH4flux, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Replicate)+
  theme_classic()


##CON5 CH4 had an odd reading? 

Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>%
  filter(Replicate == "5") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  theme_classic()

Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "CON") %>%
  filter(Replicate == "5") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  theme_classic()

Gasflux %>% 
  filter(Dur>60) %>% 
  filter(Mesocosm_Treatment== "LOW") %>%
  filter(Replicate == "4") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Shade_Per))+
  geom_point() +
  theme_classic()

Gasflux %>% 
  filter(Dur>20) %>% 
  filter(Mesocosm_Treatment== "LOW") %>%
  filter(Replicate == "4") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  theme_classic()

###Functional Diversity?? ----

Gasflux2 %>% 
  # filter(Mesocosm_Treatment!="CON") %>% 
  ggplot(aes(x=Dur, y=CO2, color=FuncDivRich)) +
  geom_point()+
  facet_wrap(~Mesocosm_Treatment)

Gasflux2 %>% 
  filter(Mesocosm_Treatment!="CON") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDipersion)) +
  geom_point()+
  facet_wrap(~Mesocosm_Treatment)


Gasflux2 %>% 
  filter(Mesocosm_Treatment=="HIGH") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDivRich)) +
  geom_point()

Gasflux2 %>% 
  filter(Mesocosm_Treatment=="LOW") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDivRich)) +
  geom_point()

Gasflux2 %>% 
  filter(Mesocosm_Treatment=="MED") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDivRich)) +
  geom_point()


Gasflux2 %>% 
  filter(Mesocosm_Treatment=="HIGH") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDipersion)) +
  geom_point()

Gasflux2 %>% 
  filter(Mesocosm_Treatment=="LOW") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDipersion)) +
  geom_point()

Gasflux2 %>% 
  filter(Mesocosm_Treatment=="MED") %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDipersion)) +
  geom_point()
  
# Example with facet_wrap()
Gasflux2 %>% 
  ggplot(aes(x=Dur, y=CO2m, color=FuncDipersion)) +
  geom_point() +
  facet_wrap(~Mesocosm_Treatment)

Gasflux2 %>% 
  ggplot(aes(x=FunXaxis, y=FunYaxis, color=CO2))+
  geom_point(size=5)+
  facet_wrap(~Mesocosm_Treatment)
Gasflux2 %>% 
  filter(Mesocosm_Treatment=="MED") %>% 
  ggplot(aes(x=FunXaxis, y=FunYaxis, color=CO2))+
  geom_point()+
  facet_wrap(~Replicate)
Gasflux2 %>% 
  filter(Mesocosm_Treatment=="HIGH") %>% 
  ggplot(aes(x=FunXaxis, y=FunYaxis, color=CO2))+
  geom_point()+
  facet_wrap(~Replicate)

### By Species------------------------------------


Mesocosm_Key <- read.csv("Data/Mesocosm_Community_Key.csv")


SpeciesTest <-
  full_join(Mesocosm_Key, Gasflux2, 
            by = c("Infaunal_Community_ID" = "Treat_Rep")) %>%
  filter(Mesocosm_Treatment != "CON")

SpeciesTest %>% 
  ggplot(aes(x=Worm.Percentage, y=CO2, color=Mesocosm_Treatment))+
  geom_point()


SpeciesTest %>% 
  ggplot(aes(x=Bivalve.Percentage, y=CO2, colour = Mesocosm_Treatment))+
  geom_point()

SpeciesTest %>% 
  ggplot(aes(y=CO2, x=Total.Worm, color= Mesocosm_Treatment))+
  geom_boxplot()

SpeciesTest %>% 
  group_by(Infaunal_Community_ID) %>%
  filter(Shade_Per =="0") %>% 
  mutate(Absent=if_else(!is.na(Arenicola), "Arenicola", "Absent")) %>% 
 ggplot(aes(x=Absent, y=CO2, color=Mesocosm_Treatment))+
  geom_boxplot()

  
SpeciesTest %>% 
  group_by()


#### Figs for Katie --------------------------------

Slope %>% #
  ggplot(aes(x=Mesocosm_Treatment, y=CO2flux, color = Shade_Per))+ 
  geom_boxplot()+
  ylab("CO2 ppm hour-1 m2-2")
  
  
  Slope %>% #
    ggplot(aes(x=Replicate, y=CH4flux, color = Shade_Per))+ 
    geom_boxplot()+
    ylab("CO2 ppm hour-1 m2-2")
  facet_wrap(~Mesocosm_Treatment)
  
  Gasflux %>% 
    filter(!Dur < 60) %>% 
    ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
    geom_point() +
    facet_wrap(~Mesocosm_Treatment)+
    theme_classic()

