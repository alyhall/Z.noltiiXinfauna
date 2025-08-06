## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Analyze GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-07-11
## Date Edited: 2025-07-30
## Lat Edited By: JW
## 
##
## Copyright (c) Alyson Hall, 2025 
## Email: achall@vims.edu
##
##
## Notes:
## I copied code over from original script from messy day in June
## It is easier to read the text file into excel (using from text/csv) and then work with excel
##
##Copied code from GHG_Analysis.R -JW
## 
## 2025-07-28 data (Air Run):
## Licore time was behind real time by 23 s
##
## 2025-07-29 data (Water Run):
## Licore time was behind real time by 23 s
##
## When reading my code it is useful to turn code soft-wrap on!
##
## Packages Needed

library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(ggpubr)


## Load in Data ----------------------------
###GHG_Water-------------
GHG_Water <- read_excel("Data/Mesocosm/Water_GHG_2025-07-29.xlsx", 
                  na = "nan", skip = 4) %>%
  {
    colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
    . <- .[-c(1, 2), ]  # Remove both the header row and units row
    . 
  }  %>% 
  select(TIME:LASER_T) %>%   # Retain only columns I want
  mutate(TIME = hms::as_hms(TIME),
         TIME = hms::as_hms(TIME - 23), # Licore was ahead by 23 s
         CO2 = as.numeric(CO2),
         CH4 = as.numeric(CH4)) 





# Read in measurements from data sheet for WATER run GHG

Measure_Water <- read_excel("Data/Mesocosm/Water_GHG_2025-07-29.xlsx", sheet = 2) %>% 
  mutate(Start_Time = hms::as_hms(Start_Time),
         End_Time = hms::as_hms(End_Time),
         Shade_Per = as.factor(Shade_Per),
         Current_Location = as.numeric(Current_Location),
         Replicate = as.factor(Replicate), 
         Run_Type = as.factor(Run_Type)  
         )



Combo_Water <- 
  fuzzy_left_join(
    GHG_Water,
    Measure_Water,
    by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
    match_fun = list(`>=`, `<=`)) 


Gasflux_Water <- 
  Combo_Water %>% 
  mutate(Dur = difftime(Combo_Water$TIME, Combo_Water$Start_Time), Date= as.factor(29)) %>% # Create a column for runtime 
  filter(!Dur == "NA")   # Remove the NA (between measurements and before machine started)

###GHG_AIR ----------------------


GHG_Air <- read_excel("Data/Mesocosm/Air_GHG_2025-07-28.xlsx", 
na = "nan", skip = 4) %>%
  {
    colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
    . <- .[-c(1, 2), ]  # Remove both the header row and units row
    . 
  } %>% 
  select(TIME:LASER_T) %>%  # Retain only columns I want
  mutate(TIME = hms::as_hms(TIME),
         TIME = hms::as_hms(TIME - 23), # Licore was ahead by 23 s
         CO2 = as.numeric(CO2),
         CH4 = as.numeric(CH4)) 




#Read in measurements from data sheet for AIR run GHG

Measure_Air <- read_excel("Data/Mesocosm/Air_GHG_2025-07-28.xlsx", sheet = 2) %>% 
  mutate(Start_Time = hms::as_hms(Start_Time),
         End_Time = hms::as_hms(End_Time),
         Shade_Per = as.factor(Shade_Per),
         Current_Location = as.numeric(Current_Location),
         Replicate = as.factor(Replicate), 
         Run_Type = as.factor(Run_Type))


Light <- read_excel("Data/Mesocosm/Mesocosm_lightfield.xlsx") %>% 
  mutate(Pot_Location = as.numeric(Pot_Location),
         Air = as.numeric(Air))



Combo_Air <- 
  fuzzy_left_join(
    GHG_Air,
    Measure_Air,
    by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
    match_fun = list(`>=`, `<=`)) 


Gasflux_Air <- 
  Combo_Air %>% 
  mutate(Dur = difftime(Combo_Air$TIME, Combo_Air$Start_Time), Date= as.factor(28)) %>% # Create a column for run time 
  filter(!Dur == "NA")   # Remove the NA (between measurements and before machine started)

###Fauna and Shoot Count---------
  FSC <- read.csv("Data/Mesocosm/Shoot_Animal_Count.csv") %>% 
          mutate(Treat_Rep = paste(Treatment, Replicate))



##Calculations------

Slope_Water <- Gasflux_Water %>% 
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  filter(Run_Type == "Water") %>% ## 07-29-2025 had some Air runs as well, SEE Slope Exploration
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Run_Type == "Water" ~ pi * (17.5^2) * (39 - 27.94) / 1000,
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60   # slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  # Now our units are in ppm CO2 per hour per 20 cm2, Our pots have 314.159cm2 and correct for the L of air
  mutate(CO2flux = ((CO2change/ (314.159 / 1e4))*Air_Volume),  
         CH4flux = ((CH4change/ (314.159 / 1e4))*Air_Volume))  %>% 
  unique()
# Now our units are in ppm CO2 per hour per m2

Slope_Air <- Gasflux_Air %>%
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Run_Type == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
      TRUE ~ NA_real_
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60   # slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  # Now our units are in ppm CO2 per hour per 20 cm2, Our pots have 314.159cm2 and correct for the L of air
  mutate(CO2flux = ((CO2change/ (314.159 / 1e4))*Air_Volume),  
         CH4flux = ((CH4change/ (314.159 / 1e4))*Air_Volume))  %>% 
  unique()



# Average for Water

Slope_Water %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2flux, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()


Slope_Water %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CH4flux, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()


#Average for Air
Slope_Air %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2flux, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()

Slope_Air %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CH4flux, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()


##Flux Exploration-------------
Gasflux_Water  %>% 
  filter(Run_Type=="Water", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCO2= mean(CO2), sdCO2= sd(CO2)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Replicate, y=MeanCO2, color=Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax=MeanCO2+sdCO2, ymin=MeanCO2-sdCO2))+
  facet_wrap(~Mesocosm_Treatment, scales = "free_y")+
  ggtitle("Water Run CO2flux")

Gasflux_Water %>% 
  filter(Run_Type=="Water", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate) %>% 
  mutate(MeanCH4 = mean(CH4), sdCH4 = sd(CH4)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Replicate, y=MeanCH4))+
  geom_point()+
  geom_errorbar(aes(ymax=MeanCH4+sdCH4, ymin=MeanCH4-sdCH4))+
  facet_wrap(~Mesocosm_Treatment)+  
  ggtitle("Water Run CH4flux")



Gasflux_Air %>% 
  filter(Run_Type=="Air", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCO2= mean(CO2), sdCO2= sd(CO2)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Replicate, y=MeanCO2, color=Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax=MeanCO2+sdCO2, ymin=MeanCO2-sdCO2))+
  facet_wrap(~Mesocosm_Treatment)+
  ggtitle("Air Run CO2flux")



Gasflux_Air %>% 
  filter(Run_Type=="Air", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCH4 = mean(CH4), sdCH4 = sd(CH4)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Replicate, y=MeanCH4, color=Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax=MeanCH4+sdCH4, ymin=MeanCH4-sdCH4))+
  facet_wrap(~Mesocosm_Treatment, scales = "free_y")+
  ggtitle("Air Run CH4flux")

####! High 2 Air run CH4 bubble??----------
Gasflux_Air %>%  
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type=="Air", Dur > 60, Dur < 800, Treat_Rep == "High 2") %>%
  ggplot(aes(x=Dur, y=CH4, color= Shade_Per))+
  geom_point()
  
Gasflux_Air %>%  
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type=="Air", Dur > 60, Dur < 800, Treat_Rep == "High 2") %>%
  ggplot(aes(x=Dur, y=CO2, color= Shade_Per))+
  geom_point()

Gasflux <- rbind(Gasflux_Air, Gasflux_Water)

####Intercept for each run?

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur > 60, Dur < 800, Treat_Rep =="Control 1") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur > 60, Dur < 800, Mesocosm_Treatment =="Medium") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)


Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur > 60, Dur < 800, Mesocosm_Treatment =="Medium") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

## Air Run Verification -------------

#Verifying Air runs on 07-29 with Air runs on 07-28 

Slope_Air_Verify <- Slope_Air %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate), Date= 28) %>% 
  filter(Treat_Rep %in% c("Low 5", "Control 4", "Medium 5"))

Slope_Water_Verify1 <- Gasflux_Water %>% 
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  filter(Run_Type == "Air") %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Run_Type == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
      TRUE ~ NA_real_
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60   # slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  # Now our units are in ppm CO2 per hour per 20 cm2, Our pots have 314.159cm2 and correct for the L of air
  mutate(CO2flux = ((CO2change/ (314.159 / 1e4))*Air_Volume),  
         CH4flux = ((CH4change/ (314.159 / 1e4))*Air_Volume),
         Date="29", 
         Treat_Rep = paste(Mesocosm_Treatment, Replicate), Date=29)  %>% 
  unique()
# Now our units are in ppm CO2 per hour per m2


Slope_Verify <- rbind(Slope_Air_Verify, Slope_Water_Verify1)


# Similar shaded runs, 
Slope_Verify %>% 
  mutate(Date = as.factor(Date)) %>% 
ggplot(aes(x=Treat_Rep, y=CO2change, color = Date))+
  geom_point()+
  facet_wrap(~Shade_Per)





## CH4 big differences between runs?
Slope_Verify  %>% 
  mutate(Date = as.factor(Date)) %>% 
ggplot(aes(x=Treat_Rep, y=CH4change, color = Date))+
  geom_point()+
  facet_wrap(~Shade_Per)


####Comparing Intercept for each run?-----
Gasflux <- rbind(Gasflux_Air, Gasflux_Water)


Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Shade_Per=="100") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Mesocosm_Treatment)
  
#### CO2 intercept

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Control 4") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)


Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Low 5") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Medium 5") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

#### CH4 Intercept

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Control 4") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)


Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Low 5") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Treat_Rep == "Medium 5") %>% 
  ggplot(aes(x=Dur, y=CH4, color=Date, fill=Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point(shape=21)+
  facet_wrap(~Shade_Per)

##PAR?---------
 
#Attempted to see if previous position PAR rating may have had an effect? 

Light <- read_excel("Data/Mesocosm/Mesocosm_lightfield.xlsx") %>% 
  mutate(Pot_Location = as.numeric(Pot_Location),
         Water = as.numeric(Water), 
         Air = as.numeric(Air))

Slope_Air_PARTEST <- left_join(Slope_Air, Light %>% 
                                 select(c("Pot_Location", "Water", "Air")), 
                               by = c("Current_Location" = "Pot_Location"))
##Checks differences in CO2 flux for AIR run
Slope_Air_PARTEST %>% 
ggplot(aes(x=Air, y=CO2flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per, scales = "free_y")



Slope_Water_PARTEST <- left_join(Slope_Water, Light %>% 
                                          select(c("Pot_Location", "Water", "Air")), 
                                           by = c("Current_Location" = "Pot_Location"))
##Checks CO2 flux for water run
Slope_Water_PARTEST %>% 
  ggplot(aes(x=Water, y=CO2flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per)


####Air Shoot Count--------------
 Air_Shoot <- Slope_Air %>% 
              mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
              left_join(FSC %>% 
                          select(c("Treat_Rep", "Shoot_Count")),
                          by = c("Treat_Rep")
                        )
    
#CO2 flux during Air run ~ Shoot count
Air_Shoot %>% 
  ggplot(aes(x=Shoot_Count, y= CO2flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per)

#Ch4 flux during Air run ~ Shoot count
Air_Shoot %>% 
  ggplot(aes(x=Shoot_Count, y= CH4flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per)


#####!!! AIR Run shaded CO2 flux ~ Shoot Count ------

AS_100 <- Air_Shoot %>% filter(Shade_Per == "100")

ggqqplot(AS_100$Shoot_Count)
ggqqplot(AS_100$CO2flux)

AS100_COR <- cor.test(AS_100$CO2flux, AS_100$Shoot_Count)

AS100_COR # r = 0.816, t(18) =  5.9793 p<0.0001 ****

AS100_Lin <- lm(data = AS_100, CO2flux ~ Shoot_Count) 

plot(AS100_Lin)

summary(AS100_Lin) #p-value < 0.0001, adjusted R^2 = 0.6465, F (1,18) = 35.75 

##### Air run NO shade CO2 flux ~ Shoot Count ------

AS_0 <- Air_Shoot %>% filter (Shade_Per == "0")

ggqqplot (AS_0$Shoot_Count)
ggqqplot (AS_0$CO2flux)

AS0_COR <- cor.test(AS_0$CO2flux, AS_0$Shoot_Count)

AS0_COR #r= -0.339, t(18)=-1.531 p<0.14 :(

AS0_Lin <- lm(data= AS_0, CO2flux ~ Shoot_Count)

plot(AS0_Lin) #Outliers? 19



####Water Shoot Count -----
Water_Shoot <- Slope_Water %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  left_join(FSC %>% 
              select(c("Treat_Rep", "Shoot_Count")),
            by = c("Treat_Rep")
  )


#Co2flux during Water run ~ shoot count
Water_Shoot %>% 
  ggplot(aes(x=Shoot_Count, y= CO2flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per)


Water_Shoot %>% 
  ggplot(aes(x=Shoot_Count, y= CH4flux, color = Mesocosm_Treatment))+
  geom_point()+
  facet_wrap(~Shade_Per)






##Slope Test



Slope_Water %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2change, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  scale_y_continuous(breaks = seq(-600, 800, 200))+
  theme_bw()

Slope_Water %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CH4change, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()


#Average for Air
Slope_Air %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2change, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  scale_y_continuous(breaks = seq(-600, 800, 200))+
  theme_bw()

Slope_Air %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CH4change, color = Shade_Per)) +
  geom_violin() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  facet_wrap(~Shade_Per) +
  theme_bw()

