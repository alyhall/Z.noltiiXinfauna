## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG_Formatting
##
## Purpose of script: Create a clean data set for gas fluxes
##
## Author: Alyson Hall
##
## Date Created: 2025-07-11
## Date Edited: 2026-05-21
## Lat Edited By: AH
## 
##
##
## Notes:
## 2025-07-28 data (Air Run):
## Licore time was behind real time by 23 s
##
## 2025-07-29 data (Water Run):
## Licore time was behind real time by 23 s
##
## When reading my code it is useful to turn code soft-wrap on!
##
## Packages Needed

library(readxl)
library(fuzzyjoin)
library(ggpubr)
library(tidyverse)


## Load in Data ----------------------------
###GHG_Water-------------
GHG_Water <- 
  read_excel("Data/Mesocosm/Water_GHG_2025-07-29.xlsx", 
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

Measure_Water <- 
  read_excel("Data/Mesocosm/Water_GHG_2025-07-29.xlsx", sheet = 2) %>% 
  mutate(Start_Time = hms::as_hms(Start_Time),
         End_Time = hms::as_hms(End_Time),
         Shade_Per = as.factor(Shade_Per),
         Current_Location = as.numeric(Current_Location),
         Replicate = as.factor(Replicate), 
         Run_Type = as.factor(Run_Type)  
  )

# Join metadata with licor raw data
Combo_Water <- 
  fuzzy_left_join(
    GHG_Water,
    Measure_Water,
    by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
    match_fun = list(`>=`, `<=`)) 

# Remove the NA (between measurements and before machine started)
Gasflux_Water <- 
  Combo_Water %>% 
  mutate(Dur = difftime(Combo_Water$TIME, Combo_Water$Start_Time), Date= as.factor(29)) %>% # Create a column for runtime 
  filter(!Dur == "NA")   

###GHG_AIR ----------------------

# Repeat with air measurements

GHG_Air <- 
  read_excel("Data/Mesocosm/Air_GHG_2025-07-28.xlsx", 
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

Measure_Air <- 
  read_excel("Data/Mesocosm/Air_GHG_2025-07-28.xlsx", sheet = 2) %>% 
  mutate(Start_Time = hms::as_hms(Start_Time),
         End_Time = hms::as_hms(End_Time),
         Shade_Per = as.factor(Shade_Per),
         Current_Location = as.numeric(Current_Location),
         Replicate = as.factor(Replicate), 
         Run_Type = as.factor(Run_Type))

# Read in light data for previous pot location 
Light <- 
  read_excel("Data/Mesocosm/Mesocosm_lightfield.xlsx") %>% 
  mutate(Pot_Location = as.numeric(Pot_Location),
         Air = as.numeric(Air))

# Combine meta + raw licor
Combo_Air <- 
  fuzzy_left_join(
    GHG_Air,
    Measure_Air,
    by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
    match_fun = list(`>=`, `<=`)) 

# remove data between measurements
Gasflux_Air <- 
  Combo_Air %>% 
  mutate(Dur = difftime(Combo_Air$TIME, Combo_Air$Start_Time), Date= as.factor(28)) %>% # Create a column for run time 
  filter(!Dur == "NA")  


###Fauna and Shoot Count---------

# Read in data about pot shoot and community counts
FSC <- 
  read.csv("Data/Mesocosm/Shoot_Animal_Count.csv") %>% 
  mutate(Treat_Rep = paste(Treatment, Replicate))


##Calculations------


# Calculate slope of middle of incubation for each incubation (Skipping first minute for settling)
Slope_Water <- 
  Gasflux_Water %>% 
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  filter(Run_Type == "Water") %>% ## 07-29-2025 had some Air runs as well, SEE Slope Exploration
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    
    # Calculating the volume of disk of air above the water (in L)
    Air_Volume = case_when(
      Run_Type == "Water" ~ pi * (17.5^2) * (39 - 27.94) / 1000
      ),
      CAVITY_P = mean(as.numeric(CAVITY_P)),
      CAVITY_T = mean(as.numeric(CAVITY_T))
     ) %>%
  
  # Unit transformations -
  
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  
  mutate(
    # CO2 is ppm per hour
    CO2_ppm_h = map_dbl(CO2_fit, ~ coef(.x)[2]) * 3600,
    # CH4 is ppb per hour
    CH4_ppb_h = map_dbl(CH4_fit, ~ coef(.x)[2]) * 3600,
    
    # LI-COR conditions
    Pressure_atm = CAVITY_P / 101.325,
    Temp_K = CAVITY_T + 273.15,
    R = 0.082057,
    
    # total mol air in chamber
    mol_air = (Pressure_atm * Air_Volume) / (R * Temp_K),
    
    # CO2: ppm = umol / mol
    CO2_umol_h = CO2_ppm_h * mol_air,
    CO2_mmol_h = CO2_umol_h / 1000,
    
    # CH4: ppb = nmol / mol
    CH4_nmol_h = CH4_ppb_h * mol_air,
    CH4_mmol_h = CH4_nmol_h / 1e6,
    
    # pot footprint area
    Area_m2 = 314.159 / 1e4,
    
    # final fluxes: mmol gas m^-2 h^-1
    CO2_flux = CO2_mmol_h / Area_m2,
    CH4_flux = CH4_mmol_h / Area_m2
  ) %>%
  
  select(
    Mesocosm_Treatment:Current_Location,
    CO2_flux,
    CH4_flux
  ) %>% 
  unique()
# Now our units are in ppm CO2 per hour per m2

#### IMPORTANT FOR WATER: We are really only measuring the net air–water chamber flux (standardized to pot surface area), which is likely different from the the full instantaneous biological production/consumption inside the water (Some CO2 and CH4 may remain dissolved in the water rather than entering the air during the measurement) ---------


# Similar to as above
Slope_Air <- 
  Gasflux_Air %>%
  filter(Dur > 60,
         Dur < 800,
         Run_Type == "Air") %>% 
  
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    
    # Calculating air volume in sealed bucket, in L
    # NOTE: We are subtracting pot volume (roughly) from total air volume
    # Bucket volume (L)
    Bucket_Volume_L = pi * (17.5^2) * 39 / 1000,
    
    # Pot displacement volume (L) (assuming 20 cm by 16cm pot)
    Pot_Volume_L = pi * (10^2) * 16 / 1000,
    
    # Actual chamber air volume (L)
    Air_Volume = Bucket_Volume_L - Pot_Volume_L,
    
    CAVITY_P = mean(as.numeric(CAVITY_P), na.rm = TRUE),
    CAVITY_T = mean(as.numeric(CAVITY_T), na.rm = TRUE)
  ) %>%
  
  mutate(
    # CO2 is ppm per hour
    CO2_ppm_h = map_dbl(CO2_fit, ~ coef(.x)[2]) * 3600,
    # CH4 is ppb per hour
    CH4_ppb_h = map_dbl(CH4_fit, ~ coef(.x)[2]) * 3600,
    
    # LI-COR conditions
    Pressure_atm = CAVITY_P / 101.325,
    Temp_K = CAVITY_T + 273.15,
    R = 0.082057,
    
    # total mol air in chamber
    mol_air = (Pressure_atm * Air_Volume) / (R * Temp_K),
    
    # CO2: ppm = umol / mol
    CO2_umol_h = CO2_ppm_h * mol_air,
    CO2_mmol_h = CO2_umol_h / 1000,
    
    # CH4: ppb = nmol / mol
    CH4_nmol_h = CH4_ppb_h * mol_air,
    CH4_mmol_h = CH4_nmol_h / 1e6,
    
    # pot footprint area
    Area_m2 = 314.159 / 1e4,
    
    # final fluxes: mmol gas m^-2 h^-1
    CO2_flux = CO2_mmol_h / Area_m2,
    CH4_flux = CH4_mmol_h / Area_m2
  ) %>%
  
  select(
    Mesocosm_Treatment:Current_Location,
    CO2_flux,
    CH4_flux
  ) %>% 
  unique()


Combined_Flux <- 
  bind_rows(
    Slope_Air,
    Slope_Water
  )

# Write csv
write.csv(
  Combined_Flux,
  "Combined_Gas_Fluxes.csv",
  row.names = FALSE
)

# Exploring raw fluxes -------------

##Flux Exploration-------------

Gasflux_Water  %>% 
  filter(Run_Type == "Water", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCO2 = mean(CO2), sdCO2 = sd(CO2)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Replicate, y = MeanCO2, color = Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax = MeanCO2 + sdCO2, ymin = MeanCO2 - sdCO2))+
  facet_wrap(~Mesocosm_Treatment, scales = "free_y")+
  ggtitle("Water Run CO2flux")

# Looks normal

Gasflux_Water %>% 
  filter(Run_Type == "Water", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCH4 = mean(CH4), sdCH4 = sd(CH4)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Replicate, y = MeanCH4, color = Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax = MeanCH4 + sdCH4, ymin = MeanCH4 - sdCH4))+
  facet_wrap(~Mesocosm_Treatment)+  
  ggtitle("Water Run CH4flux")

# Also not bad looking


Gasflux_Air  %>% 
  filter(Run_Type == "Air", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCO2 = mean(CO2), sdCO2 = sd(CO2)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Replicate, y = MeanCO2, color = Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax = MeanCO2 + sdCO2, ymin = MeanCO2 - sdCO2))+
  facet_wrap(~Mesocosm_Treatment, scales = "free_y")+
  ggtitle("Air Run CO2flux")



Gasflux_Air %>% 
  filter(Run_Type=="Air", Dur > 60, Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>% 
  mutate(MeanCH4 = mean(CH4), sdCH4 = sd(CH4)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Replicate, y = MeanCH4, color = Shade_Per))+
  geom_point()+
  geom_errorbar(aes(ymax = MeanCH4 + sdCH4, ymin = MeanCH4 - sdCH4))+
  facet_wrap(~Mesocosm_Treatment)+  
  ggtitle("Air Run CH4flux")


# Air also looks like, just a lot of overlap between treatments

# Look at raw data (Air)
Gasflux_Air %>%  
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type == "Air", Dur > 60, Dur < 800) %>%
  ggplot(aes(x = Dur, y = CH4, color = Replicate))+
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Shade_Per)
# One blip in in High rep 2

Gasflux_Air %>%  
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type == "Air", Dur > 60, Dur < 800) %>%
  ggplot(aes(x = Dur, y = CO2, color = Replicate))+
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Shade_Per)

# Look at raw data (Water)
Gasflux_Water %>%  
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type == "Water", Dur > 60, Dur < 800) %>%
  ggplot(aes(x = Dur, y = CH4, color = Replicate))+
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Shade_Per)
# Two blips in in High rep 2 and low rep 3

Gasflux_Water %>%  
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Run_Type == "Water", Dur > 60, Dur < 800) %>%
  ggplot(aes(x = Dur, y = CO2, color = Replicate))+
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Shade_Per)

## Air Run Verification -------------

#Verifying Air runs on 07-29 with Air runs on 07-28 

Slope_Air_Verify <- 
  Slope_Air %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate), 
         Date = 28) %>% 
  filter(Treat_Rep %in% c("Low 5", "Control 4", "Medium 5")) 

Slope_Water_Verify1 <- 
  Gasflux_Water %>% 
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  filter(Run_Type == "Air") %>% ## 07-29-2025 had some Air runs as well, SEE Slope Exploration
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Run_Type, Current_Location) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    
    # Calculating the volume of disk of air above the water (in L)
    Air_Volume = case_when(
      Run_Type == "Air" ~ pi * (17.5^2) * (39 - 27.94) / 1000
    ),
    CAVITY_P = mean(as.numeric(CAVITY_P)),
    CAVITY_T = mean(as.numeric(CAVITY_T))
  ) %>%
  
  # Unit transformations -
  
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  
  mutate(
    
    # ppm per hour
    CO2_ppm_h = map_dbl(CO2_fit, ~ coef(.x)[2])*60*60,  # slope of CO2 ~ hour,
    CH4_ppm_h = map_dbl(CH4_fit, ~ coef(.x)[2])*60*60,  # slope of CH4 ~ hour,
    
    # constants (from licor)
    Pressure_atm = CAVITY_P / 101.325,
    Temp_K = CAVITY_T + 273.15,
    R = 0.082057,
    
    # total mol air in chamber
    mol_air = (Pressure_atm * Air_Volume) / (R * Temp_K),
    
    # ppm = umol/mol
    CO2_umol_h = CO2_ppm_h * mol_air,
    CH4_umol_h = CH4_ppm_h * mol_air,
    
    # convert to mmol
    CO2_mmol_h = CO2_umol_h / 1000,
    CH4_mmol_h = CH4_umol_h / 1000,
    
    # Now our units are in ppm CO2 per hour per 20 cm2. Our pots have an area of 314.159cm2
    Area_m2 = 314.159 / 1e4,
    
    # final fluxes (mmol gas m⁻² h⁻¹)
    CO2_flux = CO2_mmol_h / Area_m2,
    CH4_flux = CH4_mmol_h / Area_m2
  ) %>%
  select(
    Mesocosm_Treatment:Current_Location,
    CO2_flux,
    CH4_flux
  ) %>% 
  unique() %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate), 
         Date = 29) %>% 
  filter(Treat_Rep %in% c("Low 5", "Control 4", "Medium 5")) 


Slope_Verify <- 
  rbind(Slope_Air_Verify, Slope_Water_Verify1)


# Somewhat similar 
Slope_Verify %>% 
  mutate(Date = as.factor(Date)) %>% 
  ggplot(aes(x=Treat_Rep, y=CO2_flux, color = Date))+
  geom_point()+
  facet_wrap(~Shade_Per)

# Very different
Slope_Verify  %>% 
  mutate(Date = as.factor(Date)) %>% 
  ggplot(aes(x=Treat_Rep, y=CH4_flux, color = Date))+
  geom_point()+
  facet_wrap(~Shade_Per)

####Comparing Intercept for each run?-----
Gasflux <- 
  rbind(Gasflux_Air, Gasflux_Water)


Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Shade_Per=="100") %>% 
  ggplot(aes(x = Dur, y = CO2, color = Date, fill = Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point()+
  facet_wrap(~Mesocosm_Treatment)

Gasflux %>% 
  mutate(Treat_Rep= paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur >60, Dur < 800, Shade_Per=="100") %>% 
  ggplot(aes(x = Dur, y = CH4, color = Date, fill = Run_Type))+
  scale_color_manual(values = c("red", "blue"))+
  scale_fill_manual(values = c("green", "orange"))+
  geom_point()+
  facet_wrap(~Mesocosm_Treatment)

# CH4 different among dates...
