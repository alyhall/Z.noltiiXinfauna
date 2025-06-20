## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Explore GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-06-12
## Date Edited: 2025-06-13 --- 2025-06-20 added new lines without deleting any lines (hopefully) -JW
##
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
##

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
    Combo %>%
      group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
      arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
      # Generating fluxes for gar exchange per s 
      mutate(CO2flux = (as.numeric(CO2) - lag(as.numeric(CO2), n = 1)),
             CH4flux = (as.numeric(CH4) - lag(as.numeric(CH4), n = 1))) 

## Combo does not have Dur using Gasflux instead -JW
Fluxes <- 
  Gasflux %>%
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
  arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
  # Generating fluxes for gar exchange per s 
  mutate(CO2flux = (as.numeric(CO2) - lag(as.numeric(CO2), n = 1)),
         CH4flux = (as.numeric(CH4) - lag(as.numeric(CH4), n = 1))) 



# This is a little fine detail, maybe instead we do the slope over the whole incubation...

Slope <- Gasflux %>%
  filter(Dur < 241) %>% 
  filter(Dur > 40) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
  summarise(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    .groups = "drop"
  ) %>%
  mutate(
    CO2flux = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60,  # slope of CO2 ~ min
    CH4flux = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60   # slope of CH4 ~ min
  ) %>%
  select(-CO2_fit, -CH4_fit)

# Average

Slope %>% 
  ggplot(aes( x = Mesocosm_Treatment, y = CO2flux, color = Shade_Per)) +
  geom_violin() +
  facet_wrap(~Shade_Per, scales = "free_y") +
  theme_bw()


## Data Exploration ---------------------------------

# Example of medium treatments 

Gasflux %>% 
  # filter(Dur < 241) %>% 
  # filter(Dur > 120) %>% 
  filter(Replicate == "4") %>% 
  filter(Mesocosm_Treatment == "LOW") %>% 
  filter(Shade_Per == 100) %>% 
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

## -JW Test------------

Gasflux %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur>50) %>% 
  filter(Shade_Per== "0") %>% 
  filter(Mesocosm_Treatment== "CON") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Treat_Rep, scales = "free_y")+
  theme_classic()
  
Gasflux %>% 
  mutate(Treat_Rep = paste(Mesocosm_Treatment, Replicate)) %>% 
  filter(Dur>120) %>% 
  filter(Mesocosm_Treatment== "HIGH") %>% 
  ggplot(aes(x=Dur, y=CO2, color=Shade_Per))+
  geom_point() +
  facet_wrap(~Treat_Rep, scales = "free_y")+
  theme_classic()

Gasflux %>%
    filter(Dur>120) %>% 
    ggplot(aes(x=Dur, y = CO2, color = Mesocosm_Treatment ))+
    geom_point() +
    facet_wrap(~Shade_Per, scales = "free_y")+
    theme_classic()


  