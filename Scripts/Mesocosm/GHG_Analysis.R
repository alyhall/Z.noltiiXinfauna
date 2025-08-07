## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Analyze GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-07-11
## Date Edited: 2025-07-11
## Lat Edited By: AH
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
## 2025-07-10 data:
## Licore time was behind real time by 6 s
## 

## When reading my code it is useful to turn code soft-wrap on!

## Packages Needed

HELLO!!!!

library(tidyverse)
library(readxl)
library(fuzzyjoin)

## Load in Data ----------------------------

GHG <- read_excel("Data/GHG_2025-07-10.xlsx", 
                  na = "nan", skip = 4) %>%
              {
                colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
                . <- .[-c(1, 2), ]  # Remove both the header row and units row
                . 
                } %>% 
          select(TIME:LASER_T) %>%  # Retain only columns I want
          mutate(TIME = hms::as_hms(TIME),
                 TIME = hms::as_hms(TIME - 6), # Licore was ahead by 6 s
                 CO2 = as.numeric(CO2),
                 CH4 = as.numeric(CH4)) 



# Read in measurements from data sheet

Measure <- read_excel("Data/GHG_2025-07-10.xlsx", sheet = 2) %>% 
                mutate(Start_Time = hms::as_hms(Start_Time),
                       End_Time = hms::as_hms(End_Time),
                       Shade_Per = as.factor(Shade_Per),
                       Current_Location = as.numeric(Current_Location),
                       Replicate = as.factor(Replicate))


# Light <- read_excel("Data/Mesocosm_lightfield.xlsx") %>% 
#             mutate(Pot_Location = as.numeric(Pot_Location),
#                    Water = as.numeric(Water))




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
      filter(!Dur == "NA")   # Remove the NA (between measurements and before machine started)
      


## Calculations ----------------------------------------------

# get flux per s for each treatment

# Fluxes <- 
#   Gasflux %>%
#   group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
#   arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
#   # Generating fluxes for each pot exchange per s 
#   mutate(CO2flux = (as.numeric(CO2) - lag(as.numeric(CO2), n = 1)),
#          CH4flux = (as.numeric(CH4) - lag(as.numeric(CH4), n = 1))) 



Slope <- Gasflux %>%
  filter(Dur > 60) %>% 
  filter(Dur < 800) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Substrate, Stand_Pipe) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Mesocosm_Treatment == "Background" ~ 1,
      Substrate == "Water" ~ pi * (17.5^2) * (39 - 26.7) / 1000,
      Substrate == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
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
  # Now our units are in ppm CO2 per hour per m2



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

# Looking at all Treatments

Gasflux %>% 
  filter(!Mesocosm_Treatment == "Background") %>% 
  # filter(Dur > 120) %>% 
  # filter(Dur < 800) %>% 
  ggplot(aes( x = Dur, y = CO2, color = Substrate)) +
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Stand_Pipe*Replicate*Shade_Per, scales = "free") +
  theme_bw()

Gasflux %>% 
  filter(Dur < 241) %>% 
  filter(Dur > 120) %>% 
  ggplot(aes( x = Dur, y = CO2, color = Mesocosm_Treatment)) +
  geom_point() +
  facet_wrap(~Shade_Per, scales = "free_y") +
  theme_bw()


Gasflux %>% 
  group_by(Mesocosm_Treatment, Shade_Per, Substrate) %>% 
  reframe(
    max = max(CO2),
    min = min(CO2),
    u = mean(CO2)
  )

Slope %>% 
  group_by(Mesocosm_Treatment, Shade_Per, Substrate) %>% 
  reframe(
    max = max(CO2change),
    min = min(CO2change),
    u = mean(CO2change)
  )

# Standpipe Comparison ----------------

Gasflux %>% 
  group_by(Mesocosm_Treatment, Substrate, Shade_Per) %>%
  filter(all(c("Covered", "Uncovered") %in% Stand_Pipe)) %>%
  ungroup() %>% 
  filter(!Mesocosm_Treatment == "Background") %>% 
  filter(Dur > 120) %>%
  filter(Dur < 800) %>%
  ggplot(aes( x = Dur, y = CO2, color = Stand_Pipe)) +
  geom_point() +
  facet_wrap(~Mesocosm_Treatment, scales = "free") +
  theme_bw()

Pipe_test <- 
Slope %>% 
  group_by(Mesocosm_Treatment, Substrate, Shade_Per) %>%
  filter(all(c("Covered", "Uncovered") %in% Stand_Pipe)) %>%
  ungroup()
  
  
ggboxplot(Pipe_test, x = "Stand_Pipe", y = "CO2change") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  geom_pwc(
    aes(group = Stand_Pipe), tip.length = 0,
    method = "t_test", label = "p.format") +
  labs( x = "Stand Pipe", y = "CO2 flux per hour")


shapiro.test(Pipe_test$CO2change) # normal

# Substrate Comparison ----------------

Gasflux %>% 
  filter(!Stand_Pipe == "Covered") %>% 
  filter(!Mesocosm_Treatment == "Background") %>% 
  filter(Dur > 120) %>%
  filter(Dur < 800) %>%
  ggplot(aes( x = Dur, y = CO2, color = Substrate)) +
  geom_point() +
  facet_wrap(~Mesocosm_Treatment*Shade_Per, scales = "free") +
  theme_bw()

substrate_test <- 
  Slope %>% 
  filter(!Stand_Pipe == "Covered") %>% 
  filter(!Mesocosm_Treatment == "Background") %>% 
  mutate(ID = paste(Shade_Per, Substrate, sep = ""))

substrate_test_0 <-
Slope %>%
  filter((Substrate == "Air" & Stand_Pipe == "Covered") |
      (Substrate == "Water")) %>% 
  filter(Shade_Per == 0)

shapiro.test(substrate_test$CO2change) # Normal 


ggboxplot(substrate_test_0, x = "Substrate", y = "CO2change") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  geom_pwc(
    aes(group = Substrate), tip.length = 0,
    method = "t_test", label = "p.format") +
  labs( x = "Shading", y = "CO2 flux per hour")


ggboxplot(substrate_test, x = "Shade_Per", y = "CO2change", color = "Substrate") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  geom_pwc(
    aes(group = Substrate), tip.length = 0,
    method = "t_test", label = "p.format") +
  labs( x = "Shading", y = "CO2 flux per hour")


# substrate_test$Group <- interaction(substrate_test$Substrate, substrate_test$Shade_Per)
# 
# ggboxplot(substrate_test, x = "Group", y = "CO2change", color = "Shade_Per") +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
#   geom_pwc(
#     aes(group = Group), tip.length = 0,
#     method = "wilcox_test", label = "p.format"
#   ) +
#   labs(x = "Substrate x Shade_Per", y = "CO2 flux per hour")



# Compare incubation time --------------------

# 15 mins (no first or last min)
Slope_13 <- Gasflux %>%
  filter(Dur > 60) %>%
  filter(Dur < 840) %>%
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Substrate, Stand_Pipe) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Mesocosm_Treatment == "Background" ~ 1,
      Substrate == "Water" ~ pi * (17.5^2) * (39 - 26.7) / 1000,
      Substrate == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
      TRUE ~ NA_real_
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60,
    Incubation = 15# slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  unique()

# First 5 mins (no first min)
Slope_4 <- Gasflux %>%
  filter(Dur > 60) %>% 
  filter(Dur < 240) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Substrate, Stand_Pipe) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Mesocosm_Treatment == "Background" ~ 1,
      Substrate == "Water" ~ pi * (17.5^2) * (39 - 26.7) / 1000,
      Substrate == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
      TRUE ~ NA_real_
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60,
    Incubation = 4# slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  unique()

# First 7 mins (no first min)
Slope_7 <- Gasflux %>%
  filter(Dur > 60) %>% 
  filter(Dur < 420) %>% 
  group_by(Mesocosm_Treatment, Replicate, Shade_Per, Substrate, Stand_Pipe) %>%
  reframe(
    CO2_fit = list(lm(as.numeric(CO2) ~ as.numeric(Dur))),
    CH4_fit = list(lm(as.numeric(CH4) ~ as.numeric(Dur))),
    Air_Volume = case_when(
      Mesocosm_Treatment == "Background" ~ 1,
      Substrate == "Water" ~ pi * (17.5^2) * (39 - 26.7) / 1000,
      Substrate == "Air" ~ pi * (17.5^2) * 39 / 1000, # Not quite true
      TRUE ~ NA_real_
    ) ) %>%
  # Right now our units are in ppm CO2 per s per 20 cm2 pot
  mutate(
    CO2change = (map_dbl(CO2_fit, ~ coef(.x)[[2]]))*60*60,  # slope of CO2 ~ hour
    CH4change = (map_dbl(CH4_fit, ~ coef(.x)[[2]]))*60*60,
    Incubation = 7# slope of CH4 ~ hour
  ) %>%
  select(-CO2_fit, -CH4_fit) %>% 
  unique()

Incu_test <- as_data_frame(rbind(Slope_13, Slope_4, Slope_7)) %>% 
              filter(!Mesocosm_Treatment == "Background")

shapiro.test(Incu_test$CO2change) # Not normal

# No difference in incubation time
ggboxplot(Incu_test, x = "Incubation", y = "CO2change") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  geom_pwc(
    aes(group = Incubation), tip.length = 0,
    method = "wilcox_test", label = "p.format") +
  labs( x = "Incubation Time", y = "CO2 flux per hour")
