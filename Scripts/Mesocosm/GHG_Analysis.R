## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG-CleanData
##
## Purpose of script: Analyze GHG clean data from mesocosm experiment
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
## 
## When reading my code it is useful to turn code soft-wrap on!
##
## Packages Needed

library(readxl)
library(fuzzyjoin)
library(ggpubr)
library(broom)
library(tidyverse)


## Load in Data ----------------------------

# Read in light data for previous pot location 
Light <- 
  read_excel("Data/Mesocosm/Mesocosm_lightfield.xlsx") %>% 
  mutate(Pot_Location = as.numeric(Pot_Location),
         Air_Light = as.numeric(Air),
         Water_Light = as.numeric(Water))

# Read in data about pot shoot and community counts
FSC <- 
  read.csv("Data/Mesocosm/Shoot_Animal_Count.csv") %>% 
  mutate(Treat_Rep = paste(Treatment, Replicate)) %>% 
  select(-contains("Notes")) %>% 
  mutate(
    # Create counts of worms + bivalves
    Total_Worms = rowSums(
      across(c(
        Aren_Total,
        Hedi_Total,
        Neph_Total,
        Glyc_Total,
        Lana,
        Lagis
      )),
      na.rm = TRUE
    ),
    Total_Bivalves = rowSums(
      across(c(
        Mya_Total,
        Cera_Total,
        Scro_Total
      )),
      na.rm = TRUE
    )
  )

# Combined clean flux data

Gas_Flux <- 
  read.csv("Data/Mesocosm/Exported_Data/Combined_Gas_Fluxes.csv") %>% 
  mutate(
    Shade_Per = as.factor(Shade_Per)
  )
 
## Data Explore Broad ------

Gas_Flux %>% 
  pivot_longer(
    cols = c(CO2_flux, CH4_flux),
    names_to = "Gas",
    values_to = "Flux"
  ) %>% 
  ggplot(aes(x = Mesocosm_Treatment, y = Flux, color = Shade_Per)) +
  geom_boxplot() +
  facet_grid(
    Gas ~ Run_Type,
    scales = "free_y"
  ) +
  labs(
    y = "Flux (mmol gas m⁻² h⁻¹)"
  ) +
  theme_bw()



## PAR ---------
 
# Attempt to see if previous position PAR rating may have had an effect 

PAR_test <- 
  left_join(Gas_Flux, Light %>% 
              select(c("Pot_Location", "Water_Light", "Air_Light")), 
            by = c("Current_Location" = "Pot_Location")
  ) %>% 
  pivot_longer(
    cols = c(CO2_flux, CH4_flux),
    names_to = "Gas",
    values_to = "Flux"
  )

PAR_test %>% 
  ggplot(aes(x = Air_Light, y = Flux)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_grid(
    Gas ~ Run_Type + Shade_Per,
    scales = "free_y"
  ) +
  theme_bw()

# Stats 
  PAR_test %>%
  group_by(Run_Type, Shade_Per, Gas) %>%
  do(
    tidy(lm(Flux ~ Air_Light, data = .))
  ) %>% 
  filter(!term == "(Intercept)")

# No real linear trends



## Shoot Count + other metrics --------------

Shoot_test <- 
  left_join(Gas_Flux, FSC, 
            by = c("Mesocosm_Treatment" = "Treatment", "Replicate" )
  ) %>% 
  pivot_longer(
    cols = c(CO2_flux, CH4_flux),
    names_to = "Gas",
    values_to = "Flux"
  )

Shoot_test %>% 
  ggplot(aes(x = Shoot_Count, y = Flux)) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_grid(
    Gas ~ Run_Type + Shade_Per,
    scales = "free_y"
  ) +
  theme_bw()

# Stats 
  Shoot_test %>%
  group_by(Run_Type, Shade_Per, Gas) %>%
  do(
    tidy(lm(Flux ~ Shoot_Count, data = .))
  ) %>% 
  filter(!term == "(Intercept)")

# No real linear trends

  Shoot_test %>% 
    ggplot(aes(x = Total_Worms, y = Flux)) +
    geom_point(alpha = 0.7) +
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    facet_grid(
      Gas ~ Run_Type + Shade_Per,
      scales = "free_y"
    ) +
    theme_bw()
  
  # Stats 
  Shoot_test %>%
    group_by(Run_Type, Shade_Per, Gas) %>%
    do(
      tidy(lm(Flux ~ Total_Worms, data = .))
    ) %>% 
    filter(!term == "(Intercept)")
  
  Shoot_test %>% 
    ggplot(aes(x = Total_Bivalves, y = Flux)) +
    geom_point(alpha = 0.7) +
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    facet_grid(
      Gas ~ Run_Type + Shade_Per,
      scales = "free_y"
    ) +
    theme_bw()
