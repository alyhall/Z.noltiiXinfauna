## Header and Preamble ------------------------------------------------------------
##
## Script name: Mesocosm Explore
##
## Purpose of script: look at trends
##
## Author: Alyson Hall
##
## Date Created: 2025-09-15
## Date Edited:  2025-09-15
##
## Copyright (c) Alyson Hall, 2024
## Email: achall@vims.edu
##

## Notes:
##
##
##
##

## Load up the packages we will need:
library(tidyverse)


##
## When using or reading my code, I recommended turning on code soft-wrap.
########End


# Read in -----------------------------------------------------------------

#Mesocosm

End_Bio <- 
  read_csv("Data/Mesocosm/Shoot_Animal_Count.csv") %>% 
  select(!c(Coring_Notes, Notes)) %>% 
  pivot_longer(cols = Length_1_cm:Length_5_cm,
               names_to = "Shoot_Rep",
               values_to = "CH") %>% 
  group_by(Treatment, Replicate) %>% 
  mutate(u_CH = mean(CH)) %>% 
  ungroup() %>% 
  select(!c(Shoot_Rep, CH)) %>% 
  unique()


# explore -----------------------------------------------------------------

# end shoot counts vs treatment 

End_Bio %>% 
  ggplot(
    aes(
      x = Treatment,
      y = Shoot_Count
    )
  ) +
  geom_boxplot() +
  labs(
    y = "Shoot Count"
  ) +
  theme_bw()


# End CH vs treatment

End_Bio %>% 
  ggplot(
    aes(
      x = Treatment,
      y = u_CH
    )
  ) +
  geom_boxplot() +
  labs(
    y = "Shoot Count"
  ) +
  theme_bw()


# Area disruption

End_Bio %>% 
  ggplot(
    aes(
      x = Treatment,
      y = Percentage_Sand
    )
  ) +
  geom_boxplot() +
  labs(
    y = "Shoot Count"
  ) +
  theme_bw()
