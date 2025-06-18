#Z. Noltii Field Biomass (initial)
#JW June 2025

#Found out that RMD and NB files working directory only is set by file location :( -JW 



#Required packages


library(tidyverse)




ABZ <- read_csv("Data/Lab_Core.csv", na = c("","NA")) #Had to use read_csv
ABZ$Month <- forcats::as_factor(ABZ$Month)
ABZ$Biomass_Part <- forcats::as_factor(ABZ$Biomass_Part)
ABZ <- ABZ %>% 
  mutate(TSC=(TSC*(100/pi))) %>% 
  mutate(ID=row_number()) %>% 
  rename("SGP"="Biomass_Part", "BDW"="Bio_Dry_Weight_m2") %>%  
  select(-Processing_Date, -Sample_Date, -Sampler, -Dry_Weight, -Tin_Weight, -Bio_Dry_Weight, -Date_in_out_of_oven, -Notes)


print(ABZ)