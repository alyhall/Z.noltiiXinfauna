## Header and Preamble ---------------------------
##
## Script name: 
##
## Purpose of script: 
##
## Author: Alyson & Ashitha
##
## Date Created: 2025-07-18
## Date Edited: 2025-07-22
## Last Edited by: Alyson
##

## Notes:
## 
##  
##

## Load up the packages we will need: 
library(tidyverse)
library(readxl)


##
## When using or reading my code, I recommended turning on code soft-wrap.
########End


## Data Load In ----------------

# To-Do:
# - read in ImageJ Data

ImageJ <- read_excel("Data/Ant Farm/ImageJ_Analysis.xlsx", 
                                 sheet = "ImageJ", col_names = TRUE, 
                                 skip = 29) %>%
  filter(!is.na(Image_ID)) %>%
  mutate(
    # Normalize Image_ID (e.g., turn 't_336_2A' into 't336_2A')
    Image_ID = ifelse(str_count(Image_ID, "_") == 2,
                      str_replace(Image_ID, "^([a-zA-Z]+)_([0-9]+)_", "\\1\\2_"),
                      Image_ID),# Split into two new columns
    Image_Timepoint = str_extract(Image_ID, "^[^_]+"),       # before _
    Tank_Number    = str_extract(Image_ID, "(?<=_)[^_]+")) %>% # after _
  select(!Image_ID)


# - read in treatment data 

Treatment <- read_excel("Data/Ant Farm/Ant_Farm_Setup.xlsx") %>% 
                select(!Ant_Farm_number:Ant_Farm_Side)


# Merge them together using a function from the join() family (Ex: left_join())

Image_data <- 
  ImageJ %>% 
  full_join(Treatment, 
            by = "Tank_Number") %>% 
  mutate(ODepth_Average = as.numeric(ODepth_Average),
         Species = ifelse(is.na(Species), "Control",
                           Species))

# Getting in depth profiles

file_path <- "Data/Ant Farm/Plot_Profiles.xlsx"  # Replace with your actual file path

Profiles <- excel_sheets(file_path)[-1] %>%  # Skip metadata sheet
  set_names() %>%
  map_dfr(~ read_excel(file_path, sheet = .x, skip = 2, col_names = FALSE) %>%
            setNames(c("Depth", "Grey_Value")) %>%  # Force column names
            mutate(
              Depth = as.numeric(Depth),
              Grey_Value = as.numeric(Grey_Value),
              Image_ID = .x
            ),
          .id = "Sheet_Name") %>%   # Optional: keep sheet name for traceability
        filter(!Depth == "NA")


# Now we need to split columns again to get Tank_Number and Image_Timepoint 


# Then link with treatment file as above



# Data Analysis ------------------------------------


# Find sediment surface from plot profiles from t0 to get sediment height and use for all else 



# Our ant farm measurements were all taken from the top of the tank so we need to subtract the the time 0 depth profiles (min) which is the sediment surface 



# Create a summary box plot for some of the metrics per species through time


Image_data %>% 
  filter(Image_Timepoint == "t336") %>% 
  ggplot(
    aes( x = Species,
         y = ODepth_Average) ) +
  geom_boxplot() +
  labs(title = "t336 Oxygen Depth",
       y = "O2 Depth (cm)",
       x = "Species")
