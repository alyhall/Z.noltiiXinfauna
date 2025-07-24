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

Profiles <-
Profiles %>%
  mutate(
    Name = str_match(Image_ID, "^([A-Z]+)_")[,2],
    Tank_Number = str_match(Image_ID, "_([0-9A-Z]+)_")[,2],
    Time_Point = as.numeric(str_match(Image_ID, "_([0-9]+)$")[,2]),
    Tank_Number = ifelse(Tank_Number == "B4","4B",Tank_Number)
  )

# Then link with treatment file as above

Profiles <- 
  Treatment %>% 
 mutate(Species = ifelse(is.na(Species), "Control",
             Species)) %>% 
  full_join(Profiles, 
            by = "Tank_Number")

 Profiles$Species
#scatter plot

Profiles %>% 
  filter(Time_Point == "0") %>% 
  ggplot(
    aes( x = Depth,
         y = Grey_Value,
         colour = Tank_Number ) ) +
  geom_point() +
  labs(title = "scatter plot ",
       y = "Grey Value (cm)",
       x = "Depth(cm)" ) +
   facet_wrap(~Species) 

# Data Analysis ------------------------------------


# Find sediment surface from plot profiles from t0 to get sediment height and use for all else 
# Subtract empty space from plot profile, measured for each tank at t0

#subset out one tank all time points
B1 <- subset(Profiles, Profiles$Tank_Number=="1B")
             
#cut off space above tank for this profile all time points
B1 <- subset(B1, B1$Depth > 12.729)

#Find step length for profil1e distance for each timepoint
B1_SL0 <- as.data.frame(B1[B1$Time_Point==0, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==0, "Depth"])[1,1]
B1_SL52 <- as.data.frame(B1[B1$Time_Point==52, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==52, "Depth"])[1,1]
B1_SL168 <- as.data.frame(B1[B1$Time_Point==168, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==168, "Depth"])[1,1]
B1_SL240 <- as.data.frame(B1[B1$Time_Point==240, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==240, "Depth"])[1,1]
B1_SL336 <- as.data.frame(B1[B1$Time_Point==336, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B1$Time_Point)

#Recreate adjusted depth profile distances
B1_AdjD_0 <- seq(0, (as.numeric(count(B1[B1$Time_Point==0,]))-1)*B1_SL0, by = B1_SL0)
B1_AdjD_52 <- seq(0, (as.numeric(count(B1[B1$Time_Point==52,]))-1)*B1_SL52, by = B1_SL52)
B1_AdjD_168 <- seq(0, (as.numeric(count(B1[B1$Time_Point==168,]))-1)*B1_SL168, by = B1_SL168)
B1_AdjD_240 <- seq(0, (as.numeric(count(B1[B1$Time_Point==240,]))-1)*B1_SL240, by = B1_SL240)
B1_AdjD_336 <- seq(0, (as.numeric(count(B1[B1$Time_Point==336,]))-1)*B1_SL336, by = B1_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B1$Adj_D <- c(B1_AdjD_0, B1_AdjD_52, B1_AdjD_168, B1_AdjD_240, B1_AdjD_336)


#run these 13 lines of code for all profile

#recombine all profiles back into one data set "Profiles2"





z2<- seq(0, as.numeric(count(A1[A1$Time_Point==56,]))*0.0075, by = 0.0075)
z3 <- seq(0, as.numeric(count(A1[A1$Time_Point==240,]))*0.0075, by = 0.0075)


as.data.frame(A1[A1$Time_Point==336, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==336, "Depth"])[1,1]

tail(z)

count(A1[A1$Time_Point==56,])


A1[A1$Time_Point==0,]


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
