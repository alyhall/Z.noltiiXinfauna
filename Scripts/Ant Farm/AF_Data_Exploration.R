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

ImageJ <- read_excel("C:/Users/PC1/OneDrive/Desktop/MARINE DESSERTATION/github/Z.noltiiXinfauna/Data/Ant Farm/ImageJ_Analysis.xlsx", 
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

Treatment <- read_excel("C:/Users/PC1/OneDrive/Desktop/MARINE DESSERTATION/github/Z.noltiiXinfauna/Data/Ant Farm/Ant_Farm_Setup.xlsx") %>% 
                select(!Ant_Farm_number:Ant_Farm_Side)


View(Treatment)
# Merge them together using a function from the join() family (Ex: left_join())

Image_data <- 
  ImageJ %>% 
  full_join(Treatment, 
            by = "Tank_Number") %>% 
  mutate(ODepth_Average = as.numeric(ODepth_Average),
         Species = ifelse(is.na(Species), "Control",
                           Species))

# Getting in depth profiles ()

file_path <- "C:/Users/PC1/OneDrive/Desktop/MARINE DESSERTATION/github/Z.noltiiXinfauna/Data/Ant Farm/Plot_Profiles.xlsx"  # Replace with your actual file path
#problem stARTS HERE


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

##A1

#subset out one tank all time points
A1 <- subset(Profiles, Profiles$Tank_Number=="1A")

#cut off space above tank for this profile all time points
A1 <- subset(A1, A1$Depth > 11.769)

#Find step length for profil1e distance for each timepoint
A1_SL0 <- as.data.frame(A1[A1$Time_Point==0, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==0, "Depth"])[1,1]
A1_SL52 <- as.data.frame(A1[A1$Time_Point==52, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==52, "Depth"])[1,1]
A1_SL168 <- as.data.frame(A1[A1$Time_Point==168, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==168, "Depth"])[1,1]
A1_SL240 <- as.data.frame(A1[A1$Time_Point==240, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==240, "Depth"])[1,1]
A1_SL336 <- as.data.frame(A1[A1$Time_Point==336, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A1$Time_Point)

#Recreate adjusted depth profile distances
A1_AdjD_0 <- seq(0, (as.numeric(count(A1[A1$Time_Point==0,]))-1)*A1_SL0, by = A1_SL0)
A1_AdjD_52 <- seq(0, (as.numeric(count(A1[A1$Time_Point==52,]))-1)*A1_SL52, by = A1_SL52)
A1_AdjD_168 <- seq(0, (as.numeric(count(A1[A1$Time_Point==168,]))-1)*A1_SL168, by = A1_SL168)
A1_AdjD_240 <- seq(0, (as.numeric(count(A1[A1$Time_Point==240,]))-1)*A1_SL240, by = A1_SL240)
A1_AdjD_336 <- seq(0, (as.numeric(count(A1[A1$Time_Point==336,]))-1)*A1_SL336, by = A1_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A1$Adj_D <- c(A1_AdjD_0, A1_AdjD_52, A1_AdjD_168, A1_AdjD_240, B1_AdjD_336)


##A2

#subset out one tank all time points
A2 <- subset(Profiles, Profiles$Tank_Number=="2A")

#cut off space above tank for this profile all time points
A2 <- subset(A2, A2$Depth > 13.094)

#Find step length for profil1e distance for each timepoint
A2_SL0 <- as.data.frame(A2[A2$Time_Point==0, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==0, "Depth"])[1,1]
A2_SL52 <- as.data.frame(A2[A2$Time_Point==52, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==52, "Depth"])[1,1]
A2_SL168 <- as.data.frame(A2[A2$Time_Point==168, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==168, "Depth"])[1,1]
A2_SL240 <- as.data.frame(A2[A2$Time_Point==240, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==240, "Depth"])[1,1]
A2_SL336 <- as.data.frame(A2[A2$Time_Point==336, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A2$Time_Point)

#Recreate adjusted depth profile distances
A2_AdjD_0 <- seq(0, (as.numeric(count(A2[A2$Time_Point==0,]))-1)*A2_SL0, by = A2_SL0)
A2_AdjD_56 <- seq(0, (as.numeric(count(A2[A2$Time_Point==56,]))-1)*A2_SL56, by = A2_SL56)
A2_AdjD_168 <- seq(0, (as.numeric(count(A2[A2$Time_Point==168,]))-1)*A2_SL168, by = A2_SL168)
A2_AdjD_240 <- seq(0, (as.numeric(count(A2[A2$Time_Point==240,]))-1)*A2_SL240, by = A2_SL240)
A2_AdjD_336 <- seq(0, (as.numeric(count(A2[A2$Time_Point==336,]))-1)*A2_SL336, by = A2_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A2$Adj_D <- c(A2_AdjD_0, A2_AdjD_52, A2_AdjD_168, A2_AdjD_240, B1_AdjD_336)

##B2

#subset out one tank all time points
B2 <- subset(Profiles, Profiles$Tank_Number=="2B")

#cut off space above tank for this profile all time points
B2 <- subset(B2, B2$Depth > 13.323)

#Find step length for profil1e distance for each timepoint
B2_SL0 <- as.data.frame(B2[B2$Time_Point==0, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==0, "Depth"])[1,1]
B2_SL52 <- as.data.frame(B2[B2$Time_Point==52, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==52, "Depth"])[1,1]
B2_SL168 <- as.data.frame(B2[B2$Time_Point==168, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==168, "Depth"])[1,1]
B2_SL240 <- as.data.frame(B2[B2$Time_Point==240, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==240, "Depth"])[1,1]
B2_SL336 <- as.data.frame(B2[B2$Time_Point==336, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B2$Time_Point)

#Recreate adjusted depth profile distances
B2_AdjD_0 <- seq(0, (as.numeric(count(B2[B2$Time_Point==0,]))-1)*B2_SL0, by = B2_SL0)
B2_AdjD_52 <- seq(0, (as.numeric(count(B2[B2$Time_Point==52,]))-1)*B2_SL52, by = B2_SL52)
B2_AdjD_168 <- seq(0, (as.numeric(count(B2[B2$Time_Point==168,]))-1)*B2_SL168, by = B2_SL168)
B2_AdjD_240 <- seq(0, (as.numeric(count(B2[B2$Time_Point==240,]))-1)*B2_SL240, by = B2_SL240)
B2_AdjD_336 <- seq(0, (as.numeric(count(B2[B2$Time_Point==336,]))-1)*B2_SL336, by = B2_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B2$Adj_D <- c(B2_AdjD_0, B2_AdjD_52, B2_AdjD_168, B2_AdjD_240, B2_AdjD_336)


##A3

#subset out one tank all time points
A3 <- subset(Profiles, Profiles$Tank_Number=="3A")

#cut off space above tank for this profile all time points
A3 <- subset(A3, A3$Depth > 12.48)

#Find step length for profil1e distance for each timepoint
A3_SL0 <- as.data.frame(A3[A3$Time_Point==0, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==0, "Depth"])[1,1]
A3_SL52 <- as.data.frame(A3[A3$Time_Point==52, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==52, "Depth"])[1,1]
A3_SL168 <- as.data.frame(A3[A3$Time_Point==168, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==168, "Depth"])[1,1]
A3_SL240 <- as.data.frame(A3[A3$Time_Point==240, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==240, "Depth"])[1,1]
A3_SL336 <- as.data.frame(A3[A3$Time_Point==336, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A3$Time_Point)

#Recreate adjusted depth profile distances
A3_AdjD_0 <- seq(0, (as.numeric(count(A3[A3$Time_Point==0,]))-1)*A3_SL0, by = A3_SL0)
A3_AdjD_52 <- seq(0, (as.numeric(count(A3[A3$Time_Point==52,]))-1)*A3_SL52, by = A3_SL52)
A3_AdjD_168 <- seq(0, (as.numeric(count(A3[A3$Time_Point==168,]))-1)*A3_SL168, by = A3_SL168)
A3_AdjD_240 <- seq(0, (as.numeric(count(A3[A3$Time_Point==240,]))-1)*A3_SL240, by = A3_SL240)
A3_AdjD_336 <- seq(0, (as.numeric(count(A3[A3$Time_Point==336,]))-1)*A3_SL336, by = A3_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A3$Adj_D <- c(A3_AdjD_0, A3_AdjD_52, A3_AdjD_168, A3_AdjD_240, A3_AdjD_336)



##B3

#subset out one tank all time points
B3 <- subset(Profiles, Profiles$Tank_Number=="B3")

#cut off space above tank for this profile all time points
B3 <- subset(B3, B3$Depth > 12.864)

#Find step length for profil1e distance for each timepoint
B3_SL0 <- as.data.frame(B3[B3$Time_Point==0, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==0, "Depth"])[1,1]
B3_SL52 <- as.data.frame(B3[B3$Time_Point==52, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==52, "Depth"])[1,1]
B3_SL168 <- as.data.frame(B3[B3$Time_Point==168, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==168, "Depth"])[1,1]
B3_SL240 <- as.data.frame(B3[B3$Time_Point==240, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==240, "Depth"])[1,1]
B3_SL336 <- as.data.frame(B3[B3$Time_Point==336, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B3$Time_Point)

#Recreate adjusted depth profile distances
B3_AdjD_0 <- seq(0, (as.numeric(count(B3[B3$Time_Point==0,]))-1)*B3_SL0, by = B3_SL0)
B3_AdjD_52 <- seq(0, (as.numeric(count(B3[B3$Time_Point==52,]))-1)*B3_SL52, by = B3_SL52)
B3_AdjD_168 <- seq(0, (as.numeric(count(B3[B3$Time_Point==168,]))-1)*B3_SL168, by = B3_SL168)
B3_AdjD_240 <- seq(0, (as.numeric(count(B3[B3$Time_Point==240,]))-1)*B3_SL240, by = B3_SL240)
B3_AdjD_336 <- seq(0, (as.numeric(count(B3[B3$Time_Point==336,]))-1)*B3_SL336, by = B3_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B3$Adj_D <- c(B3_AdjD_0, B3_AdjD_52, B3_AdjD_168, B3_AdjD_240, B3_AdjD_336)

##A4

#subset out one tank all time points
A4 <- subset(Profiles, Profiles$Tank_Number=="4A")

#cut off space above tank for this profile all time points
A4 <- subset(A4, A4$Depth > 13.778)

#Find step length for profil1e distance for each timepoint
A4_SL0 <- as.data.frame(A4[A4$Time_Point==0, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==0, "Depth"])[1,1]
A4_SL52 <- as.data.frame(A4[A4$Time_Point==52, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==52, "Depth"])[1,1]
A4_SL168 <- as.data.frame(A4[A4$Time_Point==168, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==168, "Depth"])[1,1]
A4_SL240 <- as.data.frame(A4[A4$Time_Point==240, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==240, "Depth"])[1,1]
A4_SL336 <- as.data.frame(A4[A4$Time_Point==336, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A4$Time_Point)

#Recreate adjusted depth profile distances
A4_AdjD_0 <- seq(0, (as.numeric(count(A4[A4$Time_Point==0,]))-1)*A4_SL0, by = A4_SL0)
A4_AdjD_52 <- seq(0, (as.numeric(count(A4[A4$Time_Point==52,]))-1)*A4_SL52, by = A4_SL52)
A4_AdjD_168 <- seq(0, (as.numeric(count(A4[A4$Time_Point==168,]))-1)*A4_SL168, by = A4_SL168)
A4_AdjD_240 <- seq(0, (as.numeric(count(A4[A4$Time_Point==240,]))-1)*A4_SL240, by = A4_SL240)
A4_AdjD_336 <- seq(0, (as.numeric(count(A4[A4$Time_Point==336,]))-1)*A4_SL336, by = A4_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A4$Adj_D <- c(A4_AdjD_0, A4_AdjD_52, A4_AdjD_168, A4_AdjD_240, A4_AdjD_336)

##B4

#subset out one tank all time points
B4 <- subset(Profiles, Profiles$Tank_Number=="4B")

#cut off space above tank for this profile all time points
B4 <- subset(B4, B4$Depth > 13.395)

#Find step length for profil1e distance for each timepoint
B4_SL0 <- as.data.frame(B4[B4$Time_Point==0, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==0, "Depth"])[1,1]
B4_SL52 <- as.data.frame(B4[B4$Time_Point==52, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==52, "Depth"])[1,1]
B4_SL168 <- as.data.frame(B4[B4$Time_Point==168, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==168, "Depth"])[1,1]
B4_SL240 <- as.data.frame(B4[B4$Time_Point==240, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==240, "Depth"])[1,1]
B4_SL336 <- as.data.frame(B4[B4$Time_Point==336, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B4$Time_Point)

#Recreate adjusted depth profile distances
B4_AdjD_0 <- seq(0, (as.numeric(count(B4[B4$Time_Point==0,]))-1)*B4_SL0, by = B4_SL0)
B4_AdjD_52 <- seq(0, (as.numeric(count(B4[B4$Time_Point==52,]))-1)*B4_SL52, by = B4_SL52)
B4_AdjD_168 <- seq(0, (as.numeric(count(B4[B4$Time_Point==168,]))-1)*B4_SL168, by = B4_SL168)
B4_AdjD_240 <- seq(0, (as.numeric(count(B4[B4$Time_Point==240,]))-1)*B4_SL240, by = B4_SL240)
B4_AdjD_336 <- seq(0, (as.numeric(count(B4[B4$Time_Point==336,]))-1)*B4_SL336, by = B4_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B4$Adj_D <- c(B4_AdjD_0, B4_AdjD_52, B4_AdjD_168, B4_AdjD_240, B4_AdjD_336)


##A5

#subset out one tank all time points
A5 <- subset(Profiles, Profiles$Tank_Number=="5A")

#cut off space above tank for this profile all time points
A5 <- subset(A5, A5$Depth > 13.395)

#Find step length for profil1e distance for each timepoint
A5_SL0 <- as.data.frame(A5[A5$Time_Point==0, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==0, "Depth"])[1,1]
A5_SL52 <- as.data.frame(A5[A5$Time_Point==52, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==52, "Depth"])[1,1]
A5_SL168 <- as.data.frame(A5[A5$Time_Point==168, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==168, "Depth"])[1,1]
A5_SL240 <- as.data.frame(A5[A5$Time_Point==240, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==240, "Depth"])[1,1]
A5_SL336 <- as.data.frame(A5[A5$Time_Point==336, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A5$Time_Point)

#Recreate adjusted depth profile distances
A5_AdjD_0 <- seq(0, (as.numeric(count(A5[A5$Time_Point==0,]))-1)*A5_SL0, by = A5_SL0)
A5_AdjD_52 <- seq(0, (as.numeric(count(A5[A5$Time_Point==52,]))-1)*A5_SL52, by = A5_SL52)
A5_AdjD_168 <- seq(0, (as.numeric(count(A5[A5$Time_Point==168,]))-1)*A5_SL168, by = A5_SL168)
A5_AdjD_240 <- seq(0, (as.numeric(count(A5[A5$Time_Point==240,]))-1)*A5_SL240, by = A5_SL240)
A5_AdjD_336 <- seq(0, (as.numeric(count(A5[A5$Time_Point==336,]))-1)*A5_SL336, by = A5_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A5$Adj_D <- c(A5_AdjD_0, A5_AdjD_52, A5_AdjD_168, A5_AdjD_240, A5_AdjD_336)


##B5

#subset out one tank all time points
B5 <- subset(Profiles, Profiles$Tank_Number=="1B")

#cut off space above tank for this profile all time points
B5 <- subset(B5, B5$Depth > 13.016)

#Find step length for profil1e distance for each timepoint
B5_SL0 <- as.data.frame(B5[B5$Time_Point==0, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==0, "Depth"])[1,1]
B5_SL52 <- as.data.frame(B5[B5$Time_Point==52, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==52, "Depth"])[1,1]
B5_SL168 <- as.data.frame(B5[B5$Time_Point==168, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==168, "Depth"])[1,1]
B5_SL240 <- as.data.frame(B5[B5$Time_Point==240, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==240, "Depth"])[1,1]
B5_SL336 <- as.data.frame(B5[B5$Time_Point==336, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B5$Time_Point)

#Recreate adjusted depth profile distances
B5_AdjD_0 <- seq(0, (as.numeric(count(B5[B5$Time_Point==0,]))-1)*B5_SL0, by = B5_SL0)
B5_AdjD_52 <- seq(0, (as.numeric(count(B5[B5$Time_Point==52,]))-1)*B5_SL52, by = B5_SL52)
B5_AdjD_168 <- seq(0, (as.numeric(count(B5[B5$Time_Point==168,]))-1)*B5_SL168, by = B5_SL168)
B5_AdjD_240 <- seq(0, (as.numeric(count(B5[B5$Time_Point==240,]))-1)*B5_SL240, by = B5_SL240)
B5_AdjD_336 <- seq(0, (as.numeric(count(B5[B5$Time_Point==336,]))-1)*B5_SL336, by = B5_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B5$Adj_D <- c(B5_AdjD_0, B5_AdjD_52, B5_AdjD_168, B5_AdjD_240, B5_AdjD_336)


##A6

#subset out one tank all time points
A6 <- subset(Profiles, Profiles$Tank_Number=="1B")

#cut off space above tank for this profile all time points
A6 <- subset(A6, A6$Depth > 13.128)

#Find step length for profil1e distance for each timepoint
A6_SL0 <- as.data.frame(A6[A6$Time_Point==0, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==0, "Depth"])[1,1]
A6_SL52 <- as.data.frame(A6[A6$Time_Point==52, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==52, "Depth"])[1,1]
A6_SL168 <- as.data.frame(A6[A6$Time_Point==168, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==168, "Depth"])[1,1]
A6_SL240 <- as.data.frame(A6[A6$Time_Point==240, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==240, "Depth"])[1,1]
A6_SL336 <- as.data.frame(A6[A6$Time_Point==336, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A6$Time_Point)

#Recreate adjusted depth profile distances
A6_AdjD_0 <- seq(0, (as.numeric(count(A6[A6$Time_Point==0,]))-1)*A6_SL0, by = A6_SL0)
A6_AdjD_52 <- seq(0, (as.numeric(count(A6[A6$Time_Point==52,]))-1)*A6_SL52, by = A6_SL52)
A6_AdjD_168 <- seq(0, (as.numeric(count(A6[A6$Time_Point==168,]))-1)*A6_SL168, by = A6_SL168)
A6_AdjD_240 <- seq(0, (as.numeric(count(A6[A6$Time_Point==240,]))-1)*A6_SL240, by = A6_SL240)
A6_AdjD_336 <- seq(0, (as.numeric(count(A6[A6$Time_Point==336,]))-1)*A6_SL336, by = A6_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A6$Adj_D <- c(A6_AdjD_0, A6_AdjD_52, A6_AdjD_168, A6_AdjD_240, A6_AdjD_336)


##B6

#subset out one tank all time points
B6 <- subset(Profiles, Profiles$Tank_Number=="6B")

#cut off space above tank for this profile all time points
B6 <- subset(B6, B6$Depth > 12.585)

#Find step length for profil1e distance for each timepoint
B6_SL0 <- as.data.frame(B6[B6$Time_Point==0, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==0, "Depth"])[1,1]
B6_SL52 <- as.data.frame(B6[B6$Time_Point==52, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==52, "Depth"])[1,1]
B6_SL168 <- as.data.frame(B6[B6$Time_Point==168, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==168, "Depth"])[1,1]
B6_SL240 <- as.data.frame(B6[B6$Time_Point==240, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==240, "Depth"])[1,1]
B6_SL336 <- as.data.frame(B6[B6$Time_Point==336, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B6$Time_Point)

#Recreate adjusted depth profile distances
B6_AdjD_0 <- seq(0, (as.numeric(count(B6[B6$Time_Point==0,]))-1)*B6_SL0, by = B6_SL0)
B6_AdjD_52 <- seq(0, (as.numeric(count(B6[B6$Time_Point==52,]))-1)*B6_SL52, by = B6_SL52)
B6_AdjD_168 <- seq(0, (as.numeric(count(B6[B6$Time_Point==168,]))-1)*B6_SL168, by = B6_SL168)
B6_AdjD_240 <- seq(0, (as.numeric(count(B6[B6$Time_Point==240,]))-1)*B6_SL240, by = B6_SL240)
B6_AdjD_336 <- seq(0, (as.numeric(count(B6[B6$Time_Point==336,]))-1)*B6_SL336, by = B6_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B6$Adj_D <- c(B6_AdjD_0, B6_AdjD_52, B6_AdjD_168, B6_AdjD_240, B6_AdjD_336)




unique(Profiles$Tank_Number=="2A")

z2<- seq(0, as.numeric(count(A2[A1$Time_Point==56,]))*0.0075, by = 0.0075)
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
