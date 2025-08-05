## Header and Preamble ---------------------------
##
## Script name: 
##
## Purpose of script: 
##
## Author: Alyson & Ashitha
##
## Date Created: 2025-07-18
## Date Edited: 2025-07-29
## Last Edited by: Ashitha
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

file_path <- "C:/Users/PC1/OneDrive/Desktop/MARINE DESSERTATION/github/Z.noltiiXinfauna/Data/Ant Farm/plot profile 280725.xlsx"  # Replace with your actual file path



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
    Time_Point = as.numeric(str_match(Image_ID, "^t(\\d+)_")[,2]),
    Tank_Number = str_match(Image_ID, "_([A-Za-z0-9]+)$")[,2])



# Then link with treatment file as above

Profiles2 <- left_join(Profiles, Treatment)




###optional

#Profiles2 <- 
#  Treatment %>% 
# mutate(Species = ifelse(is.na(Species), "Control",
#             Species)) %>% 
#  full_join(Profiles, 
 #           by = "Tank_Number")

 #Profiles$Species

# Data Analysis ------------------------------------


# Find sediment surface from plot profiles from t0 to get sediment height and use for all else 
# Subtract empty space from plot profile, measured for each tank at t0

#subset out one tank all time points

B1 <- subset(Profiles2, Profiles2$Tank_Number=="B1")
             
#cut off space above tank for this profile all time points

B1 <- subset(B1, B1$Depth > 12.729)

#Find step length for profil1e distance for each timepoint
B1_SL0 <- as.data.frame(B1[B1$Time_Point==0, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==0, "Depth"])[1,1]
B1_SL56 <- as.data.frame(B1[B1$Time_Point==56, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==56, "Depth"])[1,1]
B1_SL168 <- as.data.frame(B1[B1$Time_Point==168, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==168, "Depth"])[1,1]
B1_SL240 <- as.data.frame(B1[B1$Time_Point==240, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==240, "Depth"])[1,1]
B1_SL336 <- as.data.frame(B1[B1$Time_Point==336, "Depth"])[2,1] - as.data.frame(B1[B1$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B1$Time_Point)

#Recreate adjusted depth profile distances
B1_AdjD_0 <- seq(0, (as.numeric(count(B1[B1$Time_Point==0,]))-1)*B1_SL0, by = B1_SL0)
B1_AdjD_56 <- seq(0, (as.numeric(count(B1[B1$Time_Point==56,]))-1)*B1_SL56, by = B1_SL56)
B1_AdjD_168 <- seq(0, (as.numeric(count(B1[B1$Time_Point==168,]))-1)*B1_SL168, by = B1_SL168)
B1_AdjD_240 <- seq(0, (as.numeric(count(B1[B1$Time_Point==240,]))-1)*B1_SL240, by = B1_SL240)
B1_AdjD_336 <- seq(0, (as.numeric(count(B1[B1$Time_Point==336,]))-1)*B1_SL336, by = B1_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in

B1 <- B1[order(B1$Time_Point,B1$Depth), ]
B1$Adj_D <- c(B1_AdjD_0, B1_AdjD_56, B1_AdjD_168, B1_AdjD_240, B1_AdjD_336)


#run these 13 lines of code for all profile




#recombine all profiles back into one data set "Profiles2"

##A1

#subset out one tank all time points
A1 <- subset(Profiles2, Profiles2$Tank_Number=="A1")

unique(A1$Tank_Number)


#cut off space above tank for this profile all time points
A1 <- subset(A1, A1$Depth > 11.769)

#Find step length for profil1e distance for each timepoint
A1_SL0 <- as.data.frame(A1[A1$Time_Point==0, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==0, "Depth"])[1,1]
A1_SL56 <- as.data.frame(A1[A1$Time_Point==56, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==56, "Depth"])[1,1]
A1_SL168 <- as.data.frame(A1[A1$Time_Point==168, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==168, "Depth"])[1,1]
A1_SL240 <- as.data.frame(A1[A1$Time_Point==240, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==240, "Depth"])[1,1]
A1_SL336 <- as.data.frame(A1[A1$Time_Point==336, "Depth"])[2,1] - as.data.frame(A1[A1$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A1$Time_Point)

#Recreate adjusted depth profile distances
A1_AdjD_0 <- seq(0, (as.numeric(count(A1[A1$Time_Point==0,]))-1)*A1_SL0, by = A1_SL0)
A1_AdjD_56 <- seq(0, (as.numeric(count(A1[A1$Time_Point==56,]))-1)*A1_SL56, by = A1_SL56)
A1_AdjD_168 <- seq(0, (as.numeric(count(A1[A1$Time_Point==168,]))-1)*A1_SL168, by = A1_SL168)
A1_AdjD_240 <- seq(0, (as.numeric(count(A1[A1$Time_Point==240,]))-1)*A1_SL240, by = A1_SL240)
A1_AdjD_336 <- seq(0, (as.numeric(count(A1[A1$Time_Point==336,]))-1)*A1_SL336, by = A1_SL336)

z<- subset(A1, A1$Time_Point == 240)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A1 <- A1[order(A1$Time_Point,A1$Depth), ]
A1$Adj_D <- c(A1_AdjD_0, A1_AdjD_56, A1_AdjD_168, A1_AdjD_240, A1_AdjD_336)


as.numeric(count(A1[A1$Time_Point==168,]))*A1_SL168

##A2

#subset out one tank all time points
A2 <- subset(Profiles2, Profiles2$Tank_Number=="A2")

#cut off space above tank for this profile all time points
A2 <- subset(A2, A2$Depth > 13.094)

#Find step length for profil1e distance for each timepoint
A2_SL0 <- as.data.frame(A2[A2$Time_Point==0, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==0, "Depth"])[1,1]
A2_SL56 <- as.data.frame(A2[A2$Time_Point==56, "Depth"])[2,1] - as.data.frame(A2[A2$Time_Point==56, "Depth"])[1,1]
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

tail(A2_AdjD_56)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A2 <- A2[order(A2$Time_Point,A2$Depth), ]
A2$Adj_D <- c(A2_AdjD_0, A2_AdjD_56, A2_AdjD_168, A2_AdjD_240, A2_AdjD_336)

##B2

#subset out one tank all time points
B2 <- subset(Profiles2, Profiles2$Tank_Number=="B2")

#cut off space above tank for this profile all time points
B2 <- subset(B2, B2$Depth > 13.323)

#Find step length for profil1e distance for each timepoint
B2_SL0 <- as.data.frame(B2[B2$Time_Point==0, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==0, "Depth"])[1,1]
B2_SL56 <- as.data.frame(B2[B2$Time_Point==56, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==56, "Depth"])[1,1]
B2_SL168 <- as.data.frame(B2[B2$Time_Point==168, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==168, "Depth"])[1,1]
B2_SL240 <- as.data.frame(B2[B2$Time_Point==240, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==240, "Depth"])[1,1]
B2_SL336 <- as.data.frame(B2[B2$Time_Point==336, "Depth"])[2,1] - as.data.frame(B2[B2$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B2$Time_Point)

#Recreate adjusted depth profile distances
B2_AdjD_0 <- seq(0, (as.numeric(count(B2[B2$Time_Point==0,]))-1)*B2_SL0, by = B2_SL0)
B2_AdjD_56 <- seq(0, (as.numeric(count(B2[B2$Time_Point==56,]))-1)*B2_SL56, by = B2_SL56)
B2_AdjD_168 <- seq(0, (as.numeric(count(B2[B2$Time_Point==168,]))-1)*B2_SL168, by = B2_SL168)
B2_AdjD_240 <- seq(0, (as.numeric(count(B2[B2$Time_Point==240,]))-1)*B2_SL240, by = B2_SL240)
B2_AdjD_336 <- seq(0, (as.numeric(count(B2[B2$Time_Point==336,]))-1)*B2_SL336, by = B2_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B2 <- B2[order(B2$Time_Point,B2$Depth), ]
B2$Adj_D <- c(B2_AdjD_0, B2_AdjD_56, B2_AdjD_168, B2_AdjD_240, B2_AdjD_336)


##A3

#subset out one tank all time points
A3 <- subset(Profiles2, Profiles2$Tank_Number=="A3")

#cut off space above tank for this profile all time points
A3 <- subset(A3, A3$Depth > 12.48)

#Find step length for profil1e distance for each timepoint
A3_SL0 <- as.data.frame(A3[A3$Time_Point==0, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==0, "Depth"])[1,1]
A3_SL56 <- as.data.frame(A3[A3$Time_Point==56, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==56, "Depth"])[1,1]
A3_SL168 <- as.data.frame(A3[A3$Time_Point==168, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==168, "Depth"])[1,1]
A3_SL240 <- as.data.frame(A3[A3$Time_Point==240, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==240, "Depth"])[1,1]
A3_SL336 <- as.data.frame(A3[A3$Time_Point==336, "Depth"])[2,1] - as.data.frame(A3[A3$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A3$Time_Point)

#Recreate adjusted depth profile distances
A3_AdjD_0 <- seq(0, (as.numeric(count(A3[A3$Time_Point==0,]))-1)*A3_SL0, by = A3_SL0)
A3_AdjD_56 <- seq(0, (as.numeric(count(A3[A3$Time_Point==56,]))-1)*A3_SL56, by = A3_SL56)
A3_AdjD_168 <- seq(0, (as.numeric(count(A3[A3$Time_Point==168,]))-1)*A3_SL168, by = A3_SL168)
A3_AdjD_240 <- seq(0, (as.numeric(count(A3[A3$Time_Point==240,]))-1)*A3_SL240, by = A3_SL240)
A3_AdjD_336 <- seq(0, (as.numeric(count(A3[A3$Time_Point==336,]))-1)*A3_SL336, by = A3_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A3 <- A3[order(A3$Time_Point,A3$Depth), ]
A3$Adj_D <- c(A3_AdjD_0, A3_AdjD_56, A3_AdjD_168, A3_AdjD_240, A3_AdjD_336)



##B3

#subset out one tank all time points

B3 <- subset(Profiles2, Profiles2$Tank_Number=="B3")

#cut off space above tank for this profile all time points
B3 <- subset(B3, B3$Depth > 12.864)


#Find step length for profil1e distance for each timepoint
B3_SL0 <- as.data.frame(B3[B3$Time_Point==0, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==0, "Depth"])[1,1]
B3_SL56 <- as.data.frame(B3[B3$Time_Point==56, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==56, "Depth"])[1,1]
B3_SL168 <- as.data.frame(B3[B3$Time_Point==168, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==168, "Depth"])[1,1]
B3_SL240 <- as.data.frame(B3[B3$Time_Point==240, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==240, "Depth"])[1,1]
B3_SL336 <- as.data.frame(B3[B3$Time_Point==336, "Depth"])[2,1] - as.data.frame(B3[B3$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B3$Time_Point)

#Recreate adjusted depth profile distances
B3_AdjD_0 <- seq(0, (as.numeric(count(B3[B3$Time_Point==0,]))-1)*B3_SL0, by = B3_SL0)
B3_AdjD_56 <- seq(0, (as.numeric(count(B3[B3$Time_Point==56,]))-1)*B3_SL56, by = B3_SL56)
B3_AdjD_168 <- seq(0, (as.numeric(count(B3[B3$Time_Point==168,]))-1)*B3_SL168, by = B3_SL168)
B3_AdjD_240 <- seq(0, (as.numeric(count(B3[B3$Time_Point==240,]))-1)*B3_SL240, by = B3_SL240)
B3_AdjD_336 <- seq(0, (as.numeric(count(B3[B3$Time_Point==336,]))-1)*B3_SL336, by = B3_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
B3 <- B3[order(B3$Time_Point,B3$Depth), ]
B3$Adj_D <- c(B3_AdjD_0, B3_AdjD_56, B3_AdjD_168, B3_AdjD_240, B3_AdjD_336)

##A4

#subset out one tank all time points
A4 <- subset(Profiles2, Profiles2$Tank_Number=="A4")

#cut off space above tank for this profile all time points
A4 <- subset(A4, A4$Depth > 13.778)

#Find step length for profil1e distance for each timepoint
A4_SL0 <- as.data.frame(A4[A4$Time_Point==0, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==0, "Depth"])[1,1]
A4_SL56 <- as.data.frame(A4[A4$Time_Point==56, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==56, "Depth"])[1,1]
A4_SL168 <- as.data.frame(A4[A4$Time_Point==168, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==168, "Depth"])[1,1]
A4_SL240 <- as.data.frame(A4[A4$Time_Point==240, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==240, "Depth"])[1,1]
A4_SL336 <- as.data.frame(A4[A4$Time_Point==336, "Depth"])[2,1] - as.data.frame(A4[A4$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A4$Time_Point)

#Recreate adjusted depth profile distances
A4_AdjD_0 <- seq(0, (as.numeric(count(A4[A4$Time_Point==0,]))-1)*A4_SL0, by = A4_SL0)
A4_AdjD_56 <- seq(0, (as.numeric(count(A4[A4$Time_Point==56,]))-1)*A4_SL56, by = A4_SL56)
A4_AdjD_168 <- seq(0, (as.numeric(count(A4[A4$Time_Point==168,]))-1)*A4_SL168, by = A4_SL168)
A4_AdjD_240 <- seq(0, (as.numeric(count(A4[A4$Time_Point==240,]))-1)*A4_SL240, by = A4_SL240)
A4_AdjD_336 <- seq(0, (as.numeric(count(A4[A4$Time_Point==336,]))-1)*A4_SL336, by = A4_SL336)

#Combining all adjusted depth profiles into one column
#remember to put this timepoint back in
A4 <- A4[order(A4$Time_Point,A4$Depth), ]
A4$Adj_D <- c(A4_AdjD_0, A4_AdjD_56, A4_AdjD_168, A4_AdjD_240, A4_AdjD_336)

##B4

#subset out one tank all time points
B4 <- subset(Profiles2, Profiles2$Tank_Number=="B4")

#cut off space above tank for this profile all time points
B4 <- subset(B4, B4$Depth > 13.395)

#Find step length for profil1e distance for each timepoint
B4_SL0 <- as.data.frame(B4[B4$Time_Point==0, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==0, "Depth"])[1,1]
B4_SL56 <- as.data.frame(B4[B4$Time_Point==56, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==56, "Depth"])[1,1]
B4_SL168 <- as.data.frame(B4[B4$Time_Point==168, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==168, "Depth"])[1,1]
B4_SL240 <- as.data.frame(B4[B4$Time_Point==240, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==240, "Depth"])[1,1]
B4_SL336 <- as.data.frame(B4[B4$Time_Point==336, "Depth"])[2,1] - as.data.frame(B4[B4$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B4$Time_Point)

#Recreate adjusted depth profile distances
B4_AdjD_0 <- seq(0, (as.numeric(count(B4[B4$Time_Point==0,]))-1)*B4_SL0, by = B4_SL0)
B4_AdjD_56 <- seq(0, (as.numeric(count(B4[B4$Time_Point==56,]))-1)*B4_SL56, by = B4_SL56)
B4_AdjD_168 <- seq(0, (as.numeric(count(B4[B4$Time_Point==168,]))-1)*B4_SL168, by = B4_SL168)
B4_AdjD_240 <- seq(0, (as.numeric(count(B4[B4$Time_Point==240,]))-1)*B4_SL240, by = B4_SL240)
B4_AdjD_336 <- seq(0, (as.numeric(count(B4[B4$Time_Point==336,]))-1)*B4_SL336, by = B4_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
B4 <- B4[order(B4$Time_Point,B4$Depth), ]
B4$Adj_D <- c(B4_AdjD_0, B4_AdjD_56, B4_AdjD_168, B4_AdjD_240, B4_AdjD_336)


##A5

#subset out one tank all time points
A5 <- subset(Profiles2, Profiles2$Tank_Number=="A5")

#cut off space above tank for this profile all time points
A5 <- subset(A5, A5$Depth > 13.395)

#Find step length for profil1e distance for each timepoint
A5_SL0 <- as.data.frame(A5[A5$Time_Point==0, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==0, "Depth"])[1,1]
A5_SL56 <- as.data.frame(A5[A5$Time_Point==56, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==56, "Depth"])[1,1]
A5_SL168 <- as.data.frame(A5[A5$Time_Point==168, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==168, "Depth"])[1,1]
A5_SL240 <- as.data.frame(A5[A5$Time_Point==240, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==240, "Depth"])[1,1]
A5_SL336 <- as.data.frame(A5[A5$Time_Point==336, "Depth"])[2,1] - as.data.frame(A5[A5$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A5$Time_Point)

#Recreate adjusted depth profile distances
A5_AdjD_0 <- seq(0, (as.numeric(count(A5[A5$Time_Point==0,]))-1)*A5_SL0, by = A5_SL0)
A5_AdjD_56 <- seq(0, (as.numeric(count(A5[A5$Time_Point==56,]))-1)*A5_SL56, by = A5_SL56)
A5_AdjD_168 <- seq(0, (as.numeric(count(A5[A5$Time_Point==168,]))-1)*A5_SL168, by = A5_SL168)
A5_AdjD_240 <- seq(0, (as.numeric(count(A5[A5$Time_Point==240,]))-1)*A5_SL240, by = A5_SL240)
A5_AdjD_336 <- seq(0, (as.numeric(count(A5[A5$Time_Point==336,]))-1)*A5_SL336, by = A5_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
A5 <- A5[order(A5$Time_Point,A5$Depth), ]
A5$Adj_D <- c(A5_AdjD_0, A5_AdjD_56, A5_AdjD_168, A5_AdjD_240, A5_AdjD_336)


##B5

#subset out one tank all time points
B5 <- subset(Profiles2, Profiles2$Tank_Number=="B5")

#cut off space above tank for this profile all time points
B5 <- subset(B5, B5$Depth > 13.016)

#Find step length for profil1e distance for each timepoint
B5_SL0 <- as.data.frame(B5[B5$Time_Point==0, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==0, "Depth"])[1,1]
B5_SL56 <- as.data.frame(B5[B5$Time_Point==56, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==56, "Depth"])[1,1]
B5_SL168 <- as.data.frame(B5[B5$Time_Point==168, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==168, "Depth"])[1,1]
B5_SL240 <- as.data.frame(B5[B5$Time_Point==240, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==240, "Depth"])[1,1]
B5_SL336 <- as.data.frame(B5[B5$Time_Point==336, "Depth"])[2,1] - as.data.frame(B5[B5$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B5$Time_Point)

#Recreate adjusted depth profile distances
B5_AdjD_0 <- seq(0, (as.numeric(count(B5[B5$Time_Point==0,]))-1)*B5_SL0, by = B5_SL0)
B5_AdjD_56 <- seq(0, (as.numeric(count(B5[B5$Time_Point==56,]))-1)*B5_SL56, by = B5_SL56)
B5_AdjD_168 <- seq(0, (as.numeric(count(B5[B5$Time_Point==168,]))-1)*B5_SL168, by = B5_SL168)
B5_AdjD_240 <- seq(0, (as.numeric(count(B5[B5$Time_Point==240,]))-1)*B5_SL240, by = B5_SL240)
B5_AdjD_336 <- seq(0, (as.numeric(count(B5[B5$Time_Point==336,]))-1)*B5_SL336, by = B5_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
B5 <- B5[order(B5$Time_Point,B5$Depth), ]
B5$Adj_D <- c(B5_AdjD_0, B5_AdjD_56, B5_AdjD_168, B5_AdjD_240, B5_AdjD_336)


##A6

#subset out one tank all time points
A6 <- subset(Profiles2, Profiles2$Tank_Number=="A6")

#cut off space above tank for this profile all time points
A6 <- subset(A6, A6$Depth > 13.128)

#Find step length for profil1e distance for each timepoint
A6_SL0 <- as.data.frame(A6[A6$Time_Point==0, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==0, "Depth"])[1,1]
A6_SL56 <- as.data.frame(A6[A6$Time_Point==56, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==56, "Depth"])[1,1]
A6_SL168 <- as.data.frame(A6[A6$Time_Point==168, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==168, "Depth"])[1,1]
A6_SL240 <- as.data.frame(A6[A6$Time_Point==240, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==240, "Depth"])[1,1]
A6_SL336 <- as.data.frame(A6[A6$Time_Point==336, "Depth"])[2,1] - as.data.frame(A6[A6$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(A6$Time_Point)

#Recreate adjusted depth profile distances
A6_AdjD_0 <- seq(0, (as.numeric(count(A6[A6$Time_Point==0,]))-1)*A6_SL0, by = A6_SL0)
A6_AdjD_56 <- seq(0, (as.numeric(count(A6[A6$Time_Point==56,]))-1)*A6_SL56, by = A6_SL56)
A6_AdjD_168 <- seq(0, (as.numeric(count(A6[A6$Time_Point==168,]))-1)*A6_SL168, by = A6_SL168)
A6_AdjD_240 <- seq(0, (as.numeric(count(A6[A6$Time_Point==240,]))-1)*A6_SL240, by = A6_SL240)
A6_AdjD_336 <- seq(0, (as.numeric(count(A6[A6$Time_Point==336,]))-1)*A6_SL336, by = A6_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
A6 <- A6[order(A6$Time_Point,A6$Depth), ]
A6$Adj_D <- c(A6_AdjD_0, A6_AdjD_56, A6_AdjD_168, A6_AdjD_240, A6_AdjD_336)


##B6

#subset out one tank all time points
B6 <- subset(Profiles2, Profiles2$Tank_Number=="B6")

#cut off space above tank for this profile all time points
B6 <- subset(B6, B6$Depth > 12.585)

#Find step length for profil1e distance for each timepoint
B6_SL0 <- as.data.frame(B6[B6$Time_Point==0, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==0, "Depth"])[1,1]
B6_SL56 <- as.data.frame(B6[B6$Time_Point==56, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==56, "Depth"])[1,1]
B6_SL168 <- as.data.frame(B6[B6$Time_Point==168, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==168, "Depth"])[1,1]
B6_SL240 <- as.data.frame(B6[B6$Time_Point==240, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==240, "Depth"])[1,1]
B6_SL336 <- as.data.frame(B6[B6$Time_Point==336, "Depth"])[2,1] - as.data.frame(B6[B6$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(B6$Time_Point)

#Recreate adjusted depth profile distances
B6_AdjD_0 <- seq(0, (as.numeric(count(B6[B6$Time_Point==0,]))-1)*B6_SL0, by = B6_SL0)
B6_AdjD_56 <- seq(0, (as.numeric(count(B6[B6$Time_Point==56,]))-1)*B6_SL56, by = B6_SL56)
B6_AdjD_168 <- seq(0, (as.numeric(count(B6[B6$Time_Point==168,]))-1)*B6_SL168, by = B6_SL168)
B6_AdjD_240 <- seq(0, (as.numeric(count(B6[B6$Time_Point==240,]))-1)*B6_SL240, by = B6_SL240)
B6_AdjD_336 <- seq(0, (as.numeric(count(B6[B6$Time_Point==336,]))-1)*B6_SL336, by = B6_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
B6 <- B6[order(B6$Time_Point,B6$Depth), ]
B6$Adj_D <- c(B6_AdjD_0, B6_AdjD_56, B6_AdjD_168, B6_AdjD_240, B6_AdjD_336)

##c

#subset out one tank all time points
C <- subset(Profiles2, Profiles2$Tank_Number=="C")

#cut off space above tank for this profile all time points
C <- subset(C, C$Depth >14.3867)

#Find step length for profil1e distance for each timepoint
C_SL0 <- as.data.frame(C[C$Time_Point==0, "Depth"])[2,1] - as.data.frame(C[C$Time_Point==0, "Depth"])[1,1]
C_SL56 <- as.data.frame(C[C$Time_Point==56, "Depth"])[2,1] - as.data.frame(C[C$Time_Point==56, "Depth"])[1,1]
C_SL168 <- as.data.frame(C[C$Time_Point==168, "Depth"])[2,1] - as.data.frame(C[C$Time_Point==168, "Depth"])[1,1]
C_SL240 <- as.data.frame(C[C$Time_Point==240, "Depth"])[2,1] - as.data.frame(C[C$Time_Point==240, "Depth"])[1,1]
C_SL336 <- as.data.frame(C[C$Time_Point==336, "Depth"])[2,1] - as.data.frame(C[C$Time_Point==336, "Depth"])[1,1]

#check which timepoint are in this profile
unique(C$Time_Point)

#Recreate adjusted depth profile distances
C_AdjD_0 <- seq(0, (as.numeric(count(C[C$Time_Point==0,]))-1)*C_SL0, by = C_SL0)
C_AdjD_56 <- seq(0, (as.numeric(count(C[C$Time_Point==56,]))-1)*C_SL56, by = C_SL56)
C_AdjD_168 <- seq(0, (as.numeric(count(C[C$Time_Point==168,]))-1)*C_SL168, by = C_SL168)
C_AdjD_240 <- seq(0, (as.numeric(count(C[C$Time_Point==240,]))-1)*C_SL240, by = C_SL240)
C_AdjD_336 <- seq(0, (as.numeric(count(C[C$Time_Point==336,]))-1)*C_SL336, by = C_SL336)

#Combining all adjusted depth Profiles2 into one column
#remember to put this timepoint back in
C <- C[order(C$Time_Point,C$Depth), ]

C$Adj_D <- c(C_AdjD_0, C_AdjD_56, C_AdjD_168, C_AdjD_240, C_AdjD_336)



Profile_corrected <- do.call(rbind,list(A1,A2,A3,A4,A5,A6,B1,B2,B3,B4,B5,B6,C))

#calculated % lum from gray value
Profile_corrected$Per_Lum <- (Profile_corrected$Grey_Value*100)/255

#binning depth so it is the same across all profiles

bin_edges <- seq(0,30, by=.1)

Profile_corrected$depth_bin <- cut(
  Profile_corrected$Adj_D,
  breaks= bin_edges,
  include.lowest= TRUE,
  right= TRUE,
  labels= bin_edges[-1])


Profile_corrected[is.na(Profile_corrected$Species),"Species"] <- "Control"


## all 3 species
Profile_corrected$depth_bin <- as.numeric(as.character(Profile_corrected$depth_bin))


write.csv(Profile_corrected, file = "C:\\Users\\PC1\\OneDrive\\Desktop\\MARINE DESSERTATION\\github\\Z.noltiiXinfauna\\result\\Profiledata.csv")


#Only the top 7.5 cm
Profile_corrected <- subset(Profile_corrected, Profile_corrected$Adj_D <= 7.5)



#plot for just the final time point all species
t336 <- subset(Profile_corrected, Profile_corrected$Time_Point==336)

Sum_profile336 <- t336 %>%
  group_by(depth_bin, Species) %>%
  summarise(m=mean(Per_Lum, na.rm=T),
            se= sd(Per_Lum, na.rm=T)/sqrt(length(Per_Lum)))

Sum_profile336<- subset(Sum_profile336, Sum_profile336$Species != "Control")


ggplot(Sum_profile336, aes(x = m, y = depth_bin, color= Species, fill=Species)) +
  geom_ribbon(aes(xmin = m - se, xmax = m + se),
                          alpha = 0.7)+
  geom_path(size = 1) +  # Use geom_path to respect point order
  scale_y_reverse() +  # Depth increases downward
  scale_x_continuous(position = "top") +  # Move x-axis to top
  labs(
        x = "Percentage of luminophore",
        y = "Depth Bin",
        title = "Time = 336 hrs") +
  theme_minimal()
            
            

#plot for just the final time point all species
t240 <- subset(Profile_corrected, Profile_corrected$Time_Point==240)

Sum_profile240 <- t240 %>%
  group_by(depth_bin, Species) %>%
  summarise(m=mean(Per_Lum, na.rm=T),
            se= sd(Per_Lum, na.rm=T)/sqrt(length(Per_Lum)))

Sum_profile240<- subset(Sum_profile240, Sum_profile240$Species != "Control")


ggplot(Sum_profile240, aes(x = m, y = depth_bin, color= Species, fill=Species)) +
  geom_ribbon(aes(xmin = m - se, xmax = m + se),
              alpha = 0.7)+
  geom_path(size = 1) +  # Use geom_path to respect point order
  scale_y_reverse() +  # Depth increases downward
  scale_x_continuous(position = "top") +  # Move x-axis to top
  labs(
    x = "Percentage of luminophore",
    y = "Depth Bin",
    title = "Time = 240 hrs") +
  theme_minimal()



#plot for just the final time point all species
t168 <- subset(Profile_corrected, Profile_corrected$Time_Point==168)

Sum_profile168 <- t168 %>%
  group_by(depth_bin, Species) %>%
  summarise(m=mean(Per_Lum, na.rm=T),
            se= sd(Per_Lum, na.rm=T)/sqrt(length(Per_Lum)))

Sum_profile168<- subset(Sum_profile168, Sum_profile168$Species != "Control")


ggplot(Sum_profile168, aes(x = m, y = depth_bin, color= Species, fill=Species)) +
  geom_ribbon(aes(xmin = m - se, xmax = m + se),
              alpha = 0.7)+
  geom_path(size = 1) +  # Use geom_path to respect point order
  scale_y_reverse() +  # Depth increases downward
  scale_x_continuous(position = "top") +  # Move x-axis to top
  labs(
    x = "Percentage of luminophore",
    y = "Depth Bin",
    title = "Time = 168 hrs") +
  theme_minimal()



#plot for just the final time point all species
t56 <- subset(Profile_corrected, Profile_corrected$Time_Point==56)

Sum_profile56 <- t56 %>%
  group_by(depth_bin, Species) %>%
  summarise(m=mean(Per_Lum, na.rm=T),
            se= sd(Per_Lum, na.rm=T)/sqrt(length(Per_Lum)))

Sum_profile56<- subset(Sum_profile56, Sum_profile56$Species != "Control")


ggplot(Sum_profile56, aes(x = m, y = depth_bin, color= Species, fill=Species)) +
  geom_ribbon(aes(xmin = m - se, xmax = m + se),
              alpha = 0.7)+
  geom_path(size = 1) +  # Use geom_path to respect point order
  scale_y_reverse() +  # Depth increases downward
  scale_x_continuous(position = "top") +  # Move x-axis to top
  labs(
    x = "Percentage of luminophore",
    y = "Depth Bin",
    title = "Time = 56 hrs") +
  theme_minimal()




#plot for just the final time point all species
t0 <- subset(Profile_corrected, Profile_corrected$Time_Point== 0)

Sum_profile0 <- t0 %>%
  group_by(depth_bin, Species) %>%
  summarise(m=mean(Per_Lum, na.rm=T),
            se= sd(Per_Lum, na.rm=T)/sqrt(length(Per_Lum)))

Sum_profile0<- subset(Sum_profile0, Sum_profile0$Species != "Control")


ggplot(Sum_profile0, aes(x = m, y = depth_bin, color= Species, fill=Species)) +
  geom_ribbon(aes(xmin = m - se, xmax = m + se),
              alpha = 0.7)+
  geom_path(size = 1) +  # Use geom_path to respect point order
  scale_y_reverse() +  # Depth increases downward
  scale_x_continuous(position = "top") +  # Move x-axis to top
  labs(
    x = "Percentage of luminophore(%) ",
    y = "Depth Bin(cm)",
    title = "Time = 0 hrs") +
    theme_minimal()+ 
  theme(
    plot.title = element_text(size = 12, hjust = 0) )


##anova

install.packages("car")

library(car)
leveneTest(Per_Lum ~ interaction(Species, Time_Point), data = Profile_corrected)

##transform data

Profile_corrected <- Profile_corrected %>%
  mutate(log_Per_Lum = log(Per_Lum + 1))

Profile_corrected$sqrt_Per_Lum <- sqrt(Profile_corrected$Per_Lum)

Profile_corrected$asin_Per_Lum <- asin(sqrt(Profile_corrected$Per_Lum / 100))

leveneTest(log_Per_Lum ~ interaction(Species, Time_Point), data = Profile_corrected)

anova_model <- aov(log_Per_Lum ~ Species * Time_Point, data = Profile_corrected)
summary(anova_model)


# Make sure 'Tank_Number' and 'Species' are treated as factors
Profile_corrected$Species <- as.factor(Profile_corrected$Species)
Profile_corrected$Tank_Number <- as.factor(Profile_corrected$Tank_Number)



#example plot for one species across all time points
Profile_corrected$Time_Point <- as.character(Profile_corrected$Time_Point)
AM <- subset(Profile_corrected, Profile_corrected$Species == "Cerastoderma edule")

ggplot(AM, aes(y=depth_bin, x=Per_Lum, color=Time_Point)) + geom_point() +
  facet_wrap(~Tank_Number)




library(ggplot2)

# Example data (replace with your own summary data)
df_summary <- data.frame(
  depth = seq(0, 30, by = 0.25),
  mean_response = sin(seq(0, 30, by = 0.25)/5) + rnorm(121, 0, 0.1),
  se = runif(121, 0.05, 0.15)
)

# Ensure data is sorted by depth (so line is drawn correctly along y-axis)
df_summary <- df_summary %>% arrange(depth)

# Plot with depth on y-axis and response on x-axis




