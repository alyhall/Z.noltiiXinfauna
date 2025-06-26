##########################
##
##
##Yforyd Temperature Time Series Plot and Creek
##
##
##
##Created: 6-26-2025 -JW
##
##
##
##

##Packages need-----------

library(tidyverse) #love that lubridate is a part of tidyverse

##Loading Data---------

Creek <- read.csv("Data/April25_YForyd_Creek.csv")

(Creek$Date <- mdy_hm(Creek$Date))
  
class(Creek$Date)

Plots <- read.csv("Data/April25_Yforyd_Plots.csv")

(Plots$Date <- mdy_hm(Plots$Date))

class(Plots$Date)


##Combining Columns------

Creek <- Creek %>% 
  mutate(Location = "Creek") 
Plots <- Plots %>% 
  mutate(Location = "Plots") 


Temp <- rbind(Creek, Plots)


##Plotting-------

Temp %>% 
  ggplot(aes(x=Date, y=TempC, color=Location))+
  geom_point()+
  geom_line()
  
