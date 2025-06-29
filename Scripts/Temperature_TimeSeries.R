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
Plots <- read.csv("Data/April25_Yforyd_Plots.csv")




Creek <- Creek %>% 
  mutate(Location = "Creek", Date = mdy_hm(Date), Day = as.Date(Date))
  

Plots <- Plots %>%  
  mutate(Location = "Plots", Date = mdy_hm(Date), Day = as.Date(Date))

Temp <- rbind(Creek, Plots)




class(Temp$Date)
class(Temp$Day)

as.numeric(Temp$Date[1])
as.numeric(Temp$Date[2])

as.numeric(Temp$Date[35])





##Plotting-------

Temp %>% 
  ggplot(aes(x=Date, y=TempC, color=Location))+
  geom_line(alpha=0.5)+
  scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%B")

  
  




Temp %>%  ##Daily averages 
  group_by(Location, Day) %>% 
  mutate(DSD=sd(TempC), MTempC = mean(TempC)) %>% 
  ggplot(aes(x=Day, y=MTempC, color=Location))+
  geom_line()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%B")+
  labs(y="Temperature (°C)")
  


Temp %>%  ##Daily averages TEST
  group_by(Location, Day) %>% 
  mutate(DSD=sd(TempC), MTempC = mean(TempC), outliers = case_when(TempC > (MTempC+DSD) | TempC <(MTempC-DSD) ~ TempC, .default=NA )) %>% 
  ggplot(aes(x=Day, y=MTempC, color=Location))+
  geom_line()+
  geom_errorbar(aes(ymax=MTempC+DSD, ymin=MTempC-DSD))+  
  geom_point(aes(y=outliers), alpha=0.5, size= 0.5)+
  scale_x_date(date_breaks = "15 day", date_labels = "%d-%B-%y")


