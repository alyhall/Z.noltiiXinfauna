##########################
##
##
## Y Foryd Temperature Time Series Plot and Creek
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



Creek <- read.csv("Data/Field/April25_YForyd_Creek.csv")
Plots <- read.csv("Data/Field/April25_Yforyd_Plots.csv")
June_Plots <- read.csv("Data/Field/YFD_June_Plots_SandBar.csv")
May_T1_0 <- read.csv("Data/Field/YFD_May_T1_0.csv")



##Creek and Plots Data------
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





###Figures for  Creek and Plots-------

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
  






## June and May? -----
  
May <- May_T1_0 %>% 
  mutate(Date = mdy_hm(Date), Day = as.Date(Date))


June <- June_Plots %>%  
  mutate(Date = mdy_hm(Date), Day = as.Date(Date))



###Figures---
    

####May Transect 1?----


May %>%  ##Daily averages 
  group_by(Day) %>% 
  mutate(MTemp = mean(Temp)) %>% 
  ggplot(aes(x=Day, y=MTemp))+
  geom_line()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%B")+
  labs(y="Temperature (°C)")

####June Sandbar Plots? -----

June %>%  ##Daily averages 
  group_by(Day) %>% 
  mutate(MTemp = mean(Temp)) %>% 
  ggplot(aes(x=Day, y=MTemp))+
  geom_line()+
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%B")+
  labs(y="Temperature (°C)")


June %>% 
  ggplot(aes(x=Date, y=Temp))+
  geom_line(alpha=0.5)+
  scale_x_datetime(date_breaks = "4 day", date_labels = "%d-%B")


