# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

temp.1 <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Larvae-ThermalChallenge/data/HOBO/2020-Artedi-Temperature-Larval.xlsx", sheet = "Trt1")
temp.2 <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Larvae-ThermalChallenge/data/HOBO/2020-Artedi-Temperature-Larval.xlsx", sheet = "Trt2")
temp.3 <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Larvae-ThermalChallenge/data/HOBO/2020-Artedi-Temperature-Larval.xlsx", sheet = "Trt3")
temp.4 <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Larvae-ThermalChallenge/data/HOBO/2020-Artedi-Temperature-Larval.xlsx", sheet = "Trt4")

temp.1.hourly <- data.frame(datetime = rep(temp.1$DateTime, 3),
                            temperature = rep(temp.1$Temperature, 3),
                            temp.c = c(as.numeric(temp.1$Temp_LO_Rep1), as.numeric(temp.1$Temp_LO_Rep2), as.numeric(temp.1$Temp_LS_Rep1))) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime))

temp.2.hourly <- data.frame(datetime = rep(temp.2$DateTime, 3),
                            temperature = rep(temp.2$Temperature, 3),
                            temp.c = c(as.numeric(temp.2$Temp_LO_Rep1), as.numeric(temp.2$Temp_LO_Rep2), as.numeric(temp.2$Temp_LS_Rep1))) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime))

temp.3.hourly <- data.frame(datetime = rep(temp.3$DateTime, 3),
                            temperature = rep(temp.3$Temperature, 3),
                            temp.c = c(as.numeric(temp.3$Temp_LO_Rep1), as.numeric(temp.3$Temp_LO_Rep2), as.numeric(temp.3$Temp_LS_Rep1))) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime))

temp.4.hourly <- data.frame(datetime = rep(temp.4$DateTime, 3),
                            temperature = rep(temp.4$Temperature, 3),
                            temp.c = c(as.numeric(temp.4$Temp_LO_Rep1), as.numeric(temp.4$Temp_LO_Rep2), as.numeric(temp.4$Temp_LS_Rep1))) %>% 
  mutate(year = year(datetime),
         month = month(datetime),
         day = day(datetime),
         hour = hour(datetime))

temp.mean <- bind_rows(temp.1.hourly, temp.2.hourly, temp.3.hourly, temp.4.hourly) %>% 
  drop_na() %>% 
  group_by(year, month, day, hour) %>% 
  summarize(temp.mean.hourly = mean(temp.c)) %>% ungroup() %>% 
  summarize(temp.mean = round(mean(temp.mean.hourly), 2),
            temp.sd = round(sd(temp.mean.hourly), 2))
  


