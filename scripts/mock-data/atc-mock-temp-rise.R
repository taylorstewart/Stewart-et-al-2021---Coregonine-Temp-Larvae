## ===========================================================
## Clear the environment first
## ===========================================================
rm(list = ls(all.names = TRUE))


## ===========================================================
## Load packages
## ===========================================================
library(dplyr)
library(readxl)
library(magrittr)
library(ggplot2)
library(lubridate)
library(scales)


## ===========================================================
## Load incubation temperature data
## ===========================================================
atc.temp <- read_excel("scripts/mock-data/atc-mock-temp-rise.xlsx", sheet = "Sheet1") %>% 
  mutate(datetime = as.POSIXct(datetime, tz = "America/New_York")) %>% 
  filter(datetime > ymd_hms("2020-01-02 05:59:00"))
  
ATC.2.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "2.0-Temp") %>% 
  filter(Tank == "ATC 01", DateTime > ymd_hms("1899-12-31 06:05:00"), DateTime <= ymd_hms("1899-12-31 23:35:00")) %>% 
  mutate(DateTime = seq.POSIXt(as.POSIXct("2020-01-02 01:06:00"), as.POSIXct("2020-01-02 18:35:00"), by = "1 min", tz = "America/New_York"))


ggplot(atc.temp, aes(x = datetime, y = temp)) +
  geom_line(color = "gray80") +
  geom_line(data = ATC.2.0, aes(x = DateTime, y = Temp.C)) +
  scale_x_datetime(date_breaks = "2 hour", labels = label_date("%H:%M"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(9.8, 28.4), breaks = seq(10, 28, 2), expand = c(0, 0)) +
  labs(x = "Time", y = "Temperature (°C)") +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 20, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 20, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.ticks.length = unit(1.5, "mm"),
        plot.margin = unit(c(5, 8, 5, 5), 'mm'))

ggsave("figures/ATC-Mock-Temp.tiff", width = 12, height = 7, dpi = 600)


