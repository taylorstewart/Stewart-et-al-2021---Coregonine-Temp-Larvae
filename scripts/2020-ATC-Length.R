#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
library(ggplot2)


#### LOAD DATA -----------------------------------------------------------------------------------

ATC.2.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "2.0-Data") %>% 
  filter(include != "n", !is.na(length.mm))
ATC.4.5 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "4.5-Data") %>% 
  filter(include != "n", !is.na(length.mm))
ATC.7.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "7.0-Data") %>% 
  filter(include != "n", !is.na(length.mm))


#### CORRELATION ---------------------------------------------------------------------------------

ggplot(data = ATC.2.0, aes(x = length.mm, y = lethal.temp, color = rearing.tank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

ggplot(data = ATC.4.5, aes(x = length.mm, y = lethal.temp, color = rearing.tank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

ggplot(data = ATC.7.0, aes(x = length.mm, y = lethal.temp, color = rearing.tank)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

cor.test(~ lethal.temp + length.mm, data = filter(ATC.2.0, population == "Superior"))
cor.test(~ lethal.temp + length.mm, data = filter(ATC.2.0, population == "Ontario"))

cor.test(~ lethal.temp + length.mm, data = filter(ATC.4.5, population == "Superior"))
cor.test(~ lethal.temp + length.mm, data = filter(ATC.4.5, population == "Ontario"))

cor.test(~ lethal.temp + length.mm, data = filter(ATC.7.0, population == "Superior"))
cor.test(~ lethal.temp + length.mm, data = filter(ATC.7.0, population == "Ontario"))




