#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### SET RANDOM SEED FOR REPRODUCIBILITY -------------------------------------

set.seed(53896423)


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(magrittr)
library(ggplot2)
library(parallel)


growth.dates <- data.frame(population = factor(rep(c("Superior", "Ontario"), each = 3), ordered = TRUE, levels = c("Superior", "Ontario")),
                           treatment = factor(rep(c(2, 4.5, 7), 2), ordered = TRUE),
                           end.date = as.POSIXct(c("2020-07-09", "2020-05-27", "2020-04-15", 
                                        "2020-07-21", "2020-05-27", "2020-04-15"), format = "%Y-%m-%d"))


#### LOAD LARVAL LENGTH-AT-HATCH DATA ------------------------------------------------------------

larval.lah.ls <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Latitude-Embryo/data/Coregonine-Temperature-Experiment-LarvalMeasurements.xlsx", sheet = "LS-Larvae")
larval.lah.lo <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Latitude-Embryo/data/Coregonine-Temperature-Experiment-LarvalMeasurements.xlsx", sheet = "LO-Larvae")

# Combine each population, temperature, and species
larval.lah <- bind_rows(larval.lah.ls, larval.lah.lo) %>% 
  filter(temperature != 8.9, !is.na(length_mm), length_mm != 0) %>% 
  mutate(population = factor(population, ordered = TRUE, levels = c("superior", "ontario"), labels = c("Superior", "Ontario")),
         treatment = factor(temperature, ordered = TRUE, levels = c(2.0, 4.4, 6.9), labels = c(2.0, 4.5, 7.0)),
         group = interaction(population, treatment)) %>% 
  select(group, population, treatment, length.mm = length_mm)

rm(larval.lah.ls, larval.lah.lo)


#### LOAD LARVAL 90 DAYS LENGTH DATA -------------------------------------------------------------

larval.growth.2.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "2.0-Data")
larval.growth.4.5 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "4.5-Data")
larval.growth.7.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "7.0-Data")

## Combine data frames
larval.growth <- bind_rows(larval.growth.2.0, larval.growth.4.5, larval.growth.7.0) %>% 
  filter(!is.na(length.mm), length.mm != 0) %>% 
  mutate(treatment = factor(treatment, ordered = TRUE, levels = c(2, 4.5, 7)),
         population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")),
         group = interaction(population, treatment))

rm(larval.growth.2.0, larval.growth.4.5, larval.growth.7.0)


embryo.hatch <- read_excel("/Users/Taylor/SynologyDrive/Cisco-Climate-Change/Coregonine-Latitude-Embryo/data/Coregonine-Temperature-Experiment-NA-Hatch.xlsx", sheet = "2020HatchingData") %>% 
  filter(is.na(notes) | notes != "empty well", !is.na(hatch_date), temperature != 8.9) %>% 
  mutate(population = factor(ifelse(population == "superior", "Superior", "Ontario"), ordered = TRUE, levels = c("Superior", "Ontario")),
         treatment = factor(temperature, ordered = TRUE, levels = c(2.0, 4.4, 6.9), labels = c(2.0, 4.5, 7.0))) %>% 
  dplyr::select(population, treatment, hatch_date) %>% 
  group_by(population, treatment) %>% 
  summarize(mean.hatch.date = strptime(mean(hatch_date), format = "%Y-%m-%d")) %>% 
  left_join(growth.dates) %>% 
  mutate(growth.days = round(end.date - mean.hatch.date, 0),
         group = interaction(population, treatment)) %>% 
  dplyr::select(population, treatment, group, growth.days)


#### LOAD LARVAL DATA ----------------------------------------------------------------------------

start.tl.boot <- do.call(rbind, lapply(unique(larval.lah$group), function(grp) {
  ## Filter to only a single temperature treatment
  data.group <- larval.lah %>% filter(group == grp) %>% 
    select(group, length.mm)
  
  ## Create a bootstrapped data set from each temperature and group
  bootstrap.data <- do.call(rbind, lapply(1:10000, function(length) {
    tl.boot <- sample(data.group$length.mm, replace = T, size = nrow(data.group))
    data.tl.boot <- data.frame(group = grp, rep = length, start.length.mm = mean(tl.boot))
  }))
}))


final.tl.boot <- do.call(rbind, lapply(unique(larval.growth$group), function(grp) {
  ## Filter to only a single temperature treatment
  data.group <- larval.growth %>% filter(group == grp) %>% 
    select(group, length.mm)
  
  ## Create a bootstrapped data set from each temperature and group
  bootstrap.data <- do.call(rbind, lapply(1:10000, function(length) {
      tl.boot <- sample(data.group$length.mm, replace = T, size = nrow(data.group))
      data.tl.boot <- data.frame(group = grp, rep = length, final.length.mm = mean(tl.boot))
  }))
}))


growth.boot <- left_join(start.tl.boot, final.tl.boot) %>% 
  left_join(embryo.hatch) %>% 
  mutate(tl.diff = final.length.mm - start.length.mm,
         growth.mm = tl.diff/as.numeric(growth.days))


growth.boot.95perc <- growth.boot %>% group_by(group) %>% 
  summarize(mean.growth = mean(growth.mm),
            growth.se = sd(growth.mm),
            growth.ci.upper = quantile(growth.mm, probs = 0.975),
            growth.ci.lower = quantile(growth.mm, probs = 0.025)) %>% 
  mutate(population = factor(gsub("\\.", "", substr(group, 1, 8)), ordered = TRUE, levels = c("Superior", "Ontario")),
         temperature = factor(ifelse(population == "Superior", substr(group, 10, 12), substr(group, 9, 12)), ordered = TRUE, levels = c(2, 4.5, 7), labels = c("2.0", "4.5", "7.0")))



ggplot(growth.boot.95perc, aes(x = temperature, y = mean.growth, group = population, fill = population)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  #geom_errorbar(aes(ymin = mean.growth - growth.se, ymax = mean.growth + growth.se), position = position_dodge(0.9),
  #              size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  geom_errorbar(aes(ymin = growth.ci.upper, ymax = growth.ci.lower), position = position_dodge(0.9),
                size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  scale_fill_grey(start = 0.3, end = 0.8, labels = c("Superior   ", "Ontario")) +
  scale_y_continuous(limits = c(0, 0.07), breaks = seq(0, 0.07, 0.01), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  labs(y = expression("Absolute Growth Rate (mm day"^-1*")"), x = "Incubation Temperature (°C)") +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")

ggplot(growth.boot.95perc, aes(x = population, y = mean.growth, group = temperature, fill = temperature)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  #geom_errorbar(aes(ymin = mean.growth - growth.se, ymax = mean.growth + growth.se), position = position_dodge(0.9),
  #              size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  geom_errorbar(aes(ymin = growth.ci.upper, ymax = growth.ci.lower), position = position_dodge(0.9),
                size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  scale_fill_manual(values = c("#91bfdb", "#ffffbf", "#fc8d59"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C")) +
  scale_y_continuous(limits = c(0, 0.07), breaks = seq(0, 0.07, 0.01), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  labs(y = expression("Absolute Growth Rate (mm day"^-1*")"), x = "Population") +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")



