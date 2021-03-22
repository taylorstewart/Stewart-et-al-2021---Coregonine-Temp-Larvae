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


#### CREATE DATAFRAME WITH ATC DATES -------------------------------------------------------------

growth.dates <- data.frame(population = factor(rep(c("Superior", "Ontario"), each = 4), ordered = TRUE, levels = c("Superior", "Ontario")),
                           treatment = factor(rep(c(2, 4.5, 7, 9), 2), ordered = TRUE),
                           end.date = as.POSIXct(c("2020-07-09", "2020-05-27", "2020-04-15", "2020-03-22",
                                                   "2020-07-21", "2020-05-27", "2020-04-15", "2020-03-22"), format = "%Y-%m-%d"))


#### LOAD LARVAL LENGTH-AT-HATCH DATA ------------------------------------------------------------

larval.lah.ls <- read_excel("data/Artedi-Temperature-HatchingMeasurements.xlsx", sheet = "LS-Larvae")
larval.lah.lo <- read_excel("data/Artedi-Temperature-HatchingMeasurements.xlsx", sheet = "LO-Larvae")

# Combine each population, temperature, and species
larval.lah <- bind_rows(larval.lah.ls, larval.lah.lo) %>% 
  filter(!is.na(length_mm), length_mm != 0, include.tl == "y") %>% 
  mutate(population = factor(population, ordered = TRUE, levels = c("superior", "ontario"), labels = c("Superior", "Ontario")),
         treatment = factor(temperature, ordered = TRUE, levels = c(2.0, 4.4, 6.9, 8.9), labels = c(2.0, 4.5, 7.0, 9.0)),
         group = interaction(population, treatment)) %>% 
  select(group, population, treatment, rearing.tank, length.mm = length_mm)

rm(larval.lah.ls, larval.lah.lo)


#### LOAD LARVAL 90 DAYS LENGTH DATA -------------------------------------------------------------

larval.growth.2.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "2.0-Data")
larval.growth.4.5 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "4.5-Data")
larval.growth.7.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "7.0-Data")
larval.growth.9.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "9.0-Data")

## Combine data frames
larval.growth <- bind_rows(larval.growth.2.0, larval.growth.4.5, larval.growth.7.0, larval.growth.9.0) %>% 
  filter(!is.na(length.mm), length.mm != 0) %>% 
  mutate(treatment = factor(treatment, ordered = TRUE, levels = c(2, 4.5, 7, 9)),
         population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")),
         group = interaction(population, treatment))

rm(larval.growth.2.0, larval.growth.4.5, larval.growth.7.0, larval.growth.9.0)

## Find length of rearing
embryo.hatch.dates <- read_excel("data/Artedi-Temperature-HatchDates.xlsx", sheet = "2020HatchingData") %>% 
  filter(is.na(notes) | notes != "empty well", !is.na(hatch_date)) %>% 
  mutate(population = factor(ifelse(population == "superior", "Superior", "Ontario"), ordered = TRUE, levels = c("Superior", "Ontario")),
         treatment = factor(temperature, ordered = TRUE, levels = c(2.0, 4.4, 6.9, 8.9), labels = c(2.0, 4.5, 7.0, 9.0))) %>% 
  dplyr::select(population, treatment, rearing.tank, hatch_date) %>% 
  group_by(population, treatment, rearing.tank) %>% 
  summarize(mean.hatch.date = strptime(mean(hatch_date), format = "%Y-%m-%d")) %>% 
  left_join(growth.dates) %>% 
  mutate(growth.days = round(end.date - mean.hatch.date, 0),
         group = interaction(population, treatment)) %>% 
  dplyr::select(population, treatment, rearing.tank, group, growth.days)


#### BOOTSTRAP LENGTH DATA -----------------------------------------------------------------------

start.tl.boot <- do.call(rbind, lapply(unique(larval.lah$group), function(grp) {
  ## Filter to only a single temperature treatment
  data.group <- larval.lah %>% filter(group == grp)
  
  do.call(rbind, lapply(unique(data.group$rearing.tank), function(rt) {
    ## Filter to only a single temperature treatment
    data.group.rt <- data.group %>% filter(rearing.tank == rt) %>% 
      select(group, rearing.tank, length.mm)
    
    ## Create a bootstrapped data set from each temperature and group
    bootstrap.data <- do.call(rbind, lapply(1:5000, function(length) {
      tl.boot <- sample(data.group.rt$length.mm, replace = T, size = nrow(data.group.rt))
      data.tl.boot <- data.frame(group = grp, rearing.tank = rt, rep = length, start.length.mm = mean(tl.boot))
    }))
  }))
}))


final.tl.boot <- do.call(rbind, lapply(unique(larval.growth$group), function(grp) {
  ## Filter to only a single temperature treatment
  data.group <- larval.growth %>% filter(group == grp)
  
  do.call(rbind, lapply(unique(data.group$rearing.tank), function(rt) {
    ## Filter to only a single temperature treatment
    data.group.rt <- data.group %>% filter(rearing.tank == rt) %>% 
      select(group, rearing.tank, length.mm)
    
    ## Create a bootstrapped data set from each temperature and group
    bootstrap.data <- do.call(rbind, lapply(1:5000, function(length) {
      tl.boot <- sample(data.group.rt$length.mm, replace = T, size = nrow(data.group.rt))
      data.tl.boot <- data.frame(group = grp, rearing.tank = rt, rep = length, final.length.mm = mean(tl.boot))
    }))
  }))
}))


#### SUMMARIZE BOOTSTRAPPED DATA -----------------------------------------------------------------

growth.boot <- left_join(start.tl.boot, final.tl.boot) %>% 
  left_join(embryo.hatch.dates) %>% 
  mutate(tl.diff = final.length.mm - start.length.mm,
         growth.mm = tl.diff/as.numeric(growth.days))

growth.boot.95perc <- growth.boot %>% group_by(group) %>% 
  summarize(mean.growth = mean(growth.mm),
            growth.se = sd(growth.mm),
            growth.ci.upper = quantile(growth.mm, probs = 0.975),
            growth.ci.lower = quantile(growth.mm, probs = 0.025)) %>% 
  mutate(population = factor(gsub("\\.", "", substr(group, 1, 8)), ordered = TRUE, levels = c("Superior", "Ontario")),
         temperature = factor(ifelse(population == "Superior", substr(group, 10, 12), substr(group, 9, 12)), ordered = TRUE, levels = c(2, 4.5, 7, 9), labels = c("2.0", "4.5", "7.0", "9.0")),
         group = interaction(population, temperature)) %>% 
  select(group, population, temperature, mean.growth, growth.se, growth.ci.upper, growth.ci.lower)


#### FIND OVERLAPPING CIs AND ASSIGN CLD ---------------------------------------------------------

growth.boot.95perc.ls <- growth.boot.95perc %>% filter(population == "Superior") %>% select(-population, -temperature)
growth.boot.95perc.lo <- growth.boot.95perc %>% filter(population == "Ontario") %>% select(-population, -temperature)

group.pairwise.ls <- data.frame(do.call(rbind, combn(as.character(growth.boot.95perc.ls$group), 2, simplify = FALSE))) %>% 
  rename(group1 = X1, group2 = X2)
group.pairwise.lo <- data.frame(do.call(rbind, combn(as.character(growth.boot.95perc.lo$group), 2, simplify = FALSE))) %>% 
  rename(group1 = X1, group2 = X2)

group.pairwise.ls.diff <- group.pairwise.ls %>% left_join(growth.boot.95perc.ls, by = c("group1" = "group")) %>%
  left_join(growth.boot.95perc.ls, by = c("group2" = "group")) %>% 
  mutate(m1_ul2 = mean.growth.x - growth.ci.upper.y,
         m1_ll2 = mean.growth.x - growth.ci.lower.y,
         m2_ul1 = mean.growth.y - growth.ci.upper.x,
         m2_ll1 = mean.growth.y - growth.ci.lower.x) %>% 
  select(group1, group2, m1_ul2, m1_ll2, m2_ul1, m2_ll1) %>% 
  mutate(m1_cl2 = ifelse(m1_ul2 < 0 & m1_ll2 > 0 | m1_ul2 > 0 & m1_ll2 < 0, TRUE, FALSE),
         m2_cl1 = ifelse(m2_ul1 < 0 & m2_ll1 > 0 | m2_ul1 > 0 & m2_ll1 < 0, TRUE, FALSE))
## 2.0 = ab; 4.5 = b; 7.0 = a; 9.0 = c

group.pairwise.lo.diff <- group.pairwise.lo %>% left_join(growth.boot.95perc.lo, by = c("group1" = "group")) %>%
  left_join(growth.boot.95perc.lo, by = c("group2" = "group")) %>% 
  mutate(m1_ul2 = mean.growth.x - growth.ci.upper.y,
         m1_ll2 = mean.growth.x - growth.ci.lower.y,
         m2_ul1 = mean.growth.y - growth.ci.upper.x,
         m2_ll1 = mean.growth.y - growth.ci.lower.x) %>% 
  select(group1, group2, m1_ul2, m1_ll2, m2_ul1, m2_ll1) %>% 
  mutate(m1_cl2 = ifelse(m1_ul2 < 0 & m1_ll2 > 0 | m1_ul2 > 0 & m1_ll2 < 0, TRUE, FALSE),
         m2_cl1 = ifelse(m2_ul1 < 0 & m2_ll1 > 0 | m2_ul1 > 0 & m2_ll1 < 0, TRUE, FALSE))
## 2.0 = a; 4.5 = 1; 7.0 = a; 9.0 = b

group.pairwise.cld <- data.frame(population = rep(c("Superior", "Ontario"), each = 4),
                                 temperature = c("2.0", "4.5", "7.0", "9.0", "2.0", "4.5", "7.0", "9.0"),
                                 cld = c("ab", "b", "a", "c", "A", "A", "A", "B"))

growth.boot.95perc.cld <- left_join(growth.boot.95perc, group.pairwise.cld) %>% 
  mutate(population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")),
         temperature = factor(temperature, ordered = TRUE, levels = c("2.0", "4.5", "7.0", "9.0")))


#### VISUALIZATIONS ------------------------------------------------------------------------------

ggplot(growth.boot.95perc.cld, aes(x = population, y = mean.growth, group = temperature, fill = temperature)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  geom_errorbar(aes(ymin = growth.ci.lower, ymax = growth.ci.upper), position = position_dodge(0.9),
                size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  geom_text(aes(y = growth.ci.upper, label = cld), vjust = -0.5, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C  ", "9.0°C")) +
  scale_y_continuous(limits = c(0, 0.1075), breaks = seq(0, 0.1, 0.02), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  labs(y = expression("Mean Absolute Growth Rate (mm day"^-1*")"), x = "Population") +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")

ggsave("figures/Growth_wCLD.tiff", width = 9, height = 6, dpi = 600)

