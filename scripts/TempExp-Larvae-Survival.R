#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(magrittr)
library(ggplot2)
library(car)
library(lme4)


#### LOAD LARVAL DATA ----------------------------------------------------------------------------

larval.data <- read_excel("data/Artedi-Temperature-Larval-Survival.xlsx", sheet = "LarvalSurvival") %>% 
  mutate(treatment = factor(treatment, ordered = TRUE, levels = c(2.0, 4.5, 7.0, 9.0)),
         survival.logit = car::logit(larval.survival, percents = TRUE),
         population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")))

larval.data.summary <- larval.data %>% 
  group_by(population, treatment) %>% 
  summarize(mean.survival = mean(larval.survival),
            sd.survival = sd(larval.survival),
            se.survival = sd.survival/sqrt(n()))


ggplot(larval.data.summary, aes(x = population, y = mean.survival, fill = treatment)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  geom_errorbar(aes(ymin = mean.survival-se.survival, ymax = mean.survival+se.survival), position = position_dodge(0.9),
                size = 0.8, width = 0.2, linetype = "solid", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 52), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0.5)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61", "#d7191c"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C  ", "9.0°C")) +
  labs(x = "Population", y = "Larval Survival (%)", color = "Populations") +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")

ggsave("figures/Survival.tiff", width = 9, height = 6, dpi = 600)
