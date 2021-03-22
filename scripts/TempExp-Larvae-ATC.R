#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(afex)
library(car)
library(emmeans)


#### LOAD DATA -----------------------------------------------------------------------------------

ATC.2.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "2.0-Data")
ATC.4.5 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "4.5-Data")
ATC.7.0 <- read_excel("data/Artedi-Temperature-ATC.xlsx", sheet = "7.0-Data")


#### COMBINE TEMPERATURE TREATMENTS --------------------------------------------------------------

ATC <- bind_rows(ATC.2.0, ATC.4.5, ATC.7.0) %>% 
  filter(include != "n") %>% 
  mutate(population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")),
         treatment = paste0(formatC(treatment, digits = 1, format = "f"), "°C"),
         treatment = factor(treatment, ordered = TRUE, levels = c("2.0°C", "4.5°C", "7.0°C")),
         replicate.tank = substr(tank.id, (nchar(tank.id)+1)-1, nchar(tank.id)),
         replicate.tank = ifelse(replicate.tank == 6, 1, replicate.tank),
         replicate.tank = ifelse(replicate.tank == 7, 2, replicate.tank),
         replicate.tank = ifelse(population == "Ontario", paste0("LO-", replicate.tank), paste0("LS-", replicate.tank)),
         replicate.rearing = substr(rearing.tank, (nchar(rearing.tank)+1)-1, nchar(rearing.tank)),
         replicate.rearing = ifelse(population == "Ontario", paste0("LO-", replicate.rearing), paste0("LS-", replicate.rearing)),
         acclimation.temp = factor(acclimation.temp))


#### CALCULATE SAMPLE SIZE FOR EACH TREATMENT AND POPULATION -------------------------------------

ATC.n <- ATC %>% group_by(population, treatment) %>% 
  summarize(n = n())

ATC.summary <- ATC %>% group_by(population, treatment) %>% 
  summarize(response = mean(lethal.temp),
            sd = sd(lethal.temp),
            n = n(),
            SE = sd/sqrt(n))


#### STATISTICAL ANALYSIS ------------------------------------------------------------------------

## fit full model
cisco.glm.full <- lmer(lethal.temp ~ length.mm + treatment + population + treatment:population + 
                         (1|acclimation.temp) + (1|replicate.tank) + (1|replicate.rearing), data = ATC)

## backward elimination to select best model
( step(cisco.glm.full))
  ## Random effects removed - use linear model

## Box Cox transformation
summary(cisco.lm.bcTrans <- powerTransform(lethal.temp ~ length.mm + treatment * population, data = ATC))
cisco.glm.final <- lm(bcPower(lethal.temp, cisco.lm.bcTrans$roundlam) ~ length.mm + treatment * population, data = ATC)

## check residuals for normality
qqPlot(cisco.glm.final)
hist(rstudent(cisco.glm.final))

## check equal variance
leveneTest(lethal.temp ~ population, data = ATC)

## ANOVA
anova(cisco.glm.final)

## Estimated margin means
cisco.glm.full.emm <- emmeans(cisco.glm.final, ~ treatment | population, type = "response")

## Pairwise
pairs(cisco.glm.full.emm, simple = list("treatment"), adjust = "fdr") 


#### VISUALIZATION -------------------------------------------------------------------------------

ggplot(ATC.summary, aes(x = population, y = response, group = treatment, fill = treatment)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width = 0.3, size = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n), y = 23.05), position = position_dodge(0.9), size = 4, color = "black", vjust = 'bottom') +
  scale_y_continuous(limits = c(0, 26.5), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C")) +
  #scale_shape_manual(values = c(16, 15), labels = c("Superior    ", "Ontario")) +
  coord_cartesian(ylim = c(23, 26.25)) +
  labs(y = expression("Mean "~CT[max]*" (°C)"), x = 'Population') +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")

ggsave("figures/ATC-CT.tiff", width = 9, height = 6, dpi = 600)



ggplot(data = ATC, aes(x = population, y = lethal.temp, fill = treatment, color = treatment)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.3), alpha = 0.15, shape = 16, size = 2) +
  geom_errorbar(data = ATC.summary, aes(x = population, y = response, ymin = response-SE, ymax = response+SE),
                width = 0.25, size = 0.9, position = position_dodge(0.9)) +
  geom_point(data = ATC.summary, aes(x = population, y = response), size = 3.5, position = position_dodge(width = 0.9), shape = 21, color = "black") +
  geom_text(data = ATC.summary, aes(label = paste0("n=", n), y = 18.6), position = position_dodge(0.9), size = 3.5, color = "black", vjust = 'bottom') +
  scale_y_continuous(limits = c(18.5, 29.2), breaks = seq(20, 28, 2), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C")) +
  scale_color_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C")) +
  labs(y = expression("Mean "~CT[max]*" (°C)"), x = 'Population') +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 18, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.width = unit(1.25, 'cm'),
        legend.position = "top")

ggsave("figures/ATC-CT2.tiff", width = 9, height = 6, dpi = 600)


