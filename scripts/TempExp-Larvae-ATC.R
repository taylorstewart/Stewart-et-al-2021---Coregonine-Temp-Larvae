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
         replicate.rearing = ifelse(population == "Ontario", paste0("LO-", replicate.rearing), paste0("LS-", replicate.rearing)))


#### CALCULATE SAMPLE SIZE FOR EACH TREATMENT AND POPULATION -------------------------------------

ATC.n <- ATC %>% group_by(population, treatment) %>% 
  summarize(n = n())


#### STATISTICAL ANALYSIS ------------------------------------------------------------------------

## fit full model
cisco.glm.full <- lmer(lethal.temp ~ treatment + population + treatment:population + 
                        (1|replicate.tank) + (1|replicate.rearing), data = ATC)

## backward elimination to select best model
( step(cisco.glm.full))
  ## Random effects removed - use linear model

## Box Cox transformation
summary(cisco.lm.bcTrans <- powerTransform(lethal.temp ~ treatment + population + treatment:population, data = ATC))
cisco.glm.final <- lm(bcPower(lethal.temp, cisco.lm.bcTrans$roundlam) ~ treatment + population + treatment:population, data = ATC)

## check residuals for normality
qqPlot(cisco.glm.final)
hist(rstudent(cisco.glm.final))

## check equal variance
leveneTest(lethal.temp ~ population, data = ATC)

## ANOVA
anova(cisco.glm.final)

## Create model for back-transforming data
bctran <- make.tran("boxcox", 6.1709)
cisco.bc <- with(bctran, 
                 lm(linkfun(lethal.temp) ~ treatment + population + treatment:population, data = ATC))

## Estimated margin means
cisco.glm.full.emm <- emmeans(cisco.bc, ~ treatment | population, type = "response")
cisco.emm.data <- data.frame(cisco.glm.full.emm) %>% 
  mutate(treatment = factor(treatment, ordered = TRUE, levels = c("2.0°C", "4.5°C", "7.0°C")),
         population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario"))) %>% 
  left_join(ATC.n)

## Pairwise
pairs(cisco.glm.full.emm, simple = list("treatment"), adjust = "fdr") 


#### VISUALIZATION -------------------------------------------------------------------------------

ggplot(cisco.emm.data, aes(x = population, y = response, color = population, shape = population)) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = response-SE, ymax = response+SE),
                width = 0.3, size = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n), y = 23.05), position = position_dodge(0.9), size = 4, color = "black", vjust = 'bottom') +
  scale_y_continuous(limits = c(23, 26.5), expand = c(0, 0)) +
  scale_color_manual(labels = c("Superior    ", "Ontario"), 
                    values = c("#fc8d59", "#91bfdb")) +
  scale_shape_manual(values = c(16, 15), labels = c("Superior    ", "Ontario")) +
  labs(y = expression("Mean "~CT[max]*" (°C ± SE)"), x = 'Population') +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 22, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 22, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(1.5, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        legend.position = "none",
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~treatment, nrow = 1)

ggsave("figures/ATC-CT.tiff", width = 8, height = 6, dpi = 600)


