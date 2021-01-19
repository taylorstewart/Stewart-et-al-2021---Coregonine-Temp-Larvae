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

larval.data <- read_excel("data/2020-Artedi-Temperature-Larval-Survival.xlsx", sheet = "LarvalSurvival") %>% 
  mutate(treatment = factor(treatment, ordered = TRUE, levels = c(2.0, 4.5, 7.0, 9.0)),
         survival.logit = car::logit(larval.survival, percents = TRUE))

larval.data.summary <- larval.data %>% 
  group_by(population, treatment) %>% 
  summarize(mean.survival = mean(larval.survival),
            sd.survival = sd(larval.survival),
            se.survival = sd.survival/sqrt(n()))


## Fit model
lm <- lm(survival.logit ~ treatment, data = filter(larval.data, population == "ontario"))

## CHECK ASSUMPTIONS ============================================

##  Normality
shapiro.test(larval.data$survival.logit) 
## p = 0.2554; accept normality

## Homogeneity of Variance by source
bartlett.test(survival.logit ~ treatment, data = larval.data)
## p = 0.07; accept equal variance


anova(lm)

## Calculate estimated marginal means - be very patient!
larval.glm.emm <- emmeans(lm, ~ treatment)

## Pairwise
emm.pairs <- pairs(larval.glm.emm, simple = "treatment", adjust = "tukey", type = "response") 
emm.cld <- multcomp::cld(emm.pairs, )


ggplot(larval.data.summary, aes(x = treatment, y = mean.survival, fill = population)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9), size = 0.5, color = "black") +
  geom_errorbar(aes(ymin = mean.survival-se.survival, ymax = mean.survival+se.survival),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  scale_fill_manual(labels = c("Ontario    ", "Superior"), 
                    values = c("#fc8d59", "#91bfdb")) +
  labs(x = "Incubation Temperature (°C)", y = "Larval Survival (% ± SE)", color = "Populations") +
  theme_classic() +
  theme(axis.title.x = element_text(color = "Black", size = 20, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 20, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.0, 'cm'),
        legend.position = "top",
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

ggsave("figures/larvae/2020-Larval-Survival.png", width = 12, height = 7, dpi = 300)


##############################################################
## ANALYSIS
##############################################################
## -----------------------------------------------------------
## Fit model
## -----------------------------------------------------------
glm <- glm(larval.survival ~ population + treatment + population * treatment, 
            data = larval.data)

dg1 <- dredge(glm)                    # to select all model based on AICc
dg1

best <- get.models(dg1,"8")[[1]]    # select best model based on AICc
summary(best)

Anova(best)
Anova(best, type = "III")

# Post-hoc test:
best.emm <- emmeans(best, ~ treatment * population)
(best.emm.pair <- pairs(best.emm, simple = list("population", c("treatment")), adjust = "fdr"))
