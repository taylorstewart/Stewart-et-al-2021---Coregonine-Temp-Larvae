#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### SET RANDOM SEED FOR REPRODUCIBILITY ---------------------------------------------------------

set.seed(53896423)


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
         acclimation.temp = factor(acclimation.temp),
         group = interaction(population, treatment))


#### CALCULATE MEAN AND SAMPLE SIZE FOR EACH TREATMENT AND POPULATION  ---------------------------

LT50.summary <- ATC %>% group_by(population, treatment, group) %>% 
  summarize(median.lethal.temp = median(lethal.temp), 
            n = n())


#### STATISTICAL ANALYSIS ------------------------------------------------------------------------

## fit full model
cisco.glm.full <- lmer(lethal.temp ~ length.mm + treatment + population + treatment:population + 
                         (1|acclimation.temp) + (1|replicate.tank) + (1|replicate.rearing), data = ATC)

## backward elimination to select best model
( step(cisco.glm.full))
  ## Random effects removed - use linear model

## Box Cox transformation
summary(cisco.lm.bcTrans <- powerTransform(lethal.temp ~ treatment * population, data = ATC))
ATC <- ATC %>% mutate(lethal.trans = bcPower(lethal.temp, cisco.lm.bcTrans$roundlam))
cisco.glm.final <- lm(lethal.trans ~ treatment * population, data = ATC)

## check residuals for normality
qqPlot(cisco.glm.final)
hist(rstudent(cisco.glm.final))

## check equal variance
leveneTest(lethal.trans ~ population, data = ATC)


#### CALCULATE BOOTSTRAPPED CIS EACH TREATMENT AND POPULATION ------------------------------------

## Run loop (be patient!)
bootstrap.mean <- do.call(rbind, lapply(unique(ATC$group), function(grp) {
  ## Filter to only a single group (population x treatment)
  data.group <- ATC %>% filter(group == grp)
  
  ## Create a bootstrapped data set from each temperature and group
  bootstrap.data <- do.call(rbind, lapply(1:10000, function(length) {
    lethal.temp.boot <- sample(data.group$lethal.temp, replace = T, size = nrow(data.group))
    data.tl.boot <- data.frame(group = grp, rep = length, median.lethal.temp = median(lethal.temp.boot))
  }))
}))

## Calculate 95% CI
LT50.95perc <- bootstrap.mean %>% group_by(group) %>% 
  summarize(LT50.95CI.upper = quantile(median.lethal.temp, probs = 0.975),
            LT50.95CI.lower = quantile(median.lethal.temp, probs = 0.025)) %>% 
  mutate(population = factor(gsub("\\.", "", substr(group, 1, 8)), ordered = TRUE, levels = c("Superior", "Ontario")),
         treatment = factor(ifelse(population == "Superior", substr(group, 10, 12), substr(group, 9, 11)), ordered = TRUE, 
                              levels = c("2.0", "4.5", "7.0"), labels = c("2.0°C", "4.5°C", "7.0°C"))) %>% 
  left_join(LT50.summary)


#### FIND OVERLAPPING CIs AND ASSIGN CLD ---------------------------------------------------------

group.pairwise <- data.frame(do.call(rbind, combn(as.character(LT50.95perc$group), 2, simplify = FALSE))) %>% 
  rename(group1 = X1, group2 = X2)

group.pairwise.diff <- group.pairwise %>% left_join(LT50.95perc, by = c("group1" = "group")) %>%
  left_join(LT50.95perc, by = c("group2" = "group")) %>% 
  mutate(m1_ul2 = median.lethal.temp.x - LT50.95CI.upper.y,
         m1_ll2 = median.lethal.temp.x - LT50.95CI.lower.y,
         m2_ul1 = median.lethal.temp.y - LT50.95CI.upper.x,
         m2_ll1 = median.lethal.temp.y - LT50.95CI.lower.x) %>% 
  select(group1, group2, m1_ul2, m1_ll2, m2_ul1, m2_ll1) %>% 
  mutate(m1_cl2 = ifelse(m1_ul2 < 0 & m1_ll2 > 0 | m1_ul2 > 0 & m1_ll2 < 0, TRUE, FALSE),
         m2_cl1 = ifelse(m2_ul1 < 0 & m2_ll1 > 0 | m2_ul1 > 0 & m2_ll1 < 0, TRUE, FALSE))
## LS: 2.0 = a; 4.5 = b; 7.0 = bc
## LO: 2.0 = b; 4.5 = b; 7.0 = c

group.pairwise.cld <- data.frame(population = rep(c("Superior", "Ontario"), each = 3),
                                 treatment = c("2.0°C", "4.5°C", "7.0°C", "2.0°C", "4.5°C", "7.0°C"),
                                 cld = c("a", "b", "bc", "b", "b", "c"))

LT50.95perc.cld <- left_join(LT50.95perc, group.pairwise.cld) %>% 
  mutate(population = factor(population, ordered = TRUE, levels = c("Superior", "Ontario")),
         treatment = factor(treatment, ordered = TRUE, levels = c("2.0°C", "4.5°C", "7.0°C")))


#### VISUALIZATION -------------------------------------------------------------------------------

ggplot(LT50.95perc.cld, aes(x = population, y = median.lethal.temp, group = treatment, fill = treatment)) +
  geom_bar(stat = "identity", position = 'dodge', color = "black") +
  geom_text(aes(label = paste0("n=", n), y = 21.05), position = position_dodge(0.9), size = 4, color = "black", vjust = 'bottom') +
  geom_errorbar(aes(ymin = LT50.95CI.lower, ymax = LT50.95CI.upper),
                width = 0.3, size = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(y = LT50.95CI.upper, label = cld), vjust = -0.5, position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0, 28), expand = c(0, 0)) +
  scale_fill_manual(values = c("#2c7bb6", "#abd9e9", "#fdae61"), labels = c("2.0°C  ", "4.5°C  ", "7.0°C")) +
  #scale_shape_manual(values = c(16, 15), labels = c("Superior    ", "Ontario")) +
  coord_cartesian(ylim = c(21, 27.5)) +
  labs(y = "LT50", x = 'Population') +
  #labs(y = expression("Mean "~CT[max]*" (°C)"), x = 'Population') +
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
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.3), alpha = 0.2, shape = 16, size = 2) +
  geom_errorbar(data = CT.95perc.cld, aes(x = population, y = mean.CT, ymin = CT.95CI.lower, ymax = CT.95CI.upper),
                color = "gray20", width = 0.25, size = 0.9, position = position_dodge(0.9)) +
  geom_point(data = CT.95perc.cld, aes(x = population, y = mean.CT), size = 3.5, position = position_dodge(width = 0.9), shape = 21, color = "black") +
  geom_text(data = CT.95perc.cld, aes(y = CT.95CI.upper, label = cld), color = "gray20", vjust = -0.5, position = position_dodge(0.9)) +
  geom_text(data = CT.95perc.cld, aes(label = paste0("n=", n), y = 18.6), position = position_dodge(0.9), size = 3.5, color = "black", vjust = 'bottom') +
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


