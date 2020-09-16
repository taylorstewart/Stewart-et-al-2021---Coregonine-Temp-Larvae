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


## ===========================================================
## Load incubation temperature data
## ===========================================================
ATC.2.0 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "2.0-data")
ATC.4.5 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "4.5-data")
ATC.7.0 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "7.0-data")

ATC <- bind_rows(ATC.2.0, ATC.4.5, ATC.7.0) %>% 
  filter(include.utt != "n") %>% 
  mutate(population = factor(population),
         treatment = factor(treatment),
         replicate.tank = substr(tank.id, (nchar(tank.id)+1)-1, nchar(tank.id)),
         replicate.tank = ifelse(replicate.tank == 6, 1, replicate.tank),
         replicate.tank = ifelse(replicate.tank == 7, 2, replicate.tank),
         replicate.tank = ifelse(population == "Ontario", paste0("LO-", replicate.tank), paste0("LS-", replicate.tank)),
         replicate.rearing = substr(rearing.tank, (nchar(rearing.tank)+1)-1, nchar(rearing.tank)),
         replicate.rearing = ifelse(population == "Ontario", paste0("LO-", replicate.rearing), paste0("LS-", replicate.rearing)))



## -----------------------------------------------------------
## Calculate means and std. error for each treatment
## -----------------------------------------------------------
ATC.summary <- ATC %>% group_by(population, replicate.rearing, treatment) %>% 
  summarize(mean.utt = mean(utt.adm),
            mean.ct = mean(lethal.temp),
            sd.utt = sd(utt.adm),
            sd.ct = sd(lethal.temp),
            n = n(),
            se.utt = sd.utt/sqrt(n),
            se.ct = sd.ct/sqrt(n),
            max.utt = max(utt.adm),
            max.ct = max(lethal.temp)) %>% 
  bind_rows(data.frame(population = c("Ontario", "Ontario", "Superior"),
                       replicate.rearing = c("LO-1", "LO-2", "LS-1"),
                       treatment = factor(rep("9.0°C", 3)),
                       mean.utt = rep(0, 3),
                       mean.ct = rep(0, 3),
                       sd.utt = rep(0, 3),
                       sd.ct = rep(0, 3),
                       n = rep(0, 3),
                       se.utt = rep(0, 3),
                       se.ct = rep(0, 3),
                       max.utt = rep(0, 3),
                       max.ct = rep(0, 3)))
  
  bind_rows(data.frame(population = c("Ontario", "Ontario", "Ontario", "Ontario", "Superior", "Superior", "Superior"),
                       replicate.tank = c("LO-1", "LO-2", "LO-3", "LO-4", "LS-1", "LS-2", "LS-1"),
                       treatment = factor(c(rep("9.0°C", 6), "7.0°C")),
                       mean.utt = rep(0, 7),
                       mean.ct = rep(0, 7),
                       sd.utt = rep(0, 7),
                       sd.ct = rep(0, 7),
                       n = rep(0, 7),
                       se.utt = rep(0, 7),
                       se.ct = rep(0, 7),
                       max.utt = rep(0, 7),
                       max.ct = rep(0, 7))) 

%>% 
  bind_rows(data.frame(population = c("Ontario", "Ontario", "Superior"),
                       replicate.rearing = c("LO-1", "LO-2", "LS-1"),
                       treatment = factor(rep("9.0°C", 3)),
                       mean.utt = rep(0, 3),
                       mean.ct = rep(0, 3),
                       sd.utt = rep(0, 3),
                       sd.ct = rep(0, 3),
                       n = rep(0, 3),
                       se.utt = rep(0, 3),
                       se.ct = rep(0, 3),
                       max.utt = rep(0, 3),
                       max.ct = rep(0, 3)))


ggplot(ATC.summary, aes(x = replicate.rearing, y = mean.ct, color = population, shape = population)) +
  geom_point(size = 4, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean.ct-se.ct, ymax = mean.ct+se.ct),
                width = 0.3, size = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n), y = 23.05), position = position_dodge(0.9), size = 4, color = "black", vjust = 'bottom') +
  scale_y_continuous(limits = c(23, 26.5), expand = c(0, 0)) +
  scale_color_manual(labels = c("L. Ontario    ", "L. Superior"), 
                    values = c("#fc8d59", "#91bfdb")) +
  scale_shape_manual(values = c(16, 15), labels = c("L. Ontario    ", "L. Superior")) +
  labs(y = expression("Mean "~CT[max]*" (°C ± SE)"), x = 'Replicate Tank') +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 22, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 22, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(1.5, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        legend.position = "top",
        strip.text = element_text(size = 15),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~treatment, nrow = 1)

ggsave("figures/atc/2020-ATC-CT-RearingRep.png", width = 18, height = 10, dpi = 300)


ggplot(ATC.summary, aes(x = replicate, y = mean.utt, fill = population)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), size = 0.5, color = "black") +
  #geom_point(aes(x = treatment, y = max.utt, group = population), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean.utt-se.utt, ymax = mean.utt+se.utt),
                width = 0.3, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n), y = 6015), position = position_dodge(0.9), size = 4, color = "black", vjust = 'bottom') +
  scale_y_continuous(limits = c(0, 9000), breaks = seq(6000, 9000, 500), expand = c(0, 0)) +
  coord_cartesian(ylim = c(6000, 9000)) +
  #scale_y_continuous(limits = c(0, 11000), expand = c(0, 0)) +
  #coord_cartesian(ylim = c(6000, 11000)) +
  scale_fill_manual(labels = c("L. Ontario    ", "L. Superior"), 
                    values = c("#fc8d59", "#91bfdb")) +
  labs(y = "Mean Upper Thermal Limit (ADM °C ± SE)", x = 'Replicate Tank') +
  theme_bw() +
  theme(axis.title.x = element_text(color = "Black", size = 22, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(color = "Black", size = 22, margin = margin(0, 10, 0, 0)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        axis.ticks.length = unit(1.5, "mm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        legend.position = "top",
        strip.text = element_text(size = 15),
        plot.margin = unit(c(5, 5, 5, 5), 'mm')) +
  facet_wrap(~treatment, nrow = 1)


ggsave("figures/atc/2020-ATC-ADM.png", width = 18, height = 10, dpi = 300)


