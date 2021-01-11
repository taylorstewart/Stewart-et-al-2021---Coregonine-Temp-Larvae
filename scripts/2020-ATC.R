#### CLEAR THE ENVIRONMENT FIRST -----------------------------------------------------------------

rm(list = ls(all.names = TRUE))


#### LOAD PACKAGES -------------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
library(ggplot2)


#### LOAD DATA -----------------------------------------------------------------------------------

ATC.2.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "2.0-Data")
ATC.4.5 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "4.5-Data")
ATC.7.0 <- read_excel("data/2020-Artedi-Temperature-ATC.xlsx", sheet = "7.0-Data")


#### COMBINE TEMPERATURE TREATMENTS --------------------------------------------------------------

ATC <- bind_rows(ATC.2.0, ATC.4.5, ATC.7.0) %>% 
  filter(include != "n") %>% 
  mutate(population = factor(population),
         treatment = factor(treatment),
         replicate.tank = substr(tank.id, (nchar(tank.id)+1)-1, nchar(tank.id)),
         replicate.tank = ifelse(replicate.tank == 6, 1, replicate.tank),
         replicate.tank = ifelse(replicate.tank == 7, 2, replicate.tank),
         replicate.tank = ifelse(population == "Ontario", paste0("LO-", replicate.tank), paste0("LS-", replicate.tank)),
         replicate.rearing = substr(rearing.tank, (nchar(rearing.tank)+1)-1, nchar(rearing.tank)),
         replicate.rearing = ifelse(population == "Ontario", paste0("LO-", replicate.rearing), paste0("LS-", replicate.rearing)))


#### CALCULATE MEANS AND SE FOR EACH TREATMENT ---------------------------------------------------

ATC.summary <- ATC %>% group_by(population, replicate.rearing, treatment) %>% 
  summarize(mean.ct = mean(lethal.temp),
            sd.ct = sd(lethal.temp),
            n = n(),
            se.ct = sd.ct/sqrt(n),
            max.ct = max(lethal.temp)) %>% 
  bind_rows(data.frame(population = c("Ontario", "Ontario", "Superior"),
                       replicate.rearing = c("LO-1", "LO-2", "LS-1"),
                       treatment = factor(rep(9, 3)),
                       mean.ct = rep(0, 3),
                       sd.ct = rep(0, 3),
                       n = rep(0, 3),
                       se.ct = rep(0, 3),
                       max.ct = rep(0, 3)))


#### VISUALIZATION -------------------------------------------------------------------------------

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


