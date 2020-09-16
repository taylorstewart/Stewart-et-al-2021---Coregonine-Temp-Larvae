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
atc.2.0 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "2.0-data")
atc.2.0.temp <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "2.0-temp")
atc.4.5 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "4.5-data")
atc.4.5.temp <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "4.5-temp")
atc.7.0 <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "7.0-data")
atc.7.0.temp <- read_excel("data/ATC/2020-Artedi-ATC.xlsx", sheet = "7.0-temp")

atc <- bind_rows(atc.2.0, atc.4.5, atc.7.0) %>% 
  filter(include.utt != "n")

atc.temp <- bind_rows(atc.2.0.temp, atc.4.5.temp, atc.7.0.temp) %>% 
  select(-aclimation.temp, -temp.diff, -temp.adm)


## -----------------------------------------------------------
## Calculate means and std. error for each treatment
## -----------------------------------------------------------
atc.temp.slope <- do.call(rbind, lapply(1:nrow(atc), function(i) {
  atc.indiv <- atc[i,]
  
  start.time <- atc.indiv$end.time
  end.time <- start.time - 540
  pop <- atc.indiv$population
  trt <- atc.indiv$treatment
  tank <- atc.indiv$tank.id
  rearing <- atc.indiv$rearing.tank
  
  atc.indiv.temp <- atc.temp %>% filter(time <= start.time, time >= end.time,
                                        population == pop, treatment == trt, tank.id == tank) %>% 
    mutate(time = 1:10)
  
  atc.slope <- coef(lm(temp ~ time, data = atc.indiv.temp))[2]
  atc.roll.avg <- mean(atc.indiv.temp$temp)
  
  atc.slope.data <- data.frame(population = pop, treatment = trt, rearing.tank = rearing, tank.id = tank, indiv = i, time = start.time, slope = atc.slope, mean10 = atc.roll.avg)
}))


ggplot(atc.temp.slope, aes(x = slope)) +
  geom_histogram(binwidth = 0.02) +
  theme_bw() +
  facet_wrap(population ~ treatment + tank.id, scales = 'free', ncol = 4) +
  scale_x_continuous(limits = c(-0.25, 0.25))

ggsave("figures/atc/atc-slope-hist.png", width = 13, height = 15, dpi = 300)





atc.temp.summary <- atc.temp.slope %>% group_by(population, rearing.tank, treatment) %>% 
  summarize(mean.ct = mean(mean10),
            sd.ct = sd(mean10),
            n = n(),
            se.ct = sd.ct/sqrt(n)) %>% 
  mutate(replicate.rearing = substr(rearing.tank, (nchar(rearing.tank)+1)-1, nchar(rearing.tank)),
         replicate.rearing = ifelse(population == "Ontario", paste0("LO-", replicate.rearing), paste0("LS-", replicate.rearing))) %>% 
  bind_rows(data.frame(population = c("Ontario", "Ontario", "Superior"),
                       replicate.rearing = c("LO-1", "LO-2", "LS-1"),
                       treatment = factor(rep("9.0°C", 3)),
                       mean.ct = rep(0, 3),
                       sd.ct = rep(0, 3),
                       n = rep(0, 3),
                       se.ct = rep(0, 3)))


ggplot(atc.temp.summary, aes(x = replicate.rearing, y = mean.ct, color = population, shape = population)) +
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




