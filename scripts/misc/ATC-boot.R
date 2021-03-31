#### BOOTSTRAPPING -------------------------------------------------------------------------------

## Define the number of bootstrap samples
boot_n <- 10000

## Run loop (be very patient!)
if(file.exists("data/bootstrap/lethal-temp-1.csv") == FALSE) {
  boot.fish <- lapply(1:boot_n, function(i) {
    print(i)
    
    bootstrap.data <- do.call(rbind, lapply(1:nrow(ATC), function(l) {
      ## create a vector of randomly generated data
      lethal.temp <- sample(ATC$lethal.temp, replace = T, size = 1)
      population <- sample(ATC$population, replace = T, size = 1)
      treatment <- sample(ATC$treatment, replace = T, size = 1)
      
      id.data <- data.frame(lethal.temp, population, treatment)
    }))
    
    write.csv(bootstrap.data, paste0("data/bootstrap/lethal-temp-", i, ".csv"), row.names = FALSE)
  })
}

## Calculate and extract F statistic from the bootstrapped data (each row is an iteration)
f.values.boot <- do.call(rbind, lapply(1:boot_n, function(i) {
  print(i)
  
  ## Load bootstrapped samples
  bootstrap.data <- fread(paste0("data/bootstrap/lethal-temp-", i, ".csv")) %>% 
    mutate(lethal.trans = bcPower(lethal.temp, cisco.lm.bcTrans$roundlam))
  
  ## Calculate and extract F statistic from the bootstrapped data
  bootstrap.lm <- lm(lethal.trans ~ treatment * population, data = bootstrap.data)
  bootstrap.anova <- anova(bootstrap.lm)
  
  ## Create data frame with F statistics
  f.boot <- bootstrap.anova$`F value`
  f.boot.df <- data.frame("treatment" = f.boot[1],
                          "population" = f.boot[2],
                          "treatment.population" = f.boot[3])
}))

## Calculate the 95th percentile from bootstrapped distribution for each of the variables/interactions
fstat.boot.95perc <- do.call(rbind, lapply(1:ncol(f.values.boot), function(i) {
  perc <- quantile(f.values.boot[,i], probs = 0.95)
  data <- data.frame(variable = colnames(f.values.boot[i]),
                     perc95.f = perc)
}))

## Observed F statistics
obs.fstat <- data.frame(obs.f = anova(cisco.glm.final)$`F value`[1:3],
                        variable = c("treatment", "population", "treatment.population"))

## Calculate p-values from empirical distribution
p.value <- data.frame(p.value = c(
  1-ecdf(f.values.boot$treatment)(filter(obs.fstat, variable == "treatment")$obs.f),
  1-ecdf(f.values.boot$population)(filter(obs.fstat, variable == "population")$obs.f),
  1-ecdf(f.values.boot$treatment.population)(filter(obs.fstat, variable == "treatment.population")$obs.f)),
  variable = c("treatment", "population", "treatment.population"))

## Compare oberserved F to boostrapped F 
f.test <- left_join(fstat.boot.95perc, obs.fstat) %>% 
  left_join(p.value) %>% 
  mutate(test = ifelse(obs.f > perc95.f, "Sig.", "Not Sig."))

## Estimated margin means
cisco.glm.full.emm <- emmeans(cisco.glm.final, ~ treatment | population, type = "response")

## Pairwise
pairs(cisco.glm.full.emm, simple = list("treatment"), adjust = "fdr") 


