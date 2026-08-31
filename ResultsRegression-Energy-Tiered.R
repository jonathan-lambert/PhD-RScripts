library(lmtest)
library(sandwich)
library(car)
library(broom)
library(dplyr)

# Energy Consumption RSquared change Tables with ANOVA
# -----------------------------
# Data subsets
# -----------------------------

data.pooled <- data.all.df
data.int    <- subset(data.all.df, mode == "INT")
data.hs     <- subset(data.all.df, mode == "HS")


# -----------------------------
# Hierarchical Energy Consumption model function
# -----------------------------

run_energy_hierarchy <- function(data, include_mode = TRUE) {
  
  if (include_mode) {
    m1 <- lm(log(energyWh) ~ jvm + gcc, data = data)
    m2 <- lm(log(energyWh) ~ jvm + gcc + mode + gc, data = data)
    m3 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category, data = data)
    m4 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr, data = data)
  } else {
    m1 <- lm(log(energyWh) ~ jvm + gcc, data = data)
    m2 <- lm(log(energyWh) ~ jvm + gcc + gc, data = data)
    m3 <- lm(log(energyWh) ~ jvm + gcc + gc + category, data = data)
    m4 <- lm(log(energyWh) ~ jvm + gcc + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr, data = data)
  }
  
  models <- list(
    "Block 1: Runtime implementation" = m1,
    "Block 2: Execution strategy"     = m2,
    "Block 3: Workload classification"= m3,
    "Block 4: Microarchitecture"      = m4
  )
  
  # Model comparison
  print(anova(m1, m2, m3, m4))
  
  # R2 change table
  r2_table <- tibble(
    Model = names(models),
    R2 = sapply(models, function(x) summary(x)$r.squared),
    Adj_R2 = sapply(models, function(x) summary(x)$adj.r.squared)
  ) %>%
    mutate(
      R2_Change = R2 - lag(R2),
      R2_Change = ifelse(is.na(R2_Change), R2, R2_Change)
    )
  
  print(r2_table)
  
  # Final model conventional summary
  #print(summary(m2))
  #print(summary(m1))
  #print(summary(m3))
  #print(summary(m4))
  
  # HC3 robust standard errors
  robust_m1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
  robust_m3 <- coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
  robust_m2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
  robust_m4 <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
  print(robust_m1)
  print(robust_m2)
  print(robust_m3)
  print(robust_m4)
}

# -----------------------------
# Run energy model
# -----------------------------

energy_pooled <- run_energy_hierarchy(data.pooled, include_mode = TRUE)
energy_int <- run_energy_hierarchy(data.int, include_mode = FALSE)
energy_hs <- run_energy_hierarchy(data.hs, include_mode = FALSE)
