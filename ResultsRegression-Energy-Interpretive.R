library(lmtest)
library(sandwich)
library(car)
library(broom)
library(dplyr)

# Energy Consumption Interpretive Execution
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
  print(summary(m1))
  print(summary(m2))
  print(summary(m3))
  print(summary(m4))
  
  # HC3 robust standard errors
  robust_m1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
  robust_m2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
  robust_m3 <- coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
  robust_m4 <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
  print(robust_m1)
  print(robust_m2)
  print(robust_m3)
  print(robust_m4)
}

# -----------------------------
# Run pooled power model
# -----------------------------

energy_pooled <- run_energy_hierarchy(data.int, include_mode = FALSE)




# Diagnostics
print(vif(m4))
print(bptest(m4))

# Cook's distance
cooks <- cooks.distance(m4)
cook_threshold <- 4 / nrow(data)

cat("\nCook's distance threshold:", cook_threshold, "\n")
cat("Number of cases above threshold:", sum(cooks > cook_threshold), "\n")
cat("Maximum Cook's distance:", max(cooks), "\n")

return(list(
  models = models,
  r2_table = r2_table,
  robust_final = robust_final,
  cooks = cooks
))


m1 <- lm(log(energyWh) ~ jvm + gcc, data = data.all.df)
m2 <- lm(log(energyWh) ~ jvm + gcc + mode + gc, data = data.all.df)
m3 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category, data = data.all.df)
m4 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category +
           bmr + l1dlmr + l2dlmr + pfmr, data = data.all.df)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

# HC3 Robust Standard Errors
m1.hc3 <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
m2.hc3 <- coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
m3.hc3 <- coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
m4.hc3 <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
print(m1.hc3)
print(m2.hc3)
print(m3.hc3)
print(m4.hc3)



# -----------------------------
# Run pooled power model
# -----------------------------

energy_pooled <- run_energy_hierarchy(data.pooled, include_mode = TRUE)



# -----------------------------
# Sensitivity Analysis Removing Influential Points
# -----------------------------
m1 <- lm(log(energyWh) ~ jvm + gcc, data = data.all.df)
m2 <- lm(log(energyWh) ~ jvm + gcc + mode + gc, data = data.all.df)
m3 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category, data = data.all.df)
m4 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category +
           bmr + l1dlmr + l2dlmr + pfmr, data = data.all.df)

cd <- cooks.distance(m4)
influential <- which(cd > (4/nrow(data.pooled)))

m4.sensitivity <- lm(
  log(energyWh) ~ jvm + gcc + mode + gc + category + bmr + l1dlmr + l2dlmr + pfmr,
  data = data.pooled[-influential,]
)

summary(m4.sensitivity)


table(data.pooled[influential, ]$jvm)
table(data.pooled[influential, ]$mode)
table(data.pooled[influential, ]$category)
table(data.pooled[influential, ]$gc)

table(
  data.pooled[influential, ]$category,
  data.pooled[influential, ]$mode
)
