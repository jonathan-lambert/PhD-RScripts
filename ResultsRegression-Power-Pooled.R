library(lmtest)
library(sandwich)
library(car)
library(broom)
library(dplyr)


summary(lm(meanW ~ jvm + gcc + mode + gc + category + bmr + l1dlmr + l2dlmr + pfmr 
           + (mode * category) 
           + (gcc * category) 
           + (gc * category), data = data.all.df))

# -----------------------------
# Data subsets
# -----------------------------

data.pooled <- data.all.df
data.int    <- subset(data.all.df, mode == "INT")
data.hs     <- subset(data.all.df, mode == "HS")


# -----------------------------
# Hierarchical model function
# -----------------------------

run_power_hierarchy <- function(data, include_mode = TRUE) {
  
  if (include_mode) {
    m1 <- lm(meanW ~ jvm + gcc, data = data)
    m2 <- lm(meanW ~ jvm + gcc + mode + gc, data = data)
    m3 <- lm(meanW ~ jvm + gcc + mode + gc + category, data = data)
    m4 <- lm(meanW ~ jvm + gcc + mode + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr, data = data)
  } else {
    m1 <- lm(meanW ~ jvm + gcc, data = data)
    m2 <- lm(meanW ~ jvm + gcc + gc, data = data)
    m3 <- lm(meanW ~ jvm + gcc + gc + category, data = data)
    m4 <- lm(meanW ~ jvm + gcc + gc + category +
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
  print(summary(m4))
  
  # HC3 robust standard errors
  robust_final <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
  print(robust_final)
  
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
}


# -----------------------------
# Run pooled power model
# -----------------------------

power_pooled <- run_power_hierarchy(data.pooled, include_mode = TRUE)



# -----------------------------
# Sensitivity Analysis Removing Influential Points
# -----------------------------

m1 <- power_pooled$models[["Block 1: Runtime implementation"]]
m2 <- power_pooled$models[["Block 2: Execution strategy"]]
m3 <- power_pooled$models[["Block 3: Workload classification"]]
m4 <- power_pooled$models[["Block 4: Microarchitecture"]]
summary(m1)
summary(m2)
summary(m3)
summary(m4)



coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
coeftest(m4, vcov = vcovHC(m4, type = "HC3"))



cd <- cooks.distance(m4)
influential <- which(cd > (4/nrow(data.pooled)))

m4.sensitivity <- lm(
  meanW ~ jvm + gcc + mode + gc + category + bmr + l1dlmr + l2dlmr + pfmr,
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



# -----------------------------
# Run interpretive-only power model
# -----------------------------

power_int <- run_power_hierarchy(data.int, include_mode = FALSE)


# -----------------------------
# Run tiered-only power model
# -----------------------------

power_hs <- run_power_hierarchy(data.hs, include_mode = FALSE)