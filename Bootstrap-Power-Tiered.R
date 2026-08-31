library(boot)
library(dplyr)
library(knitr)

# -----------------------------
# Data subsets
# -----------------------------

data.pooled <- data.all.df
data.int    <- subset(data.all.df, mode == "INT")
data.hs     <- subset(data.all.df, mode == "HS")


power_formula <- meanW ~ jvm + gcc + gc + category +
  bmr + l1dlmr + l2dlmr + pfmr

power_model <- lm(power_formula, data = data.hs)

# Create strata variable from experimental design cells
data.boot.df <- data.hs %>%
  mutate(
    bootstrap_strata = interaction(
      jvm, gcc, gc, category,
      drop = TRUE
    )
  )

boot_power_fn <- function(data, indices) {
  d <- data[indices, ]
  fit <- lm(power_formula, data = d)
  coef(fit)
}

set.seed(1234)

boot_power_stratified <- boot(
  data = data.boot.df,
  statistic = boot_power_fn,
  R = 1000,
  strata = data.boot.df$bootstrap_strata
)

boot_coef_summary <- data.frame(
  Predictor = names(coef(power_model)),
  Original_Beta = coef(power_model),
  Bootstrap_Mean = apply(boot_power_stratified$t, 2, mean, na.rm = TRUE),
  Bootstrap_SE = apply(boot_power_stratified$t, 2, sd, na.rm = TRUE),
  CI_Lower = apply(boot_power_stratified$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  CI_Upper = apply(boot_power_stratified$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)


knitr::kable(
  boot_coef_summary,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  row.names = FALSE,
  caption = "Stratified bootstrap coefficient stability analysis for the tiered execution power use regression model.",
  label = "tab:BootstrapPowerPooled"
)
