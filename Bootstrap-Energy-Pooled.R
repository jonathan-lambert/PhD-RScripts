library(boot)
library(dplyr)
library(knitr)

energy_formula <- log(energyWh) ~ jvm + gcc + mode + gc + category +
  bmr + l1dlmr + l2dlmr + pfmr

energy_model <- lm(energy_formula, data = data.all.df)

# Create strata variable from experimental design cells
data.boot.df <- data.all.df %>%
  mutate(
    bootstrap_strata = interaction(
      jvm, gcc, mode, gc, category,
      drop = TRUE
    )
  )

boot_energy_fn <- function(data, indices) {
  d <- data[indices, ]
  fit <- lm(energy_formula, data = d)
  coef(fit)
}

set.seed(1234)

boot_energy_stratified <- boot(
  data = data.boot.df,
  statistic = boot_energy_fn,
  R = 1000,
  strata = data.boot.df$bootstrap_strata
)

boot_coef_summary <- data.frame(
  Predictor = names(coef(energy_model)),
  Original_Beta = coef(energy_model),
  Bootstrap_Mean = apply(boot_energy_stratified$t, 2, mean, na.rm = TRUE),
  Bootstrap_SE = apply(boot_energy_stratified$t, 2, sd, na.rm = TRUE),
  CI_Lower = apply(boot_energy_stratified$t, 2, quantile, probs = 0.025, na.rm = TRUE),
  CI_Upper = apply(boot_energy_stratified$t, 2, quantile, probs = 0.975, na.rm = TRUE)
)

knitr::kable(
  boot_coef_summary,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  row.names = FALSE,
  caption = "Stratified bootstrap coefficient stability analysis for the pooled energy consumption regression model.",
  label = "tab:BootstrapEnergyPooled"
)
