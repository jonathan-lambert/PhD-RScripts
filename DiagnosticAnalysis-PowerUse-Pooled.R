library(car)
library(dplyr)
library(knitr)

# Diagnostics

# -----------------------------
# Data subsets
# -----------------------------

data.pooled <- data.all.df
data.int    <- subset(data.all.df, mode == "INT")
data.hs     <- subset(data.all.df, mode == "HS")


# Final pooled power model
m4 <- lm(
  meanW ~ jvm + gcc + mode + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.pooled
)


# Basic residual summaries
summary(residuals(m4))

# Standardised residuals
std_resid <- rstandard(m4)
summary(std_resid)

# Residual histogram
hist(
  residuals(m4),
  breaks = 50,
  main = "Residual Distribution: Pooled Power Model",
  xlab = "Residuals"
)

# Q-Q plot
qqnorm(residuals(m4))
qqline(residuals(m4))

# Shapiro-Wilk test on residuals
shapiro.test(residuals(m4))


# Assessing for Heteroskedastcity
bptest(m4)

coeftest(m4, vcov = vcovHC(m4, type = "HC3"))


# Assessing for Multicolinearity


# Fully specified regression model
power_model_full <- lm(
  powerW ~ jvm + gcc + mode + gc + category +
    ipc + bmr + l1dlmr + l2dlmr + pfmr +
    bpi + cai + pai,
  data = data.all.df
)

# Calculate VIF / GVIF
vif_raw <- car::vif(m4)

# Convert to table
vif_table <- as.data.frame(vif_raw)

# If categorical predictors are present, car::vif() returns GVIF, Df, GVIF^(1/(2*Df))
vif_table <- vif_table %>%
  tibble::rownames_to_column("Predictor") %>%
  rename(
    GVIF = GVIF,
    Df = Df,
    Adjusted_GVIF = `GVIF^(1/(2*Df))`
  )

# View table
print(vif_table)

# LaTeX output
kable(
  vif_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  caption = "Adjusted Generalised Variance Inflation Factor statistics for predictors included within the pooled power use regression model.",
  label = "tab:RegressionDiagnostics-VIF"
)


# Assessment of Linearity
library(ggplot2)

# Final pooled power model
power_model <- lm(
  meanW ~ jvm + gcc + mode + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.all.df
)

# Create diagnostic dataframe
diag_power_df <- data.frame(
  fitted_values = fitted(power_model),
  residuals = resid(power_model),
  standardised_residuals = rstandard(power_model)
)

# Residuals vs fitted values
ggplot(diag_power_df, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals versus Fitted Values: Pooled Power Use Model"
  ) +
  theme_minimal()
