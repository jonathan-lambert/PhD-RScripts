library(lmtest)
library(sandwich)
library(dplyr)
library(knitr)

# -----------------------------
# Data subsets
# -----------------------------

data.pooled <- data.all.df
data.int    <- subset(data.all.df, mode == "INT")
data.hs     <- subset(data.all.df, mode == "HS")

# Final tiered execution model
m4 <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.hs
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
  main = "Residual Distribution: Interpretive Power Model",
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



# Sensitivity Analysis Interpretive Execution
# Cook's distance threshold
# Final tiered execution model
m4 <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.hs
)

cook_threshold_hs <- 4 / nrow(data.hs)

# Identify influential observations
cooks_hs <- cooks.distance(m4)

data.hs.sens <- data.hs[cooks_hs <= cook_threshold_hs, ]

cat("Cook's distance threshold:", cook_threshold_hs, "\n")
cat("Number of influential observations removed:", sum(cooks_hs > cook_threshold_hs), "\n")
cat("Original n:", nrow(data.hs), "\n")
cat("Sensitivity n:", nrow(data.hs.sens), "\n")

# Refit sensitivity model
hs_m4_sens <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.hs.sens
)

# HC3 robust coefficient tables
hs_full_hc3 <- coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
hs_sens_hc3 <- coeftest(hs_m4_sens, vcov = vcovHC(hs_m4_sens, type = "HC3"))

print(hs_full_hc3)
print(hs_sens_hc3)


# Model fit comparison
fit_comparison_hs <- data.frame(
  Model = c("Full Tiered Execution Model", "Sensitivity model"),
  R2 = c(summary(m4)$r.squared, summary(hs_m4_sens)$r.squared),
  Adj_R2 = c(summary(m4)$adj.r.squared, summary(hs_m4_sens)$adj.r.squared),
  Residual_SE = c(summary(m4)$sigma, summary(hs_m4_sens)$sigma),
  n = c(nrow(data.hs), nrow(data.hs.sens))
)

print(fit_comparison_hs)

# Helper function to format coefficient table
make_coef_df <- function(ct) {
  data.frame(
    Predictor = rownames(ct),
    Beta = ct[, 1],
    SE = ct[, 2],
    P = ct[, 4],
    row.names = NULL
  )
}

full_df <- make_coef_df(hs_full_hc3) %>%
  rename(
    Full_Beta = Beta,
    Full_SE = SE,
    Full_P = P
  )

sens_df <- make_coef_df(hs_sens_hc3) %>%
  rename(
    Sens_Beta = Beta,
    Sens_SE = SE,
    Sens_P = P
  )

sensitivity_table_hs <- full_df %>%
  left_join(sens_df, by = "Predictor")

print(sensitivity_table_hs)


# LaTeX output
kable(
  sensitivity_table_hs,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  row.names = FALSE,
  caption = "Interpretive execution power use regression model with HC3 robust standard errors and sensitivity analysis excluding influential observations.",
  label = "tab:power-int-sensitivity"
)
