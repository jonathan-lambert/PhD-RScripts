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

# Final interpretive execution model
int_m4 <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.int
)


# Sensitivity Analysis Interpretive Execution
# Cook's distance threshold
cook_threshold_int <- 4 / nrow(data.int)

# Identify influential observations
cooks_int <- cooks.distance(int_m4)

data.int.sens <- data.int[cooks_int <= cook_threshold_int, ]

cat("Cook's distance threshold:", cook_threshold_int, "\n")
cat("Number of influential observations removed:", sum(cooks_int > cook_threshold_int), "\n")
cat("Original n:", nrow(data.int), "\n")
cat("Sensitivity n:", nrow(data.int.sens), "\n")

# Refit sensitivity model
int_m4_sens <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.int.sens
)

# HC3 robust coefficient tables
int_full_hc3 <- coeftest(int_m4, vcov = vcovHC(int_m4, type = "HC3"))
int_sens_hc3 <- coeftest(int_m4_sens, vcov = vcovHC(int_m4_sens, type = "HC3"))

print(int_full_hc3)
print(int_sens_hc3)

# Model fit comparison
fit_comparison_int <- data.frame(
  Model = c("Full interpretive model", "Sensitivity model"),
  R2 = c(summary(int_m4)$r.squared, summary(int_m4_sens)$r.squared),
  Adj_R2 = c(summary(int_m4)$adj.r.squared, summary(int_m4_sens)$adj.r.squared),
  Residual_SE = c(summary(int_m4)$sigma, summary(int_m4_sens)$sigma),
  n = c(nrow(data.int), nrow(data.int.sens))
)

print(fit_comparison_int)

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

full_df <- make_coef_df(int_full_hc3) %>%
  rename(
    Full_Beta = Beta,
    Full_SE = SE,
    Full_P = P
  )

sens_df <- make_coef_df(int_sens_hc3) %>%
  rename(
    Sens_Beta = Beta,
    Sens_SE = SE,
    Sens_P = P
  )

sensitivity_table_int <- full_df %>%
  left_join(sens_df, by = "Predictor")

print(sensitivity_table_int)

# Optional: clean predictor names for thesis table
sensitivity_table_int$Predictor <- recode(
  sensitivity_table_int$Predictor,
  "(Intercept)" = "Intercept",
  "jvmj10" = "JRE 10",
  "jvmj11" = "JRE 11",
  "jvmj12" = "JRE 12",
  "jvmj13" = "JRE 13",
  "jvmj14" = "JRE 14",
  "gccGCC6" = "GCC 6",
  "gccGCC7" = "GCC 7",
  "gccGCC8" = "GCC 8",
  "gcCPGC" = "CPGC",
  "gcG1GC" = "G1GC",
  "gcPGC" = "PGC",
  "gcSGC" = "SGC",
  "categoryconcurrency" = "Concurrency",
  "categorydatabase" = "Database",
  "categoryfunctional" = "Functional",
  "categoryscala" = "Scala",
  "categoryweb" = "Web",
  "bmr" = "BMR",
  "l1dlmr" = "L1DLMR",
  "l2dlmr" = "L2DLMR",
  "pfmr" = "PFMR"
)

# LaTeX output
kable(
  sensitivity_table_int,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  row.names = FALSE,
  caption = "Interpretive execution power use regression model with HC3 robust standard errors and sensitivity analysis excluding influential observations.",
  label = "tab:power-int-sensitivity"
)





# Final pooled power model
m4 <- lm(
  meanW ~ jvm + gcc + gc + category +
    bmr + l1dlmr + l2dlmr + pfmr,
  data = data.int
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


# Fully specified regression model
power_model_full <- lm(
  meanW ~ jvm + gcc + gc + category +
    ipc + bmr + l1dlmr + l2dlmr + pfmr +
    bpi + cai + pai,
  data = data.int
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