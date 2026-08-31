library(dplyr)
library(rstatix)

# GNU GCC Workload Interaction Effects
# Energy Consumption
# Inferential Statistical Analysis

# Shapiro-Wilk normality test for each GCC version within each workload category
normality_gcc_category <- data.all.df %>%
  group_by(category, gcc) %>%
  shapiro_test(energyWh) %>%
  arrange(category, gcc)

print(normality_gcc_category, n = Inf)


# Kruskall-Wallace tests for each GCC within each workload category
kw_gcc_category <- data.all.df %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gcc)

print(kw_gcc_category)



# Execution mode (Tiered)
kw_gcc_category_hs <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gcc)

print(kw_gcc_category_hs, n = Inf)


# Execution mode (Interpretive)
kw_gcc_category_int <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gcc)

print(kw_gcc_category_int, n = Inf)

# Interpretive execution (INT) - Web
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "web")$energyWh,
  subset(data.all.df, mode == "INT" & category == "web")$gcc,
  p.adjust.method = "bonferroni"
)

