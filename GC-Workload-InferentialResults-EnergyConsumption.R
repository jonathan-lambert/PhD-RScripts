library(dplyr)
library(rstatix)

# Garbage Collector Workload Interaction Effects
# Power Use
# Inferential Statistical Analysis

# Shapiro-Wilk normality test for each GCC version within each workload category
normality_gc_category <- data.all.df %>%
  group_by(category, gc) %>%
  shapiro_test(energyWh) %>%
  arrange(category, gc)

print(normality_gc_category, n = Inf)


# Kruskall-Wallace tests for each GCC within each workload category
kw_gc_category <- data.all.df %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gc)

print(kw_gc_category)



# Execution mode (Tiered)
kw_gc_category_hs <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gc)

print(kw_gc_category_hs, n = Inf)


# Tiered execution (HS) - Concurrency
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "concurrency")$energyWh,
  subset(data.all.df, mode == "HS" & category == "concurrency")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Database
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "database")$energyWh,
  subset(data.all.df, mode == "HS" & category == "database")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Web
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "web")$energyWh,
  subset(data.all.df, mode == "HS" & category == "web")$gc,
  p.adjust.method = "bonferroni"
)


# Execution mode (Interpretive)
kw_gc_category_int <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category) %>%
  kruskal_test(energyWh ~ gc)

print(kw_gc_category_int, n = Inf)

# Interpretive execution (INT) - Database
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "database")$energyWh,
  subset(data.all.df, mode == "INT" & category == "database")$gc,
  p.adjust.method = "bonferroni"
)
