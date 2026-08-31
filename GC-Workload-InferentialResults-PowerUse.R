library(dplyr)
library(rstatix)

# Garbage Collector Workload Interaction Effects
# Power Use
# Inferential Statistical Analysis

# Shapiro-Wilk normality test for each GCC version within each workload category
normality_gc_category <- data.all.df %>%
  group_by(category, gc) %>%
  shapiro_test(meanW) %>%
  arrange(category, gc)

print(normality_gc_category, n = Inf)


# Kruskall-Wallace tests for each GCC within each workload category
kw_gc_category <- data.all.df %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gc)

print(kw_gc_category)


# Pairwise bonferroni corrected tests
pairwise.wilcox.test(
  subset(data.all.df, category == "concurrency")$meanW,
  subset(data.all.df, category == "concurrency")$gc,
  p.adjust.method = "bonferroni"
)


pairwise.wilcox.test(
  subset(data.all.df, category == "database")$meanW,
  subset(data.all.df, category == "database")$gc,
  p.adjust.method = "bonferroni"
)




# Execution mode (Tiered)
library(dplyr)
library(rstatix)

kw_gc_category_hs <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gc)

print(kw_gc_category_hs, n = Inf)

# Tiered execution (HS) - Apache
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "apache")$meanW,
  subset(data.all.df, mode == "HS" & category == "apache")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Concurrency
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "concurrency")$meanW,
  subset(data.all.df, mode == "HS" & category == "concurrency")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Database
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "database")$meanW,
  subset(data.all.df, mode == "HS" & category == "database")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Functional
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "functional")$meanW,
  subset(data.all.df, mode == "HS" & category == "functional")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Scala
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "scala")$meanW,
  subset(data.all.df, mode == "HS" & category == "scala")$gc,
  p.adjust.method = "bonferroni"
)

# Tiered execution (HS) - Web
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "web")$meanW,
  subset(data.all.df, mode == "HS" & category == "web")$gc,
  p.adjust.method = "bonferroni"
)


# Execution mode (Interpretive)
kw_gc_category_int <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gc)

print(kw_gc_category_int, n = Inf)

# Interpretive execution (INT) - Database
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "database")$meanW,
  subset(data.all.df, mode == "INT" & category == "database")$gc,
  p.adjust.method = "bonferroni"
)



