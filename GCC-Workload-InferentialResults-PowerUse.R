library(dplyr)
library(rstatix)

# GNU GCC Workload Interaction Effects
# Power Use
# Inferential Statistical Analysis

# Shapiro-Wilk normality test for each GCC version within each workload category
normality_gcc_category <- data.all.df %>%
  group_by(category, gcc) %>%
  shapiro_test(meanW) %>%
  arrange(category, gcc)

print(normality_gcc_category, n = Inf)


# Kruskall-Wallace tests for each GCC within each workload category
kw_gcc_category <- data.all.df %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gcc)

print(kw_gcc_category)


# Pairwise bonferroni corrected tests
pairwise.wilcox.test(
  subset(data.all.df, category == "concurrency")$meanW,
  subset(data.all.df, category == "concurrency")$gcc,
  p.adjust.method = "bonferroni"
)


pairwise.wilcox.test(
  subset(data.all.df, category == "web")$meanW,
  subset(data.all.df, category == "web")$gcc,
  p.adjust.method = "bonferroni"
)




# Execution mode (Tiered)
library(dplyr)
library(rstatix)

kw_gcc_category_hs <- data.all.df %>%
  filter(mode == "HS") %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gcc)

print(kw_gcc_category_hs, n = Inf)


# Tiered execution (HS) - Web
pairwise.wilcox.test(
  subset(data.all.df, mode == "HS" & category == "web")$meanW,
  subset(data.all.df, mode == "HS" & category == "web")$gcc,
  p.adjust.method = "bonferroni"
)


# Execution mode (Interpretive)
kw_gcc_category_int <- data.all.df %>%
  filter(mode == "INT") %>%
  group_by(category) %>%
  kruskal_test(meanW ~ gcc)

print(kw_gcc_category_int, n = Inf)

# Interpretive execution (INT) - Apache
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "apache")$meanW,
  subset(data.all.df, mode == "INT" & category == "apache")$gcc,
  p.adjust.method = "bonferroni"
)

# Interpretive execution (INT) - Concurrency
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "concurrency")$meanW,
  subset(data.all.df, mode == "INT" & category == "concurrency")$gcc,
  p.adjust.method = "bonferroni"
)

# Interpretive execution (INT) - Web
pairwise.wilcox.test(
  subset(data.all.df, mode == "INT" & category == "web")$meanW,
  subset(data.all.df, mode == "INT" & category == "web")$gcc,
  p.adjust.method = "bonferroni"
)

