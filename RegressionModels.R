# Building a bootstrap model
# There's not enough data points to cluster sample
# at the jvm, mode, gcc, gc, benchmark level - actually only a single power measurment.
# Load all Power consumption data. 
# Load Power data for INT: Interpretation
# Load Power data for HS: Hotspot Teired Compilation
#
# Load all Perf consumption data
# Load Perf data for INT: Interpretation
# Load Perf data for HS: Hotspot Teired Compilation
#
library(readxl)
library(ggplot2)
library(factoextra)
library(apaTables)
library(vcd)
library(ggpubr)
library(car)
library(Metrics)
library(lmtest)

excel.table.hs.power <- read_excel("/Users/Jlambert_1/Documents/Research/PHD-Maynooth/OpenJDKPerformanceAnalysis/PhDThesisData/power-hs-all.xlsx")
excel.table.int.power <- read_excel("/Users/Jlambert_1/Documents/Research/PHD-Maynooth/OpenJDKPerformanceAnalysis/PhDThesisData/power-int-all.xlsx")
excel.table.hs.perf <- read_excel("/Users/Jlambert_1/Documents/Research/PHD-Maynooth/OpenJDKPerformanceAnalysis/PhDThesisData/perf-hs-all.xlsx")
excel.table.int.perf <- read_excel("/Users/Jlambert_1/Documents/Research/PHD-Maynooth/OpenJDKPerformanceAnalysis/PhDThesisData/perf-int-all.xlsx")

data.hs.power.df <- as.data.frame(excel.table.hs.power)
data.hs.power.df$jvm <- as.factor(data.hs.power.df$jvm)
data.hs.power.df$mode <- as.factor(data.hs.power.df$mode)
data.hs.power.df$gcc <- as.factor(data.hs.power.df$gcc)
data.hs.power.df$gc <- as.factor(data.hs.power.df$gc)  
data.hs.power.df$benchmark <- as.factor(data.hs.power.df$benchmark)

data.int.power.df <- as.data.frame(excel.table.int.power)
data.int.power.df$jvm <- as.factor(data.int.power.df$jvm)
data.int.power.df$mode <- as.factor(data.int.power.df$mode)
data.int.power.df$gcc <- as.factor(data.int.power.df$gcc)
data.int.power.df$gc <- as.factor(data.int.power.df$gc)  
data.int.power.df$benchmark <- as.factor(data.int.power.df$benchmark)

data.power.all.df <- na.omit(rbind(data.hs.power.df, data.int.power.df))
# data.power.all.df$modeI <- ifelse(data.power.all.df$mode == "HS", 1, 0)

data.hs.perf.df	<-	as.data.frame(excel.table.hs.perf)
data.hs.perf.df$jvm	<-	as.factor(data.hs.perf.df$jvm)
data.hs.perf.df$mode	<-	as.factor(data.hs.perf.df$mode)
data.hs.perf.df$gcc	<-	as.factor(data.hs.perf.df$gcc)
data.hs.perf.df$gc	<-	as.factor(data.hs.perf.df$gc)
data.hs.perf.df$benchmark	<-	as.factor(data.hs.perf.df$benchmark)

data.int.perf.df	<-	as.data.frame(excel.table.int.perf)
data.int.perf.df$jvm	<-	as.factor(data.int.perf.df$jvm)
data.int.perf.df$mode	<-	as.factor(data.int.perf.df$mode)
data.int.perf.df$gcc	<-	as.factor(data.int.perf.df$gcc)
data.int.perf.df$gc	<-	as.factor(data.int.perf.df$gc)
data.int.perf.df$benchmark	<-	as.factor(data.int.perf.df$benchmark)

data.perf.all.df <- rbind(data.hs.perf.df, data.int.perf.df)


# Merge Power and Perf data into a single Dataframe
# Merging Power with Perf, both need to be summariesed
data.agg.perf.df <- aggregate(x = data.perf.all.df, 
                              by = list(data.perf.all.df$jvm, 
                                        data.perf.all.df$mode, 
                                        data.perf.all.df$gcc, 
                                        data.perf.all.df$gc, 
                                        data.perf.all.df$benchmark),
                              FUN = mean,
                              na.rm = TRUE)

data.agg.power.df <- aggregate(x = data.power.all.df, 
                               by = list(data.power.all.df$jvm, 
                                         data.power.all.df$mode, 
                                         data.power.all.df$gcc, 
                                         data.power.all.df$gc, 
                                         data.power.all.df$benchmark),
                               FUN = mean,
                               na.rm = TRUE)

# Updating dropping unused columns and colnames
data.agg.perf.df <- data.agg.perf.df[,c(1:5,11:37)]
cn <- colnames(data.perf.all.df)
colnames(data.agg.perf.df) <- cn

data.agg.power.df <- data.agg.power.df[,c(1:5,12:30)]
cn <- colnames(data.power.all.df)
cn <- cn[c(1:5, 7:25)]
colnames(data.agg.power.df) <- cn

# Merging power and perf dataframes
data.all.df <- merge(data.agg.perf.df, data.agg.power.df)


axis.labels.24 <- c("akka-uct", "chi-square", "db-shootout", "dec-tree", "dotty", "finagle-chirper", 
                    "finagle-http", "fj-kmeans", "future-genetic", "gauss-mix", "log-regression", "mnemonics",
                    "movie-lens", "naive-bayes", "neo4j-analytics", "page-rank", "par-mnemonics", "philosophers",    
                    "reactors", "rx-scrabble", "scala-doku", "scala-kmeans", "scala-stm-bench7", "scrabble")
axis.labels.12a <- c("akka-uct", "chi-square", "db-shootout", "dec-tree", "dotty", "finagle-chirper", 
                     "finagle-http", "fj-kmeans", "future-genetic", "gauss-mix", "log-regression", "mnemonics")
axis.labels.12b <- c("movie-lens", "naive-bayes", "neo4j-analytics", "page-rank", "par-mnemonics", "philosophers",    
                     "reactors", "rx-scrabble", "scala-doku", "scala-kmeans", "scala-stm-bench7", "scrabble")


# Workload Category
apache <- c("chi-square", "dec-tree", "gauss-mix", "log-regression", "movie-lens", "naive-bayes", "page-rank")
concurrency <- c("akka-uct", "fj-kmeans", "reactors")
database <- c("db-shootout", "neo4j-analytics")
functional <- c("future-genetic", "mnemonics", "par-mnemonics", "rx-scrabble", "scrabble")
scala <- c("dotty", "philosophers", "scala-doku", "scala-kmeans", "scala-stm-bench7")
web <- c("finagle-chirper", "finagle-http")    

data.all.df$category <- ifelse(data.all.df$benchmark %in% apache, "apache",
                               ifelse(data.all.df$benchmark %in% concurrency, "concurrency",
                                      ifelse(data.all.df$benchmark %in% database, "database",
                                             ifelse(data.all.df$benchmark %in% functional, "functional",
                                                    ifelse(data.all.df$benchmark %in% scala, "scala",
                                                           ifelse(data.all.df$benchmark %in% web, "web", "other"))))))
data.all.df$category <- as.factor(data.all.df$category)

# Reordering columns, bringing new category factor forward
data.all.df <- data.all.df[,c(1:5, 52, 6:51)]



# Message-passing
message.passing <- c("akka-uct", "finagle-http", "reactors")
data.all.df$catmp <- ifelse(data.all.df$benchmark %in% message.passing, "mp", "nmp")
data.all.df$catmp <- as.factor(data.all.df$catmp)


# Atomics
atomics <- c("finagle-chirper", "philosophers", "page-rank", "scala-kmeans")
data.all.df$catat <- ifelse(data.all.df$benchmark %in% atomics, "at", "nat")
data.all.df$catat<- as.factor(data.all.df$catat)


# Memory-bound
memory.bound <- c("mnemonics", "par-mnemonics", "scala-stm-bench7", "scrabble")
data.all.df$catmb<- ifelse(data.all.df$benchmark %in% memory.bound, "mb", "nmb")
data.all.df$catmb <- as.factor(data.all.df$catmb)


# Data-parallel
data.parallel <- c("chi-square", "dec-tree", "log-regression", "movie-lens", "naive-bayes",
                   "page-rank", "par-mnemonics", "scala-stm-bench7", "scrabble")
data.all.df$catdp <- ifelse(data.all.df$benchmark %in% data.parallel, "dp", "ndp")
data.all.df$catdp <- as.factor(data.all.df$catdp)


# Data-Structures
data.structures <- c("db-shootout", "dotty", "fj-kmeans")
data.all.df$catds<- ifelse(data.all.df$benchmark %in% data.structures, "ds", "nds")
data.all.df$catds <- as.factor(data.all.df$catds)


# Machine-Learning
machine.learning <- c("chi-square", "dec-tree", "log-regression", "naive-bayes", "gauss-mix")
data.all.df$catml <- ifelse(data.all.df$benchmark %in% machine.learning, "ml", "nml")
data.all.df$catml <- as.factor(data.all.df$catml)


# Invoke Dynamic
invoke.dynamic <- c("scrabble", "neo4j-analytics", "dotty", "future-genetic", 
                    "rx-scrabble", "db-shootout", "finagle-chirper", "finagle-http",
                    "movie-lens")
data.all.df$catid <- ifelse(data.all.df$benchmark %in% invoke.dynamic, "id", "nid")
data.all.df$catid <- as.factor(data.all.df$catid)



# Method Invocation Load
method.high <- c("neo4j-analytics", "reactors", "db-shootout")
method.mid <- c("movie-lens", "philosophers", "page-rank", "finagle-chirper", "naive-bayes",
                "finagle-http", "dec-tree", "chi-square", "akka-uct", "log-regression", 
                "future-genetic","dotty")
method.low <- c("scala-stm-bench7", "fj-kmeans", "scrabble", "rx-scrabble")

data.all.df$catmi <- ifelse(data.all.df$benchmark %in% method.high, "mihigh",
                            ifelse(data.all.df$benchmark %in% method.mid, "mimid", "milow"))
data.all.df$catmi<- as.factor(data.all.df$catmi)


# Reorder the columns to bring category factors up front
data.all.df <- data.all.df[,c(1:6, 53:60, 7:52)]



###################################################################
#########################  Model 1:       #########################
###################################################################

    # Heirarchical
    # Intercept
    lm.model1.intercept <- lm(meanW ~ 1, data = data.all.df)
    summary(lm.model1.intercept)
    
    # JRE Version
    lm.model1.jre <- lm(meanW ~ jvm, data = data.all.df)
    summary(lm.model1.jre)
    
    # GCC Version
    lm.model1.gcc <- lm(meanW ~ jvm + gcc, data = data.all.df)
    summary(lm.model1.gcc)
    
    # Mode of Execution
    lm.model1.mode <- lm(meanW ~ jvm + gcc + mode, data = data.all.df)
    summary(lm.model1.mode)
    
    # GC Version
    lm.model1.gc <- lm(meanW ~ jvm + gcc + mode + gc, data = data.all.df)
    summary(lm.model1.gc)
    
    # Workload Category
    lm.model1.category <- lm(meanW ~ jvm + gcc + mode + gc + 
                               category, data = data.all.df)
    summary(lm.model1.category)
    
    # Workload Characteristics
    lm.model1.characteristics <- lm(meanW ~ jvm + gcc + mode + gc + 
                                      category + 
                                      catmp + catat + catmb + catdp + 
                                      catds + catml + catid + catmi, 
                                    data = data.all.df)
    summary(lm.model1.characteristics)

    # Branch Misrates
    lm.model1.bmr <- lm(meanW ~ jvm + gcc + mode + gc + 
                          category + catmp + catat + catmb + catdp + catds + catml + catid + catmi +
                          tbrpti + brmps + bmr, data = data.all.df)
    summary(lm.model1.bmr) 
    
    # L1 Data Cache Misrates
    lm.model1.l1dlmr <- lm(meanW ~ jvm + gcc + mode + gc + 
                             category + catmp + catat + catmb + catdp + catds + catml + catid + catmi +
                             tbrpti + brmps + bmr + 
                             tl1dlpti + l1dlmps + l1dlmr, 
                           data = data.all.df)
    summary(lm.model1.l1dlmr)
    
    # L2 Data Cache Misrates
    lm.model1.l2dlmr <- lm(meanW ~ jvm + gcc + mode + gc + 
                             category + catmp + catat + catmb + catdp + catds + catml + catid + catmi +
                             tbrpti + brmps + bmr + 
                             tl1dlpti + l1dlmps + l1dlmr + 
                             tl2dlpti + l2dlmps + l2dlmr, 
                           data = data.all.df)
    summary(lm.model1.l2dlmr)
    
    # Page Faults
    lm.model1.pfmr <- lm(meanW ~ jvm + gcc + mode + gc + 
                           category + catmp + catat + catmb + catdp + catds + catml + catid + catmi +
                           tbrpti + brmps + bmr + 
                           tl1dlpti + l1dlmps + l1dlmr + 
                           tl2dlpti + l2dlmps + l2dlmr +
                           tpfpti + pfmr + pfkps, 
                         data = data.all.df)
    summary(lm.model1.pfmr)



# The Full Model (82%)
lm.model1 <- lm(meanW ~ jvm + gcc + mode + gc + 
                  category + catmp + catat + catmb + catdp + catds + catml + catid + catmi +
                  tbrpti + brmps + bmr + 
                  tl1dlpti + l1dlmps + l1dlmr + 
                  tl2dlpti + l2dlmps + l2dlmr +
                  tpfpti + pfmr + pfkps, data = data.all.df)

summary(lm.model1)


# Estimate R-square Change
lm.model1.anova <- anova(lm.model1.intercept, lm.model1.jre, lm.model1.gcc, lm.model1.mode, lm.model1.gc,
                         lm.model1.category, lm.model1.characteristics,
                         lm.model1.bmr, lm.model1.l1dlmr, lm.model1.l2dlmr, lm.model1.pfmr)    

summary(lm.model1.anova)
lm.model1.anova

# Assessing Gauss-Markov Assumptions
# Normaility of Regression Residuals: Histogram
data.lm.res <- lm.model1$residuals
data.lm.res.df <- data.frame(data.lm.res)
colnames(data.lm.res.df) <- c("residuals")

ggplot(aes(residuals), data = data.lm.res.df) + 
  geom_histogram(aes(y = ..density..), colour= "black", fill= "white") +
  stat_function(fun = dnorm, args = list(mean = mean(data.lm.res.df$residuals), sd = sd(data.lm.res.df$residuals))) +
  xlab("Residuals") +
  ylab("Frequency") 


# Linearity: Plot of the Observed vs Predicted
data.lm.predicted <- lm.model1$fitted.values
data.lm.observed <- data.all.df$meanW

data.lm.df <- data.frame(data.lm.observed, data.lm.predicted)
colnames(data.lm.df) <- c("observed", "predicted")
lm.fit <- lm(observed ~ predicted, data = data.lm.df)

ggplot(data = data.lm.df, aes(x = predicted, y = observed, fill = mode)) +
  geom_point(color = "blue") +
  geom_abline(intercept = lm.fit$coefficients[[1]], slope = lm.fit$coefficients[[2]], color = "yellow") +
  xlab("Predicted") +
  ylab("Observed") +
  theme(legend.position="none")


# Heteroskedasticity
data.lm.df <- cbind(data.lm.df, data.lm.res)
colnames(data.lm.df) <- c("observed", "predicted", "residuals")

lm.hetero <- lm(residuals ~ predicted, data = data.lm.df)

ggplot(data = data.lm.df, aes(x = predicted, y = residuals)) +
  geom_point(color = "blue") +
  geom_abline(intercept = lm.hetero$coefficients[[1]], slope = lm.hetero$coefficients[[2]], color = "yellow") +
  xlab("Predicted") +
  ylab("Residuals") +
  ylim(c(-0.7, 0.7)) +
  theme(legend.position="none")


# Breusch-Pagan Test
bptest(lm.model1)


# Multicolinearity
vif(lm.model1)


# Testing for Autocorrelation
durbinWatsonTest(lm.model1)

# lag Autocorrelation D-W Statistic p-value
# 1      0.02244216      1.954621   0.084
# Alternative hypothesis: rho != 0






###################################################################
################  Parsimonious Model (model2):  ###################
###################################################################

    # Check Correlations and Reduce number of Predictors
    data.all.df[, c("tbrpti", "brmps", "bmr",
                    "tl1dlpti", "l1dlmps", "l1dlmr", 
                    "tl2dlpti", "l2dlmps", "l2dlmr",
                    "tpfpti", "pfkps", "pfmr")] %>% 
      cor(use="pairwise.complete.obs") %>% 
      ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
    
    
    lm.model2.parsimonious <- lm(meanW ~ jvm + mode + 
                                   category + catat + catmb + catml + catmi +
                                   l1dlmr + 
                                   l2dlmps +
                                   pfmr, 
                                 data = data.all.df)
    
    # Correlation Plot of Reduced Predictors
    data.all.df[, c("l1dlmr", "l2dlmps", "pfmr")] %>% 
      cor(use="pairwise.complete.obs") %>% 
      ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
    
    
    corr.table <- data.all.df[, c("tbrpti", "brmps", "bmr", 
                                  "tl1dlpti", "l1dlmps", "l1dlmr", 
                                  "tl2dlpti", "l2dlmps", "l2dlmr",
                                  "tpfpti", "pfkps")] %>% 
      cor(use="pairwise.complete.obs")
    
    cormat <- cor(data.all.df[, c("tbrpti", "brmps", "bmr", 
                                  "tl1dlpti", "l1dlmps", "l1dlmr", 
                                  "tl2dlpti", "l2dlmps", "l2dlmr",
                                  "tpfpti", "pfkps")], use="pairwise.complete.obs")
    
    for(row in 1:11) {
      corr.row <- cormat[row, 1:11] 
      print(mean(abs(corr.row)))
    }
    
    
    # Heirarchical
    # Intercept
    lm.model2.intercept <- lm(meanW ~ 1, 
                              data = data.all.df)
    summary(lm.model2.intercept)
    
    # JRE Version
    lm.model2.jre <- lm(meanW ~ jvm, 
                        data = data.all.df)
    summary(lm.model2.jre)
    
    # Mode of Execution
    lm.model2.mode <- lm(meanW ~ jvm + mode, 
                         data = data.all.df)
    summary(lm.model2.mode)
    
    # Workload Category
    lm.model2.category <- lm(meanW ~ jvm + mode + 
                               category, 
                             data = data.all.df)
    summary(lm.model2.category)
    
    # Workload Characteristics
    lm.model2.characteristics <- lm(meanW ~ jvm + mode + 
                                      category + catat + catmb + catml + catmi, 
                                    data = data.all.df)
    summary(lm.model2.characteristics)
    
    # L1 Data Cache Misrates
    lm.model2.l1dlmr <- lm(meanW ~ jvm + mode + 
                             category + catat + catmb + catml + catmi +
                             l1dlmr, 
                           data = data.all.df)
    summary(lm.model2.l1dlmr)
    
    # L2 Data Cache Misrates
    lm.model2.l2dlmr <- lm(meanW ~ jvm + mode + 
                             category + catat + catmb + catml + catmi +
                             l1dlmr + 
                             l2dlmps, 
                           data = data.all.df)
    summary(lm.model2.l2dlmr)
    
    # Page Faults
    lm.model2.pfmr <- lm(meanW ~ jvm + mode + 
                           category + catat + catmb + catml + catmi +
                           l1dlmr + 
                           l2dlmps +
                           pfmr, 
                         data = data.all.df)
    summary(lm.model2.pfmr)
    
    
    # The Full Model
    lm.model2.parsimonious <- lm(meanW ~ jvm + mode + 
                                   category + catat + catmb + catml + catmi +
                                   l1dlmr + 
                                   l2dlmps +
                                   pfmr, 
                                 data = data.all.df)
    
    summary(lm.model2.parsimonious)
    
    
    
    # Estimate R-square Change
    lm.anova <- anova(lm.model2.intercept, lm.model2.jre, lm.model2.mode,
                      lm.model2.category, lm.model2.characteristics,
                      lm.model2.l1dlmr, lm.model2.l2dlmr, lm.model2.pfmr)    
    
    summary(lm.anova)
    lm.anova
    
    # Assessing Gauss-Markov Assumptions
    # Normaility of Regression Residuals: Histogram
    data.lm.res <- lm.model2.parsimonious$residuals
    data.lm.res.df <- data.frame(data.lm.res)
    colnames(data.lm.res.df) <- c("residuals")
    
    ggplot(aes(residuals), data = data.lm.res.df) + 
      geom_histogram(aes(y = ..density..), colour= "black", fill= "white") +
      stat_function(fun = dnorm, args = list(mean = mean(data.lm.res.df$residuals), sd = sd(data.lm.res.df$residuals))) +
      xlab("Residuals") +
      ylab("Frequency") 
    
    
    # Linearity: Plot of the Observed vs Predicted
    data.lm.predicted <- lm.model2.parsimonious$fitted.values
    data.lm.observed <- data.all.df$meanW
    data.lm.mode <- data.all.df$mode
    
    data.lm.df <- data.frame(data.lm.mode, data.lm.observed, data.lm.predicted)
    colnames(data.lm.df) <- c("mode", "observed", "predicted")
    lm.fit <- lm(observed ~ predicted, data = data.lm.df)
    
    ggplot(data = data.lm.df, aes(x = predicted, y = observed, fill = mode)) +
      geom_point(color = "blue") +
      geom_abline(intercept = lm.fit$coefficients[[1]], slope = lm.fit$coefficients[[2]], color = "yellow") +
      xlab("Predicted") +
      ylab("Observed") +
      theme(legend.position="none")
    
    
    # Heteroskedasticity
    data.lm.df <- cbind(data.lm.df, data.lm.res)
    colnames(data.lm.df) <- c("mode", "observed", "predicted", "residuals")
    
    lm.hetero <- lm(residuals ~ predicted, data = data.lm.df)
    
    ggplot(data = data.lm.df, aes(x = predicted, y = residuals, color = mode)) +
      geom_point() +
      geom_abline(intercept = lm.hetero$coefficients[[1]], slope = lm.hetero$coefficients[[2]], color = "yellow") +
      xlab("Predicted") +
      ylab("Residuals") +
      ylim(c(-0.7, 0.7)) +
      theme(legend.position="none")
    
    # Breusch-Pagan Test
    bptest(lm.model2.parsimonious)
    
    
    # Multicolinearity
    library(ISLR)
    vif(lm.model2.parsimonious)
    
    
    # Testing for Autocorrelation
    durbinWatsonTest(lm.model2.parsimonious)
    
    # lag Autocorrelation D-W Statistic p-value
    # 1      0.02244216      1.954621   0.084
    # Alternative hypothesis: rho != 0
    
    
    # Assessing Goodness-of-fit 
    logLik(lm.model2.parsimonious)
    mae(actual = data.lm.df$observed, predicted = data.lm.df$predicted)
    


