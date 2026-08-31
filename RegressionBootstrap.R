# Analysing working set size 




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
                    "movie-lens", "mnemonics", "par-mnemonics")
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


# Syncronisation


# Reorder the columns to bring category factors up front
data.all.df <- data.all.df[,c(1:6, 53:60, 7:52)]




########################################################################
####################### Working Set Size Analysis ######################
########################################################################

data.workingset.df <- data.all.df[which(data.all.df$benchmark %in% c("chi-square", "dec-tree", "gauss-mix",
                                                                     "logistic-regression", "movie-lens", "naive-bayes")),]

data.workingset.df <- data.all.df[which(data.all.df$benchmark %in% c("chi-square", "dec-tree", "gauss-mix",
                                                                     "logistic-regression", "movie-lens")),]

data.workingset.df$ws <- ifelse(data.workingset.df$benchmark == "chi-square", 33,
                       ifelse(data.workingset.df$benchmark == "dec-tree", 10.5,
                       ifelse(data.workingset.df$benchmark == "gauss-mix", 0.6,
                       ifelse(data.workingset.df$benchmark == "logistic-regression", 41.9,
                       ifelse(data.workingset.df$benchmark == "movie-lens", 0.458, 0)))))
                       ifelse(data.workingset.df$benchmark == "naive-bayes", 837.9, 0))))))

data.int.df <- data.workingset.df[which(data.workingset.df$mode == "INT"),]
data.hs.df <- data.workingset.df[which(data.workingset.df$mode == "HS"),]


# INT
plot(data.int.df$ws, data.int.df$bmr)
cor.test(data.int.df$ws, data.int.df$bmr)

plot(data.int.df$ws, data.int.df$l1dlmr)
cor.test(data.int.df$ws, data.int.df$l1dlmr)

plot(data.int.df$ws, data.int.df$l2dlmr)
cor.test(data.int.df$ws, data.int.df$l2dlmr)

plot(data.int.df$ws, data.int.df$pfmr)
cor.test(data.int.df$ws, data.int.df$pfmr)


# HS
plot(data.hs.df$ws, data.hs.df$bmr)
cor.test(data.hs.df$ws, data.hs.df$bmr)

plot(data.hs.df$ws, data.hs.df$l1dlmr)
cor.test(data.hs.df$ws, data.hs.df$l1dlmr)

plot(data.hs.df$ws, data.hs.df$l2dlmr)
cor.test(data.hs.df$ws, data.hs.df$l2dlmr)

plot(data.hs.df$ws, data.hs.df$pfmr)
cor.test(data.hs.df$ws, data.hs.df$pfmr)


# Lets Agregate across all levels
data.all.agg.df <- aggregate(x = data.all.df, 
                              by = list(data.all.df$benchmark),
                              FUN = mean,
                              na.rm = TRUE)

data.all.agg.df

cn <- colnames(data.all.agg.df)
agg.cn <- cn[c(6, 16:61)]
data.all.agg.df <- data.all.agg.df[, c(1, 16:61)]
colnames(data.all.agg.df) <- agg.cn

data.agg.all.df$category <- ifelse(data.agg.all.df$benchmark %in% apache, "apache",
                            ifelse(data.agg.all.df$benchmark %in% concurrency, "concurrency",
                            ifelse(data.agg.all.df$benchmark %in% database, "database",
                            ifelse(data.agg.all.df$benchmark %in% functional, "functional",
                            ifelse(data.agg.all.df$benchmark %in% scala, "scala",
                            ifelse(data.agg.all.df$benchmark %in% web, "web", "other"))))))


invoke.dynamic <- c("scrabble", "neo4j-analytics", "dotty", "future-genetic", 
                    "rx-scrabble", "db-shootout", "finagle-chirper", "finagle-http",
                    "movie-lens", "mnemonics", "par-mnemonics")

data.agg.all.df$catid <- ifelse(data.agg.all.df$benchmark %in% invoke.dynamic, "id", "nid")
data.agg.all.df$catid <- as.factor(data.agg.all.df$catid)








# Consider differences between Parallel and Serial Collectors
data.all.df$ParallelGC <- ifelse(data.all.df$gc %in% c("CGC", "CPGC", "G1GC"), "parallel", "not-parrallel")
data.all.df$ParallelGC <- as.factor(data.all.df$ParallelGC)




# Difference between serial and parallel collector. Both have noi concurrency.
data.all.df.spgc <- data.all.df[which(data.all.df$gc %in% c("SGC", "PGC")),]
t.test(meanW ~ gc, data = data.all.df.spgc)
Desc(meanW ~ gc, data = data.all.df.spgc)








paper.bench <- c("akka-uct", "chi-square", "db-shootout", "fj-kmeans", "future-genetic",
                 "gauss-mix", "log-regression", "mnemonics", "movie-lens", "naive-bayes",
                 "neo4j-analytics", "page-rank", "par-mnemonics", "philosophers", "reactors",
                 "rx-scrabble", "scala-doku", "scala-kmeans" "scala-stm-bench7", "scrabble")


