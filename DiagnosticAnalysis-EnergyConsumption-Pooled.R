library(car)
library(lmtest)

#-------------------------------------
# Assessment of Linearity
#-------------------------------------
# Raw energy model
m4.raw <- lm(energyWh ~ jvm + gcc + mode + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr,
             data = data.all.df)

# Log energy model
m4.log <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr,
             data = data.all.df)

par(mfrow=c(1,2))

par(mfrow=c(1,1))

plot(m4.raw$fitted.values,
     resid(m4.raw),
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0, lty=2)

plot(m4.log$fitted.values,
     resid(m4.log),
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0, lty=2)


crPlots(m4.raw)
crPlots(m4.log)

cor.test(data.all.df$energyWh,
         data.all.df$rtp,
         method="pearson")

summary(m4.raw)$r.squared
summary(m4.log)$r.squared

resettest(m4.raw)
resettest(m4.log)




# Residual Normality
#par(mfrow=c(1,2))

hist(resid(m4),
     main="Histogram of Residuals",
     xlab="Residuals",
     breaks=40)

qqnorm(resid(m4),
       main="Normal Q-Q Plot")
qqline(resid(m4))


shapiro.test(resid(m4))



# Influential points
cooks <- cooks.distance(m4)
summary(cooks)

threshold <- 4/nrow(data.all.df)
threshold

sum(cooks > threshold)

sum(cooks > threshold)/nrow(data.all.df)*100

influential <- which(cooks > threshold)

length(influential)

table(data.all.df$jvm[influential])
table(data.all.df$mode[influential])
table(data.all.df$category[influential])
table(data.all.df$category[influential], data.all.df$mode[influential])


# Homoskedasticity
plot(m4$fitted.values,
     resid(m4),
     xlab="Fitted values",
     ylab="Residuals",
     main="Residuals vs Fitted")

abline(h=0,lty=2)

library(lmtest)
bptest(m4)

# Multicolinearity
vif(m4)


# -----------------------------
# Sensitivity Analysis Removing Influential Points
# Pooled Energy Consumption
# -----------------------------
m1 <- lm(log(energyWh) ~ jvm + gcc, data = data.all.df)
m2 <- lm(log(energyWh) ~ jvm + gcc + mode + gc, data = data.all.df)
m3 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category, data = data.all.df)
m4 <- lm(log(energyWh) ~ jvm + gcc + mode + gc + category +
           bmr + l1dlmr + l2dlmr + pfmr, data = data.all.df)

cd <- cooks.distance(m4)
influential <- which(cd > (4/nrow(data.pooled)))

m4.sensitivity <- lm(
  log(energyWh) ~ jvm + gcc + mode + gc + category + bmr + l1dlmr + l2dlmr + pfmr,
  data = data.pooled[-influential,]
)

summary(m4.sensitivity)


table(data.pooled[influential, ]$jvm)
table(data.pooled[influential, ]$mode)
table(data.pooled[influential, ]$category)
table(data.pooled[influential, ]$gc)

table(
  data.pooled[influential, ]$category,
  data.pooled[influential, ]$mode
)
