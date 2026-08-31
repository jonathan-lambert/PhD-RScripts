library(car)
library(lmtest)

#-------------------------------------
# Assessment of Linearity
#-------------------------------------
# Raw energy model(Interpretive)
m4.raw <- lm(energyWh ~ jvm + gcc + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr,
             data = data.hs)

# Log energy model
m4.log <- lm(log(energyWh) ~ jvm + gcc + gc + category +
               bmr + l1dlmr + l2dlmr + pfmr,
             data = data.hs)

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

hist(resid(m4.log),
     main="Histogram of Residuals",
     xlab="Residuals",
     breaks=40)

qqnorm(resid(m4.log),
       main="Normal Q-Q Plot")
qqline(resid(m4.log))


shapiro.test(resid(m4.log))

summary(resid(m4.log))


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
bptest(m4.log)

# Multicolinearity
vif(m4.log)


# -----------------------------
# Sensitivity Analysis Removing Influential Points
# Pooled Energy Consumption
# -----------------------------
m1 <- lm(log(energyWh) ~ jvm + gcc, data = data.hs)
m2 <- lm(log(energyWh) ~ jvm + gcc + gc, data = data.hs)
m3 <- lm(log(energyWh) ~ jvm + gcc + gc + category, data = data.hs)
m4 <- lm(log(energyWh) ~ jvm + gcc + gc + category +
           bmr + l1dlmr + l2dlmr + pfmr, data = data.hs)

cd <- cooks.distance(m4)
influential <- which(cd > (4/nrow(data.hs)))

m4.sensitivity <- lm(
  log(energyWh) ~ jvm + gcc + gc + category + bmr + l1dlmr + l2dlmr + pfmr,
  data = data.hs[-influential,]
)

summary(m4.sensitivity)


table(data.hs[influential, ]$jvm)
table(data.hs[influential, ]$mode)
table(data.hs[influential, ]$category)
table(data.hs[influential, ]$gc)

table(
  data.hs[influential, ]$jvm,
  data.hs[influential, ]$category
)
