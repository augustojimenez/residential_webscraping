# To do:
## Usar KNN para crear una variable de "clase socioeconomica" para clasificar los sectores

library(robustbase)
library(tidyverse)
library(lmtest)
library(e1071)
library(MASS)

trn <- training %>%
  select(price.usd, area, neighborhood, status, parking, bedrooms, gimnasio, lobby, seller) %>%
  na.omit()
summary(trn$seller)
trn <- trn %>%
  filter(seller == "CityMax Santo Domingo")

summary(trn)

nonOutliers <- adjOutlyingness(trn[, c("area", "price.usd")])$nonOut
trn <- trn[nonOutliers, ]

nbh <- unique(trn$neighborhood)
summary(trn)
summary(trn$seller)

par(mfrow = c(1, 1))
plot(trn$area, trn$price.usd)

# Viewing how prices changes as the amount of parking spots increases
ggplot(trn, aes(x = factor(parking), y = price.usd, colour = factor(parking))) +
  geom_boxplot()

# It seems as if more than 4 are one category on itself
trn$prk <- ifelse(trn$parking >= 4, "4 or more", trn$parking) %>%
  factor(ordered = is.ordered(.))
summary(trn$prk)

ggplot(trn, aes(x = prk, y = price.usd, colour = prk)) +
  geom_boxplot()

# Viewing the effect of status
ggplot(trn, aes(x = , status, y = price.usd, colour = status)) + geom_boxplot()

trn <- filter(trn, status != "Fideicomiso")

summary(trn$status)

trn$status <- factor(trn$status,
                     levels = c("En Planos", "En Construcción","Nueva",
                                "Remodelada", "Segundo Uso"))
summary(trn$status)

ggplot(trn, aes(x = status, y = price.usd, colour = status)) + geom_boxplot()

# Viewing gimnasio
t.test(price.usd ~ gimnasio, data = trn)

ggplot(trn, aes(y = price.usd, colour = gimnasio)) + geom_boxplot()

# Viewing bedrooms
ggplot(trn, aes(x = factor(bedrooms), y = price.usd, colour = factor(bedrooms))) +
  geom_boxplot()

trn$bdrm <- ifelse(trn$bedrooms >= 4, "4 or more", trn$bedrooms) %>%
  factor(ordered = is.ordered(.))
summary(trn$bdrm)

ggplot(trn, aes(x = bdrm, y = price.usd, colour = bdrm)) + geom_boxplot()

b_price <- MASS::boxcox(lm(price.usd ~ 1, data = trn))
lambda_price <- b_price$x[which.max(b_price$y)]
new_price <- (trn$price.usd ^ lambda_price - 1) / lambda_price
shapiro.test(new_price)
hist(new_price)

b_area <- MASS::boxcox(lm(area ~ 1, data = trn))
lambda_area <- b_price$x[which.max(b_price$y)]
new_area <- (trn$area ^ lambda_area - 1) / lambda_area
shapiro.test(new_area)
hist(new_area)
hist(trn$area)

fit.2 <- lm(new_price ~ new_area)
summary(fit.2)
gvlma(fit.2)

fost <- lm(log(price.usd) ~ log(area), data = trn)
fost <- lm(price.usd ~ area, data = trn)
gvlma(fost)
summary(fost)
par(mfrow = c(2, 2))
plot(fost)
shapiro.test(residuals(fost))

form <- formula("log(price.usd) ~ log(area)")
form <- formula("log(price.usd) ~ log(area) * neighborhood")
form <- formula("price.usd ~ area")
form <- formula("log(price.usd) ~ log(area) + prk + gimnasio + bdrm + status")
form <- formula("price.usd ~ area + prk + bdrm + status + gimnasio") # This one

model.1 <- lm(price.usd ~ area * prk + neighborhood, data = trn)

wts <- 1/fitted(lm(abs(residuals(model.1)) ~ fitted(model.1))) ^ 2
wts_2 <- abs(1/fitted(lm(abs(residuals(model.1)) ^ 2 ~ fitted(model.1))))

model.2 <- lm(price.usd ~ area * prk + neighborhood, data = trn, weights = wts)
model.3 <- lm(price.usd ~ area * prk + neighborhood, data = trn, weights = wts_2)

summary(model.1)
summary(model.2)
summary(model.3)
gvlma(model.1)
gvlma(model.2)
gvlma(model.3)

plot(model.3)

model.1 <- lm(form, data = trn[-c(74, 75), ])

wts <- 1/fitted(lm(abs(residuals(model.1)) ~ fitted(model.1))) ^ 2
wts_2 <- abs(1/fitted(lm(abs(residuals(model.1)) ^ 2 ~ fitted(model.1))))

model.2 <- lm(form, data = trn[-c(74, 75), ], weights = wts)
model.3 <- lm(form, data = trn[-c(74, 75), ], weights = wts_2)

fit <- lm(form, data = trn)
fit.1 <- lm(form, data = trn, weights = 1/area)
fit.2 <- lm(form, data = trn, weights = 1/area^2)
fit.3 <- lm(form, data = trn, weights = 1/abs(fitted(fit)))
fit.4 <- lm(form, data = trn, weights = 1/fitted(fit)^2)
fit.5 <- lm(form, data = trn, weights = 1/resid(fit)^2)
fit.6 <- lm(form, data = trn, weights = 1/abs(resid(fit)))

library(stargazer)
stargazer(fit, fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, type = "text")
resid_auxpanel(residuals = sqrt(1/fitted(fit)) * resid(fit.3),
               predicted = fitted(fit.3),
               plots = c("resid", "index"))
gvlma(fit)
gvlma(fit.1)
gvlma(fit.2)
gvlma(fit.3)
gvlma(fit.4)
gvlma(fit.5)
gvlma(fit.6)

with(trn, plot(area, price.usd))
abline(fit)
plot(fit)

summary(fit.1)
summary(model.2)
summary(fit.3)
# Cuando se hace una regresion normal (no log-log), existe presencia de pletykurtosis
model.1 <- lm(log(price.usd) ~ log(area), data = trn[-c(74, 75), ])

plot(log(trn$area), log(trn$price.usd))
abline(model.1, col = "red")
summary(model.1)

plot(fitted(model.1), residuals(model.1))

wts <- 1/fitted(lm(abs(residuals(model.1)) ~ fitted(model.1))) ^ 2
wts_2 <- abs(1/fitted(lm(abs(residuals(model.1)) ^ 2 ~ fitted(model.1))))

model.2 <- lm(log(price.usd) ~ log(area), data = trn[-1, ], weights = wts)

model.3 <- lm(log(price.usd) ~ log(area), data = trn[-1, ], weights = wts_2)



plot(log(trn$area), log(trn$price.usd))
abline(model.1, col = "red")
abline(model.2, col = "blue")
abline(model.3, col = "green")
summary(model.1)
summary(model.2)
summary(model.3)
plot(fitted(model.1), residuals(model.1))
plot(fitted(model.2), residuals(model.2))
plot(fitted(model.3), residuals(model.3))

# Distribucion no normal
shapiro.test(residuals(model.3))

# Presencia de heterocedasticidad
bptest(model.3)

# test de autocorrelacion en los errores
dwtest(model.1)

# Mesokurtic distribution
kurtosis(residuals(model.3))
