library(splines)
library(modelr)
library(gvlma)

training <- training %>%
  mutate(area = ifelse(area > 100000, area / 1000, area),
         area_per_br = area / bedrooms,
         price_per_m2 = price.usd / area) %>%
  filter(usage == "Residencial",
         price.usd > 10000,
         price_per_m2 > 200,
         price_per_m2 < 6000,
         neighborhood == "Naco",
         price.usd < 900000,
         area > 50)

mod1 <- lm(price.usd ~ ns(area, 1), data = training)
mod2 <- lm(price.usd ~ ns(area, 2), data = training)
mod3 <- lm(price.usd ~ ns(area, 3), data = training)
mod4 <- lm(price.usd ~ ns(area, 4), data = training)
mod5 <- lm(price.usd ~ ns(area, 5), data = training)

grid <- training %>% 
  data_grid(area = seq_range(area, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "price.usd")

ggplot(training, aes(area, price.usd)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

mod2 <- lm(log(price.usd) ~ log(area) + I(log(area) ^ 2), data = training)
gvlma(mod2)
hist(residuals(mod2))
summary(residuals(mod2))
summary(mod2)
which.max(resid(mod2))
which.min(resid(mod2))
with(training, plot(log(area), log(price.usd)))
abline(lm(log(price.usd) ~ log(area), data = training))
mod2 <- lm(log(price.usd) ~ log(area) * status, data = training)
gvlma(mod2)
hist(resid(mod2))
which.min(resid(mod2))
