# Non-linear function on brms. Example 3 from Bürkner (2018)
# https://journal.r-project.org/archive/2018/RJ-2018-017/index.html
library(brms)


url <- paste0("https://raw.githubusercontent.com/mages/",
              "diesunddas/master/Data/ClarkTriangle.csv")
loss <- read.csv(url)
head(loss)

nlform <- bf(cum ~ ult * (1 - exp(-(dev / theta)^omega)),
             ult ~ 1 + (1|AY), 
             omega ~ 1, 
             theta ~ 1, 
             nl = TRUE)

nlprior <- c(prior(normal(5000, 1000), nlpar = "ult"),
             prior(normal(1, 2), nlpar = "omega"),
             prior(normal(45, 10), nlpar = "theta"))

fit_loss1 <- brm(formula = nlform, data = loss, family = gaussian(),
                 prior = nlprior, control = list(adapt_delta = 0.9))


summary(fit_loss1)

conditional_effects(fit_loss1)

conditions <- data.frame(AY = unique(loss$AY))
rownames(conditions) <- unique(loss$AY)
me_year <- conditional_effects(fit_loss1, conditions = conditions,
                            re_formula = NULL, method = "predict")
plot(me_year, ncol = 5, points = TRUE)
