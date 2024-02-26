# Attempting to fit a Richards model to a dengue epidemic curve
library(tidyverse)
library(brms)


# Data prep ---------------------------------------------------------------

# Rio de Janeiro geocode: 3304557 (Código de 7 dígitos do IBGE)
geocode = 3304557

# Infodengue API code to download data from a geocode, all epiweeks from 2015 to 2024
url <- paste0("https://info.dengue.mat.br/api/alertcity?",
              "geocode=", geocode, "&disease=dengue&format=csv&ew_start=1&",
              "ew_end=53&ey_start=2015&ey_end=2024")

# getting data
dadosRio <- read_csv(file = url) 

dadosRio %>% 
  mutate(eyear = epiyear(data_iniSE),
         eweek = epiweek(data_iniSE)) %>% 
  group_by(eyear) %>% 
  summarise(
    casos = sum(casos)
  )

dadosRio.ag <- dadosRio %>% 
  mutate(eyear = epiyear(data_iniSE),
         eweek = epiweek(data_iniSE)) %>% 
  group_by(eyear) %>% arrange(eweek, .by_group = T) %>% 
  transmute(
    eyear,
    eweek,
    casos = casos,
    casosc = casos_est,
    cum_cases = cumsum(casos),
    cum_cases2 = cumsum(casos_est),
    cum_cases2 = ifelse(casos == casosc, NA, cum_cases2)) 

dadosRio.ag %>% 
  # filter(eweek < 41) %>% 
  ggplot() + 
  geom_line(aes(x = eweek, y = cum_cases, color = "Observed")) + 
  geom_line(aes(x = eweek, y = cum_cases2, color = "Estimated")) + 
  facet_wrap(~eyear, scales = "free_y") + 
  theme_bw() + 
  



# Modelling attempts-------------------------------------------------------

nlform <- bf(cum_cases ~ K * (1 + exp(- r(eweek - eweekpeak)^(1/a))),
             K ~ 1 + (1|AY), 
             r ~ 1, 
             eweekpeak ~ 1,
             a ~ 1,
             nl = TRUE)

nlprior <- c(prior(normal(5000, 1000), nlpar = "K"),
             prior(normal(1, 2), nlpar = "omega"),
             prior(normal(45, 10), nlpar = "theta"))

