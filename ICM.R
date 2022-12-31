library(dplyr)
library(epidemia)
library(rstanarm)
data("EuropeCovid")
source("Israel_SEIR_predictions.R")

israel = as.data.frame(israel_pred_df())
israel = israel%>%mutate(country = "Israel")
data <- EuropeCovid$data
colnames(data)

rt <- epirt(formula = R(country, date) ~ 0 + date,
            prior = rstanarm::normal(log(2),0.1))

inf <- epiinf(gen = EuropeCovid$si, seed_days = 6L,
              prior_seeds = rstanarm::exponential(rate = 1))

#rate = avg nnumber of daily cases of 1st 6 days
#check 1/mean or mean

deaths <- epiobs(formula = deaths(country, date) ~ , prior_intercept = rstanarm::normal(location = 0.08, scale = 0.05),
                 prior_aux = rstanarm::normal(location = 10, scale = 2), link = "identity", i2o = EuropeCovid$inf2death)
                

head(data)

options(mc.cores = parallel::detectCores())

fm <- epim(rt = rt, inf = inf, obs = deaths, data = israel,
           group_subset = "Israel", algorithm = "sampling", iter = 1e3,
           seed = 12345, refresh = 0)

plot_obs(fm, type = "deaths")
