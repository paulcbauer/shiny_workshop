library(haven)
library(dplyr)
library(sf)
library(smoothr)
library(rnaturalearthdata)

ess <- read_dta("data/raw/ESS9e03_2-subset.dta")

ess <- ess %>%
  select(
    idno,
    country = cntry,
    internet_use = netusoft,
    trust_parliament = trstprl,
    trust_legal = trstlgl,
    trust_police = trstplc,
    trust_politicians = trstplt,
    trust_parties = trstprt,
    trust_eu = trstep,
    trust_un = trstun,
    left_right = lrscale,
    happiness = happy,
    age = agea,
    income_feeling = hincfel
  )

countries <- countries50["iso_a2"]

ess_geo <- ess %>%
  group_by(country) %>%
  summarise(across(everything(), .fns = ~mean(.x, na.rm = TRUE))) %>%
  left_join(countries, by = c("country" = "iso_a2")) %>%
  st_as_sf() %>%
  drop_crumbs(1e+10)

saveRDS(ess, "data/ess_trust.rds")
saveRDS(ess_geo, "data/ess_trust_geo.rds")