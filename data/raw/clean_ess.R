library(dplyr)
library(sf)
library(smoothr)
library(rnaturalearthdata)

ess <- read.csv("data/raw/ESS10-subset.csv")

ess <- ess %>%
  as_tibble() %>%
  select(idno, cntry, nwspol, netusoft, netustm, ppltrst, pplfair, pplhlp) %>%
  mutate(
    nwspol = if_else(nwspol %in% c(7777, 8888, 9999), NA, nwspol),
    netusoft = factor(case_match(
      if_else(netusoft %in% c(7, 8, 9), NA, netusoft),
      1 ~ "Never",
      2 ~ "Only occasionally",
      3 ~ "A few times a week",
      4 ~ "Most days",
      5 ~ "Every day"
    )),
    netustm = if_else(netustm %in% c(6666, 7777, 8888, 9999), NA, netustm),
    ppltrst = if_else(ppltrst %in% c(77, 88, 99), NA, ppltrst),
    pplfair = if_else(pplfair %in% c(77, 88, 99), NA, pplfair),
    pplhlp = if_else(pplhlp %in% c(77, 88, 99), NA, pplhlp),
    
    # make ordinal variables
    netusoft = ordered(
      netusoft,
      levels = c(
        "Never",
        "Only occasionally",
        "A few times a week",
        "Most days",
        "Every day"
      )
    ),
    ppltrst = ordered(
      ppltrst,
      levels = as.character(0:10)
    ),
    pplfair = ordered(
      pplfair,
      levels = as.character(0:10)
    ),
    pplhlp = ordered(
      pplhlp,
      levels = as.character(0:10)
    )
  )

saveRDS(ess, "data/ess_clean.rds")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

countries <- countries50["iso_a2"]

ess_geo <- ess %>%
  group_by(cntry) %>%
  summarise(
    nwspol = mean(nwspol, na.rm = TRUE),
    netusoft = Mode(netusoft),
    netustm = mean(netustm, na.rm = TRUE),
    ppltrst = quantile(ppltrst, 0.5, type = 1, na.rm = TRUE),
    pplfair = quantile(pplfair, 0.5, type = 1, na.rm = TRUE),
    pplhlp = quantile(pplhlp, 0.5, type = 1, na.rm = TRUE)
  ) %>%
  left_join(countries, by = c("cntry" = "iso_a2")) %>%
  st_as_sf() %>%
  drop_crumbs(1e+09)

saveRDS(ess_geo, "data/ess_geo.rds")