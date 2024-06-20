library(haven)
library(readr)
library(dplyr)
library(stringr)

rm(list = ls())

# Load datasets, subset and generate objects
datasets <- dir(
  "data/raw/",
  pattern = "ESS[0-9]e[0-9][0-9]_[0-9].dta|ESS[0-9][0-9].dta",
  full.names = TRUE
)
datasets <- datasets[c(2:10, 1)]
years <- 2002 + seq(0, 18, 2) # adapt

for (i in seq_along(years)) {
  data <- read_dta(datasets[i])
  data <- data %>%
    select(
      essround, idno, cntry, trstprl, trstlgl, 
      trstplc, trstplt, trstep, trstun, agea, 
      eduyrs, eisced, hincfel, lrscale, mnactic, gndr,
      dweight, pweight, pspwght
    )
  
  # Education, income, employment, age
  data <- data %>%
    dplyr::rename(
      trust_parliament = trstprl, 
      trust_legalsystem = trstlgl, 
      trust_police = trstplc, 
      trust_politicians = trstplt, 
      trust_euparliament = trstep, 
      trust_un = trstun,
      age = agea,
      hincome_feeling = hincfel,
      leftright = lrscale,
      gender = gndr,
      mainact = mnactic
    )
  
  # names(data) <- paste(names(data), "_", str_extract(str_replace(i, "\\./", ""), "^.{4}"), sep="")
  var_name <- paste(
    str_extract(str_replace(datasets[i], "\\./raw_data/", ""), "^.{4}"),
    "_",
    years[i],
    sep = ""
  )
  assign(var_name, data.frame(data))
  cat(i, "\n")
}
rm(data, datasets, i)


# Create longformat
lst2 <- lst <- do.call(list, lapply(ls(pattern = "data"), get))

# Generate long format dataset
for (i in 1:10) {
  lst2[[i]] <- lst2[[i]] %>% zap_labels() %>% mutate(year = years[i])
}
data <- as_tibble(bind_rows(lst2))

write_rds(data, "data/ess_trust_ts.rds")