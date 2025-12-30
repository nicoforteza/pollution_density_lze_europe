library(tidyverse)
library(fixest)
library(arrow)
library(countrycode)
library(sf)
library(modelsummary)
library(kableExtra)
library(knitr)
library(sf)
library(gdata)

# data loading ######################## 

source("config.R")

# drop water cells ####
df = read_parquet("data/within/lez.pqt.gzip")

codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[3] = "United Kingdom"
# paises[32] = "United Kingdom"

df = right_join(df, tibble(country_id=codes, country=paises), 
                by="country_id") %>% 
  mutate(country=as.factor(country))

df = df %>% 
  mutate(log_pol = log(pol + 0.00001), 
         log_pop = log(pop + 0.00001),
         log_lez_d = log(km_to_lez + 0.000001),
         lez_0_1 = as.integer(km_to_lez > 0 & km_to_lez <= 1),
         lez_1_3 = as.integer(km_to_lez > 1 & km_to_lez <= 3),
         lez_3_5 = as.integer(km_to_lez > 3 & km_to_lez <= 5),
         lez_not_spill = as.integer(km_to_lez > 5),
         post = case_when(
           timetotreat > 0 ~ 1,
           T ~ 0
         )
  ) %>% 
  rename(
    mean_prec=prec,
    mean_temp=temp,
    mean_wind=wind
  )

gc()


# other characteristics ####
# quartile of area, quartile of area w.r.t. cities with lez, lez area quartile

# df %>% select(id_city, area) %>% distinct() %>% drop_na() %>% 

lez_size_rel = df %>% 
  filter((id_city > 0 | id_lez > 0) & year == 2020 & is_city == 1) %>% 
  group_by(id_lez, id_city) %>% 
  summarise(
    area=n()
  ) %>% 
  mutate(
    is_lez=ifelse(id_lez > 0, 1, 0)
  ) %>% 
  group_by(id_city, is_lez) %>% 
  summarise(
    lez_size=sum(area)
  ) %>% 
  pivot_wider(
    names_from = is_lez,
    values_from = lez_size
  ) %>% 
  mutate(
    `1`=replace_na(`1`, 0),
    total_size = `1` + `0`,
    lez_size_rel=`1` / total_size
  ) %>% 
  filter(lez_size_rel > 0)

quantiles <- quantile(lez_size_rel$lez_size_rel, probs = c(0.25, 0.5, 0.75, 1))

lez_size_rel <- lez_size_rel %>%
  mutate(
    lez_size_rel = case_when(
      lez_size_rel < quantiles[1] ~ "Q1",
      lez_size_rel < quantiles[2] ~ "Q2",
      lez_size_rel < quantiles[3] ~ "Q3",
      TRUE ~ "Q4" 
    )
  ) %>% 
  select(id_city, lez_size_rel)

df %>% 
  filter(year == 2020 & id_city > 0) %>% 
  group_by(id_city) %>% 
  summarise(area=n()) %>% 
  arrange(desc(area))

area_quart = df %>%# select(id_city, area) %>% distinct() %>% drop_na() %>% 
  filter(year == 2020 & id_city > 0 & id_city %in% lez_size_rel$id_city) %>% 
  group_by(id_city) %>% 
  summarise(area=n()) %>% 
  arrange(desc(area)) %>% 
  mutate(
    area_quart=case_when(
      area < as.numeric(quantile(area, 0.25)) ~ "Q1",
      as.numeric(quantile(area, 0.25)) <= area & area < as.numeric(quantile(area, 0.5)) ~ "Q2",
      as.numeric(quantile(area, 0.5)) <= area & area < as.numeric(quantile(area, 0.75)) ~ "Q3",
      as.numeric(quantile(area, 0.75)) <= area & area <= as.numeric(quantile(area, 1)) ~ "Q4"
    ) 
  ) %>% 
  select(id_city, area_quart)

data_counts <- df %>%
  filter(year == 2020 & id_lez > 0) %>%
  group_by(id_lez) %>%
  count() %>%
  rename(ncount = n)

quantiles <- quantile(data_counts$ncount, probs = c(0.25, 0.5, 0.75, 1))

lez_size_quart <- data_counts %>%
  mutate(
    lez_size_quart = case_when(
      ncount < quantiles[1] ~ "Q1",
      ncount < quantiles[2] ~ "Q2",
      ncount < quantiles[3] ~ "Q3",
      TRUE ~ "Q4" 
    )
  ) %>% 
  select(id_lez_5km=id_lez, lez_area=ncount, lez_size_quart)

df = df %>% left_join(
  area_quart
) %>% 
  left_join(
    lez_size_rel
  )

df = df %>% left_join(
  lez_size_quart
) %>% 
  mutate(
    area_quart=replace_na(area_quart, "Outside City"),
    lez_size_rel=replace_na(lez_size_rel, "Outside City")
  ) %>% 
  left_join(
    df %>% select(city_name, id_lez_5km) %>% distinct() %>% group_by(city_name) %>% summarise(n_lezs=n())
  )

df = df %>% left_join(
  df %>% select(city_name, id_lez_5km) %>% distinct() %>% group_by(city_name) %>% summarise(n_lezs=n())
)

keep(df, theme_aux, sure=T)

gc()

