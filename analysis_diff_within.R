setwd("/Users/nicoforteza/Desktop/tfm")
library(tidyverse)
library(fixest)
library(arrow)
library(countrycode)
library(sf)
library(modelsummary)
library(kableExtra)
library(knitr)
library(sf)

# data loading ######################## 

source("config.R")

df = read_parquet("data/within/within_clean_lze.pqt.gzip") %>% 
  select(!c("__index_level_0__")) #%>% 
  # filter(country_id != "MT")

codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[7] = "United Kingdom"
paises[32] = 'Greece'

df = right_join(df, tibble(country_id=codes, country=paises), 
                by="country_id") %>% 
  mutate(country=as.factor(country))

df = df |> 
  st_as_sf(coords = c("long", "lat"), crs="EPSG:3035")
st_crs(df)
df = df %>% st_transform("EPSG:4326")
coords = df %>% 
  st_coordinates() %>% 
  as.tibble() %>% 
  rename(lon=X, lat=Y)
df$lon = coords$lon
df$lat = coords$lat

df = df %>% 
  select(-geometry) %>% 
  mutate(log_pol = log(pol), 
         log_pop=log(pop),
         water_dist = replace(water_dist, water_dist == -1, 0),
         coast_city = replace(coast_city, coast_city == -1, 0),
         lze_0_1 = as.integer(!lze & lze_1km == 1),
         lze_1_3 = as.integer(!lze & lze_3km == 1 & !lze_0_1),
         lze_3_5 = as.integer(!lze & lze_5km == 1 & !lze_0_1 & !lze_1_3),
         lze_5_10 = as.integer(!lze & lze_10km == 1 & !lze_0_1 & !lze_1_3 & !lze_3_5),
         lze_10_25 = as.integer(!lze & lze_25km == 1 & !lze_0_1 & !lze_1_3 & !lze_3_5 & !lze_5_10)
         ) 

treated_ids = df %>% 
  filter(yeartreat != Inf) %>% 
  as_tibble() %>% 
  select(id) %>% 
  unique() %>% 
  arrange(id) %>% 
  deframe()

df_cities = df %>% 
  filter(city==1)

# config ######################## 

setFixest_dict(
  "log_pop"="Log(Pop.)",
  "country-year"="Country-Year",
  "country"="Country",
  "year"="Year",
  "lze"= "LZE",
  "lze_0_1"="LZE between 0-1 km.",
  "lze_1_3"="LZE between 1-3 km.",
  "lze_3_5"="LZE between 3-5 km.",
  "lze_5_10"="LZE between 5-10 km.",
  "lze_10_25"="LZE between 10-25 km.",
)

style = style.tex(
  model.title = "",
  var.title = "",
  fixef.title = "Fixed Effects",
  stats.title = "\\midrule",
  model.format = NULL
)

notes_all_countries = c("Temporal coverage: 2007-2021. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Malta, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland, United Kingdom")

vcov_arg = conley(2, distance='triangular')


# descriptive ######################## 

## tables ######
db = df %>% 
  mutate(country=factor(country)) %>% 
  select(-c(country_id)) %>% 
  mutate(mean_wind=mean_wind * 1000) %>% 
  select(`Population` = pop,
         `PM2.5` = pol,
         `Temperature` = mean_temp,
         `Precipitation` = mean_precip,
         `Wind Speed` = mean_wind,
         `Ruggedness` = mean_ruggedness,
         `Coast City` = coast_city,
         `Water Distance` = water_dist,
         `Power Plant` = pw_dist,
  )

caption_desc_bt = "Descriptive Statistics (with 5km Buffer)"

datasummary(All(db) ~ (Mean + SD),
            data = db, caption=caption_desc_bt,
            sparse_header = TRUE, output='tables/desc_all.tex', notes=notes_all_countries)


db = df %>% 
  mutate(country=factor(country)) %>% 
  select(-c(country_id)) %>% 
  filter(city==1) %>% 
  mutate(mean_wind=mean_wind * 1000) %>% 
  select(`Population` = pop,
         `PM2.5` = pol,
         `Temperature` = mean_temp,
         `Precipitation` = mean_precip,
         `Wind Speed` = mean_wind,
         `Ruggedness` = mean_ruggedness,
         `Coast City` = coast_city,
         `Water Distance` = water_dist,
         `Power Plant` = pw_dist,
  )

caption_desc_bt = "Descriptive Statistics of Cities Cells"

datasummary(All(db) ~ (Mean + SD),
            data = db, caption=caption_desc_bt,
            sparse_header = TRUE, output='tables/desc_cities.tex',  notes=notes_all_countries)

bind_rows(
  df_cities %>% 
    as_tibble() %>% 
    group_by(id) %>% 
    summarise(`Cells`=n()) %>% 
    summarise(
      Min=min(Cells),
      `P20`=quantile(Cells, 0.20),
      Median=median(Cells),
      Mean=mean(Cells), 
      `P75`=quantile(Cells, 0.75),
      `P90`=quantile(Cells, 0.9),
      `P95`=quantile(Cells, 0.95),
      Max=max(Cells),
      `Std.`=sd(Cells)
    ),
  df_cities %>% 
    as_tibble() %>% 
    filter(lze==1) %>% 
    group_by(id) %>% 
    summarise(`Cells`=n()) %>% 
    summarise(
      Min=min(Cells),
      `P20`=quantile(Cells, 0.20),
      Median=median(Cells),
      Mean=mean(Cells), 
      `P75`=quantile(Cells, 0.75),
      `P90`=quantile(Cells, 0.9),
      `P95`=quantile(Cells, 0.95),
      Max=max(Cells),
      `Std.`=sd(Cells)
    )
) %>% 
  kable(format='latex', digits=2, label='tab:cell_desc') %>% 
  save_kable(file="tables/desc_cells.tex")

aux = df_cities %>% 
  filter(id %in% treated_ids) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  select(id, timetotreat, year, yeartreat) %>% 
  unique()

aux1 = aux %>% 
  group_by(id) %>% 
  summarise(
    `Min. Observed Year`=min(timetotreat),
    `Max. Observed Year`=max(timetotreat),
    )

aux2 = aux %>% 
  select(id, yeartreat) %>% 
  unique() %>% 
  arrange(id) %>% 
  rename(`LZE Year`=yeartreat)
sf_use_s2(FALSE)
aux5 = st_read("data/uar/uar_data_new.geojson") %>% 
  st_convex_hull()
st_crs(aux5)
aux5 = aux5 %>% 
  st_transform("EPSG:4326") %>% 
  mutate(lze_area=as.vector(aux5 %>% st_area() / 1e6)) %>% 
  select(city, lze_area, id) %>% 
  rename(city_name=city) %>% 
  as_tibble() %>% 
  select(-geometry)
  

aux6 = st_read("data/cities/2020/clean/cities.geojson") %>% 
  st_transform("EPSG:4326") 
aux6 = aux6 %>% 
  mutate(city_area=as.vector(aux6 %>% st_area() / 1e6)) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  select(id, city_area)

aux = merge(aux5, aux6)

lze_area = aux %>% 
  group_by(id) %>% 
  summarise(lze_area=sum(lze_area))

city_area = aux %>% 
  group_by(id) %>% 
  summarise(city_area=mean(city_area))

by = join_by(id)
auxfin = left_join(
  left_join(
    left_join(aux1, aux2, by=by),
    aux6, by=by
  ),
  aux5, by=by
)  %>% 
  arrange(city_name) %>% 
  select(id, city_name, `LZE Year`, `Min. Observed Year`, `Max. Observed Year`, city_area, lze_area) %>% 
  mutate(
    rel_share=(lze_area/city_area)*100,
    city_name=str_to_title(city_name)
    ) 

good_lezs = auxfin %>% filter(rel_share < 100) %>% select(id) %>% as_vector()

auxfin %>% 
  select(-id) %>% 
  rename(
    `City Area (km2)`=city_area, 
    `City Name`=city_name,
    `LZE Area (km2)`=lze_area,
    `LZE Land Share (%)`=rel_share
    ) %>% 
  as_tibble() %>% 
  kable(format='latex', digits=2, label='tab:lze_desc') %>% 
  save_kable(file="tables/lze_desc.tex")


# regressions ######################## 

## ols ####

ols1 = feols(log_pol ~  log_pop, data=df_cities, vcov = vcov_arg)
ols2 = feols(log_pol ~  log_pop + ..c2, data=df_cities, vcov = vcov_arg)
ols3 = feols(log_pol ~  log_pop + ..c2 | ..fe, data=df_cities, vcov = vcov_arg)

etable(
  ols1, ols2, ols3,
  fitstat=c('n', 'f', 'wh', 'sargan'),
  digits=3,
  # headers = c("Historical Population", "Soil", "Aquifers"),
  title='OLS Estimates',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/ols1.tex",
  label = 'tab:ols1',
  notes=notes_all_countries,
  replace=T
)

## iv ####

iv_01 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..hist, df_cities, vcov = vcov_arg)
iv_02 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..soil, df_cities, vcov = vcov_arg)
iv_03 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..aquif, df_cities, vcov = vcov_arg)

etable(
  iv_01, iv_02, iv_03,
  fitstat=c('n', 'f', 'wh', 'sargan'),
  digits=3,
  headers = c("Historical Population", "Soil", "Aquifers"),
  title='2SLS Estimates',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/iv1.tex",
  label = 'tab:iv1',
  notes=notes_all_countries,
  replace=T
)

# treatment #####

m1 = feols(log_pol ~ lze + ..c2 + log_pop | country + year, vcov=conley(2), data=df)
m2 = feols(log_pol ~ lze + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(city==1))
m3 = feols(log_pol ~ lze + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(id %in% treated_ids))
m4 = feols(log_pol ~ lze + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(id %in% treated_ids & city==1))

etable(
  m1, m2, m3, m4,
  fitstat=c('n'),
  digits=3,
  headers = c("City with 5Km Buffer", "City Cells", "Treated City Cells with 5Km Buffer", "Treated City Cells"),
  title='Treatment Effect Estimates',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant', 'Log(Pop.)'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/te.tex",
  label = 'tab:te',
  notes=notes_all_countries,
  replace=T
)

m1 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=conley(2), data=df)
m2 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(city==1))
m3 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(id %in% treated_ids))
m4 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=conley(2), data=df %>% filter(id %in% treated_ids & city==1))

etable(
  m1, m2, m3, m4,
  fitstat=c('n'),
  digits=3,
  headers = c("City with 5Km Buffer", "City Cells", "Treated City Cells with 5Km Buffer", "Treated City Cells"),
  title='Treatment Effect Estimates with Spatial Spillovers',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant', 'Log(Pop.)'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/te_sp.tex",
  label = 'tab:te_sp',
  notes=notes_all_countries,
  replace=T
)


# playground #######


tc = df %>% 
  st_drop_geometry() %>% 
  filter(id %in% treated_ids & yeartreat >= 2007 & yeartreat <= 2021)

tc %>% 
  select(id, timetotreat) %>% 
  distinct() %>% 
  group_by(timetotreat) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=timetotreat, y=n)) + 
  geom_col()

x = tc %>% 
  group_by(year, city_name) %>% 
  mutate(sumpop = sum(pop)) %>% 
  select(year, city_name, sumpop) %>% 
  distinct() %>% 
  group_by(city_name) %>% 
  mutate(avgpop=mean(sumpop)) %>% 
  select(city_name, avgpop) %>% 
  distinct() 

quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q), q = q)
}

x %>% glimpse()

x %>% 
  group_by(city_name) %>% 
  mutate(pct = percent_rank(avgpop, na.rm = TRUE))

a = x %>% 
  mutate(percentil = case_when(
    quantile(avgpop, 0.25) >= avgpop ~ "A",
    quantile(avgpop, 0.25) <= avgpop ~ "B"
  ))
x
  mutate(
    percentiles = list(quantile(avgpop, probs = c(0.25, 0.5, 0.7, 0.9, 0.95))),
    percentil = case_when(
      avgpop <= percentiles[[1]][1] ~ "0-25",
      avgpop <= percentiles[[1]][2] ~ "25-50",
      avgpop <= percentiles[[1]][3] ~ "50-70",
      avgpop <= percentiles[[1]][4] ~ "70-90",
      avgpop <= percentiles[[1]][5] ~ "90-95",
      TRUE ~ "95-100"
    )
  ) %>%
  select(-percentiles)

tyears = seq(-5, 5, 1)

vector = c("lze", "lze_0_1", "lze_1_3", "lze_3_5", "lze_5_10", "lze_10_25")

tc %>% 
  filter(timetotreat %in% tyears) %>% 
  select(vector, timetotreat, pop, pol) %>% 
  gather("Type", "Implemented", lze:lze_10_25) %>% 
  group_by(Type, Implemented, timetotreat) %>% 
  summarise(avg=weighted.mean(pol, pop)) %>% 
  mutate(
    Implemented=factor(Implemented),
    Type=case_when(
      Type == 'lze' ~ "LZE",
      Type == 'lze_0_1' ~ "1km Spillover",
      Type == 'lze_1_3' ~ "1-3km Spillover",
      Type == 'lze_3_5' ~ "3-5km Spillover",
      Type == 'lze_5_10' ~ "5-10km Spillover",
      Type == 'lze_10_25' ~ "10-25km Spillover",
    )
    ) %>% 
  rename("Affected Cells"=Implemented) %>% 
  ggplot(aes(x=timetotreat, y=avg, color=`Affected Cells`))+
  geom_line() + 
  theme_bw() + 
  geom_vline(xintercept=0, linetype="dashed", alpha=.4) +
  facet_wrap(~factor(Type, 
                     levels=c("LZE", "1km Spillover", "1-3km Spillover", 
                              "3-5km Spillover", "5-10km Spillover", "10-25km Spillover"))) + 
  ylab("PM2.5 Weighted Exposure") +
  xlab("Time to LZE Implementation")+
  ggtitle("All Grid Cells inside Treated Cities")+
  theme(text=element_text(family="Palatino"))

ggsave("figures/trends.png", dpi=300, width=8, height=4)

edata = df %>% 
  st_drop_geometry() %>% 
  filter(id %in% treated_ids & yeartreat >= 2007 & yeartreat <= 2021) %>% 
  filter(timetotreat %in% tyears)

m1 = feols(log_pol ~ i(timetotreat, lze, ref = -1) + ..c2 + lze_0_1 + lze_1_3 + lze_3_5 + lze_5_10 | ..fe, 
      vcov =vcov_arg,
      data = edata)

merge(
  edata, 
  edata %>% 
    group_by(year, city_name) %>% 
    mutate(sumpop = sum(pop)) %>% 
    select(year, city_name, sumpop) %>% 
    distinct() %>% 
    group_by(city_name) %>% 
    mutate(avgpop=mean(sumpop)) %>% 
    select(city_name, avgpop) %>% 
    distinct()
  ) %>% 
  mutate()

edata %>% 
  group_by(year, city_name) %>% 
  mutate(sumpop = sum(pop)) %>% 
  select(year, city_name, sumpop) %>% 
  distinct() %>% 
  group_by(city_name) %>% 
  mutate(avgpop=mean(sumpop)) %>% 
  select(city_name, avgpop) %>% 
  distinct() %>% 
  summary()

edata %>% filter(city_name != '')



allm = feols(log_pol ~ 
              i(timetotreat, lze, ref = -1) + 
              i(timetotreat, lze_0_1, ref = -1) + 
              i(timetotreat, lze_1_3, ref = -1) + 
              i(timetotreat, lze_3_5, ref = -1) +
              i(timetotreat, lze_5_10, ref = -1) +
              ..c2 | ..fe + id, 
      cluster = ~ year ^ country + year + country + id,
      data = edata)

allm %>% 
  broom::tidy() %>% 
  filter(stringr::str_detect(term, "timetotreat::")) |>
  mutate(
    time = stringr::str_extract_all(term, "-?\\d+(?:\\.\\d+)?")) %>% 
  unnest(time) %>%
  mutate(time = as.numeric(time)) %>% 
  select(time, estimate, std.error) %>% 
  add_row(tibble(time = -1, estimate = 0, std.error = 0)) |>
  filter(time < 10) %>% 
  group_by(time) %>% 
  mutate(mest=mean(estimate), msd=mean(std.error)) %>% 
  select(time, mest, msd) %>% 
  distinct() %>% 
  mutate(
    ub = mest + 1.96 * msd,
    lb = mest - 1.96 * msd
  ) %>% 
  ggplot() +
  geom_vline(xintercept = -0.5, color = "grey50") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(x = time, y = mest), color = "#bf616a") +
  geom_errorbar(aes(x = time, ymin = lb, ymax = ub), color = "#bf616a", alpha = 0.8)

m2 = feols(log_pol ~ i(timetotreat, lze_0_1, ref = -1) + ..c2 | ..fe, 
           vcov = vcov_arg, 
           data = edata)
m3 = feols(log_pol ~ i(timetotreat, lze_1_3, ref = -1) + ..c2 | ..fe, 
           vcov = vcov_arg, 
           data = edata)
m4 = feols(log_pol ~ i(timetotreat, lze_3_5, ref = -1) + ..c2 | ..fe, 
           vcov = vcov_arg, 
           data = edata)
m5 = feols(log_pol ~ i(timetotreat, lze_5_10, ref = -1) + ..c2 | ..fe, 
           vcov = vcov_arg, 
           data = edata)
m6 = feols(log_pol ~ i(timetotreat, lze_10_25, ref = -1) + ..c2 |..fe, 
           vcov = vcov_arg, 
           data = edata)

library(ggfixest)
ggiplot(
  list(
    "LZE"=m1,
    "0-1km"=m2, 
    "1-3km"=m3, 
    "3-5km"=m4, 
    "5-10"=m5,
    "10-25"=m6
    )
  )

