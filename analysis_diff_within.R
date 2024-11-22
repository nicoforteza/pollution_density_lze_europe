setwd(dirname(rstudioapi::getSourceEditorContext()$path))
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
  filter(lze==1) %>% 
  select(id) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  deframe()

df_cities = df %>% 
  filter(city==1)

# config ######################## 

setFixest_dict(
  "log_pop"="Log(Pop.)",
  "country-year"="Country-Year",
  "country"="Country",
  "year"="Year",
  "lze"= "LEZ",
  "lze_0_1"="LEZ between 0-1 km.",
  "lze_1_3"="LEZ between 1-3 km.",
  "lze_3_5"="LEZ between 3-5 km.",
  "lze_5_10"="LEZ between 5-10 km."
)

style = style.tex(
  model.title = "",
  var.title = "",
  fixef.title = "Fixed Effects",
  stats.title = "\\midrule",
  model.format = NULL
)

notes_all_countries = c("Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Malta, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland, United Kingdom")

vcov_arg = conley(2, distance='triangular')


# descriptive ######################## 

## cities cells - no buffer ####

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
            data = db, 
            caption=caption_desc_bt,
            sparse_header = TRUE, 
            output='tables/v2/descriptive_within_cities.tex',  
            notes=notes_all_countries)

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
  kable(
    format='latex', 
    digits=2, 
    label='tab:cell_desc') %>% 
  save_kable(
    file="tables/v2/descriptive_within_n_cells.tex"
  )

## heterogeneity table ####

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
  rename(`LEZ Year`=yeartreat)
sf_use_s2(FALSE)
aux5 = st_read("data/uar/uar_data_new_euro.geojson") %>% 
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
  select(id, city_name, `LEZ Year`, `Min. Observed Year`, `Max. Observed Year`, city_area, lze_area) %>% 
  mutate(
    rel_share=(lze_area/city_area)*100,
    city_name=str_to_title(city_name)
    ) %>% 
  rename(
    `City Area (km2)`=city_area, 
    `City Name`=city_name,
    `LEZ Area (km2)`=lze_area,
    `LEZ Land Share (%)`=rel_share
  )

between_sample = read_parquet("data/between/cities_clean.pqt") %>% 
  select(!c("__index_level_0__")) %>% 
  filter(country_id != "MT")

by = join_by(id)

auxfin = left_join(
  auxfin, 
  between_sample %>% filter(year == 2015) %>% select(id, log_exp_w, log_pop_ras, log_dens_exp), 
by=by) %>% 
  mutate(
    `Exposure Quartile`=as.factor(
      case_when(
        log_exp_w < as.numeric(quantile(log_exp_w, 0.25)) ~ "Q1",
        as.numeric(quantile(log_exp_w, 0.25)) <= log_exp_w & log_exp_w < as.numeric(quantile(log_exp_w, 0.5)) ~ "Q2",
        as.numeric(quantile(log_exp_w, 0.5)) <= log_exp_w & log_exp_w < as.numeric(quantile(log_exp_w, 0.75)) ~ "Q3",
        as.numeric(quantile(log_exp_w, 0.75)) <= log_exp_w & log_exp_w <= as.numeric(quantile(log_exp_w, 1)) ~ "Q4"
      )
    ),
    `Density Quartile`=case_when(
        log_pop_ras < as.numeric(quantile(log_pop_ras, 0.25)) ~ "Q1",
        as.numeric(quantile(log_pop_ras, 0.25)) <= log_pop_ras & log_pop_ras < as.numeric(quantile(log_pop_ras, 0.5)) ~ "Q2",
        as.numeric(quantile(log_pop_ras, 0.5)) <= log_pop_ras & log_pop_ras < as.numeric(quantile(log_pop_ras, 0.75)) ~ "Q3",
        as.numeric(quantile(log_pop_ras, 0.75)) <= log_pop_ras & log_pop_ras <= as.numeric(quantile(log_pop_ras, 1)) ~ "Q4"
    ),
    `Exp. Density Quartile`=case_when(
        log_dens_exp < as.numeric(quantile(log_dens_exp, 0.25)) ~ "Q1",
        as.numeric(quantile(log_dens_exp, 0.25)) <= log_dens_exp & log_dens_exp < as.numeric(quantile(log_dens_exp, 0.5)) ~ "Q2",
        as.numeric(quantile(log_dens_exp, 0.5)) <= log_dens_exp & log_dens_exp < as.numeric(quantile(log_dens_exp, 0.75)) ~ "Q3",
        as.numeric(quantile(log_dens_exp, 0.75)) <= log_dens_exp & log_dens_exp <= as.numeric(quantile(log_dens_exp, 1)) ~ "Q4"
    )
  ) %>% 
  select(-log_pop_ras, -log_exp_w, -log_dens_exp) %>% 
  filter(id %in% treated_ids)

aux = st_read("data/uar/uar_data_new_euro.geojson") %>% 
  select(id, type, schedule, contains("EUR")) %>% 
  st_drop_geometry() %>% 
  rename(Type=type, Schedule=schedule)

by = join_by(id)

auxfin = left_join(auxfin, aux %>% distinct()) %>% 
  arrange(desc(id)) %>% 
  mutate(lzesize=`LEZ Land Share (%)`) 

auxfin = auxfin %>% drop_na()

auxfin = auxfin %>% 
  mutate(
    lzesize_decile = case_when(
      lzesize < as.numeric(quantile(lzesize, 0.25)) ~ "Q1",
      as.numeric(quantile(lzesize, 0.25)) <= lzesize & lzesize < as.numeric(quantile(lzesize, 0.5)) ~ "Q2",
      as.numeric(quantile(lzesize, 0.5)) <= lzesize & lzesize < as.numeric(quantile(lzesize, 0.75)) ~ "Q3",
      as.numeric(quantile(lzesize, 0.75)) <= lzesize & lzesize <= as.numeric(quantile(lzesize, 1)) ~ "Q4"
    )
  ) %>% 
  rename(
    `LEZ Size Quartile`=lzesize_decile
  )

table = auxfin

table %>% 
  select(-id) %>% 
  arrange(`City Name`) %>% 
  kable(format='latex', digits=2, label='tab:lze_desc') %>% 
  save_kable(
    file="tables/v2/descriptive_within_lezs.tex"
    )


rm(aux, aux1, aux2, aux5, aux6, by, db, lze_area, between_sample, auxfin)

## visualizations ####

# paris = 281, londres = 318

# one city - lze vs outer 

id_ = 281
city_sample = df %>% 
  filter(id==id_ & city==1)

metadata = table %>% 
  filter(id==as.double(id_)) %>% 
  select(`City Name`, `LEZ Year`)

city_sample %>% 
  group_by(year) %>% 
  summarise(
    avg_pm_lze=mean(pol[lze==1]),
    avg_pm=mean(pol[lze==0]),
    sd_lze = sd(pol[lze==1]),
    sd_no_lze = sd(pol[lze==0])
  )  %>% 
  select(year, starts_with("avg")) %>% 
  st_drop_geometry %>% 
  pivot_longer(!year, names_to = "circle", values_to = "pm") %>% 
  ggplot(aes(x=year, y=pm, color=circle)) + 
  geom_line() +
  ggtitle(paste(metadata$`City Name`, "LZE year:", metadata$`LEZ Year`)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2007, 2022, 1))

# all cities - lze vs outer 

df_tc = df_cities %>% 
  left_join(table) %>% 
  filter(id %in% treated_ids) 


library(wesanderson)

df_tc %>% 
  st_drop_geometry() %>% 
  group_by(year, `Density Quartile`) %>% 
  summarise(
    avg_pm_lze=mean(pol[lze==1]),
    avg_pm_lze_0_1=mean(pol[lze_0_1==0]),
    avg_pm_lze_1_3=mean(pol[lze_1_3==0]),
    avg_pm_lze_3_5=mean(pol[lze_3_5==0]),
    avg_pm_lze_5_10=mean(pol[lze_5_10==0])
    ) %>% 
  drop_na() %>% 
  filter(`Density Quartile`=='Q3') %>% 
  pivot_longer(!any_of(c("year", "Density Quartile")), names_to = "circle", values_to = "pm") %>% 
  ggplot(aes(x=year, y=pm, color=circle)) + 
  geom_line(aes(linetype = `Density Quartile`), linewidth=0.65) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2007, 2022, 1)) +
  facet_wrap(.~`Density Quartile`) + 
  scale_color_manual(values = wes_palette("Darjeeling1", 5, 'discrete'))
  

# regressions ######################## 

## ols ####

# good_lezs = auxfin %>% filter(rel_share < 100) %>% select(id) %>% as_vector()

ols1 = feols(log_pol ~  log_pop, data=df_cities)
ols2 = feols(log_pol ~  log_pop + ..c2, data=df_cities)
ols3 = feols(log_pol ~  log_pop + ..c2 | ..fe, data=df_cities)

etable(ols1, ols2, ols3)

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
  file="tables/v2/within_lez_ols.tex",
  label = 'tab:ols1',
  notes=notes_all_countries,
  replace=T
)

## iv ####

df_cities = df_cities %>% 
  left_join(table)

iv_01 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..hist_between, df_cities, vcov = vcov_arg)
iv_02 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..soil, df_cities, vcov = vcov_arg)
iv_03 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..aquif, df_cities, vcov = vcov_arg)

etable(iv_01, iv_02, iv_03)

etable(
  iv_01, iv_02, iv_03,
  fitstat=c('n', 'f', 'wh', 'sargan'),
  digits=3,
  headers = c("Historical Population", "Soil", "Aquifers"),
  title='IV Estimates For Cities Grid Cells and Spatially Robust Standard Errors',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/v2/within_lez_iv_all_conley.tex",
  label = 'tab:within_lez_iv_all_conley',
  notes=notes_all_countries,
  replace=T
)

iv_01 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..hist_between, df_cities)
iv_02 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..soil, df_cities)
iv_03 = feols(log_pol ~ ..c2 | ..fe | log_pop ~ ..aquif, df_cities)

etable(
  iv_01, iv_02, iv_03,
  fitstat=c('n', 'f', 'wh', 'sargan'),
  digits=3,
  headers = c("Historical Population", "Soil", "Aquifers"),
  title='IV Estimates For Cities Grid Cells',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  file="tables/v2/within_lez_iv_all.tex",
  label = 'tab:within_lez_iv_all',
  notes=notes_all_countries,
  replace=T
)


## treatment ####

# df = df %>% mutate(log_pol=log(pol), log_pop=log(pop))

df = df %>% 
  rename(
    mean_prec=mean_precip,
    sum_hist1800=hist1800,
    sum_hist1500=hist1500,
    sum_hist1000=hist1000,
    sum_hist100=hist100
    )

m1 = feols(log_pol ~ lze + log_pop + ..c2 | country + year, data=df, cluster = ~ country + year)
m2 = feols(log_pol ~ lze + log_pop + ..c2 | country + year, data=df %>% filter(id %in% treated_ids), cluster = ~ country + year)


etable(
  m1, m2,
  fitstat=c('n'),
  digits=3,
  headers = c("All Cities", "Treated Cities"),
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
  file="tables/v2/te_ols.tex",
  label = 'tab:te',
  notes=notes_all_countries,
  replace=T
)

m1 = feols(log_pol ~ lze + log_pop + ..c2 | country + year, data=df, vcov=vcov_arg)
m2 = feols(log_pol ~ lze + log_pop + ..c2 | country + year, data=df %>% filter(id %in% treated_ids), vcov=vcov_arg)


etable(
  m1, m2,
  fitstat=c('n'),
  digits=3,
  headers = c("All Cities", "Treated Cities"),
  title='Treatment Effect Estimates with Spatially Autocorrelated Errors',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant', 'Log(Pop.)'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  # file="tables/v2/te_ols_conley.tex",
  label = 'tab:te_conley',
  notes=notes_all_countries,
  replace=T
)

m1 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=vcov_arg, data=df)
m2 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, vcov=vcov_arg, data=df %>% filter(id %in% treated_ids))

etable(m1, m2)

etable(
  m1, m2,
  fitstat=c('n'),
  digits=3,
  headers = c("All Cities", "Treated Cities"),
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
  file="tables/v2/te_spillovers_conley.tex",
  label = 'tab:te_spillovers_conley',
  notes=notes_all_countries,
  replace=T
)

m1 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, cluster = ~ country + year, data=df)
m2 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year, cluster = ~ country + year, data=df %>% filter(id %in% treated_ids))

etable(
  m1, m2,
  fitstat=c('n'),
  digits=3,
  headers = c("All Cities", "Treated Cities"),
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
  # file="tables/v2/te_spillovers.tex",
  label = 'tab:te_spillovers',
  notes=notes_all_countries,
  replace=T
)


## heterogeneity #####

hetdf = df %>% filter(id %in% treated_ids & city==1 & yeartreat >= 2007 & yeartreat <= 2022)
by = join_by(id)
hetdf = left_join(hetdf, table, by=by)

tyears = seq(-5, 5, 1)

vector = c("lze", "lze_0_1", "lze_1_3", "lze_3_5", "lze_5_10")

# 86 Madrid, 281 Paris, 386 Londres

hetdf %>% 
  # filter(`Density Quartile`=="Q2") %>% 
  filter(timetotreat %in% tyears) %>% 
  select(vector, timetotreat, pop, pol) %>% 
  gather("Type", "Implemented", lze:lze_5_10) %>% 
  group_by(Type, Implemented, timetotreat) %>% 
  summarise(avg=weighted.mean(pol, pop)) %>% 
  mutate(
    Implemented=factor(Implemented),
    Type=case_when(
      Type == 'lze' ~ "LEZ",
      Type == 'lze_0_1' ~ "1km Spillover",
      Type == 'lze_1_3' ~ "1-3km Spillover",
      Type == 'lze_3_5' ~ "3-5km Spillover",
      Type == 'lze_5_10' ~ "5-10km Spillover"
    )
  ) %>% 
  rename("Affected Cells"=Implemented) %>% 
  ggplot(aes(x=timetotreat, y=avg, color=`Affected Cells`))+
  geom_line() + 
  theme_bw() + 
  geom_vline(xintercept=0, linetype="dashed", alpha=.4) +
  facet_wrap(~factor(Type, 
                     levels=c("LEZ", "1km Spillover", "1-3km Spillover", 
                              "3-5km Spillover", "5-10km Spillover"))) + 
  ylab("PM2.5 Weighted Exposure") +
  xlab("Time to LEZ Implementation")+
  ggtitle("All Grid Cells inside Treated Cities")+
  theme(text=element_text(family="Palatino"))

m5 = feols(log_pol ~ ..lze_t + log_pop | country + year, 
           vcov=vcov_arg, #cluster=~country + year + id,
           data=hetdf, #%>% filter(!(id %in% c(281, 318, 86, 184))), 
           split = ~ `LEZ Size Quartile`)

etable(
  m5,
  fitstat=c('n', "r2", "ar2"),
  digits=3,
  title='Treatment Effect Estimates with Spatial Spillovers and Density Quartile',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'Constant', 'Log(Pop.)', 'log_pop'),
  fontsize='small',
  tex = T,
  style.tex = style,
  digits.stats=3,
  # file="tables/v2/het_densexp_spill",
  label = 'tab:het_densexp_spill',
  notes=notes_all_countries,
  replace=T
)

m5 = feols(log_pol ~ ..lze_t + ..c2 + log_pop | country + year + id, 
           vcov=vcov_arg, #cluster=~country + year + id,
           data=hetdf %>% filter(id==86)) #%>% filter(!(id %in% c(281, 318, 86, 184))), 
           split = ~ `LEZ Size Quartile`)


# event study #####

edata = df %>% filter(id %in% treated_ids)

allm = feols(log_pol ~ 
              i(timetotreat, lze, ref = -1) + 
              i(timetotreat, lze_0_1, ref = -1) + 
              i(timetotreat, lze_1_3, ref = -1) + 
              i(timetotreat, lze_3_5, ref = -1) +
              i(timetotreat, lze_5_10, ref = -1) +
               log_pop + ..c2 | ..fe, 
             vcov = vcov_arg,
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


m1 = feols(log_pol ~ i(timetotreat, lze, ref = -1) + ..c2 | ..fe, 
           vcov = vcov_arg, 
           data = edata)
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

library(ggfixest)
iplot(
  list(
    "LEZ"=m1,
    "0-1km"=m2, 
    "1-3km"=m3, 
    "3-5km"=m4, 
    "5-10"=m5
    )
  )

iplot(m2)
