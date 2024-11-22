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

df = read_parquet("data/within/within_clean_lze.pqt.gzip") %>% 
  select(!c("__index_level_0__")) #%>% 
# filter(country_id != "MT")

codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[8] = "United Kingdom"
paises[33] = 'Greece'

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
         outside_city = ifelse(id < 0, 1, 0),
         city = ifelse(id > 0, 1, 0)
  ) 

treated_ids = df %>% 
  filter(lze==1 & id > 0) %>% 
  select(id) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  deframe()

df_cities = df %>% 
  filter(city==1)

df_cities_treated = df_cities %>% 
  filter(id %in% treated_ids)

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

df_cities_treated = df_cities_treated %>% 
  rename(
    mean_prec=mean_precip,
    sum_hist1800=hist1800,
    sum_hist1500=hist1500,
    sum_hist1000=hist1000,
    sum_hist100=hist100
  )


vcov_arg = conley(2, distance='triangular')


keep(table, df_cities_treated, vcov_arg, sure=T)
gc()

