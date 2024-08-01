setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fixest)
library(modelsummary)
library(arrow)
library(glue)
library(countrycode)
library(kableExtra)
library(hdm)


# within ######################## 
files = list.files("data/within")
setwd("data/within")
df <- lapply(files, read_parquet) %>% 
  bind_rows()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  

df = df %>% 
  filter(country_id != "MT")

codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[1] = "Greece"
paises[8] = 'United Kingdom'

df = right_join(df, tibble(country_id=codes, country=paises), 
                by="country_id") %>% 
  mutate(country=as.factor(country))


setFixest_fml(..c1 = ~ mean_temp + mean_precip + mean_wind,
              ..c2 = ~ mean_temp + mean_precip + mean_wind + lat + mean_ruggedness + 
                coast_city + water_dist + pw_dist,
              
              ..temp = ~ mean_temp + mean_precip + mean_wind,
              ..geo = ~ lat + mean_ruggedness,
              ..water = ~ coast_city + water_dist,
              ..pw = ~ pw_dist,
              
              ..fe = ~ country ^ year + country + year,
              ..fe_country = ~ country,
              ..fe_year = ~ year,
              ..fe_country_year = ~ country ^ year,
              
              
              ..aquif = ~ aquif1 + aquif2 + aquif3 + aquif4 + aquif5 + aquif6,
              ..hist= ~ hist1800,
              ..eq = ~ mean_eqhz,
              ..soil = ~ nutrient_tox_1 + nutrient_tox_2 + nutrient_tox_3 + 
                nutrient_tox_4 + nutrient_tox_5 + nutrient_av_1 + nutrient_av_2 + nutrient_av_3 + 
                nutrient_av_4 + nutrient_av_5 + nutrient_ret_1 + nutrient_ret_2 + nutrient_ret_3 + 
                nutrient_ret_4 + nutrient_ret_5 + nutrient_root_1 + nutrient_root_2 + nutrient_root_3 + 
                nutrient_root_4 + nutrient_root_5 + nutrient_ox_1 + nutrient_ox_2 + nutrient_ox_3 + 
                nutrient_ox_4 + nutrient_ox_5 + nutrient_sa_1 + nutrient_sa_2 + nutrient_sa_3 + 
                nutrient_sa_4 + nutrient_sa_5 + nutrient_work_1 + nutrient_work_2 + nutrient_work_3 + 
                nutrient_work_4 + nutrient_work_5,
              ..soil_tox = ~ nutrient_tox_1 + nutrient_tox_2 + nutrient_tox_3 + 
                nutrient_tox_4 + nutrient_tox_5, 
              ..soil_av = ~ nutrient_av_1 + nutrient_av_2 + nutrient_av_3 + 
                nutrient_av_4 + nutrient_av_5,
              ..soil_ret = ~ nutrient_ret_1 + nutrient_ret_2 + nutrient_ret_3 + 
                nutrient_ret_4 + nutrient_ret_5,
              ..soil_root = ~ nutrient_root_1 + nutrient_root_2 + nutrient_root_3 + 
                nutrient_root_4 + nutrient_root_5,
              ..soil_ox = ~ nutrient_ox_1 + nutrient_ox_2 + nutrient_ox_3 + 
                nutrient_ox_4 + nutrient_ox_5,
              ..soil_sa = ~ nutrient_sa_1 + nutrient_sa_2 + nutrient_sa_3 + 
                nutrient_sa_4 + nutrient_sa_5,
              ..soil_work = ~ nutrient_work_1 + nutrient_work_2 + nutrient_work_3 + 
                nutrient_work_4 + nutrient_work_5
              )

df = df %>% 
  mutate(log_exp_w=log(pol), log_pop_ras=log(pop))

## OLS Within models density ########

ols1 = feols(log_exp_w ~ log_pop_ras , data=df)
ols2 = feols(log_exp_w ~ log_pop_ras + ..c2, data=df)
ols3 = feols(log_exp_w ~ log_pop_ras + ..c2 | ..fe, data=df)

etable(
  ols1, ols2, ols3,
  fitstat=c('n', 'ar2'),
  digits=3,
  title='Within OLS Estimates for Population Density',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  tex = T,
  replace=T,
  dict = c(log_pop_ras = "Log (Population)"),
  file = "tables/within_ols.tex",
  digits.stats=3
)

rm(ols1, ols2, ols3)
gc()

## IV Within models density ########

iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..soil_tox, df, nthreads = 60)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ hist1800 + hist1500 + hist1000 + hist100, df, nthreads = 60)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..aquif, df, nthreads = 60)

etable(
  iv_02, iv_03, iv_04,
  fitstat=c('n', 'r2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Within Cities PM2.5 Exposure IV Estimates with Population Counts',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist", "coast_dist"),
    "Power Plants"=c("pw_dist")
  ),
  fixef.group = TRUE,
  tex = T,
  replace=T,
  # dict = c(log_pop_ras = "Log (Population)"),
  file = "tables/within_iv.tex",
  digits.stats=3,
  style.df = style.df(fixef.title = "", fixef.suffix = " FE", yesNo = c("Yes", 'No')),
  notes="Fixed Effects: Country-year, country and year"
)

rm(iv_02, iv_03, iv_04)
gc()
## IV Within models robustness historical ########

iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ hist100, df, nthreads = 60)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ hist1000, df, nthreads = 60)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ hist1500, df, nthreads = 60)
iv_05 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ hist1800, df, nthreads = 60)

etable(
  iv_02, iv_03, iv_04, iv_05,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Within Cities PM2.5 Exposure IV Estimates with Historical Densities',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist", "coast_dist"),
    "Power Plants"=c("pw_dist")
  ),
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  # dict = c(log_pop_ras = "Log (Population)"),
  style.df = style.df(fixef.title = "", fixef.suffix = " FE", yesNo = c("Yes", 'No')),
  notes="Fixed Effects: Country-year, country and year"
)

rm(iv_02, iv_03, iv_04, iv_05)
gc()