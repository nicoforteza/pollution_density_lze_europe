library(tidyverse)
library(fixest)
library(modelsummary)
library(glue)
library(countrycode)
library(kableExtra)
library(hdm)
library(nanoparquet)


# data ######################## 
df <- read_parquet("data/within/iv.pqt")
vcov_arg = conley(3, distance='triangular')
setFixest_fml(..c1 = ~ temp + prec + wind,
              ..c2 = ~ temp + prec + wind + latitude + mean_ruggedness + 
                coast_dist + river_dist + pw_dist,
              
              ..temp = ~ temp + prec + wind,
              ..geo = ~ latitude + mean_ruggedness,
              ..water = ~ coast_dist + river_dist,
              ..pw = ~ pw_dist,
              
              ..fe = ~ country_id ^ year + country_id + year,
              ..fe_country = ~ country_id,
              ..fe_year = ~ year,
              ..fe_country_year = ~ country_id ^ year,
              
              
              ..aquif = ~ aquif1 + aquif2 + aquif3 + aquif4 + aquif5 + aquif6,
              ..hist= ~ hist1800,
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

ols1 = feols(log_exp_w ~ log_pop_ras , data=df, fsplit=~is_city %keep% c("1"), vcov=vcov_arg)
ols2 = feols(log_exp_w ~ log_pop_ras + ..c2, data=df, fsplit=~is_city %keep% c("1"), vcov=vcov_arg)
ols3 = feols(log_exp_w ~ log_pop_ras + ..c2 | country_id + year, data=df, fsplit=~is_city %keep% c("1"), vcov=vcov_arg)

etable(
  list("No Controls"=ols1, "Controls"=ols2, "+FE"=ols3),
  fitstat=c('n', 'ar2'),
  digits=3,
  title='Within OLS Estimates for Population Density',
  depvar=F,
  se.below = T,
  drop=c("temp", "prec", "wind", "latitude", 
         "mean_ruggedness", "river_dist", 
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

vcov_arg = conley(3, distance='triangular')

iv_02 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ ..soil, 
              df, vcov=vcov_arg, fsplit=~is_city %keep% c("1"))
iv_03 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ hist1800, 
              df, vcov=vcov_arg, fsplit=~is_city %keep% c("1"))
iv_04 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ ..aquif, 
              df, vcov=vcov_arg, fsplit=~is_city %keep% c("1"))

etable(
  iv_02, iv_03, iv_04,
  fitstat=c('n', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Within Cities PM2.5 Exposure IV Estimates',
  depvar=F,
  se.below = T,
  drop=c("temp", "prec", "wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'river_dist'),
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
  notes="Fixed Effects: country and year."
)

rm(iv_02, iv_03, iv_04)
gc()

## IV Within models robustness historical ########

iv_02 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ hist100, df, vcov=vcov_arg)
iv_03 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ hist1000, df, vcov=vcov_arg)
iv_04 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ hist1500, df, vcov=vcov_arg)
iv_05 = feols(log_exp_w ~ ..c2 | country_id + year | log_pop_ras ~ hist1800, df, vcov=vcov_arg)

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
  file="tables/iv_hist_rob.tex",
  # dict = c(log_pop_ras = "Log (Population)"),
  style.df = style.df(fixef.title = "", fixef.suffix = " FE", yesNo = c("Yes", 'No')),
  notes="Fixed Effects: country and year"
)

rm(iv_02, iv_03, iv_04, iv_05)
gc()