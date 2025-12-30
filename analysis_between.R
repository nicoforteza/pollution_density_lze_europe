setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(fixest)
library(modelsummary)
library(arrow)
library(glue)
library(countrycode)
library(kableExtra)
library(hdm)


# Betweeen ######################## 

source("config.R")

df = read_parquet("data/between/cities_clean_v2.pqt") %>% 
  select(!c("__index_level_0__"))


codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[1] = "Greece"
paises[11] = 'United Kingdom'

df = right_join(df, tibble(country_id=codes, country=paises), 
                by="country_id") %>% 
  mutate(country=as.factor(country))

require(foreign)
write.dta(df, "data/cities_clean_v2.dta")


## Descriptive statistics #######

# between

db = df %>% 
  mutate(country=factor(country)) %>% 
  select(-c(country_id)) %>% 
  mutate(sum_pop=sum_pop/ 1e6, expden=expden/1e6, mean_wind=mean_wind * 1000) %>% 
  select(`Population` = sum_pop,
         `Experienced Density` = expden,
         `Pollution Exposure` = exposure,
         `Temperature` = mean_temp,
         `Precipitation` = mean_prec,
         `Wind Speed` = mean_wind,
         `Ruggedness` = mean_ruggedness,
         `Coast City` = coast_city,
         `Water Distance` = water_dist,
         `Coast Distance` = coast_dist,
         `Power Plant Distance` = pw_dist,
         )

db1 = df %>% 
  mutate(sum_hist100=sum_hist100/ 1e6, 
         sum_hist1000=sum_hist1000/1e6,
         sum_hist1500=sum_hist1500/1e6,
         sum_hist1800=sum_hist1800/1e6,
         ) %>% 
  select(
         `Population in 100 A.D.` = sum_hist100,
         `Population in 1000 A.D.` = sum_hist1000,
         `Population in 1500 A.D.` = sum_hist1500,
         `Population in 1800 A.D.` = sum_hist1800,
         `Highly productive porous aquifers` = aquif1,
         `Low and moderately productive porous aquifers` = aquif2,
         `Highly productive fissured aquifers (including karstified rocks)` = aquif3,
         `Low and moderately productive fissured aquifers (including karstified rocks)` = aquif4,
         `Locally aquiferous rocks, porous or fissured` = aquif5,
         `Practically non-aquiferous rocks, porous or fissured` = aquif6,
  )

caption_desc_bt = "Descriptive Statistics of Between Cities Sample"

datasummary(All(db) ~ (Mean + SD),
            data = db1, caption=caption_desc_bt,
            sparse_header = TRUE, output='latex')

datasummary(All(db1) ~ (Mean + SD),
            data = db1, caption=caption_desc_bt,
            sparse_header = TRUE, output='latex')

merged = cbind(db, db1) %>% 
  pivot_longer(everything()) %>% 
  rename(Variable=name)


aux = rbind(
  tibble("Variable"=colnames(db), "Type"="Control"),
  tibble("Variable"=colnames(db1), "Type"="Instrument")
)

by <- join_by(Variable)
final_desc_between = left_join(merged, aux, by)

notes = "Notes: Yearly (2007-2022) averages and standard deviation for Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary,
Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia,
Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom. Population and experienced density is expressed in millions of persons, temperature in Celsius degrees, precipitation in millimiters per day, wind speed in meters per second (in thousands) at a 10 meter height above the surface, and all distance variables are expressed in kilometers (km)."

datasummary(Type * Variable * All(final_desc_between) ~ 
               (Mean + SD) * DropEmpty(), 
            data=final_desc_between, 
            caption=caption_desc_bt,
            sparse_header = TRUE, 
            output='tables/v2/descriptive_between.tex', 
            notes = notes)


# instruments 2

db = df %>% 
  select(
    `Nutrient availability: No or slight limitations`= nutrient_av_1,
    `Nutrient availability: Moderate limitations`= nutrient_av_2,
    `Nutrient availability: Sever limitations`= nutrient_av_3,
    `Nutrient availability: Very severe limitations`= nutrient_av_4,
    `Nutrient availability: Mainly non-soil`= nutrient_av_5,
    `Nutrient availability: Water bodies`= nutrient_av_7,
    
    `Nutrient retention: No or slight limitations`= nutrient_ret_1,
    `Nutrient retention: Moderate limitations`= nutrient_ret_2,
    `Nutrient retention: Sever limitations`= nutrient_ret_3,
    `Nutrient retention: Very severe limitations`= nutrient_ret_4,
    `Nutrient retention: Mainly non-soil`= nutrient_ret_5,
    `Nutrient retention: Water bodies`= nutrient_ret_7,
    
    `Rooting conditions: No or slight limitations`= nutrient_root_1,
    `Rooting conditions: Moderate limitations`= nutrient_root_2,
    `Rooting conditions: Sever limitations`= nutrient_root_3,
    `Rooting conditions: Very severe limitations`= nutrient_root_4,
    `Rooting conditions: Mainly non-soil`= nutrient_root_5,
    `Rooting conditions: Water bodies`= nutrient_root_7, 
    
    `Oxygen availability: No or slight limitations`= nutrient_root_1,
    `Oxygen availability: Moderate limitations`= nutrient_root_2,
    `Oxygen availability: Sever limitations`= nutrient_root_3,
    `Oxygen availability: Very severe limitations`= nutrient_root_4,
    `Oxygen availability: Mainly non-soil`= nutrient_root_5,
    `Oxygen availability: Water bodies`= nutrient_root_7, 
    
    `Excess Salts: No or slight limitations`= nutrient_root_1,
    `Excess Salts: Moderate limitations`= nutrient_root_2,
    `Excess Salts: Sever limitations`= nutrient_root_3,
    `Excess Salts: Very severe limitations`= nutrient_root_4,
    `Excess Salts: Mainly non-soil`= nutrient_root_5,
    `Excess Salts: Water bodies`= nutrient_root_7, 
    
    `Toxicity: No or slight limitations`= nutrient_root_1,
    `Toxicity: Moderate limitations`= nutrient_root_2,
    `Toxicity: Sever limitations`= nutrient_root_3,
    `Toxicity: Very severe limitations`= nutrient_root_4,
    `Toxicity: Mainly non-soil`= nutrient_root_5,
    `Toxicity: Water bodies`= nutrient_root_7,
    
    `Workability: No or slight limitations`= nutrient_root_1,
    `Workability: Moderate limitations`= nutrient_root_2,
    `Workability: Sever limitations`= nutrient_root_3,
    `Workability: Very severe limitations`= nutrient_root_4,
    `Workability: Mainly non-soil`= nutrient_root_5,
    `Workability: Water bodies`= nutrient_root_7
  ) %>% 
  pivot_longer(everything(), names_to = "Soil", values_to = "Share") %>% 
  mutate(Type=Soil) %>% 
  mutate("Quality Level"= str_split_i(Type, ": ", i=2)) %>% 
  mutate(`Characteristic` = str_split_i(Type, ": ", i=1)) %>% 
  select(-Soil, -Type)

desc_bet_iv_2 <- db %>% 
  group_by(`Quality Level`, Characteristic) %>% 
  summarise(`Mean Share of Land`=mean(Share, na.rm=T)) %>% 
  pivot_wider(values_from = "Mean Share of Land",
              names_from="Characteristic") %>% 
  kable(format = 'latex', booktabs = TRUE, digits=2, caption='tab:soil')

writeLines(desc_bet_iv_2, 'tables/v2/descriptive_between_iv_2.tex')


## OLS between models density ########

setFixest_dict(
  c(
    log_pop_ras = "Log (Population)",
    log_dens_exp="Log (Exp. Density)",
    country="Country",
    year="Year",
    fit_log_pop_ras="Log (Population)"
  )
)

ols1 = feols(log_exp_w ~ sw(log_pop_ras, log_dens_exp) , data=df)
ols2 = feols(log_exp_w ~ sw(log_pop_ras, log_dens_exp) + ..c2, data=df)
ols3 = feols(log_exp_w ~ sw(log_pop_ras, log_dens_exp) + ..c2 | ..fe, data=df, cluster = ~ year + country)


etable(
  ols1, ols2, ols3,
  fitstat=c('n', 'ar2', 'f'),
  digits=3,
  title='Between OLS Estimates for PM2.5 Exposure',
  depvar=F,
  se.below = T,
  # keep=c(log_pop_ras),
  drop=c("mean_temp", "mean_prec", "mean_wind", "mean_ruggedness", "coast_city", "water_dist", "Constant", "pw_dist"),
  fontsize='small',
  tex = T,
  digits.stats=3,
  label='tab:ols_between',
  file = "tables/v2/between_ols.tex",
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: Country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom.",
  replace = T
)

## IV between models density ########

# pop normal appendix

iv_02 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_pop_ras ~ ..soil, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_pop_ras ~ ..hist, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_pop_ras ~ ..aquif, df, cluster = ~ year + country)

etable(
  list("Soil Quality"=iv_02, "Historical Density"=iv_03, "Aquifers"=iv_04),
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Population Density',
  depvar=F,
  se.below = T,
  #drop=c("mean_temp", "mean_prec", "mean_wind", "mean_ruggedness", "coast_city", "water_dist", "Constant", "pw_dist"),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist"),
    "Power Plants"=c("pw_dist")
    ),
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label='tab:iv_bet_appendix',
  replace=T,
  file = "tables/v2/between_iv_pop_appendix.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
    ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)


# pop normal resumido

iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..soil, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..hist, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..aquif, df, cluster = ~ year + country)

etable(
  list("Soil Quality"=iv_02, "Historical Density"=iv_03, "Aquifers"=iv_04),
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Population Density',
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
  replace=T,
  label='tab:iv_bet_pop',
  file = "tables/v2/between_iv_pop.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)



## IV between models experienced density ########


iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..hist, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..aquif, df, cluster = ~ year + country)

etable(
  list("Soil Quality"=iv_02, "Historical Density"=iv_03, "Aquifers"=iv_04),
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Experienced Density',
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
  replace=T,
  label='tab:iv_bet_expden',
  file = "tables/v2/between_iv_expden.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)

iv_02 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_dens_exp ~ ..soil, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_dens_exp ~ ..hist, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ csw(..temp, ..geo, ..water, ..pw) | ..fe | log_dens_exp ~ ..aquif, df, cluster = ~ year + country)

etable(
  list("Soil Quality"=iv_02, "Historical Density"=iv_03, "Aquifers"=iv_04),
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Experienced Density',
  depvar=F,
  se.below = T,
  #drop=c("mean_temp", "mean_prec", "mean_wind", "mean_ruggedness", "coast_city", "water_dist", "Constant", "pw_dist"),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist"),
    "Power Plants"=c("pw_dist")
  ),
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label='tab:iv_bet_appendix_expden',
  replace=T,
  file = "tables/v2/between_iv_expden_appendix.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)



## Robustness historical density ####

iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ sum_hist100, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ sum_hist1000, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ sum_hist1500, df, cluster = ~ year + country)
iv_05 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ sum_hist1800, df, cluster = ~ year + country)

group=list(
  "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
  "Geological"=c("lat", "mean_ruggedness"),
  "Water"=c("coast_city", "water_dist", "coast_dist"),
  "Power Plants"=c("pw_dist")
)

etable(
  iv_02, iv_03, iv_04, iv_05,
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='IV Estimates with Experienced Density for Historical Density Instruments',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  group = group,
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  replace=T,
  label="tab:iv_hist_rob",
  file = "tables/v2/between_iv_hist_rob.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)

## Robustness Geological Instruments ####

iv_01 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox , df, cluster = ~ year + country)
iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox + ..soil_ret, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox + ..soil_ret + ..soil_av, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox + ..soil_ret + ..soil_av + ..soil_ox, df, cluster = ~ year + country)
iv_05 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox + ..soil_ret+ ..soil_av+ ..soil_ox+ ..soil_work, df, cluster = ~ year + country)
iv_06 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox + ..soil_ret+ ..soil_av+ ..soil_ox+ ..soil_work+ ..soil_root, df, cluster = ~ year + country)
iv_07 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil, df, cluster = ~ year + country)

etable(
  iv_01, iv_02, iv_03, iv_04, iv_05, iv_06, iv_07,
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Soil Quality (Incrementally)',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  group = group,
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label="tab:iv_soil_rob1",
  file = "tables/v2/between_iv_soil_rob1.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)

iv_01 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_tox , df, cluster = ~ year + country)
iv_02 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_ret, df, cluster = ~ year + country)
iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_av, df, cluster = ~ year + country)
iv_04 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_ox, df, cluster = ~ year + country)
iv_05 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_work, df, cluster = ~ year + country)
iv_06 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_root, df, cluster = ~ year + country)
iv_07 = feols(log_exp_w ~ ..c2 | ..fe | log_dens_exp ~ ..soil_sa, df, cluster = ~ year + country)


etable(
  iv_01, iv_02, iv_03, iv_04, iv_05, iv_06, iv_07,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Between Cities PM2.5 Exposure IV Estimates with Soil Quality (Independent)',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area'),
  fontsize='small',
  group = group,
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label="tab:iv_soil_rob2",
  file = "tables/v2/between_iv_soil_rob2.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)

# city size and more than 1 instrument
iv_01 = feols(log_exp_w ~ ..c2 + csw0(sum_pop, sum_pop**2, sum_pop**3) | ..fe | log_dens_exp ~ ..aquif + ..hist, df, cluster = ~ year + country)

etable(
  iv_01,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Robustness Check in Between Cities PM2.5 Exposure IV Estimates for City Size',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'sum*'),
  fontsize='small',
  group = group,
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label="tab:iv_size_rob1",
  file = "tables/v2/between_iv_size_rob1.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)
iv_02 = feols(log_exp_w ~ ..c2 + csw0(sum_pop, sum_pop**2, sum_pop**3) + area | ..fe | log_dens_exp ~ ..aquif + ..hist, df, cluster = ~ year + country)
etable(
  iv_02,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Robustness Check in Between Cities PM2.5 Exposure IV Estimates for City Size adding City Area',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'sum*'),
  fontsize='small',
  group = group,
  fixef.group = TRUE,
  tex = T,
  digits.stats=3,
  label="tab:iv_size_rob2",
  file = "tables/v2/between_iv_size_rob2.tex",
  style.df = style.df(
    fixef.title = "", 
    fixef.suffix = " FE", 
    yesNo = c("Yes", 'No')
  ),
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country and year) standard-errors in parentheses. Fixed Effects: country and year. Temporal coverage: 2007-2022. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom."
)

## Visualization ####


library(wesanderson)

dfa = df %>% filter(year == 2019 & log_dens_exp > 10 & country %in% c("United Kingdom", "Spain", "Italy", "France", "Germany"))
dfa %>% ggplot(aes(x=log_dens_exp, y=log_exp_w, fill=country, color=country)) +
  geom_point(size=2, alpha=.75) +
  geom_smooth(method = 'lm', se=F) +
  theme_bw() + 
  scale_color_manual(values = wes_palette("Darjeeling1", 5, 'discrete')) +
  xlab("Log (Experienced Density)") + ylab("Log (PM2.5 Exposure)") +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_blank()) +
  theme(
    axis.line.x = element_line(color = "black"),  # Bordes del eje x
    axis.line.y = element_line(color = "black"),  # Bordes del eje y
    axis.line.x.top = element_line(color = "transparent"),  # Borde superior del eje x
    axis.line.y.right = element_line(color = "transparent")  # Borde derecho del eje y
  ) +
  theme(
    text = element_text(family = "Palatino")
  )

ggsave("figures/scat.png", dpi=300, width=5, height=5)

iv_03 = feols(log_exp_w ~ ..c2 | ..fe | log_pop_ras ~ ..hist, df)
f = fixef(iv_03)
fe = tibble(FE=f$`country^year`, val=names(f$`country^year`)) %>% 
  separate_wider_delim(val, delim = "_", names = c("Country", "Year"))

library(gghighlight)

fe %>% 
  ggplot() +
  geom_line(aes(x=Year, y=FE, group=Country), alpha=.3, color='red') +
  gghighlight(unhighlighted_params =list(colour='grey')) +
  facet_wrap(~ Country) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        panel.border = element_blank(),
        axis.line = element_blank()) +
  theme(
    text = element_text(family = "Palatino")
  ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = seq(2007, 2021, by = 4)) +
  ylab("Country - Year Fixed Effects")

ggsave("figures/fe_counties.png", dpi=300, width=9, height=6)


