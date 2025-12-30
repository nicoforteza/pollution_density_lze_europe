setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)
library(fixest)
library(modelsummary)
library(arrow)
library(glue)
library(countrycode)
library(kableExtra)
library(hdm)
library(did2s)
library(ggfixest)

library(showtext)
source("config.R")

showtext_auto() 

# Data Loading ######################## 

df = read_parquet("data/between/cities_clean_uar.pqt") %>% 
  select(!c("__index_level_0__")) %>% 
  filter(country_id != "MT")

codes = df %>% select(country_id) %>% unique() %>% as_vector()
paises = countrycode(codes, "iso2c", "country.name")
paises[1] = "Greece"
paises[10] = 'United Kingdom'

df = right_join(df, tibble(country_id=codes, country=paises), 
                by="country_id") %>% 
  mutate(country=as.factor(country))

treated_countries = df %>% filter(treat == 1) %>% select(country_id) %>% unique() %>% as.vector()
treated_ids = df %>% filter(treat == 1) %>% select(id) %>% unique() %>% as.vector()

df_countries = df %>% filter(country_id %in% treated_countries$country_id) %>% mutate(post=ifelse(year>yeartreat, 1, 0))
df_ids = df %>% filter(id %in% treated_ids$id)%>% mutate(post=ifelse(year>yeartreat, 1, 0))

# Static DiD  #####

iv1 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_pop_ras ~ ..hist_between, df_ids)
iv2 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_dens_exp ~ ..hist_between, df_ids)

etable(
  iv1, iv2,
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Treatment Effect of Low Emission Zone on PM2.5 Exposure',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'log'),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist", "coast_dist"),
    "Power Plants"=c("pw_dist")
  ),
  tex = T,
  digits.stats=3,
  label='tab:did_all',
  # file = "tables/v2/static_did_all.tex",
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country-year) standard-errors in parentheses. Fixed Effects: Country-year, country and year. Temporal coverage: 2007-2021. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom.",
  replace = T
)

iv1 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_pop_ras ~ ..hist_between, df_countries, split = ~ country)

etable(
  iv1,
  fitstat=c('n', 'ar2', 'ivf1', "wh", "sargan"),
  digits=3,
  title='Treatment Effect of Low Emission Zone on PM2.5 Exposure',
  depvar=F,
  se.below = T,
  drop=c("mean_temp", "mean_prec", "mean_wind", "lat", 
         "mean_ruggedness", "coast_city", "water_dist", 
         "coast_dist", "pw_dist", 'area', 'log'),
  fontsize='small',
  group=list(
    "Weather"=c("mean_temp", "mean_prec", "mean_wind"),
    "Geological"=c("lat", "mean_ruggedness"),
    "Water"=c("coast_city", "water_dist", "coast_dist"),
    "Power Plants"=c("pw_dist")
  ),
  tex = T,
  digits.stats=3,
  label='tab:did_country',
  file = "tables/v2/static_did_country.tex",
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Clustered (country-year) standard-errors in parentheses. Fixed Effects: Country-year, country and year. Temporal coverage: 2007-2021. Countries: Albania, Austria, Belgium, Bulgaria, Croatia, Cyprus, Czechia, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Iceland, Ireland, Italy, Latvia, Lithuania, Montenegro, Netherlands, North Macedonia, Norway, Poland, Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland and United Kingdom.",
  replace = T
)

# Event Time Study ####


dfm = df_countries %>% 
  mutate(treat = replace(treat, id %in% treated_ids$id %>% as.vector(), 1))

est_sa20 = feols(
  log_exp_w ~  log_dens_exp + ..c2 + sunab(yeartreat, year, -1) | id + ..fe,
  data = df_countries, cluster = ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

ggiplot(est_sa20,
      xlab="Time to Low Emission Zone",
        main= "",
        geom_style = 'ribbon',
        ref.line = -1,
        multi_style = 'facet'
        ) +
  scale_x_continuous(breaks=seq(-5, 5, 1), limits = c(-5, 5))+
  theme(text = element_text(family = "Palatino")) +
  scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))

ggsave("figures/es1.png", dpi=300, width=5, height=5)

est_sa20 = feols(
  exposure ~  log_dens_exp + ..c2 + sunab(yeartreat, year, -1) | id + ..fe,
  data = df_countries, cluster = ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

ggiplot(est_sa20,
        xlab="Time to Low Emission Zone",
        main= "",
        geom_style = 'ribbon',
        ref.line = -1,
        multi_style = 'facet'
) +
  scale_x_continuous(breaks=seq(-5, 5), limits = c(-5, 5))+
  theme(text = element_text(family = "Palatino")) +
  scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))

ggsave("figures/es2.png", dpi=300, width=5, height=5)

# Validation of Research Design: 1 - Predicting Timing

pt1 = feols(
  yeartreat ~ log_pop_ras + ..c2 - treat + area | country,
  data = df_ids %>% filter(timetotreat %in% c(-1)),
  split=~timetotreat
) 
pt2 = feols(
  yeartreat ~ log_pop_ras + ..c2 - treat + area | country,
  data = df_ids %>% filter(timetotreat %in% c(-1)),
  split=~timetotreat,
  weights=~sum_pop
) 

etable(
  pt1, pt2,
  fitstat=c('n', 'ar2', 'r2', "f"),
  digits=3,
  title='Prediction of LEZ Year of Implementation',
  depvar=F,
  se.below = T,
  fontsize='small',
  tex = T,
  digits.stats=3,
  label='tab:pred_v1_between',
  # file = "tables/v2/static_did_all.tex",
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. IID standard-errors in parentheses. Fixed Effects: Country. Temporal coverage: 2007-2022.",
  replace = T
)

# Validation of Research Design: 2 - Predicting PM2.5 with LEZ year

pt1 = feols(
  log_exp_w ~ yeartreat + ..c2 - treat + area | country,
  data = df_ids %>% filter(timetotreat < 0),
) 
pt2 = feols(
  log_exp_w ~ yeartreat + ..c2 - treat + area | country,
  data = df_ids %>% filter(timetotreat < 0),
  weights=~sum_pop
) 

etable(
  pt1, pt2,
  fitstat=c('n', 'ar2', 'r2', "f"),
  digits=3,
  title='Prediction of LEZ Year of Implementation',
  depvar=F,
  se.below = T,
  fontsize='small',
  keep='yeartreat',
  tex = T,
  digits.stats=3,
  label='tab:pred_2',
  # file = "tables/v2/static_did_all.tex",
  notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. IID standard-errors in parentheses. Fixed Effects: Country. Temporal coverage: 2007-2022.",
  replace = T
)