setwd("/Users/nicoforteza/Desktop/tfm")
library(tidyverse)
library(fixest)
library(modelsummary)
library(arrow)
library(glue)
library(countrycode)
library(kableExtra)
library(hdm)
require(WeightIt)
library(did2s)
library(ggiplot)
library(showtext)
source("config.R")
font_add("Palatino", "C:/Windows/Fonts/pala.ttf")

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

wm <- weightit(treat ~ log_pop_ras + log_exp_w + country, 
               data = df, 
               stabilize = T)
df = df %>% 
  mutate(w=as.vector(wm$weights))

treated_countries = df %>% filter(treat == 1) %>% select(country_id) %>% unique() %>% as.vector()
treated_ids = df %>% filter(treat == 1) %>% select(id) %>% unique() %>% as.vector()

df_countries = df %>% filter(country_id %in% treated_countries$country_id)
df_ids = df %>% filter(id %in% treated_ids$id)

# Static DiD  #####

iv1 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_pop_ras ~ ..hist, df_countries)
iv2 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_dens_exp ~ ..hist, df_countries)

etable(
  iv1, iv2,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Treatment Effect of Urban Access Regulation on PM2.5 Exposure',
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
  style.df = style.df(fixef.title = "", fixef.suffix = " FE", yesNo = c("Yes", 'No'))
)

iv1 = feols(log_exp_w ~ ..c2 + treat | ..fe | log_pop_ras ~ ..hist, df_countries, split = ~ country)

etable(
  iv1,
  fitstat=c('n', 'ar2', 'ivf1'),
  digits=3,
  title='Treatment Effect of Urban Access Regulation on PM2.5 Exposure',
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
  style.df = style.df(fixef.title = "", fixef.suffix = " FE", yesNo = c("Yes", 'No'))
)

# Event Time Study ####


dfm = df_countries %>% 
  mutate(treat = replace(treat, id %in% treated_ids$id %>% as.vector(), 1))

est_twfe = feols(
  log_exp_w ~ log_dens_exp + ..c2 + i(timetotreat, treat, c(-1, -Inf)) | id + ..fe,
  data = dfm, vcov = ~ ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

est_sa20 = feols(
  log_exp_w ~  log_dens_exp + ..c2 + sunab(yeartreat, year, -1) | id + ..fe,
  data = df_countries, vcov = cluster ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

iplot(list("SunAb"=est_sa20, "TWFE"=est_twfe),
        xlab="Time to Urban Access Regulation",
        main= "",
        geom_style = 'ribbon',
        ref.line = -1,
        multi_style = 'facet'
        ) +
  scale_x_continuous(breaks=seq(-6, 6), limits = c(-6, 6))+
  theme(text = element_text(family = "Palatino")) +
  scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))

ggsave("es1.png", dpi=300, width=5, height=5)

est_twfe = feols(
  exposure ~ sum_pop + ..c2 + i(timetotreat, treat, c(-1, -Inf)) | id + ..fe,
  data = dfm, vcov = ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

est_sa20 = feols(
  exposure ~ sum_pop + ..c2 + sunab(yeartreat, year, -1) | id + ..fe,
  data = df_countries, vcov = cluster ~ id + country + year + country^year,  panel.id = c('id', 'year')
)

iplot(list("SunAb"=est_sa20, "TWFE"=est_twfe),
        xlab="Time to Urban Access Regulation",
        main= "",
        geom_style = 'ribbon',
        ref.line = -1,
        multi_style = 'facet'
) +
  scale_x_continuous(breaks=seq(-6, 6), limits = c(-6, 6))+
  theme(text = element_text(family = "Palatino")) +
  scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))

ggsave("es2.png", dpi=300, width=5, height=5)

est_twfe = feols(
  exposure ~ sum_pop + ..c2 + i(timetotreat, treat, c(-1, -Inf)) | id + year,
  data = dfm, vcov = ~ id + year,  panel.id = c('id', 'year'), split=~country
)

est_sa20 = feols(
  exposure ~ sum_pop + ..c2 + sunab(yeartreat, year, -1) | id + ..fe,
  data = df_countries, vcov = cluster ~ id + year + country^year,  panel.id = c('id', 'year'),
  split= ~country
)

ggiplot(list("did"=est_sa20),
        xlab="Time to Urban Access Regulation",
        main= "",
        geom_style = 'ribbon',
        ref.line = -1,
        facet_args = list(ncol = 4, scales='free_y', labeller = labeller(id = \(x) gsub(".*: ", "", x))),
        multi_style = 'facet'
) +
  scale_x_continuous(breaks=seq(-3, 3), limits = c(-3, 3))+
  theme_minimal()+
  theme(text = element_text(family = "Palatino")) +
  scale_fill_brewer(palette = 'Set1', aesthetics = c("colour", "fill"))
