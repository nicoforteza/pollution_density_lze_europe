library(tidyverse)
library(fixest)
library(arrow)
library(countrycode)
library(sf)
library(modelsummary)
library(kableExtra)
library(knitr)
library(sf)
library(paletteer)

# data loading ######################## 

source("analysis_diff_prepare.R")

# visualization ####
## relative years distribution ####
### cities ####

df %>% 
  filter(country != "Switzerland" & !is.na(city_name) & lez_5km_itf == 0) %>% 
  select(id_city, yeartreat, country) %>% 
  distinct() %>% 
  group_by(country, yeartreat) %>% 
  summarise(
    n=n()
  ) %>% 
  ggplot(aes(x=yeartreat, y=n, fill=country)) + 
  geom_col() +
  theme_bw()+
  xlab("Year of LEZ") + 
  ylab("N. of Cities") +
  scale_y_continuous(n.breaks=12) +
  scale_x_continuous(n.breaks=10) +
  scale_fill_paletteer_d("ggthemes::Tableau_10") +
  guides(fill=guide_legend(title="Country")) +
  theme_aux
ggsave("figures/lez_country_count.png", width=7, height=3.5)

df %>% 
  filter(lez_5km_itf == 0) %>% 
  select(id_city, timetotreat, city_affected) %>% 
  distinct() %>% 
  group_by(timetotreat, city_affected) %>% 
  summarise(n=n()) %>% 
  filter(!is.infinite(timetotreat)) %>% 
  ggplot(aes(x=timetotreat, y=n)) +
  geom_col() + 
  theme_bw() +
  facet_wrap(.~city_affected) + 
  scale_y_continuous(n.breaks = 15) +
  ylab("N. of Cities") + 
  xlab("Time to LEZ Implementation") +
  scale_x_continuous(n.breaks = 16) + theme_aux
ggsave("figures/lez_histogram_city_affected.png", width=4, height=2.5)

### km2 units
df %>% 
  filter(is_city == 1 & country != "Switzerland") %>% 
  mutate(
    n_lezs=ifelse(n_lezs > 1, "Multiple", "One")
  ) %>% 
  group_by(timetotreat, n_lezs, country) %>% 
  summarise(n=n()) %>% 
  filter(!is.infinite(timetotreat)) %>% 
  ggplot(aes(x=timetotreat, y=n)) +
  geom_col(aes(fill=country)) + 
  theme_bw() +
  facet_wrap(.~n_lezs) +
  scale_y_continuous(n.breaks = 15) +
  ylab("N. of Grid Cells (km2)") + 
  xlab("Time to LEZ Implementation") +
  scale_fill_paletteer_d("ggthemes::Tableau_10") +
  scale_x_continuous(n.breaks = 16) + theme_aux
ggsave("figures/lez_histogram_countries.png", width=8, height=4.5)


## parallel trends ####

### one city relative ####
city_plot_rel = function(df, cityid, wrap, bands){
  df = df %>% 
    as.data.table()
  df = df[id_city == cityid, ]
  df = as_tibble(df)
  p = df %>% 
    st_drop_geometry() %>% 
    select(timetotreat, is_lez, lez_0_1, lez_1_3, lez_3_5,
           lez_not_spill, id_city, log_pol) %>% 
    pivot_longer(
      cols = c("is_lez", "lez_0_1", "lez_1_3", "lez_3_5", 
               "lez_not_spill", "id_city"), 
      names_to = "typeof",      
      values_to = "zone"
    ) %>% 
    filter(zone==1) %>% 
    select(-zone) %>% 
    group_by(timetotreat, typeof) %>%
    summarise(
      mean_value = mean(log_pol),
      sd_value = sd(log_pol),
      .groups = 'drop'
    ) %>%
    mutate(
      lower_band = mean_value - sd_value,
      upper_band = mean_value + sd_value,
    ) %>%
    ggplot(aes(x = timetotreat, y = mean_value)) +
    geom_line(aes(color = typeof), linewidth=0.6) +  # Added + here
    theme_bw() +  # Added + here
    xlab("") + ylab("Log(PM2.5)") +
    scale_x_continuous(n.breaks = 15) +
    scale_color_paletteer_d("ggsci::nrc_npg") +
    geom_vline(xintercept=0, color="red", size=0.5, linetype="dashed")
  
  if (wrap == T){
    p = p + facet_wrap(.~typeof)
  }
  if (bands == T){
    p = p + geom_ribbon(aes(ymin = lower_band, ymax = upper_band, fill = typeof), alpha = 0.2)
  }
  p
}
# city_plot_rel(df, 496, F, F) + ggtitle("London") # london
# city_plot_rel(df, 334, F, F) + ggtitle("Paris") # paris
# city_plot_rel(df, 149, F, F) + ggtitle("Milan") # milan

### set of cities relative ####
cities_plot_rel_zones = function(df, wrap, bands){
  p = df %>% 
    # filter(lez_5km_itf == 0) %>% 
    select(timetotreat, is_lez, lez_0_1, lez_1_3, lez_3_5,
           lez_not_spill, id_city, log_pol) %>% 
    mutate(
      lez_not_spill = case_when(
        lez_not_spill == 1 | lez_3_5 == 1 ~ 1,
        T ~ 0
      )
    ) %>% 
    pivot_longer(
      cols = c("is_lez", "lez_0_1", "lez_1_3", 
               "lez_not_spill", "id_city"), 
      names_to = "typeof",      
      values_to = "zone"
    ) %>% 
    filter(zone==1) %>% 
    select(-zone) %>% 
    group_by(timetotreat, typeof) %>%
    summarise(
      mean_value = mean(log_pol),
      sd_value = sd(log_pol),
      .groups = 'drop'
    ) %>%
    mutate(
      lower_band = mean_value - sd_value,
      upper_band = mean_value + sd_value,
      typeof = case_when(
        typeof == 'is_lez' ~ "LEZ",
        typeof == 'lez_0_1' ~ "Adjacent",
        typeof == 'lez_1_3' ~ "1-3km Ring",
        # typeof == 'lez_3_5' ~ "3-5km Ring",
        typeof == 'lez_not_spill' ~ ">3km"
        )
      )%>% 
      mutate(typeof = fct_relevel(
          typeof, "LEZ", "Adjacent", "1-3km Ring", ">3km")
    ) %>% 
    ggplot(aes(x = timetotreat, y = mean_value, 
               group=typeof)) +
    geom_line(aes(color = typeof), linewidth=0.6) +  # Added + here
    theme_bw() +  # Added + here
    xlab("") + ylab("Log(PM2.5)") +
    scale_x_continuous(n.breaks = 15) +
    scale_color_brewer(palette = "Dark2") +
    geom_vline(xintercept=0, color="red", size=0.5, linetype="dashed")
  
  if (wrap == T){
    p = p + facet_wrap(.~typeof)
  }
  if (bands == T){
    p = p + geom_ribbon(aes(ymin = lower_band, ymax = upper_band, fill = typeof), alpha = 0.2)
  }
  p
}
cities_plot_rel_zones(
  df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1), F, F) +
  theme_aux + xlab("Time to Low Emission Zone Implementation")
ggsave("figures/logpm_timetolez_all.png", width=5.5, height=3.5)

## size of spillovers ####

df %>% 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1) %>% 
  mutate(
    lez_not_spill = case_when(
      lez_not_spill == 1 | lez_3_5 == 1 ~ 1,
      T ~ 0
    )
  ) %>% 
  st_drop_geometry() %>%
  select(timetotreat, is_lez, lez_0_1, lez_1_3,
         lez_not_spill, id_city, log_pol) %>% 
  pivot_longer(
    cols = c("is_lez", "lez_0_1", "lez_1_3",
             "lez_not_spill", "id_city"), 
    names_to = "typeof",      
    values_to = "zone"
  ) %>% 
  filter(zone==1) %>% 
  select(-zone) %>% 
  group_by(timetotreat, typeof) %>% 
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>% 
  mutate(
    typeof = case_when(
      typeof == 'is_lez' ~ "Low Emission Zone",
      typeof == 'lez_0_1' ~ "Adjacent Cells",
      typeof == 'lez_1_3' ~ "1-3km Ring",
      # typeof == 'lez_3_5' ~ "3-5km Ring",
      typeof == 'lez_not_spill' ~ ">3km Zones"
    )
  )%>% 
  mutate(typeof = fct_relevel(
    typeof, "Low Emission Zone", "Adjacent Cells", "1-3km Ring", ">3km Zones")
  ) %>% 
  drop_na() %>% 
  ggplot(aes(x = timetotreat, y = n, fill=typeof)) +
  geom_area(linewidth=0.6) +
  theme_bw() + 
  xlab("Time to Low Emission Zone") + ylab("N. of Grid Cells (km2)") +
  scale_x_continuous(n.breaks = 15) +
  # scale_color_paletteer_d("ggthemes::Tableau_10") +
  scale_fill_brewer(palette="Set2")  +
  # facet_wrap(.~is_city)+
  # scale_color_manual(values = wes_palette("Darjeeling1", 6, 'discrete')) +
  geom_vline(xintercept=0, color="red", size=0.5, linetype="dashed") +
  theme_aux

ggsave("figures/ncells_by_ring_city.png", width=5.5, height=3.5)


df %>% 
  filter(lez_5km_itf == 0 & pol > 0 & is_city == 1) %>% 
  select(timetotreat, is_lez, lez_0_1, lez_1_3, lez_3_5,
         lez_not_spill, id_city, log_pol, lez_size_rel) %>% 
  pivot_longer(
    cols = c("is_lez", "lez_0_1", "lez_1_3", "lez_3_5", 
             "lez_not_spill", "id_city"), 
    names_to = "typeof",      
    values_to = "zone"
  ) %>% 
  filter(zone==1) %>% 
  select(-zone) %>% 
  group_by(timetotreat, typeof, lez_size_rel) %>% 
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>% 
  mutate(
    typeof = case_when(
      typeof == 'is_lez' ~ "Low Emission Zone",
      typeof == 'lez_0_1' ~ "Adjacent Cells",
      typeof == 'lez_1_3' ~ "1-3km Ring",
      typeof == 'lez_3_5' ~ "3-5km Ring",
      typeof == 'lez_not_spill' ~ ">5km Zones"
    )
  )%>% 
  mutate(typeof = fct_relevel(
    typeof, "Low Emission Zone", "Adjacent Cells", "1-3km Ring", 
    "3-5km Ring", ">5km Zones")
  ) %>% 
  drop_na() %>% 
  ggplot(aes(x = timetotreat, y = n, fill=typeof)) +
  geom_area(linewidth=0.6) +
  facet_wrap(.~lez_size_rel) +
  theme_bw() + 
  xlab("Time to Low Emission Zone") + ylab("N. of Grid Cells (km2)") +
  scale_x_continuous(n.breaks = 15) +
  # scale_color_paletteer_d("ggthemes::Tableau_10") +
  scale_fill_brewer(palette="Set2")  +
  # facet_wrap(.~is_city)+
  # scale_color_manual(values = wes_palette("Darjeeling1", 6, 'discrete')) +
  geom_vline(xintercept=0, color="red", size=0.5, linetype="dashed") +
  theme_aux

ggsave("figures/ncells_by_lez_size_rel.png", width=5, height=3)

df %>% 
  filter(lez_5km_itf == 0 & pol > 0 & is_city == 1) %>% 
  select(timetotreat, is_lez, lez_0_1, lez_1_3, lez_3_5,
         lez_not_spill, id_city, log_pol, area_quart) %>% 
  pivot_longer(
    cols = c("is_lez", "lez_0_1", "lez_1_3", "lez_3_5", 
             "lez_not_spill", "id_city"), 
    names_to = "typeof",      
    values_to = "zone"
  ) %>% 
  filter(zone==1) %>% 
  select(-zone) %>% 
  group_by(timetotreat, typeof, area_quart) %>% 
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>% 
  mutate(
    typeof = case_when(
      typeof == 'is_lez' ~ "Low Emission Zone",
      typeof == 'lez_0_1' ~ "Adjacent Cells",
      typeof == 'lez_1_3' ~ "1-3km Ring",
      typeof == 'lez_3_5' ~ "3-5km Ring",
      typeof == 'lez_not_spill' ~ ">5km Zones"
    )
  )%>% 
  mutate(typeof = fct_relevel(
    typeof, "Low Emission Zone", "Adjacent Cells", "1-3km Ring", 
    "3-5km Ring", ">5km Zones")
  ) %>% 
  drop_na() %>% 
  ggplot(aes(x = timetotreat, y = n, fill=typeof)) +
  geom_area(linewidth=0.6) +
  facet_wrap(.~area_quart) +
  theme_bw() + 
  xlab("Time to Low Emission Zone") + ylab("N. of Grid Cells (km2)") +
  scale_x_continuous(n.breaks = 15) +
  # scale_color_paletteer_d("ggthemes::Tableau_10") +
  scale_fill_brewer(palette="Set2")  +
  # facet_wrap(.~is_city)+
  # scale_color_manual(values = wes_palette("Darjeeling1", 6, 'discrete')) +
  geom_vline(xintercept=0, color="red", size=0.5, linetype="dashed") +
  theme_aux

ggsave("figures/ncells_by_area_size.png", width=5, height=3)

keep(df, theme_aux, sure=T)
gc()

## spillovers distance to lez ####

df %>% 
  filter(!is.infinite(pol) & km_to_lez > 0 & lez_5km_itf == 0 & pol > 0) %>% 
  mutate(
    area_quart=replace_na(area_quart, "Outside City")
  ) %>% 
  ggplot(aes(x=log_lez_d, y=log_pol)) +
  geom_point(aes(x=log_lez_d, y=log_pol), alpha=.5)+
  geom_smooth(method='lm') + 
  theme_bw()+
  theme_aux +
  facet_wrap(.~area_quart) +
  xlab("Distance to LEZ") +
  ylab("Log (PM2.5)") + 
  scale_fill_viridis_c()
ggsave("figures/gradient_by_area.png", width=5, height=3) 


df %>% 
  filter(!is.infinite(pol) & km_to_lez > 0 & lez_5km_itf == 0 & pol > 0) %>% 
  ggplot(aes(x=log_lez_d, y=log_pol)) +
  geom_point(aes(x=log_lez_d, y=log_pol), alpha=.5)+
  geom_smooth(method='lm') + 
  theme_bw()+
  theme_aux +
  facet_wrap(.~lez_size_rel) +
  xlab("Distance to LEZ") +
  ylab("Log (PM2.5)") + 
  scale_fill_viridis_c()
ggsave("figures/gradient_by_lez_area.png", width=5, height=3) 

# validation of research design ####

df %>% 
  filter(!is.infinite(pol) & lez_5km_itf == 0 & pol > 0) %>% 
  filter(timetotreat < 0) %>% 
  group_by(id_lez_5km) %>% 
  summarise(
    pop = mean(pop, na_rm=T),
    temp = mean(mean_temp, na_rm=T),
    prec = mean(mean_prec, na_rm=T),
    wind = mean(mean_wind, na_rm=T),
    lat = mean(latitude, na_rm=T),
    rugg = mean(mean_ruggedness, na_rm=T),
    cc = mean(coast_dist, na_rm=T),
    water = mean(river_dist, na_rm=T),
    yeartreat = mean(yeartreat, na_rm=T)
  ) %>% 
  left_join(
    df %>% select(id_lez_5km, country) %>% distinct()
  ) %>% 
  feols(
    yeartreat ~ pop + temp + prec + wind + lat + rugg + cc + water | country,
    cluster = ~ country
  ) %>% 
  etable(
  fitstat=c('n', 'ar2', 'r2'),
  digits=3,
  title='Determinants of When LEZ were Implemented',
  depvar=F,
  se.below = T,
  fontsize='small',
  tex = T,
  digits.stats=3,
  # vcov = "hetero",
  label='tab:pred_timetotreat',
  file = "tables/v2/pred_timetotreat.tex",
  # notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. IID standard-errors in parentheses. Fixed Effects: Country. Temporal coverage: 2007-2022.",
  replace = T
)

df %>% 
  filter(!is.infinite(pol) & lez_5km_itf == 0 & pol > 0) %>% 
  filter(timetotreat < 0) %>% 
  group_by(id_lez_5km) %>% 
  summarise(
    pop = mean(pop, na_rm=T),
    log_pol=mean(log_pol, na_rm=T),
    temp = mean(mean_temp, na_rm=T),
    prec = mean(mean_prec, na_rm=T),
    wind = mean(mean_wind, na_rm=T),
    lat = mean(latitude, na_rm=T),
    rugg = mean(mean_ruggedness, na_rm=T),
    cc = mean(coast_dist, na_rm=T),
    water = mean(river_dist, na_rm=T),
    yeartreat = mean(yeartreat, na_rm=T)
  ) %>% 
  left_join(
    df %>% select(id_lez_5km, country) %>% distinct()
  ) %>% 
  feols(
    log_pol ~ yeartreat + pop + temp + prec + wind + lat + rugg + cc + water | country,
    cluster = ~ country
  ) %>% 
  etable(
    fitstat=c('n', 'ar2', 'r2'),
    digits=3,
    title='PM2.5 vs Year of Implementation',
    depvar=F,
    se.below = T,
    fontsize='small',
    # keep='yeartreat',
    tex = T,
    vcov='cluster',
    digits.stats=3,
    label='tab:pred_logpol',
    file = "tables/v2/pred_logpol.tex",
    #notes="Notes: Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. IID standard-errors in parentheses. Fixed Effects: Country. Temporal coverage: 2007-2022.",
    replace = T
  )

# twfe ####

setFixest_dict(
  "is_lez:post"= "LEZ",
  "lez_0_1:post"= "Adjacent",
  "lez_1_3:post"= "Spillover 1-3km",
  "lez_3_5:post"= "Spillover 3-5km",
  "lez_5_8:post"= "Spillover 5-8km",
  "lez_not_spill:post"= "Spillover >5km",
  "post:lez_0_1"= "Adjacent",
  "post:lez_1_3"= "Spillover 1-3km",
  "post:lez_3_5"= "Spillover 3-5km",
  "post:lez_not_spill"= "Spillover >5km",
  "post:lez_5_8"="Spillover 5-8km",
  "post:min_distance_km"="Distance x Post",
  "log_lez_d"='Log(Distance)',
  "log_lez_d:post"='Log(Distance) x Post',
  "timetotreat"="Time to LEZ",
  "id_lez"="LEZ",
  "id_city"="City",
  "km_to_lez"="Km to LEZ",
  "km_to_lez:post"="Km to LEZ x Post",
  "year"="Year",
  "log_pop"="Log(p"
)

## exploration ####

df = df %>% 
  mutate(
    lez_not_spill = case_when(
      lez_not_spill == 1 | lez_3_5 == 1 ~ 1,
      T ~ 0
    )
  )

df %>% 
  select(id_lez) %>% 
  unique() %>% 
  dim()

df %>% 
  select(id_city) %>% 
  unique() %>% 
  dim()

df %>% 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1) %>% 
  select(id_lez) %>% 
  unique() %>% 
  dim()

df %>% 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1) %>% 
  select(id_city) %>% 
  unique() %>% 
  dim()

fechado = df %>% 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1) %>% 
  select(city_name, yeartreat) %>% distinct() %>% 
  arrange(city_name)

tabla_1 <- fechado[1:20, ]
tabla_2 <- fechado[21:41, ]
tabla_3 <- fechado[42:62, ]

latex_1 <- kable(tabla_1, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 7)
latex_2 <- kable(tabla_2, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 7)
latex_3 <- kable(tabla_3, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"), font_size = 7)

cat(latex_1)
cat(latex_2)
cat(latex_3)

filtered = df %>% 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1) %>% 
  mutate(
    is_lez = as.factor(is_lez),
    lez_0_1 = as.factor(lez_0_1),
    lez_1_3 = as.factor(lez_1_3),
    lez_not_spill = as.factor(lez_not_spill),
    mean_wind = mean_wind*10,
    mean_prec = mean_prec*100,
    coast_dist = coast_dist*100,
    river_dist = river_dist*100
  )

datasummary(
   mean * (is_lez + lez_0_1 + lez_1_3 + lez_not_spill) ~ log_pol+ 
     log_pop+ mean_temp+mean_prec+mean_wind+mean_elevation+coast_dist
   +river_dist, data=filtered, output='latex')

## static - all ####

vcov_arg = conley(3, distance='triangular')

df = df %>% 
  mutate(
    lez_not_spill = case_when(
      lez_not_spill == 1 | lez_3_5 == 1 ~ 1,
      T ~ 0
    )
  )

twfe_static_1 = feols(
  log_pol ~ is_lez*post | year + id_lez + id_city,
  vcov=vcov_arg,
  # weights = ~pop,
  data=df %>% 
    # remove cells with interference, water and cities with multiple lezs (rotterdam - hague, dusseldorf, westfalia area)
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1 & lez_0_1 == 0 & lez_1_3 == 0)
)

twfe_static_2 = feols(
  log_pol ~ is_lez*post + lez_0_1*post | year + id_lez + id_city,
  vcov=vcov_arg,
  # weights = ~pop,
  data=df %>% 
    # remove cells with interference, water and cities with multiple lezs (rotterdam - hague, dusseldorf, westfalia area)
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1  & lez_1_3 == 0)
)

twfe_static_3 = feols(
  log_pol ~ is_lez*post + lez_0_1*post + lez_1_3*post | year + id_lez + id_city,
  vcov=vcov_arg,
  # weights = ~pop,
  data=df %>% 
    # remove cells with interference, water and cities with multiple lezs (rotterdam - hague, dusseldorf, westfalia area)
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1)
)

twfe_static_4 = feols(
  log_pol ~ is_lez*post + lez_0_1*post + lez_1_3*post + log_pop + csw(log_pop, ..temp + coast_dist + river_dist + mean_ruggedness) | year + id_lez + id_city,
  vcov=vcov_arg,
  # weights = ~pop,
  data=df %>% 
    # remove cells with interference, water and cities with multiple lezs (rotterdam - hague, dusseldorf, westfalia area)
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1)
)

etable(
  twfe_static_1, twfe_static_2, twfe_static_3, twfe_static_4,
  keep="(LEZ|Adj|Spill|spill)",
  digits=3,
  fitstat=list("N", "ar2"),
  label='tab:twfe_spillovers_static',
  group=list(
    "Density"=c("log_pop"),
    "Other Controls"=c("mean_temp", "mean_wind", "mean_prec", "coast_dist")),
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 3km of the LEZ border. Interferred cells are discarded from the sample. Other controls are: temperature, wind, precipitation, coast distance and terrain ruggedness.",
  # file = "tables/v2/twfe_spillovers_static.tex",
  replace=T
)


df |> 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1 & timetotreat %in% seq(-7, 9)) %>%  
  fixest::feols(
    log_pol ~ i(timetotreat, is_lez, -1) +  
      i(timetotreat, lez_0_1, -1) + 
      i(timetotreat, lez_1_3, -1) +  
      log_pop + mean_ruggedness + mean_wind + coast_dist  + mean_temp + latitude + mean_prec
    | id_city + timetotreat + year + cell_id, 
    vcov = vcov_arg 
  ) %>%  
  broom::tidy() |> 
  mutate(
    matches = str_match(term, "::(-?\\d+):([^:]+)"),
    time = as.integer(matches[,2]),
    variable = matches[,3]
  ) |> 
  select(time, estimate, std.error, variable) |> 
  add_row(time = -1, estimate = 0, std.error = 0, variable = "is_lez") |> 
  add_row(time = -1, estimate = 0, std.error = 0, variable = "lez_0_1") |> 
  add_row(time = -1, estimate = 0, std.error = 0, variable = "lez_1_3") |> 
  mutate(
    ub = estimate + 1.96 * std.error,
    lb = estimate - 1.96 * std.error,
    variable = case_when(
      variable == "is_lez" ~ "LEZ",
      variable == "lez_0_1" ~ "Adjacent Cells",
      variable == "lez_1_3" ~ "1-3km Spillover",
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(variable)) |> 
  mutate(
    variable = factor(variable, levels = c("LEZ", "Adjacent Cells", "1-3km Spillover"))
  ) |> 
  ggplot(aes(x = time, y = estimate, color = variable, group = variable)) +
  geom_vline(xintercept = -0.5, color = "black", size = 0.3) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  geom_line(position = position_dodge(width = 0.6), linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
                width = 0.2,
                position = position_dodge(width = 0.6),
                linewidth = 0.4,
                alpha = 0.8) + 
  scale_x_continuous(n.breaks = 9) + 
  scale_y_continuous(n.breaks = 9) + 
  theme_bw() +
  theme_aux + 
  labs(x = "Time to Treatment", y = "Estimate", color = NULL)
ggsave("figures/dynamic_twfe_long.png", width=7, height=3.5)

library(did)
library(digest)

aux = df |> 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1) %>% 
  mutate(
    yeartreat=case_when(
      is_lez == 1 ~ yeartreat,
      T ~ 0
    )
  )

cs = att_gt(
  yname='log_pol',
  tname='year',
  idname=c("cell_id"),
  gname='yeartreat',
  # xformla = ~ log_pop + ,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux
)

cs

cs_dyn <- aggte(cs, type = "dynamic", na.rm=T)
cs_dyn
did::ggdid(cs_dyn)


## static dist - all ####

twfe_static_dist_1 = feols(
  log_pol ~ log_lez_d*post +..temp + coast_dist + river_dist + mean_ruggedness + csw0(log_pop*post) | year + id_lez + id_city,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 &  n_lezs == 1 & is_city == 1)
)

twfe_static_dist_2 = feols(
  log_pop ~ log_lez_d*post + csw0(..temp + coast_dist + river_dist + mean_ruggedness) | year + id_lez + id_city,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 &  n_lezs == 1 & is_city == 1)
)

etable(
  list("1"=twfe_static_dist_2),
  digits=3,
  fitstat=list("N", "ar2"),
  label='tab:twfe_spillovers_static_dist',
  tex=T,
  # drop="post",
  se.row=F,
  fontsize = "footnotesize",
  group=list(
    # "Dens. Control"=c("log_pop"),
    # }"Pollution Control"=c("log_pol"),
    "Other Controls"=c("mean_temp", "mean_wind", "mean_prec", "coast_dist", 'mean_ruggedness', 'river_dist')),
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Other controls are: temperature, wind, precipitation, coast distance and terrain ruggedness.",
  # file = "tables/v2/twfe_spillovers_static_dist.tex",
  replace=T
)

## static by density ####

twfe_static_area = feols(
  log_pol ~ is_lez*post + lez_0_1*post + lez_1_3*post + 
    lez_3_5*post + log_pop + ..temp | year + id_city + id_lez,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city==1),
  split=~area_quart
)

etable(
  twfe_static_area,
  keep="(LEZ|Adj|Spill|spill)",
  digits=3,
  fitstat=list("N", "R2"),
  label='tab:twfe_spillovers_static_dens',
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Timy varying controls are population, temperature, wind, and precipitation variables.",
  file = "tables/v2/twfe_spillovers_static_dens.tex",
  replace=T
)

## static dist by density ####

twfe_static_dist_dens = feols(
  log_pol ~ log_lez_d*post + log_pop + ..temp | year + id_lez + id_city,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 &  n_lezs == 1 & is_city==1 & km_to_lez > 0),
  split=~area_quart
)

etable(
  twfe_static_dist_dens,
  digits=3,
  drop='post',
  fitstat=list("N", "ar2"),
  label='tab:twfe_spillovers_static_dist_dens',
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  group=list("Time Varying controls"=c("log_pop", "mean_temp", "mean_wind", "mean_prec")),
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Timy varying controls are population, temperature, wind, and precipitation variables.",
  # file = "tables/v2/twfe_spillovers_static_dist_dens.tex",
  replace=T
)

## static by lez ####

twfe_static_lez_q1 = feols(
  log_pol ~ is_lez*post + log_pop + ..temp | year + id_city + id_lez,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & lez_size_rel == "Q1")
)

twfe_static_lez_q2 = feols(
  log_pol ~ is_lez*post +lez_0_1*post + log_pop + ..temp | year + id_city + id_lez,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & lez_size_rel == "Q2")
)

twfe_static_lez_q3 = feols(
  log_pol ~ is_lez*post +lez_0_1*post + lez_1_3*post + log_pop + ..temp | year + id_city + id_lez,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & lez_size_rel == "Q3")
)

twfe_static_lez_q4 = feols(
  log_pol ~ is_lez*post +lez_0_1*post + lez_1_3*post + lez_3_5*post + log_pop + ..temp | year + id_city + id_lez,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & lez_size_rel == "Q4")
)

etable(
  twfe_static_lez_q1, twfe_static_lez_q2, twfe_static_lez_q3, twfe_static_lez_q4,
  keep="(LEZ|Adj|Spill|spill)",
  digits=3,
  fitstat=list("N", "R2"),
  label='tab:twfe_spillovers_static_lez',
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Timy varying controls are population, temperature, wind, and precipitation variables.",
  file = "tables/v2/twfe_spillovers_static_lez.tex",
  replace=T
)

## static dist by lez ####

twfe_static_dist_lez = feols(
  log_pol ~ log_lez_d*post + log_pop + ..temp | year + id_lez + id_city,
  vcov=vcov_arg,
  data=df %>% 
    filter(pol > 0 & lez_5km_itf == 0 &  n_lezs == 1 & is_city == 1 & km_to_lez > 0),
  split=~lez_size_rel
)

etable(
  twfe_static_dist_lez,
  digits=3,
  drop='post',
  fitstat=list("N", "ar2"),
  label='tab:twfe_spillovers_static_dist_lez',
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  group=list("Time Varying controls"=c("log_pop", "mean_temp", "mean_wind", "mean_prec")),
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Timy varying controls are population, temperature, wind, and precipitation variables.",
  file = "tables/v2/twfe_static_dist_lez.tex",
  replace=T
)


# robustness ####

## time to treat ####

twfe_static_rob1 = feols(
  log_pol ~ is_lez*post + 
    csw0(lez_0_1*post, lez_1_3*post, lez_3_5*post
    ) + log_pop + ..temp| year + id_lez + id_city,
  vcov=vcov_arg,
  data=df %>% 
    # remove cells with interference, water and cities with multiple lezs (rotterdam - hague, dusseldorf, westfalia area)
    filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1 & timetotreat %in% seq(-5, 5))
)

etable(
  twfe_static_rob1,
  keep="(LEZ|Adj|Spill|spill)",
  digits=3,
  fitstat=list("N", "ar2"),
  label='tab:twfe_spillovers_static',
  group=list("Time Varying controls"=c("log_pop", "mean_temp", "mean_wind", "mean_prec")),
  tex=T,
  se.row=F,
  fontsize = "footnotesize",
  depvar = F,
  title = "Two Way Fixed Effects Estimation of LEZ with Spillovers",
  notes="Notes: Conley (3km) standard-errors in parentheses. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1. Temporal coverage: 2007-2022. Control group are grid cells farther away than 5km of the LEZ border. Interferred cells are discarded from the sample. Timy varying controls are population, temperature, wind, and precipitation variables. Time to treat goes from -5 to 5.",
  file = "tables/v2/twfe_spillovers_static_rob1.tex",
  replace=T
)


## callaway - sant anna ####
library(did)

aux = df |> 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1 & timetotreat %in% seq(-7, 9)) %>% 
  mutate(
    yeartreat_did=case_when(
      is_lez == 1 ~ yeartreat,
      T ~ 0
    )
  )

cs0 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat_did',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(is_lez == 1 | lez_not_spill == 1)
)

cs1 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat_did',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(lez_0_1 == 1 | lez_not_spill == 1) %>% 
    mutate(
      yeartreat_did=case_when(
        lez_0_1 == 1 ~ yeartreat,
        T ~ 0
      )
    )
)

cs2 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  xformla = ~ log_pop,
  gname='yeartreat_did',
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(lez_1_3 == 1 | lez_not_spill == 1) %>% 
    mutate(
      yeartreat_did=case_when(
        lez_1_3== 1 ~ yeartreat,
        T ~ 0
      )
    )
)
# is_lez vs not spill
cs0agg <- aggte(cs0, type = "simple", na.rm=T)
# lez_0_1 vs not spill
cs1agg <- aggte(cs1, type = "simple", na.rm=T)_:
# lez_1_3 vs not spill
cs2agg <- aggte(cs2, type = "simple", na.rm=T)

cs0agg
cs1agg
cs2agg


aux = df |> 
  filter(pol > 0 & lez_5km_itf == 0 & n_lezs == 1 & is_city == 1 & timetotreat %in% seq(-5, 5)) %>% 
  mutate(
    yeartreat_did=case_when(
      is_lez == 1 ~ yeartreat,
      T ~ 0
    )
  )

cs0 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat_did',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(is_lez == 1 | lez_not_spill == 1)
)

cs1 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat_did',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(lez_0_1 == 1 | lez_not_spill == 1) %>% 
    mutate(
      yeartreat_did=case_when(
        lez_0_1 == 1 ~ yeartreat,
        T ~ 0
      )
    )
)

cs2 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  xformla = ~ log_pop,
  gname='yeartreat_did',
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=aux %>% 
    filter(lez_1_3 == 1 | lez_not_spill == 1) %>% 
    mutate(
      yeartreat_did=case_when(
        lez_1_3== 1 ~ yeartreat,
        T ~ 0
      )
    )
)
# is_lez vs not spill
cs0agg <- aggte(cs0, type = "simple", na.rm=T)
# lez_0_1 vs not spill
cs1agg <- aggte(cs1, type = "simple", na.rm=T)
# lez_1_3 vs not spill
cs2agg <- aggte(cs2, type = "simple", na.rm=T)

cs0agg
cs1agg
cs2agg


cs8 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=df %>% 
    filter(pol > 0 & is_city == 1 & lez_5km_itf == 0 & timetotreat %in% seq(-5, 5) & 
             n_lezs == 1 & is_lez == 0 & lez_0_1 == 0) %>% 
    mutate(
      yeartreat=case_when(
        lez_1_3 == 1 ~ yeartreat,
        T ~ 0
      )
    )
)

cs9 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=df %>% 
    filter(pol > 0 & is_city == 1 & lez_5km_itf == 0 & timetotreat %in% seq(-5, 5) &
             (lez_1_3 == 1 | lez_not_spill==1) & n_lezs == 1) %>% 
    mutate(
      yeartreat=case_when(
        lez_1_3 == 1 ~ yeartreat,
        T ~ 0
      )
    )
)

cs10 = att_gt(
  yname='log_pol',
  tname='year',
  idname="cell_id",
  gname='yeartreat',
  xformla = ~ log_pop,
  allow_unbalanced_panel = T,
  panel = T,
  bstrap=F,
  control_group = "notyettreated",
  data=df %>% 
    filter(pol > 0 & is_city == 1 & lez_5km_itf == 0 & timetotreat %in% seq(-5, 5) &
             (lez_1_3 == 1 | lez_3_5==1) & n_lezs == 1) %>% 
    mutate(
      yeartreat=case_when(
        lez_1_3 == 1 ~ yeartreat,
        T ~ 0
      )
    )
)

# lez 1-3 vs all
cs8agg <- aggte(cs8, type = "simple", na.rm=T)
# lez 1-3 vs outer
cs9agg <- aggte(cs9, type = "simple", na.rm=T)
# lez 1-3 vs 3_5
cs10agg <- aggte(cs10, type = "simple", na.rm=T)

# Función para crear tabla en formato LaTeX
crear_tabla_latex <- function(filas) {
  # Verificar que todas las filas tienen la misma longitud
  longitudes <- sapply(filas, function(x) length(x$valores$estimaciones))
  if (length(unique(longitudes)) != 1) {
    stop("Todas las filas deben tener el mismo número de estimaciones y errores estándar.")
  }
  
  # Crear el encabezado de la tabla
  n_columnas <- longitudes[1]
  tabla <- "\\begin{tabular}{l" 
  tabla <- paste0(tabla, paste(rep("c", n_columnas), collapse = ""), "}\n")
  tabla <- paste0(tabla, "\\hline\n")
  
  # Agregar filas
  for (fila in filas) {
    # Redondear estimaciones y errores estándar
    estimaciones_redondeadas <- round(fila$valores$estimaciones, 3)
    errores_redondeados <- round(fila$valores$errores, 3)
    
    # Agregar el nombre de la fila
    tabla <- paste0(tabla, fila$nombre, " & ")
    
    # Crear la primera fila con estimaciones
    estimaciones <- paste0("$", estimaciones_redondeadas, "$", collapse = " & ")
    tabla <- paste0(tabla, estimaciones, " \\\\\n")
    
    # Crear la segunda fila con errores estándar
    tabla <- paste0(tabla, " & ")
    errores <- paste0("($", errores_redondeados, "$)", collapse = " & ")
    tabla <- paste0(tabla, errores, " \\\\\n")
  }
  
  # Finalizar la tabla
  tabla <- paste0(tabla, "\\hline\n")
  tabla <- paste0(tabla, "\\end{tabular}")
  
  return(tabla)
}
# Ejemplo de uso
# Crear filas
filas <- list(
  list(
    nombre = "LEZ vs:",
    valores = list(
      estimaciones = c(cs0agg$overall.att, cs1agg$overall.att, cs2agg$overall.att, cs3agg$overall.att),
      errores = c(cs0agg$overall.se,  cs1agg$overall.se, cs2agg$overall.se, cs3agg$overall.se)
    )
  ),
  list(
    nombre = "Adjacent vs:",
    valores = list(
      estimaciones = c(cs4agg$overall.att, cs5agg$overall.att, cs6agg$overall.att, cs7agg$overall.att),
      errores = c(cs4agg$overall.se,  cs5agg$overall.se, cs6agg$overall.se, cs7agg$overall.se)
    )
  ),
  list(
    nombre = "1-3Km vs:",
    valores = list(
      estimaciones = c(cs8agg$overall.att, cs9agg$overall.att, cs10agg$overall.att, 0),
      errores = c(cs8agg$overall.se,  cs9agg$overall.se, cs10agg$overall.se, 0)
    )
  )
)

# Generar tabla LaTeX
tabla_latex <- crear_tabla_latex(filas)

# Imprimir tabla LaTeX
cat(tabla_latex)

a %>% select(City=city, Year=yeartreat, Area=area) %>% st_drop_geometry() %>% arrange(City)
