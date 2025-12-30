setFixest_fml(..c1 = ~ mean_temp + mean_prec + mean_wind,
              ..c2 = ~ mean_temp + mean_prec + mean_wind + latitude + mean_ruggedness + 
              pw_dist + coast_dist + river_dist,
              
              ..temp = ~ mean_temp + mean_prec + mean_wind,
              ..geo = ~ lat + mean_ruggedness,
              ..water = ~ coast_city + water_dist,
              ..pw = ~ pw_dist,
              
              ..fe = ~ country + year,
              ..fe_country = ~ country,
              ..fe_year = ~ year,
              ..fe_country_year = ~ country ^ year,
              
              ..aquif = ~ aquif1 + aquif2 + aquif3 + aquif4 + aquif5 + aquif6,
              ..hist_between = ~ log(sum_hist1800) + log(sum_hist1500) + log(sum_hist1000) + log(sum_hist100),
              ..hist = ~ sum_hist1800,
              ..eq = ~ mean_eqhz,
              ..soil = ~ nutrient_tox_1 + nutrient_tox_2 + nutrient_tox_3 + 
                nutrient_tox_4 + nutrient_tox_5 + nutrient_av_1 + nutrient_av_2 + nutrient_av_3 + 
                nutrient_av_4 + nutrient_av_5 + nutrient_ret_1 + nutrient_ret_2 + nutrient_ret_3 + 
                nutrient_ret_4 + nutrient_ret_5 + nutrient_root_1 + nutrient_root_2 + nutrient_root_3 + 
                nutrient_root_4 + nutrient_root_5 + nutrient_ox_1 + nutrient_ox_2 + nutrient_ox_3 + 
                nutrient_ox_4 + nutrient_ox_5 + nutrient_sa_1 + nutrient_sa_2 + nutrient_sa_3 + 
                nutrient_sa_4 + nutrient_sa_5 + nutrient_work_1 + nutrient_work_2 + nutrient_work_3 + 
                nutrient_work_4 + nutrient_work_5,
              ..soil_lasso = ~ nutrient_av_1+nutrient_av_2+nutrient_av_3+nutrient_av_4+nutrient_av_5+nutrient_av_6+nutrient_av_7+nutrient_ret_1+nutrient_ret_2+nutrient_ret_3+nutrient_ret_4+nutrient_ret_5+nutrient_ret_6+nutrient_ret_7+nutrient_root_1+nutrient_root_2+nutrient_root_3+nutrient_root_4+nutrient_root_5+nutrient_root_6+nutrient_ox_1+nutrient_ox_2+nutrient_ox_3+nutrient_ox_4+nutrient_ox_5+nutrient_ox_6+nutrient_sa_1+nutrient_sa_2+nutrient_sa_3+nutrient_sa_4+nutrient_sa_5+nutrient_sa_6+nutrient_tox_1+nutrient_tox_2+nutrient_tox_3+nutrient_tox_4+nutrient_tox_5+nutrient_tox_7+nutrient_work_1+nutrient_work_2+nutrient_work_3+nutrient_work_4+nutrient_work_5+aquif1+aquif2+aquif3+aquif4+aquif5+aquif6+aquif0,
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
                nutrient_work_4 + nutrient_work_5,
              
              ..lze_t = ~ lze + lze_0_1 + lze_1_3 + lze_3_5 + lze_5_10
)

theme_aux <- theme(legend.position="bottom") +
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
