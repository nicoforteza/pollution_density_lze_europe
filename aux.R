# Cargar librerías necesarias
library(sf)
library(dplyr)
library(ggplot2)
library(kableExtra)
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

df = read_parquet("data/gdp/0_25deg/gdp_dens.pqt") %>% 
  select(-`__index_level_0__`) %>% 
  filter(gdp > 0 & dens > 0)

df %>% 
  mutate(gdplog=log(gdp), denslog=log(dens)) %>% 
  ggplot(aes(x=denslog, y=gdplog)) +
  geom_point(color='lightgray') + 
  geom_smooth(method='lm', color='red', linewidth=0.5) +
  xlab("Log(Density)") + 
  ylab("Log(GDP)") +
  theme_bw() +
  theme_aux 

ggsave("figures/gdp_dens.png", dpi = 300, width = 4, height=2)

summary(feols(log(gdp) ~ log(dens), data=df))

# data loading ######################## 

source("analysis_diff_prepare.R")

# Ver estructura del dataset
print(st_geometry_type(data))
print(head(data))

# Calcular el área de cada celda (suponiendo que los datos están en un sistema métrico)
auxdf = df %>% 
  filter(timetotreat == 0 & is_city == 1) %>% 
  mutate(area=1)

# Agregar información por ciudad
summary_table <- auxdf %>%
  group_by(city_name) %>%
  summarise(
    total_area = sum(area, na.rm = TRUE),
    lez_area = sum(area[is_lez == 1], na.rm = TRUE),
    lez_ratio = lez_area / total_area,
    total_pop = sum(pop, na.rm = TRUE),
    lez_pop = sum(pop[is_lez == 1], na.rm = TRUE),
    lez_density = lez_pop / lez_area, # Densidad de población en LEZ
    total_density = total_pop / total_area # Densidad de población total
  )


datasummary(All(summary_table %>% 
                  select(-id_city)) ~ (Mean + SD),
            data = summary_table %>% 
              select(-id_city), 
            sparse_header = TRUE, output='latex')


# Convertir la tabla a formato LaTeX
latex_table <- summary_table %>%
  mutate(across(where(is.numeric), round, 2)) %>% # Redondear valores numéricos
  kable(format = "latex", booktabs = TRUE, caption = "Descripción de Zonas de Bajas Emisiones por Ciudad") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Guardar la tabla en un archivo LaTeX
writeLines(latex_table, "summary_table.tex")

# KDE plot del tamaño de las LEZ por ciudad
ggplot(summary_table, aes(x = lez_area)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución del Tamaño de Zonas de Bajas Emisiones",
       x = "Área de la zona LEZ (m²)",
       y = "Densidad") +
  theme_minimal()
