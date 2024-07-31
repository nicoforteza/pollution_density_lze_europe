# Pollution, Density and Low Emission Zones: European Evidence

## How to Replicate the results

For data generation: ```6_between.py```, ```7_within.py```, ```8_uar.py``` and ```9_lze_grid.py```

1) Between (OLS + IV): ```10_clean.py``` with function ```clean_between``` --> ```analysis_between.R```
      for regressions using ```cities_clean.pqt```
2) Within (OLS + IV):  ```10_clean.py``` with function ```clean_within``` --> ```analysis_within.R```
      for regressions using ```cities_within.pqt```
3) Effect of Low Emission Zones:
   1) For the between sample: ```analysis_diff_between.R``` for regressions using ```cities_clean_uar.pqt```
   2) For the between sample:  ```10_clean.py``` with function ```clean_within_lze``` --> ```analysis_diff_within.R``` for regressions using ```within_clean_lze.pqt.gzip```


## Data:

### Main Variables

 - Population:
    - Panel: [GHSL](https://ghsl.jrc.ec.europa.eu/ghs_pop2023.php)
    - Final Version: [Oak Ridge Lab](https://landscan.ornl.gov/)
    - Grid comparison [here](https://www.popgrid.org/data-docs-table1)
 - Pollution:
    - Panel: [SEDAC](https://sedac.ciesin.columbia.edu/data/set/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03/data-download)
    - NO2 Panel: [Washington University](https://zenodo.org/record/5424752#.YesGatHMJaQ)
    - Final Version PM2.5: [EEA](https://www.eea.europa.eu/en/datahub/datahubitem-view/938bea70-07fc-47e9-8559-8a09f7f92494?activeAccordion=1087827%2C1084794%2C1084806%2C1084809%2C1084785%2C1084813)
 - Population and cities: 
   - [GHSL](https://ghsl.jrc.ec.europa.eu/download.php?ds=DUC) and [GADM](https://gadm.org/download_world.html)

### Instruments:

 - Earthquakes: 
   - [Final](http://hazard.efehr.org/en/web-services/hazard-map-data/)
   - [SEDAC](https://sedac.ciesin.columbia.edu/data/set/ndh-earthquake-frequency-distribution)
 - Soil quality: 
   - [European soil database](https://esdac.jrc.ec.europa.eu/eform/test-dataset/confirm?entityform_id=73944)
   - [Harmonized Soil Database](https://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v12/en/)
 - Historical settlement density: 
   - [Here](https://pubmed.ncbi.nlm.nih.gov/27271481/)
   - Spatial grid [here](https://public.yoda.uu.nl/geo/UU01/8K9D7F.html)

### Controls
These are factors that can also influence the climate.
Control for potential observable factors that may be correlated with population density and pollution

1) [Climatological](https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=form)
2) [Geographical](https://www.naturalearthdata.com/downloads/10m-physical-vectors/)
3) [Green space area in the city](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf?t=1683540422)
4) [Power plants (coal-fired and other highly pollutants)](https://datasets.wri.org/dataset/globalpowerplantdatabase)
5) [Ruggedness](https://scholar.harvard.edu/sites/scholar.harvard.edu/files/nunn/files/ruggedness.pdf): grid-cell average difference in elevation between a point and the terrain surrounding it.

![Europe Pollution](figures/eurpol.png "Europe Pollution")
