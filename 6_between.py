from config import *
import numpy as np
import rioxarray
from expden import experienced_density

eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2022, 2023)
save_path = r"data/between/{}"


def get_year_controls(y):
    y = get_prior_year(y)
    return gpd.read_file(r"data/controls/{}_clean_between.geojson".format(y))


def get_year_instruments(y):
    y = get_prior_year(y)
    return gpd.read_file(r"data/instruments/{}_clean_between.geojson".format(y))


for year in years:
    print("Between cities sample for {}".format(year))
    print("=== Merge with static controls and instruments")
    cities = get_cities(year).rename(columns={'year': 'city_ref_year'})
    st_controls = get_year_controls(year).drop("country_id", axis=1)
    st_instruments = get_year_instruments(year).drop("country_id", axis=1)
    cities = pd.merge(cities,
                      pd.merge(
                          st_controls.drop("geometry", axis=1),
                          st_instruments.drop("geometry", axis=1).drop("area", axis=1),
                          on=['id']
                      ).drop(
                          "area", axis=1
                      ),
                      on='id')
    print("=== Main variables")
    pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
    pop = pop.rio.set_nodata(np.nan)
    pop.rio.to_raster(r"data/population/{}/pop_nodata.tif".format(year))
    pop = rioxarray.open_rasterio(r"data/population/{}/pop_nodata.tif".format(year)).sel(band=1)
    pol = rioxarray.open_rasterio(r"data/pollution/{}/pol.tif".format(year)).sel(band=1)
    pol = pol.rio.set_nodata(np.nan)
    pol.rio.to_raster(r"data/pollution/{}/pol_nodata.tif".format(year))
    pol = rioxarray.open_rasterio(r"data/pollution/{}/pol_nodata.tif".format(year)).sel(band=1)
    cities = zonal_stats(pop, cities, 'sum', 'pop')
    cities = experienced_density(pop, cities)
    num_ras = pop * pol
    cities = zonal_stats(num_ras, cities, 'sum', 'exp')
    cities['exposure'] = cities['sum_exp'].values.ravel() / cities['sum_pop'].values.ravel()
    cities = cities.drop("sum_exp", axis=1)
    cities['log_dens_exp'] = np.log(cities['expden'])
    cities['log_pop_ras'] = np.log(cities['sum_pop'])
    cities['log_exp_w'] = np.log(cities['exposure'])
    print("=== Controls")
    temp = rioxarray.open_rasterio(r"data/controls/climate/{}_temp.tif".format(year)).sel(band=1)
    prec = rioxarray.open_rasterio(r"data/controls/climate/{}_wind.tif".format(year)).sel(band=1)
    wind = rioxarray.open_rasterio(r"data/controls/climate/{}_prec.tif".format(year)).sel(band=1)
    cities = zonal_stats(temp, cities, 'mean', 'temp')
    cities = zonal_stats(prec, cities, 'mean', 'prec')
    cities = zonal_stats(wind, cities, 'mean', 'wind')
    cities['lat'] = cities.to_crs(4326).centroid.y
    cities['year'] = year
    print("=== Saving")
    if not os.path.exists(save_path.format(year)):
        os.makedirs(save_path.format(year))
    cities.to_file(os.path.join(save_path.format(year), "between_v1.geojson"))



