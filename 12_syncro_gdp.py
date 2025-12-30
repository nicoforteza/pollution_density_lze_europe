import os

os.environ['USE_PYGEOS'] = '0'

import rioxarray

from config import *
from rasterio.transform import from_origin
import geopandas as gpd
import numpy as np
import rasterio
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns
from rasterio.enums import Resampling


# create the distance of each cell to the closes LEZ and city
eur = get_europe()
base_year = 2020  # we need a reference year, take 2020
eurproj = eur.to_crs(4326)
col_name = "predicted_GCP_current_USD"

# cities original as reference to clip all rasters
cities = get_cities(base_year).rename(columns={'year': 'city_ref_year'}).to_crs(4326)

pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(base_year)).sel(band=1)
pop = pop.rio.reproject("EPSG:4326")
pop = pop.rio.write_nodata(np.nan)
# cpop = pop.rio.clip(eur.geometry)  # rio.clip(cities.geometry)

gdf = pd.read_csv(r"data/gdp/0_25deg/final_GDP_0_25deg_postadjust_pop_dens_0_adjust.csv")
gdf = gdf[gdf.year == 2020]
gdf = gpd.GeoDataFrame(gdf[
   [col_name, "latitude", "longitude"]],
                      geometry=gpd.points_from_xy(gdf.longitude, gdf.latitude),
                      crs="EPSG:4326")


def geodataframe_to_raster(gdf, column, resolution=0.25):
    # Obtener los límites geográficos
    minx, miny, maxx, maxy = gdf.total_bounds

    # Calcular dimensiones del raster
    width = int(np.ceil((maxx - minx) / resolution))
    height = int(np.ceil((maxy - miny) / resolution))

    # Crear raster vacío
    raster = np.full((height, width), np.nan)

    # Crear transformación para rasterio
    transform = from_origin(minx, maxy, resolution, resolution)

    # Interpolar valores
    for index, row in gdf.iterrows():
        # Calcular coordenadas de píxel
        col = int(np.floor((row.geometry.x - minx) / resolution))
        row_idx = int(np.floor((maxy - row.geometry.y) / resolution))

        # Asignar valor al raster
        if 0 <= row_idx < height and 0 <= col < width:
            raster[row_idx, col] = row[column]

    # Save raster
    with rasterio.open(
            r"data/gdp/0_25deg/gdf.tif", "w",
            driver="GTiff",
            height=raster.shape[0],
            width=raster.shape[1],
            count=1,
            dtype=raster.dtype,
            crs="EPSG:4326",
            transform=transform
    ) as dst:
        dst.write(raster, 1)

    ras = rioxarray.open_rasterio( r"data/gdp/0_25deg/gdf.tif")

    return ras, transform


gdp, transform = geodataframe_to_raster(gdf, col_name)
gdpr = gdp.rio.clip(eurproj.geometry, all_touched=True)

popr = pop.rio.reproject_match(gdpr, nodata=np.nan, resampling=Resampling.bilinear)
popr = popr.rio.clip(eurproj.geometry, all_touched=True)

f, ax = plt.subplots(1, 1, figsize=(13, 13))
popr.plot.imshow(ax=ax, add_colorbar=False, robust=True, cmap='inferno')
# eur.boundary.plot(ax=ax, color='lightgreen', linewidth=0.15)
ax.set_axis_off();
ax.set_title("");

vals = pd.DataFrame.from_dict({"gdp": gdpr.values.ravel(),
                               "dens": popr.values.ravel()},
                              orient='index').dropna(axis=1).T
vals.to_parquet(r"data/gdp/0_25deg/gdp_dens.pqt")

