import os

os.environ['USE_PYGEOS'] = '0'
import geopandas as gpd
from xarray.core.dataarray import DataArray
from geocube.api.core import make_geocube
import pandas as pd
import numpy as np
import xarray
import geopandas
from rasterio.features import rasterize

cities_raw_path = r"data/cities/{year}/raw/GHS_SMOD_E{year}_GLOBE_R2023A_54009_1000_UC_V1_0.shp"
cities_clean_path = r"data/cities/{year}/clean/cities.geojson"
pop_raw_path = r"data/population/{year}/raw/GHS_POP_E{year}_GLOBE_R2023A_54009_1000_V1_0.tif"
pop_clean_path = r"data/population/{year}/clean/clipped_europe_3035.tif"
pol_raw_path = r"data/pollution/{year}/sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif"
pol_clean_path = r"data/pollution/{year}/clean/clipped_europe_3035.tif"
europe_raw_path = r"data/cities/NUTS_RG_01M_2021_3035.geojson"
name_latn_exclude = ['Guadeloupe', 'Guyane', 'Martinique', 'La Réunion', 'Mayotte']
in_sample_years = [2020]  # 2000, 2005, 2010, 2015,
projection_int = 3035
projection_str = "EPSG:3035"
discard_countries = ['TR', 'LI', 'LU']


def do_europe():
    eur = gpd.read_file(europe_raw_path)
    eur = eur[(~eur.NAME_LATN.isin(name_latn_exclude)) & (~eur.CNTR_CODE.isin(discard_countries))]
    eur = eur[eur.LEVL_CODE == 3].dissolve(by='CNTR_CODE')
    eur.to_crs(projection_int).to_file(r"data/cities/europe.geojson")


def get_europe():
    return gpd.read_file(r"data/cities/europe.geojson", engine='pyogrio')


def get_cities(year):
    year = get_prior_year(2020)
    return gpd.read_file(r"data/cities/{}/clean/cities.geojson".format(year), engine='pyogrio')


def get_prior_year(year):
    if year >= 2020:
        year = 2020
    elif (year >= 2015) and (year < 2020):
        year = 2015
    elif (year >= 2010) and (year < 2015):
        year = 2010
    elif (year >= 2005) and (year < 2010):
        year = 2005
    return year


def zonal_stats(raster: DataArray,
                vector: geopandas.GeoDataFrame,
                op_type="sum",
                name=''):

    crs = raster.rio.crs.to_epsg()
    if crs is None:
        crs = raster.rio.crs.to_string()
    vector = vector.to_crs(crs)
    # vector = vector.assign(id=list(range(vector.geometry.shape[0])))

    clipped_raster = raster.rio.clip(
        vector.geometry.values,
        vector.crs, from_disk=True) # .drop("band")
    ras_vec = make_geocube(vector_data=vector,
                           measurements=["id"],
                           like=clipped_raster,
                           fill=np.nan)
    ras_vec['stat'] = (clipped_raster.dims, clipped_raster.values,
                       clipped_raster.attrs, clipped_raster.encoding)
    grouped = ras_vec.drop("spatial_ref")

    if op_type == 'sum':
        res = grouped.groupby("id").sum().rename(
            {"stat": "{}_{}".format(op_type, name)})
    elif op_type == 'mean':
        res = grouped.groupby("id").mean().rename(
            {"stat": "{}_{}".format(op_type, name)})
    elif op_type == 'max':
        res = grouped.groupby("id").max().rename(
            {"stat": "{}_{}".format(op_type, name)})
    elif op_type == 'median':
        res = grouped.groupby("id").median().rename(
            {"stat": "{}_{}".format(op_type, name)})
    elif op_type == 'std':
        res = grouped.groupby("id").std().rename(
            {"stat": "{}_{}".format(op_type, name)})

    resdf = xarray.merge([res]).to_dataframe()

    return pd.merge(vector, resdf, on='id')


def main():
    do_europe()


if __name__ == '__main__':

    main()
    pass


