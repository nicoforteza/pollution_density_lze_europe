import os

os.environ['USE_PYGEOS'] = '0'

import rioxarray
import functools

from rasterio.enums import Resampling
from config import *
from geocube.rasterize import rasterize_image
import geopandas as gpd
import numpy as np
import rasterio
import pandas as pd
from shapely.geometry import Point
from pyproj import Transformer
from scipy.spatial import cKDTree
from rasterio.enums import MergeAlg


lze_directory = r"data/uar/uars"
ring_size = [1e3, 3e3, 5e3, 10e3]
eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2007, 2023) # range(2007, 2023)

rasterize_image_false = rasterize_image
rasterize_image_function = functools.partial(rasterize_image, all_touched=True)
rasterize_image_function_merge = functools.partial(rasterize_image, all_touched=True, merge_alg=MergeAlg.add)


def get_weather(year, variable, target):

    varname = None
    if variable == 'temp':
        varname = "2m_temperature"
    elif variable == 'prec':
        varname = 'total_precipitation'
    elif variable == 'wind':
        varname = '10m_wind_speed'

    raw = rioxarray.open_rasterio(
        r"data/controls/climate/{}_{}.grib".format(year, varname), engine='cfgrib'
    )
    average = raw[0].copy()
    average.values[:] = np.nan

    for i in range(12):
        if i == 1:
            average.values = raw[i]
            if variable == 'temp':
                average.values = raw[i] - 273.15
        else:
            vals = raw[i].values
            if variable == 'temp':
                vals = raw[i].values - 273.15
            average.values = (average.values + vals) / 2

    average = average.rio.set_crs(4326)
    minx, miny, maxx, maxy = eurproj.total_bounds.ravel()
    tmpb = average.rio.clip_box(
        minx=minx - 2,
        miny=miny - 2,
        maxx=maxx + 2,
        maxy=maxy + 2
    )
    tmpr = tmpb.rio.reproject(
        average.rio.crs,
        shape=target.shape,
        resampling=Resampling.nearest,
    )
    tmpr = tmpr.rio.reproject_match(target, nodata=-1)
    tmpr = tmpr.rio.clip(eur.geometry.values)
    tmpr.values[tmpr.values == tmpr.rio.nodata] = 0
    return tmpr


def raster_to_df(raster, colname, not_d=True):
    data = raster.data  # Assuming single band
    height, width = raster.data.shape
    # Create arrays of row and col indices
    rows, cols = np.where(~np.isnan(data))
    # Get the coordinates for each cell
    xs, ys = rasterio.transform.xy(raster.rio.transform(), rows, cols)
    # Create a DataFrame with cell IDs and EPSG:3035 coordinates
    if not not_d:
        return pd.DataFrame({
            'cell_id': range(len(xs)),
            'x': xs,
            'y': ys,
            colname: data[rows, cols]  # Assuming your binary column is in the raster
        })
    else:
        return pd.DataFrame({
            'cell_id': range(len(xs)),
            colname: data[rows, cols]  # Assuming your binary column is in the raster
        }).set_index("cell_id")


def get_distance_to_group(df, group_name, colname):
    transformer = Transformer.from_crs("EPSG:3035", "EPSG:4326", always_xy=True)
    lons, lats = transformer.transform(df['x'].values, df['y'].values)
    df['latitude'] = lats
    df['longitude'] = lons
    geometry = [Point(x, y) for x, y in zip(df['x'], df['y'])]
    gdf = gpd.GeoDataFrame(df, geometry=geometry, crs="EPSG:3035")
    a_points = gdf[gdf[group_name] == 1][['x', 'y']].values
    all_points = gdf[['x', 'y']].values
    tree = cKDTree(a_points)
    distances, _ = tree.query(all_points)
    gdf[colname] = distances / 1000
    return gdf[['cell_id', group_name, colname, 'latitude', 'longitude']].set_index("cell_id")


def do_all():

    # create the distance of each cell to the closes LEZ and city
    base_year = 2020  # we need a reference year, take 2020

    # cities original as reference to clip all rasters
    cities = get_cities(base_year).rename(columns={'year': 'city_ref_year'})

    # population with same shape
    pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(base_year)).sel(band=1)
    pop = pop.rio.write_nodata(np.nan)
    cpop = pop.rio.clip(cities.buffer(25e3).geometry).rio.clip(eur.geometry)

    # we need: distance to city, is_city, id_city, id_city_25km, is_city_25km_collapse

    # rasterize cities
    cities['is_city'] = 1
    cc = make_geocube(
        vector_data=cities,
        measurements=["is_city"],
        resolution=cpop.rio.resolution(),
        fill=0,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='is_city').rio.reproject_match(cpop, nodata=0)

    # raster to df with cell id - cities
    print("Calculating distance to nearest city ...")
    aux = raster_to_df(cc, "is_city", not_d=False)
    print("Is city: ", aux.shape)
    df = get_distance_to_group(aux, 'is_city', 'km_to_city')
    del aux
    print("Distance to city: ", df.shape)

    # cities
    cities = cities.rename(columns={'id': 'id_city'})
    id_city = make_geocube(
        vector_data=cities,
        measurements=["id_city"],
        resolution=cpop.rio.resolution(),
        fill=-1,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='id_city').rio.reproject_match(cpop, nodata=-1)
    id_city = raster_to_df(id_city, "id_city")
    print("ID city: ", id_city.shape)
    df = pd.merge(df, id_city, left_index=True, right_index=True, how='left')
    print("First merge: ", df.shape)

    cities_buffer = cities.copy()
    cities_buffer['geometry'] = cities_buffer.buffer(25e3)
    cities_buffer = cities_buffer.rename(columns={'is_city': 'is_city_25km_collapse'})
    is_city_25km_collapse = make_geocube(
        vector_data=cities_buffer,
        measurements=["is_city_25km_collapse"],
        resolution=cpop.rio.resolution(),
        fill=0,
        rasterize_function=rasterize_image_function_merge
    ).to_array().sel(variable='is_city_25km_collapse').rio.reproject_match(cpop, nodata=0)
    is_city_25km_collapse = raster_to_df(is_city_25km_collapse, "is_city_25km_collapse")
    print("is_city_25km_collapse shape: ", is_city_25km_collapse.shape)
    df = pd.merge(df, is_city_25km_collapse, left_index=True, right_index=True, how='left')
    print("Second merge: ", df.shape)

    cities_buffer = cities_buffer.rename(columns={'id_city': 'id_city_25km'})
    id_city_25km = make_geocube(
        vector_data=cities_buffer,
        measurements=["id_city_25km"],
        resolution=cpop.rio.resolution(),
        fill=-1,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='id_city_25km').rio.reproject_match(cpop, nodata=-1)
    id_city_25km = raster_to_df(id_city_25km, "id_city_25km")
    print("id_city_25km shape: ", id_city_25km.shape)
    df = pd.merge(df, id_city_25km, left_index=True, right_index=True, how='left')
    print("Third merge: ", df.shape)

    # we need: distance to lez, is_lez, id_lez, id_lez_5km, is_city_10km_collapse
    # raster to df with cell id - lezs
    lzes = gpd.read_file(r"data/uar/uar_data_new_euro.geojson")
    lzes['is_lez'] = 1
    lzes = lzes.rename(columns={'id_uar': 'id_lez'})

    # rasterize cities
    is_lez = make_geocube(
        vector_data=lzes,
        measurements=["is_lez"],
        resolution=cpop.rio.resolution(),
        fill=0,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='is_lez').rio.reproject_match(cpop, nodata=0)

    # raster to df with cell id - lzes
    print("Calculating distance to nearest LEZ ...")
    aux = raster_to_df(is_lez, "is_lez", not_d=False)
    print("Is LEZ shape: ", aux.shape)
    distance_to_lez = get_distance_to_group(aux, 'is_lez', 'km_to_lez').drop(["latitude", 'longitude'], axis=1)
    print("Distance to LEZ shape: ", distance_to_lez.shape)
    del aux
    df = pd.merge(df, distance_to_lez, left_index=True, right_index=True, how='left')
    print("Fourth merge: ", df.shape)

    lzes = lzes.rename(columns={'id_uar': 'id_lez'})
    id_lez = make_geocube(
        vector_data=lzes,
        measurements=["id_lez"],
        resolution=cpop.rio.resolution(),
        fill=-1,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='id_lez').rio.reproject_match(cpop, nodata=-1)
    id_lez = raster_to_df(id_lez, "id_lez")
    print("ID LEZ shape: ", id_lez.shape)
    df = pd.merge(df, id_lez, left_index=True, right_index=True, how='left')
    print("Fifth merge: ", df.shape)

    lzes_buffer = lzes.copy()
    lzes_buffer['geometry'] = lzes_buffer.buffer(5e3)
    lzes_buffer = lzes_buffer.rename(columns={'is_lez': 'is_lez_5km_collapse'})
    is_lez_5km_collapse = make_geocube(
        vector_data=lzes_buffer,
        measurements=["is_lez_5km_collapse"],
        resolution=cpop.rio.resolution(),
        fill=0,
        rasterize_function=rasterize_image_function_merge
    ).to_array().sel(variable='is_lez_5km_collapse').rio.reproject_match(cpop, nodata=0)
    is_lez_5km_collapse = raster_to_df(is_lez_5km_collapse, "is_lez_5km_collapse")
    print("is_lez_5km_collapse LEZ shape: ", is_lez_5km_collapse.shape)
    df = pd.merge(df, is_lez_5km_collapse, left_index=True, right_index=True, how='left')
    print("Sixth merge: ", df.shape)

    lzes_buffer = lzes_buffer.rename(columns={'id_lez': 'id_lez_5km'})
    id_lez_5km = make_geocube(
        vector_data=lzes_buffer,
        measurements=["id_lez_5km"],
        resolution=cpop.rio.resolution(),
        fill=-1,
        rasterize_function=rasterize_image_function
    ).to_array().sel(variable='id_lez_5km').rio.reproject_match(cpop, nodata=-1)
    id_lez_5km = raster_to_df(id_lez_5km, "id_lez_5km")
    print("id_lez_5km LEZ shape: ", id_lez_5km.shape)
    df = pd.merge(df, id_lez_5km, left_index=True, right_index=True, how='left')
    print("Seventh merge: ", df.shape)

    df = df[(df.id_lez_5km > 0) | (df.id_city_25km >= 0)]
    print("Post filter shape", df.shape)
    print("Post filter head:", df.head())

    # now we can start to do each year operation
    for year in years:

        # population
        pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
        pop = pop.rio.write_nodata(-1)
        cpop = pop.rio.clip(cities_buffer.geometry).rio.clip(eur.geometry)
        yeardf = raster_to_df(cpop, 'pop')
        print("Year {} population shape:".format(year), yeardf.shape)
        yeardf.loc[yeardf['pop'] < 0, 'pop'] = np.nan
        yeardf = pd.merge(yeardf, df.copy(), how='right', left_index=True, right_index=True)
        print("Year {} first merge:", yeardf.shape)

        # pollution
        pol = rioxarray.open_rasterio(r"data/pollution/{}/pol.tif".format(year)).sel(band=1)
        pol = pol.rio.write_nodata(-1)
        cpol = pol.rio.clip(cities_buffer.geometry).rio.clip(eur.geometry)
        cpoldf = raster_to_df(cpol, "pol")
        print("Year {} polution shape:".format(year), cpoldf.shape)
        cpoldf.loc[cpoldf['pol'] < 0, 'pol'] = np.nan
        yeardf = pd.merge(cpoldf, yeardf, how='right', left_index=True, right_index=True)
        print("Year {} second merge:", yeardf.shape)

        # temperature
        temp = get_weather(year, "temp", cpop)
        yeardf = pd.merge(raster_to_df(temp, "temp"), yeardf, how='right', left_index=True, right_index=True)
        print("Year {} third merge:", yeardf.shape)

        wind = get_weather(year, "wind", cpop)
        yeardf = pd.merge(raster_to_df(wind, "wind"), yeardf, how='right', left_index=True, right_index=True)
        print("Year {} fourth merge:", yeardf.shape)

        prec = get_weather(year, "prec", cpop)
        yeardf = pd.merge(raster_to_df(prec, "prec"), yeardf, how='right', left_index=True, right_index=True)
        print("Year {} fifth merge:", yeardf.shape)

        print("Final Shape for year {}: {}".format(year, yeardf.shape))
        print("columns:", yeardf.columns)
        if not os.path.exists(r"data/within/{}".format(year)):
            os.makedirs(r"data/within/{}".format(year), exist_ok=True)
        yeardf.to_parquet(r"data/within/{}/step1_lze.pqt".format(year))


def harmonize():

    instruments = pd.read_parquet(r"data/within/instruments_lez.pqt")
    terrain = pd.read_parquet(r"data/within/static_lez.pqt")

    for year in years:
        df = pd.read_parquet(r"data/within/{}/step1_lze.pqt".format(year))
        df = pd.merge(df, instruments, how='left', left_index=True, right_index=True)
        df = pd.merge(df, terrain, how='left', left_index=True, right_index=True)
        df.to_parquet(r"data/within/{}/within_lze.pqt".format(year))


if __name__ == '__main__':

    do_all()
    harmonize()





