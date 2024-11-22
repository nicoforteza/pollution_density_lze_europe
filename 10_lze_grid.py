import glob
import os

os.environ['USE_PYGEOS'] = '0'
from pathlib import Path

from shapely import Point
import rioxarray
import rasterio
import geocube
import functools
import matplotlib.pyplot as plt

from rasterio.enums import Resampling
from xrspatial.utils import ngjit
from xrspatial.convolution import custom_kernel, circle_kernel
from xrspatial.focal import apply
from config import *
from shapely import Polygon, MultiLineString, MultiPolygon, LineString
from geocube.vector import vectorize
from geocube.rasterize import rasterize_image

lze_directory = r"data/uar/uars"
ring_size = [1e3, 3e3, 5e3, 10e3]
eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2007, 2023) # range(2007, 2023)

rasterize_image_false = rasterize_image
rasterize_image_true = functools.partial(rasterize_image, all_touched=True)

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


def del_to_poly(x):
    if isinstance(x, Polygon) | isinstance(x, MultiPolygon):
        return x
    elif isinstance(x, MultiLineString) | isinstance(x, LineString):
        return x.convex_hull

def do_all():
    for year in years:
        print("======================= YEAR {}".format(year))
        print("=== Main variables")
        pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
        pop = pop.rio.write_nodata(np.nan)
        pop = pop.rio.clip(eur.geometry.values)
        pop.values = pop.values.astype(np.dtype("float32"))
        pop.values[pop.values == pop.rio.nodata] = 0
        pol = rioxarray.open_rasterio(r"data/pollution/{}/pol.tif".format(year)).sel(band=1)
        pol = pol.rio.write_nodata(np.nan)
        pol = pol.rio.clip(eur.geometry.values)
        pol.values = pol.values.astype(np.dtype("float32"))
        pol.values[pol.values == pol.rio.nodata] = 0

        cities = get_cities(year).rename(columns={'year': 'city_ref_year'})
        lzes = gpd.read_file(r"data/uar/uar_data_new_euro.geojson").to_crs(3035)
        lzes["geometry"] = lzes.geometry.apply(del_to_poly)

        # population
        cpop = pop.rio.clip(cities.geometry)
        # pollution
        cpol = pol.rio.clip(cities.geometry)

        # cities
        cc = make_geocube(
            vector_data=cities,
            measurements=["id"],
            resolution=cpol.rio.resolution(),
            fill=-1,
            rasterize_function=rasterize_image_false
        ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
        cc.values[cc.values < 0] = -1

        # lze
        lze = make_geocube(
            vector_data=lzes,
            measurements=["id"],
            resolution=cpol.rio.resolution(),
            fill=-1,
            rasterize_function=rasterize_image_true
        ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
        lze.values[lze.values < 0] = -1

        print("=== Doing buffers")
        # lze ring
        lze_rings = list()
        for ring in ring_size:
            lze_buffer = lzes.copy()
            lze_buffer['geometry'] = lze_buffer.buffer(ring)
            lze_ring = make_geocube(
                vector_data=lze_buffer,
                measurements=["id"],
                resolution=cpol.rio.resolution(),
                fill=-1,
                rasterize_function=rasterize_image_true
            ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
            lze_ring.values[lze_ring.values < 0] = -1
            lze_rings.append(lze_ring)

        print("=== Climate variables")
        # temperature
        temp = get_weather(year, "temp", cpop)
        wind = get_weather(year, "wind", cpop)
        prec = get_weather(year, "prec", cpop)

        aux = pd.DataFrame(np.vstack((
            cpop.values.ravel(),
            cpol.values.ravel(),
            cc.values.ravel(),
            lze.values.ravel(),
            lze_rings[0].values.ravel(),
            lze_rings[1].values.ravel(),
            lze_rings[2].values.ravel(),
            lze_rings[3].values.ravel(),
            temp.values.ravel(),
            wind.values.ravel(),
            prec.values.ravel(),
        )).T)
        coords = cpop.to_dataframe('pop').reset_index()[['y', 'x']]
        df = pd.concat([aux, coords], axis=1)
        df.columns = ['pop', 'pol', 'city', "lze",
                      "lze_1km", "lze_3km", "lze_5km", "lze_10km",
                      'mean_temp', 'mean_precip', 'mean_wind',
                      'lat', 'long']
        df.loc[df['city'] < 0, 'city'] = -1
        if not os.path.exists(r"data/within/{}".format(year)):
            os.makedirs(r"data/within/{}".format(year), exist_ok=True)
        df.to_parquet(r"data/within/{}/step1_lze.pqt".format(year))


def harmonize():

    instruments = pd.read_parquet(r"data/within/instruments_lez.pqt")
    terrain = pd.read_parquet(r"data/within/static_lez.pqt")

    for year in years:

        df = pd.concat(
            [
                pd.read_parquet(r"data/within/{}/step1_lze.pqt".format(year)).assign(year=year),
                instruments,
                terrain
            ],
            axis=1
        )

        df.to_parquet(r"data/within/{}/within_lze.pqt".format(year))


if __name__ == '__main__':

    harmonize()





