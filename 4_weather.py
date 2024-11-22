from config import *
import numpy as np
import rioxarray
from rasterio.enums import Resampling

eur = get_europe()
eurproj = eur.to_crs(4326)
variables = ['prec', 'wind', 'temp']  # 'temp',
years = range(2007, 2023)
clean_path = r"data/controls/climate/{}_{}.tif"


def get_weather(year, variable):
    pop = rioxarray.open_rasterio(
        r"data/population/{}/pop.tif".format(year)
    ).sel(band=1)
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
        shape=pop.shape,
        resampling=Resampling.nearest,
    )
    tmpr = tmpr.rio.reproject_match(pop)
    tmpr = tmpr.rio.clip(eur.geometry.values)
    tmpr.values[tmpr.values == tmpr.rio.nodata] = 0
    return tmpr


for var in variables:
    for year in years:
        ras = get_weather(year, var)
        ras.rio.write_nodata(np.nan, encoded=True, inplace=True)
        ras.rio.to_raster(clean_path.format(year, var))