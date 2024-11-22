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

if __name__ == '__main__':

    print("=====Static features over time======")

    year = 2020
    print("=== Main variables to compute them")
    pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
    pol = rioxarray.open_rasterio(r"data/pollution/{}/pol.tif".format(year)).sel(band=1)
    pop = pop.rio.write_nodata(np.nan)
    pol = pol.rio.write_nodata(np.nan)

    cities = get_cities(year).rename(columns={'year': 'city_ref_year'})

    # population
    cpop = pop.rio.clip(cities.geometry)
    # pollution
    cpol = pol.rio.clip(cities.geometry)

    print("=== Terrain variables")
    # ruggedness
    dem = rioxarray.open_rasterio(r"data/controls/geographical/elevation/elevationeurope1x1.tif").sel(band=1)
    dem = dem.rio.reproject_match(cpop).rio.clip(eur.geometry.values)
    dem.values = dem.values.astype(np.dtype("float32"))
    dem.values[dem.values == dem.rio.nodata] = 0

    @ngjit
    def _calc_ruggedness(array):
        return np.sqrt(np.sum((np.delete(array, 4) - array[1, 1]) ** 2))

    kernel = custom_kernel(np.array([[1, 1, 1], [1, 1, 1], [1, 1, 1]]))
    ruggedness = apply(dem, kernel, _calc_ruggedness, "ruggedness")
    ruggedness = ruggedness.assign_attrs({"_FillValue": 0})
    crug = ruggedness.rio.clip(eur.geometry)

    print("=== Water and Coast variables")
    eurp = eur.to_crs(4326)
    xmin, ymin, xmax, ymax = eurp.total_bounds

    cos = gpd.read_file(r"data/controls/geographical/ne_10m_coastline/ne_10m_coastline.shp")
    cosp = cos.cx[xmin:xmax, ymin:ymax].to_crs(3035)

    wat = gpd.read_file(r"data/controls/geographical/ne_10m_rivers_europe/ne_10m_rivers_europe.shp")
    wat = wat.cx[xmin:xmax, ymin:ymax].to_crs(3035)

    cosp["coast"] = 1
    wat['water'] = 1

    # malla de costas
    coast_grid = make_geocube(
        vector_data=cosp,
        measurements=["coast"],
        resolution=pol.rio.resolution(),
        fill=0
    ).to_array().sel(variable='coast').rio.reproject_match(cpop, nodata=-1)

    # malla de aguas
    water_grid = make_geocube(
        vector_data=wat,
        measurements=["water"],
        resolution=pol.rio.resolution(),
        fill=0
    ).to_array().sel(variable='water').rio.reproject_match(cpop, nodata=-1)


    @ngjit
    def _calc_coast_city(array):
        return int(np.any(array > 0))


    kernel = circle_kernel(1, 1, 50)
    kernel_river = circle_kernel(1, 1, 10)

    output = apply(coast_grid, kernel, _calc_coast_city, "coast_city")
    coast = output.rio.clip(eur.geometry)

    output_water = apply(water_grid, kernel_river, _calc_coast_city, "water_city")
    water = output_water.rio.clip(eur.geometry)

    print("=== Power Plants variables")
    # power plants
    pw = pd.read_csv(r"data/controls/powerplants/global_power_plant_database.csv")
    pw = pw[pw.primary_fuel.isin(["Gas", "Oil", "Coal"])]
    pw = pw[['name', 'latitude', 'longitude', 'primary_fuel']]
    geometry = pw.apply(lambda x: Point([x.latitude, x.longitude]), axis=1)
    pw['geometry'] = geometry
    pw = gpd.GeoDataFrame(pw, crs=4326).to_crs(3035)
    pw = pw.clip(eur)
    pw["geometry"] = pw.buffer(80000)
    pw['power_plant'] = 1
    pwgrid = make_geocube(
        vector_data=pw,
        measurements=["power_plant"],
        resolution=pol.rio.resolution(),
        fill=0
    ).to_array().sel(variable='power_plant').rio.reproject_match(cpop, nodata=-1)
    pwgrid.values[pwgrid.values == pwgrid.rio.nodata] = 0

    # final
    aux = pd.DataFrame(np.vstack((
        crug.values.ravel(),
        dem.values.ravel(),
        coast.values.ravel(),
        water.values.ravel(),
        pwgrid.values.ravel()
    )).T)
    aux.columns = ['mean_ruggedness', 'mean_elevation', 'coast_city', 'water_dist', 'pw_dist']  # 'coast_city', 'water_dist',
    aux.to_parquet(r"data/within/static_lez.pqt")
    print("Saved static LEZ with shape {}".format(aux.shape))
    print("NANs:")
    print(aux.isnull().sum(0))

    print("===== Instrument variables")
    # aquifier
    aquifv = gpd.read_file(r"data/instruments/aquifers/IHME1500_v12/shp/ihme1500_aquif_ec4060_v12_poly.shp")
    aquifv = aquifv[aquifv.AQUIF_CODE.isin([1, 2, 3, 4, 5, 6])]
    aquifv["AQUIF_CODE"] = aquifv["AQUIF_CODE"].astype(int)
    aquifv = aquifv.to_crs(4326).to_crs(3035)

    aquif = make_geocube(
        vector_data=aquifv,
        measurements=["AQUIF_CODE"],
        resolution=pol.rio.resolution(),
        fill=0
    ).to_array().sel(variable='AQUIF_CODE').rio.reproject_match(cpop, nodata=-1)
    aquif.values[aquif.values == aquif.rio.nodata] = 0
    aquif = aquif.rio.clip(eur.geometry)
    aquif.values[aquif.values == aquif.rio.nodata] = 0

    # soil quality
    n = 1
    soil1 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil1.values[soil1.values == soil1.rio.nodata] = 0

    n = 2
    soil2 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil2.values[soil2.values == soil2.rio.nodata] = 0

    n = 3
    soil3 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil3.values[soil3.values == soil3.rio.nodata] = 0

    n = 4
    soil4 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil4.values[soil4.values == soil4.rio.nodata] = 0

    n = 5
    soil5 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil5.values[soil5.values == soil5.rio.nodata] = 0

    n = 6
    soil6 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil6.values[soil6.values == soil6.rio.nodata] = 0

    n = 7
    soil7 = rioxarray.open_rasterio(
        r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
    ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(cpop, nodata=-1).rio.clip(
        eur.geometry)
    soil7.values[soil7.values == soil7.rio.nodata] = 0

    # historical density
    hist_years = [1800, 1500, 1000, 100]
    h = rioxarray.open_rasterio(
        r"data/instruments/hist_dens/popc_{}AD.asc".format(1800)
    ).sel(band=1)
    h.values[h.values < 0] = 0
    h.rio.write_nodata(0, inplace=True)
    h.rio.write_crs(4326, inplace=True)
    ch1 = h.rio.reproject_match(cpop, nodata=-1,
                                resampling=rasterio.enums.Resampling(1)
                                ).rio.clip(eur.geometry)

    h = rioxarray.open_rasterio(
        r"data/instruments/hist_dens/popc_{}AD.asc".format(1500)
    ).sel(band=1)
    h.values[h.values < 0] = 0
    h.rio.write_nodata(0, inplace=True)
    h.rio.write_crs(4326, inplace=True)
    ch2 = h.rio.reproject_match(cpop, nodata=-1,
                                resampling=rasterio.enums.Resampling(1)
                                ).rio.clip(eur.geometry)

    h = rioxarray.open_rasterio(
        r"data/instruments/hist_dens/popc_{}AD.asc".format(1000)
    ).sel(band=1)
    h.values[h.values < 0] = 0
    h.rio.write_nodata(0, inplace=True)
    h.rio.write_crs(4326, inplace=True)
    ch3 = h.rio.reproject_match(cpop, nodata=-1,
                                resampling=rasterio.enums.Resampling(1)
                                ).rio.clip(eur.geometry)

    h = rioxarray.open_rasterio(
        r"data/instruments/hist_dens/popc_{}AD.asc".format(100)
    ).sel(band=1)
    h.values[h.values < 0] = 0
    h.rio.write_nodata(0, inplace=True)
    h.rio.write_crs(4326, inplace=True)
    ch4 = h.rio.reproject_match(cpop, nodata=-1,
                                resampling=rasterio.enums.Resampling(1)
                                ).rio.clip(eur.geometry)

    # save all
    aux = pd.DataFrame(np.vstack((
        (aquif == 1).astype(int).values.ravel(),
        (aquif == 2).astype(int).values.ravel(),
        (aquif == 3).astype(int).values.ravel(),
        (aquif == 4).astype(int).values.ravel(),
        (aquif == 5).astype(int).values.ravel(),
        (aquif == 6).astype(int).values.ravel(),
        (aquif == 0).astype(int).values.ravel(),
        ch1.values.ravel(),
        ch2.values.ravel(),
        ch3.values.ravel(),
        ch4.values.ravel()
    )).T)
    aux.columns = ['aquif1', 'aquif2', 'aquif3', 'aquif4',
                   'aquif5', 'aquif6', 'aquif0', 'hist1800',
                   'hist1500', "hist1000", "hist100"]

    aux2 = pd.DataFrame(np.vstack((
        (soil1 == 1).astype(int).values.ravel(),
        (soil1 == 2).astype(int).values.ravel(),
        (soil1 == 3).astype(int).values.ravel(),
        (soil1 == 4).astype(int).values.ravel(),
        (soil1 == 5).astype(int).values.ravel(),
        (soil1 == 6).astype(int).values.ravel(),
        (soil1 == 7).astype(int).values.ravel(),
    )).T)
    aux2.columns = ['nutrient_av_1', 'nutrient_av_2', 'nutrient_av_3', 'nutrient_av_4',
                    'nutrient_av_5', 'nutrient_av_6', 'nutrient_av_7']

    aux3 = pd.DataFrame(np.vstack((
        (soil2 == 1).astype(int).values.ravel(),
        (soil2 == 2).astype(int).values.ravel(),
        (soil2 == 3).astype(int).values.ravel(),
        (soil2 == 4).astype(int).values.ravel(),
        (soil2 == 5).astype(int).values.ravel(),
        (soil2 == 6).astype(int).values.ravel(),
        (soil2 == 7).astype(int).values.ravel(),
    )).T)
    aux3.columns = ['nutrient_ret_1', 'nutrient_ret_2', 'nutrient_ret_3', 'nutrient_ret_4',
                    'nutrient_ret_5', 'nutrient_ret_6', 'nutrient_ret_7']

    aux4 = pd.DataFrame(np.vstack((
        (soil3 == 1).astype(int).values.ravel(),
        (soil3 == 2).astype(int).values.ravel(),
        (soil3 == 3).astype(int).values.ravel(),
        (soil3 == 4).astype(int).values.ravel(),
        (soil3 == 5).astype(int).values.ravel(),
        (soil3 == 6).astype(int).values.ravel(),
        (soil3 == 7).astype(int).values.ravel(),
    )).T)
    aux4.columns = ['nutrient_root_1', 'nutrient_root_2', 'nutrient_root_3', 'nutrient_root_4',
                    'nutrient_root_5', 'nutrient_root_6', 'nutrient_root_7']

    aux5 = pd.DataFrame(np.vstack((
        (soil4 == 1).astype(int).values.ravel(),
        (soil4 == 2).astype(int).values.ravel(),
        (soil4 == 3).astype(int).values.ravel(),
        (soil4 == 4).astype(int).values.ravel(),
        (soil4 == 5).astype(int).values.ravel(),
        (soil4 == 6).astype(int).values.ravel(),
        (soil4 == 7).astype(int).values.ravel(),
    )).T)
    aux5.columns = ['nutrient_ox_1', 'nutrient_ox_2', 'nutrient_ox_3', 'nutrient_ox_4',
                    'nutrient_ox_5', 'nutrient_ox_6', 'nutrient_ox_7']

    aux6 = pd.DataFrame(np.vstack((
        (soil5 == 1).astype(int).values.ravel(),
        (soil5 == 2).astype(int).values.ravel(),
        (soil5 == 3).astype(int).values.ravel(),
        (soil5 == 4).astype(int).values.ravel(),
        (soil5 == 5).astype(int).values.ravel(),
        (soil5 == 6).astype(int).values.ravel(),
        (soil5 == 7).astype(int).values.ravel(),
    )).T)
    aux6.columns = ['nutrient_sa_1', 'nutrient_sa_2', 'nutrient_sa_3', 'nutrient_sa_4',
                    'nutrient_sa_5', 'nutrient_sa_6', 'nutrient_sa_7']

    aux7 = pd.DataFrame(np.vstack((
        (soil6 == 1).astype(int).values.ravel(),
        (soil6 == 2).astype(int).values.ravel(),
        (soil6 == 3).astype(int).values.ravel(),
        (soil6 == 4).astype(int).values.ravel(),
        (soil6 == 5).astype(int).values.ravel(),
        (soil6 == 6).astype(int).values.ravel(),
        (soil6 == 7).astype(int).values.ravel(),
    )).T)
    aux7.columns = ['nutrient_tox_1', 'nutrient_tox_2', 'nutrient_tox_3', 'nutrient_tox_4',
                    'nutrient_tox_5', 'nutrient_tox_6', 'nutrient_tox_7']

    aux8 = pd.DataFrame(np.vstack((
        (soil7 == 1).astype(int).values.ravel(),
        (soil7 == 2).astype(int).values.ravel(),
        (soil7 == 3).astype(int).values.ravel(),
        (soil7 == 4).astype(int).values.ravel(),
        (soil7 == 5).astype(int).values.ravel(),
        (soil7 == 6).astype(int).values.ravel(),
        (soil7 == 7).astype(int).values.ravel(),
    )).T)
    aux8.columns = ['nutrient_work_1', 'nutrient_work_2', 'nutrient_work_3', 'nutrient_work_4',
                    'nutrient_work_5', 'nutrient_work_6', 'nutrient_work_7']

    x = pd.concat([aux, aux2, aux3, aux4, aux5, aux6, aux7, aux8], axis=1)

    x.to_parquet(r"data/within/instruments_lez.pqt")
    print("Saved instruments LEZ with shape {}".format(x.shape))
    print("NANs:")
    print(x.isnull().sum(0))

