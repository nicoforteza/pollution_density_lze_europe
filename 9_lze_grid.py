import glob
import os

os.environ['USE_PYGEOS'] = '0'
from pathlib import Path

from shapely import Point
import rioxarray
import rasterio
import geocube
from rasterio.enums import Resampling
from xrspatial.utils import ngjit
from xrspatial.convolution import custom_kernel, circle_kernel
from xrspatial.focal import apply
from config import *
from shapely import Polygon, MultiLineString, MultiPolygon, LineString

lze_directory = r"data/uar/uars"
ring_size = [1e3, 3e3, 5e3, 10e3, 25e3]
eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2011, 2022)


def extend_gpd_read(path):
    path = Path(path)
    if "_" in path.parent.name:
        city_id = int(path.parent.parent.name)
    else:
        city_id = int(path.parent.name)
    lze_id = int(path.name.split('.')[0].split('_')[1])
    return gpd.read_file(path).assign(id=city_id, id_uar=lze_id).to_crs(4326)[['geometry', 'id', 'id_uar']]


def concat_all_lze():

    l1 = glob.glob(r"data/uar/uars/*/*.geojson")
    l2 = glob.glob(r"data/uar/uars/*/*.shp")
    l3 = glob.glob(r"data/uar/uars/*/*/*.geojson")
    l4 = glob.glob(r"data/uar/uars/*/*/*.shp")
    lze = l1 + l2 + l3 + l4

    df = pd.concat([
        extend_gpd_read(file) for file in lze
    ])

    related = pd.read_excel(r"data/uar/docs/id_uar.xlsx")
    aux = gpd.GeoDataFrame(pd.merge(related, df, on=['id', 'id_uar']))
    aux = aux.dissolve(by=['id', 'id_uar'])

    cities = get_cities(2020).to_crs(4326)

    raise NotImplementedError


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
    tmpr = tmpr.rio.reproject_match(pop, nodata=-1)
    tmpr = tmpr.rio.clip(eur.geometry.values)
    tmpr.values[tmpr.values == tmpr.rio.nodata] = 0
    return tmpr


def get_year_controls(y):
    y = get_prior_year(y)
    return gpd.read_file(r"data/controls/{}_clean_between.geojson".format(y))


def get_year_instruments(y):
    y = get_prior_year(y)
    return gpd.read_file(r"data/instruments/{}_clean_between.geojson".format(y))


def del_to_poly(x):
    if isinstance(x, Polygon) | isinstance(x, MultiPolygon):
        return x
    elif isinstance(x, MultiLineString) | isinstance(x, LineString):
        return x.convex_hull


def get_buffers():

    lzes = gpd.read_file(r"data/uar/uar_data.geojson").to_crs(3035)
    cities = get_cities(2020).to_crs(3035)
    lzes = lzes.geometry.apply(del_to_poly)

    a = 1


if __name__ == '__main__':

    for year in years:
        print("======================= YEAR {}".format(year))
        print("=== Main variables")
        pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
        pol = rioxarray.open_rasterio(r"data/pollution/{}/pol.tif".format(year)).sel(band=1)
        pop = pop.rio.write_nodata(np.nan)
        pol = pol.rio.write_nodata(np.nan)

        print("=== Doing buffers")
        cities = get_cities(year).rename(columns={'year': 'city_ref_year'})
        cities_buffer = cities.copy()
        cities_buffer['geometry'] = cities_buffer.buffer(25e3)
        lzes = gpd.read_file(r"data/uar/uar_data.geojson")# .to_crs(3035)
        lzes["geometry"] = lzes.geometry.apply(del_to_poly)

        # population
        cpop = pop.rio.clip(cities_buffer.geometry)
        # pollution
        cpol = pol.rio.clip(cities_buffer.geometry)

        # cities
        cc = make_geocube(
            vector_data=cities,
            measurements=["id"],
            resolution=cpol.rio.resolution(),
            fill=-1
        ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
        cc.values[cc.values < 0] = -1

        # cities with buffer
        ccbuffer = make_geocube(
            vector_data=cities_buffer,
            measurements=["id"],
            resolution=cpol.rio.resolution(),
            fill=-1
        ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
        ccbuffer.values[ccbuffer.values < 0] = -1

        # lze
        lze = make_geocube(
            vector_data=lzes,
            measurements=["id"],
            resolution=cpol.rio.resolution(),
            fill=-1
        ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
        lze.values[lze.values < 0] = -1

        # lze ring
        lze_rings = list()
        for ring in ring_size:
            lze_buffer = lzes.copy()
            lze_buffer['geometry'] = lze_buffer.buffer(ring)
            lze_ring = make_geocube(
                vector_data=lze_buffer,
                measurements=["id"],
                resolution=cpol.rio.resolution(),
                fill=-1
            ).to_array().sel(variable='id').rio.reproject_match(cpol, nodata=-1)
            lze_ring.values[lze_ring.values < 0] = -1
            lze_rings.append(lze_ring)

        aux = pd.DataFrame(np.vstack((
            cpop.values.ravel(),
            cpol.values.ravel(),
            cc.values.ravel(),
            ccbuffer.values.ravel(),
            lze.values.ravel(),
            lze_rings[0].values.ravel(),
            lze_rings[1].values.ravel(),
            lze_rings[2].values.ravel(),
            lze_rings[3].values.ravel(),
            lze_rings[4].values.ravel(),
        )).T)
        coords = cpop.to_dataframe('pop').reset_index()[['y', 'x']]
        df = pd.concat([aux, coords], axis=1)
        df.columns = ['pop', 'pol', 'city', "city_buffer", "lze",
                      "lze_1km", "lze_3km", "lze_5km", "lze_10km", "lze_25km",
                      'lat', 'long']
        df.loc[df['city'] < 0, 'city'] = -1
        if not os.path.exists(r"data/within/{}".format(year)):
            os.makedirs(r"data/within/{}".format(year), exist_ok=True)
        df.to_parquet(r"data/within/{}/step1_lze.pqt".format(year))

        print("=== Climate variables")
        # temperature
        temp = get_weather(year, "temp")
        wind = get_weather(year, "wind")
        prec = get_weather(year, "prec")

        print("=== Terrain variables")
        # ruggedness
        dem = rioxarray.open_rasterio(r"data/controls/geographical/elevation/elevationeurope1x1.tif").sel(band=1)
        dem = dem.rio.reproject_match(pop).rio.clip(eur.geometry.values)
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
        ).to_array().sel(variable='coast').rio.reproject_match(pol, nodata=-1)

        # malla de aguas
        water_grid = make_geocube(
            vector_data=wat,
            measurements=["water"],
            resolution=pol.rio.resolution(),
            fill=0
        ).to_array().sel(variable='water').rio.reproject_match(pol, nodata=-1)


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
        ).to_array().sel(variable='power_plant').rio.reproject_match(pol, nodata=-1)
        pwgrid.values[pwgrid.values == pwgrid.rio.nodata] = 0

        # final
        aux = pd.DataFrame(np.vstack((
            temp.values.ravel(),
            prec.values.ravel(),
            wind.values.ravel(),
            crug.values.ravel(),
            dem.values.ravel(),
            coast.values.ravel(),
            water.values.ravel(),
            pwgrid.values.ravel(),
        )).T)
        aux.columns = ['mean_temp', 'mean_precip', 'mean_wind', 'mean_ruggedness',
                       'mean_elevation', 'coast_city', 'water_dist', 'pw_dist'] # 'coast_city', 'water_dist',
        df = pd.read_parquet(r"data/within/{}/step1_lze.pqt".format(year))
        df = pd.concat([df, aux], axis=1)
        df.to_parquet(r"data/within/{}/step2_lze.pqt".format(year))

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
        ).to_array().sel(variable='AQUIF_CODE').rio.reproject_match(pol, nodata=-1)
        aquif.values[aquif.values == aquif.rio.nodata] = 0
        aquif = aquif.rio.clip(eur.geometry)
        aquif.values[aquif.values == aquif.rio.nodata] = 0

        # soil quality
        n = 1
        soil1 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil1.values[soil1.values == soil1.rio.nodata] = 0

        n = 2
        soil2 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil2.values[soil2.values == soil2.rio.nodata] = 0

        n = 3
        soil3 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil3.values[soil3.values == soil3.rio.nodata] = 0

        n = 4
        soil4 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil4.values[soil4.values == soil4.rio.nodata] = 0

        n = 5
        soil5 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil5.values[soil5.values == soil5.rio.nodata] = 0

        n = 6
        soil6 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil6.values[soil6.values == soil6.rio.nodata] = 0

        n = 7
        soil7 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326).rio.clip(eurproj.geometry).rio.reproject_match(pop, nodata=-1).rio.clip(eur.geometry)
        soil7.values[soil7.values == soil7.rio.nodata] = 0

        # historical density
        hist_years = [1800, 1500, 1000, 100]
        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(1800)
        ).sel(band=1)
        h.values[h.values < 0] = 0
        h.rio.write_nodata(0, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch1 = h.rio.reproject_match(pop, nodata=-1,
                                    resampling=rasterio.enums.Resampling(1)
                                    ).rio.clip(eur.geometry)

        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(1500)
        ).sel(band=1)
        h.values[h.values < 0] = 0
        h.rio.write_nodata(0, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch2 = h.rio.reproject_match(pop, nodata=-1,
                                    resampling=rasterio.enums.Resampling(1)
                                    ).rio.clip(eur.geometry)

        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(1000)
        ).sel(band=1)
        h.values[h.values < 0] = 0
        h.rio.write_nodata(0, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch3 = h.rio.reproject_match(pop, nodata=-1,
                                    resampling=rasterio.enums.Resampling(1)
                                    ).rio.clip(eur.geometry)

        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(100)
        ).sel(band=1)
        h.values[h.values < 0] = 0
        h.rio.write_nodata(0, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch4 = h.rio.reproject_match(pop, nodata=-1,
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

        print("===== Merge all and save")
        df = pd.read_parquet(r"data/within/{}/step2_lze.pqt".format(year))
        df = pd.concat([df, x], axis=1)
        df.to_parquet(r"data/within/{}/within_lze.pqt".format(year))





