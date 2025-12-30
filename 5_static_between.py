from config import *
import numpy as np
import rioxarray
from shapely import Point
from xrspatial.convolution import custom_kernel
from xrspatial.focal import apply
from xrspatial.utils import ngjit
import rasterio
import warnings
import time
from datetime import timedelta
warnings.filterwarnings("ignore")

eur = get_europe()
eurproj = eur.to_crs(4326)
eurp = eur.to_crs(4326)

#######################################################
# ################ CONTROLS ###########################
#######################################################

start = time.time()

for year in [2005, 2010, 2015, 2020]:

    if year == 2005:
        pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(2007)).sel(band=1)
    else:
        pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
    vector = get_cities(year)

    # ================= COAST LINES ================
    xmin, ymin, xmax, ymax = eurp.total_bounds

    cos = gpd.read_file(r"data/controls/geographical/ne_10m_coastline/ne_10m_coastline.shp", engine='pyogrio')
    cosp = cos.cx[xmin:xmax, ymin:ymax].to_crs(3035)

    wat = gpd.read_file(r"data/controls/geographical/ne_10m_rivers_europe/ne_10m_rivers_europe.shp", engine='pyogrio')
    wat = wat.cx[xmin:xmax, ymin:ymax].to_crs(3035)

    coast_city_between = vector.groupby("id").apply(
        lambda x: any((cosp.distance(x.geometry.item()).sort_values() / 1e3) < 50)).astype(int)
    distance_to_water_between = vector.groupby("id").apply(
        lambda x: (wat.distance(x.geometry.item()).sort_values() / 1e3).iloc[0].round(2))
    distance_to_coast_between = vector.groupby("id").apply(
        lambda x: (cosp.distance(x.geometry.item()).sort_values() / 1e3).iloc[0].round(2))
    f = pd.concat([coast_city_between, distance_to_coast_between, distance_to_water_between], axis=1)
    f.columns = ['coast_city', 'water_dist', 'coast_dist']
    f = f.reset_index()
    vector = pd.merge(vector, f, on='id')

    # ================= AVERAGE RUGGEDNESS ================
    dem = rioxarray.open_rasterio(r"data/controls/geographical/elevation/elevationeurope1x1.tif").sel(band=1)
    dem = dem.rio.reproject_match(pop).rio.clip(eur.geometry.values)
    dem.values = dem.values.astype(np.dtype("float32"))
    dem.values[dem.values == dem.rio.nodata] = 0

    @ngjit
    def _calc_ruggedness(array):
        return np.sqrt(np.sum((np.delete(array, 4) - array[1, 1]) ** 2))

    kernel = custom_kernel(np.array([[1, 1, 1], [1, 1, 1], [1, 1, 1]]))
    ruggedness = apply(dem, kernel, _calc_ruggedness, "ruggedness")
    ruggedness.coords.update({'band': 1})
    ruggedness.assign_attrs({"_FillValue": np.nan})
    vector = zonal_stats(ruggedness, vector, "mean", "ruggedness")

    # ================= DISTANCE TO POWER PLANTS ================
    pw = pd.read_csv(r"data/controls/powerplants/global_power_plant_database.csv")
    pw = pw[pw.primary_fuel.isin(["Gas", "Oil", "Coal"])]
    pw = pw[['name', 'latitude', 'longitude', 'primary_fuel']]
    geometry = pw.apply(lambda x: Point([x.latitude, x.longitude]), axis=1)
    pw['geometry'] = geometry
    pw = gpd.GeoDataFrame(pw, crs=4326).to_crs(3035)
    pw = pw.clip(eur)
    distance_to_power = vector.groupby("id").apply(
        lambda x: (pw.distance(x.geometry.item()).sort_values() / 1e3).iloc[0].round(2)
    ).rename("pw_dist").reset_index()
    vector = gpd.GeoDataFrame(pd.merge(vector, distance_to_power, on='id'))

    vector.drop(['year'], axis=1).to_file(
        r"data/controls/{}_clean_between.geojson".format(year)
    )

    #######################################################
    # ################ INSTRUMENTS ####################
    #######################################################
    vector = get_cities(year)

    # ============= EARTHQUAKE HAZARD ==================================
    e = gpd.read_file(r"data/instruments/earthquake/hmap491/hmap491.shp", engine='pyogrio')
    er = e.to_crs(eur.crs)
    er_ras = make_geocube(vector_data=er,
                          measurements=["HPVALUE"],
                          like=pop,
                          fill=np.nan)
    er_ras = er_ras.HPVALUE.rio.clip(eur.geometry)
    vector = zonal_stats(er_ras, vector, 'mean', 'eqhz')

    # ============= HISTORICAL POPULATION DENSITY ======================
    hist_years = [1800, 1500, 1000, 100]
    for hist_year in hist_years:

        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(hist_year)
        ).sel(band=1)
        h.values[h.values < 0] = 0
        h.rio.write_nodata(0, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch = h.rio.reproject_match(pop,
                                   resampling=rasterio.enums.Resampling(1)
                                   ).rio.clip(eur.geometry)

        vector = zonal_stats(ch, vector, 'sum', 'hist{}'.format(hist_year))

    # ============= SOIL QUALITY ======================
    def get_soil_data(n):

        """
        soil = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1)
        soil = soil.rio.write_crs(4326)
        soil.values = soil.values.astype(np.dtype("float32"))
        soil.rio.write_nodata(np.nan, inplace=True)
        soilcrp = soil.rio.reproject_match(pop).rio.clip(eur.geometry)
        soilcrp.values[soilcrp.values == 0] = np.nan
        """
        soil = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326)
        soilcrp = soil.rio.reproject_match(pop)

        clipped_raster = soilcrp.rio.clip(vector.geometry).drop("band")
        ras_vec = make_geocube(vector_data=vector,
                               measurements=["id"],
                               like=clipped_raster,
                               fill=np.nan)
        ress = {}
        for id_ in vector.id.unique():
            vals = clipped_raster.data[(ras_vec.id == id_)]
            unique_vals = np.unique(vals)
            res = dict(zip(unique_vals, [(vals == i).sum() / vals.shape[0] for i in unique_vals]))
            ress.update({id_: res})

        name = None
        if n == 1:
            name = 'nutrient_av_{i}'
        elif n == 2:
            name = 'nutrient_ret_{i}'
        elif n == 3:
            name = 'nutrient_root_{i}'
        elif n == 4:
            name = 'nutrient_ox_{i}'
        elif n == 5:
            name = 'nutrient_sa_{i}'
        elif n == 6:
            name = 'nutrient_tox_{i}'
        elif n == 7:
            name = 'nutrient_work_{i}'

        df = pd.DataFrame(ress).T.fillna(0).round(3).sort_index(axis=1)[[1, 2, 3, 4, 5, 7]]
        df.columns = [int(i) for i in df.columns]
        df.columns = [name.format(i=i) for i in df.columns]
        return df

    for n in range(1, 8):
        df = get_soil_data(n)
        vector = pd.merge(vector, df.reset_index().rename(columns={'index': 'id'}), on='id')

    # ============= AQUIFIERS ======================
    soil = gpd.read_file(r"data/instruments/aquifers/IHME1500_v12/shp/ihme1500_aquif_ec4060_v12_poly.shp", engine='pyogrio')
    soil = soil[soil.AQUIF_CODE.isin([1, 2, 3, 4, 5, 6])]
    soil["AQUIF_CODE"] = soil["AQUIF_CODE"].astype(int)
    soil = soil.to_crs(4326)
    vector = vector.to_crs(4326)


    def get_soil_pcts(x):
        x = gpd.GeoDataFrame(x.to_frame()).T
        x = x.set_crs(4326)
        clipped = soil.clip(x)
        clipped['area'] = clipped.area
        clipped['area'] = clipped.area / clipped.area.sum()
        # clipped['area'].drop('area', axis=1, inplace=True)
        clipped = clipped.groupby("AQUIF_CODE")["area"].sum()
        clipped = clipped.reindex(soil.AQUIF_CODE.unique()).fillna(0)
        clipped['id'] = x.id.item()
        return clipped

    # vector = vector.set_geometry("geometry")
    soil_pct = vector.apply(lambda x: get_soil_pcts(x), axis=1)
    vector = pd.merge(vector, soil_pct, on='id')
    vector = vector.rename(columns=dict(zip(range(1, 7), ['aquif{}'.format(i) for i in range(1, 7)])))
    vector = vector.to_crs(3035)

    #######################################################
    # ################ 6 - SAVE ###########################
    #######################################################
    vector.drop(['year'], axis=1).to_file(r"data/instruments/{}_clean_between.geojson".format(year))
    del vector

elapsed = (time.time() - start)

print(str(timedelta(seconds=elapsed)))

