import os

os.environ['USE_PYGEOS'] = '0'

from shapely import Point
import rioxarray
import rasterio
from scipy.ndimage import distance_transform_edt
from xrspatial.utils import ngjit
from xrspatial.convolution import custom_kernel
from xrspatial.focal import apply
from config import *
import time

lze_directory = r"data/uar/uars"
ring_size = [1e3, 3e3, 5e3, 10e3]
eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2007, 2023) # range(2007, 2023)


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


if __name__ == '__main__':

    year = 2020

    # cities
    cities = get_cities(year).rename(columns={'year': 'city_ref_year'})
    cities['geometry'] = cities.buffer(25e3)
    # population
    pop = rioxarray.open_rasterio(r"data/population/{}/pop.tif".format(year)).sel(band=1)
    pop = pop.rio.write_nodata(np.nan)
    cpop = pop.rio.clip(cities.geometry).rio.clip(eur.geometry)

    print("===== Instrument variables")

    # soil quality
    soils = list()
    for n in [1, 2, 3, 4, 5, 6, 7]:
        soil1 = rioxarray.open_rasterio(
            r"https://www.fao.org/fileadmin/user_upload/soils/docs/HWSD/Soil_Quality_data/sq{}.asc".format(n)
        ).sel(band=1).rio.set_crs(4326)
        soil1.values[(soil1.values < 0) | (np.isnan(soil1))] = -1
        soil1 = soil1.rio.write_nodata(-1).rio.set_crs(4326)
        soil1 = soil1.rio.reproject_match(cpop, nodata=-1).rio.clip(eur.geometry)
        soil1.values = soil1.values.astype(np.float32)
        if n == 1:
            name = 'av'
        elif n == 2:
            name = "ret"
        elif n == 3:
            name = "root"
        elif n == 4:
            name = "ox"
        elif n == 5:
            name = "sa"
        elif n == 6:
            name = "tox"
        elif n == 7:
            name = "work"
        soil1df = pd.get_dummies(
            raster_to_df(soil1, "nutrient_{}".format(name)).astype('int').astype('category')).astype('int')
        soil1df = soil1df.iloc[:, 2:]
        print("Soil {} shape:".format(n), soil1df.shape)
        soils.append(soil1df)
        time.sleep(5)

    # aquifers
    aquifv = gpd.read_file(r"data/instruments/aquifers/IHME1500_v12/shp/ihme1500_aquif_ec4060_v12_poly.shp")
    aquifv = aquifv[aquifv.AQUIF_CODE.isin([1, 2, 3, 4, 5, 6])]
    aquifv["AQUIF_CODE"] = aquifv["AQUIF_CODE"].astype(int)
    aquifv = aquifv.to_crs(4326).to_crs(3035)
    aquif = make_geocube(
        vector_data=aquifv,
        measurements=["AQUIF_CODE"],
        resolution=cpop.rio.resolution(),
        fill=-1
    ).to_array().sel(variable='AQUIF_CODE').rio.reproject_match(cpop, nodata=-1)
    aquif.values[(aquif.values < 0) | (np.isnan(aquif.values))] = -1
    aquif_df = raster_to_df(aquif, "aquif")
    aquif_df = pd.get_dummies(aquif_df.astype('int').astype('category'), prefix_sep='').astype('int')
    print("Aquif shape:", aquif_df.shape)

    # historical density
    hist_years = [1800, 1500, 1000, 100]
    dfh = list()
    for hy in hist_years:
        h = rioxarray.open_rasterio(
            r"data/instruments/hist_dens/popc_{}AD.asc".format(hy)
        ).sel(band=1)
        h.values[(h.values < 0) | (np.isnan(h))] = -1
        h.rio.write_nodata(-1, inplace=True)
        h.rio.write_crs(4326, inplace=True)
        ch1 = h.rio.reproject_match(cpop,
                                    nodata=-1,
                                    resampling=rasterio.enums.Resampling(1)
                                    ).rio.clip(eur.geometry)
        ch1 = raster_to_df(ch1, "hist{}".format(hy))
        dfh.append(ch1)
        print("Histo {} dens:".format(hy), ch1.shape)

    # save all
    aux = pd.merge(aquif_df, pd.concat(dfh, axis=1), left_index=True, right_index=True)
    aux.index.name = 'cell_id'
    print("Aquif and Historidcal Densities shape:", aux.shape)

    x = pd.concat([aux, pd.concat(soils, axis=1)], axis=1)
    x.index.name = "cell_id"
    x.to_parquet(r"data/within/instruments_lez.pqt")

    print("Saved instruments LEZ with shape {}".format(x.shape))

    print("===== Control variables")

    print("=== 1.1) Terrain variables.")

    # elevation
    dem = rioxarray.open_rasterio(r"data/controls/geographical/elevation/elevationeurope1x1.tif").sel(band=1)
    dem = dem.rio.write_nodata(255)
    dem = dem.rio.reproject_match(cpop, nodata=255).rio.clip(eur.geometry.values)
    dem.values[dem.values == dem.rio.nodata] = 0
    dem.values = dem.values.astype(np.dtype("float32"))
    dem_df = raster_to_df(dem, "mean_elevation")
    print("Elevation shape:", dem_df.shape)

    # ruggedness
    @ngjit
    def _calc_ruggedness(array):
        return np.sqrt(np.sum((np.delete(array, 4) - array[1, 1]) ** 2))

    kernel = custom_kernel(np.array([[1, 1, 1], [1, 1, 1], [1, 1, 1]]))
    ruggedness = apply(dem, kernel, _calc_ruggedness, "ruggedness")
    ruggedness = ruggedness.assign_attrs({"_FillValue": 0})
    crug = ruggedness.rio.clip(eur.geometry)
    crug.values = crug.values.astype(np.dtype("float32"))
    crug = raster_to_df(crug, "mean_ruggedness")
    print("Ruggedness shape:", crug.shape)

    # power plants
    print("=== 1.2) Power Plants. ")
    pw = pd.read_csv(r"data/controls/powerplants/global_power_plant_database.csv")
    pw = pw[pw.primary_fuel.isin(["Gas", "Oil", "Coal"])]
    pw = pw[['name', 'latitude', 'longitude', 'primary_fuel']]
    geometry = pw.apply(lambda x: Point([x.latitude, x.longitude]), axis=1)
    pw['geometry'] = geometry
    pw = gpd.GeoDataFrame(pw, crs=4326).to_crs(3035)
    pw['power_plant'] = 1
    xmin, ymin, xmax, ymax = cities.total_bounds
    pw = pw.cx[xmin:xmax, ymin:ymax]
    pw["geometry"] = pw.buffer(80000)
    pwgrid = make_geocube(
        vector_data=pw,
        measurements=["power_plant"],
        resolution=cpop.rio.resolution(),
        fill=0
    ).to_array().sel(variable='power_plant').rio.reproject_match(cpop, nodata=0).rio.clip(eur.geometry.values)
    pw_dist = raster_to_df(pwgrid, "pw_dist")
    print("PW Distance shape:", pw_dist.shape)

    print("=== 1.3) Water and Coast distance")
    eurp = eur.to_crs(4326)
    xmin, ymin, xmax, ymax = eurp.total_bounds
    cos = gpd.read_file(r"data/controls/geographical/ne_10m_coastline/ne_10m_coastline.shp")
    cos = cos.cx[xmin:xmax, ymin:ymax].to_crs(3035)
    wat = gpd.read_file(r"data/controls/geographical/ne_10m_rivers_europe/ne_10m_rivers_europe.shp")
    wat = wat.cx[xmin:xmax, ymin:ymax].to_crs(3035)
    # river distance
    transform = cpop.rio.transform()
    shape = cpop.rio.shape

    river_mask = rasterize(
        shapes=[(geom, 1) for geom in wat.geometry],
        out_shape=shape,
        transform=transform,
        fill=0,
        dtype=np.float32
    )
    # river_mask[np.isnan(cpop.values)] = np.nan
    river_mask_grid = cpop.copy()
    river_mask_grid.values = river_mask
    river_mask_grid = river_mask_grid.rio.clip(eur.geometry)
    river_mask_grid.values = (distance_transform_edt(
        (~river_mask_grid.astype(bool)).astype(np.uint8)
    ) * transform[0]) / 1000
    water_dist = raster_to_df(river_mask_grid, "river_dist")
    print("River Distance shape:", water_dist.shape)

    coast_mask = rasterize(
        shapes=[(geom, 1) for geom in cos.geometry],
        out_shape=shape,
        transform=transform,
        fill=0,
        dtype=np.float32
    )
    # river_mask[np.isnan(cpop.values)] = np.nan
    coast_mask_grid = cpop.copy()
    coast_mask_grid.values = coast_mask
    coast_mask_grid = coast_mask_grid.rio.clip(eur.geometry)
    coast_mask_grid.values = (distance_transform_edt(
        (~coast_mask_grid.astype(bool)).astype(np.uint8)
    ) * transform[0]) / 1000
    coast_dist = raster_to_df(coast_mask_grid, "coast_dist")
    print("Coast Distance shape:", coast_dist.shape)

    # final
    aux = pd.concat([
        crug, dem_df, coast_dist, water_dist, pw_dist
    ], axis=1)
    aux.index.name = 'cell_id'
    aux.to_parquet(r"data/within/static_lez.pqt")

    print("Saved Time Invariante Covariates with shape {}".format(aux.shape))