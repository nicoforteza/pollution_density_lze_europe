import rioxarray
from rasterio import features
from shapely import Polygon
import time
from datetime import timedelta
from config import *

years = [2005, 2010, 2015, 2020]
europe = get_europe()
raw_path = r"data/cities/{}/raw/GHS_SMOD_E{}_GLOBE_R2023A_54009_1000_V1_0.tif"
clean_path = r"data/cities/{}/clean/cities.geojson"


def construct_city(obj):
    return Polygon(obj[0]['coordinates'][0])

start = time.time()

for year in years:
    print("Doing for year {}".format(year))
    raw = rioxarray.open_rasterio(raw_path.format(year, year))
    mask = raw == 30  # id of cities
    cc = list(features.shapes(raw, mask=mask, transform=raw.rio.transform()))
    cc_ = [construct_city(c) for c in cc]
    cities = gpd.GeoDataFrame(geometry=cc_, crs="ESRI:54009")
    cities = cities.reset_index().rename(columns={'index': 'id'}).to_crs(3035).clip(europe.geometry)
    cities = cities.sjoin(europe)
    cities = cities[
            ['geometry', 'CNTR_CODE']].rename(
            columns={'CNTR_CODE': 'country_id'})\
        .reset_index().drop('index', axis=1).reset_index().rename(
            columns={'index': 'id'}).assign(
            year=year).drop_duplicates(subset='geometry')
    # cities['id'] = cities.id.astype(str) + '-' + cities.year.astype(str)
    cities['area'] = cities.to_crs(4326).area * 10000
    print("Shape for {}: {}".format(year, cities.shape))
    cities.to_file(
            r"data/cities/{}/clean/cities.geojson".format(year)
    )
print("Done ==== ")

elapsed = (time.time() - start)

print(str(timedelta(seconds=elapsed)))