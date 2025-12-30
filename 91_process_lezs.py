import geopandas as gpd
from glob import glob
from pathlib import Path
import pandas as pd
import os
from shapely import Polygon, MultiLineString, MultiPolygon, LineString

lze_info = pd.read_excel(r"/Users/nicoforteza/Desktop/pollution_density_lze_europe/data/uar/uar_euro.xlsx")
files = glob(r"/Users/nicoforteza/Desktop/pollution_density_lze_europe/data/uar/uars/*")

geojsons = glob(r"/Users/nicoforteza/Desktop/pollution_density_lze_europe/data/uar/uars/**/*geojson", recursive=True)
shps = glob(r"/Users/nicoforteza/Desktop/pollution_density_lze_europe/data/uar/uars/**/*shp", recursive=True)
files = geojsons + shps

res = list()

def del_to_poly(x):
    if isinstance(x, Polygon) | isinstance(x, MultiPolygon):
        return x
    elif isinstance(x, MultiLineString) | isinstance(x, LineString):
        return x.convex_hull


for path in files:
    id_ = int(Path(path).name.split('.')[0].split('_')[-1])
    aux = gpd.read_file(path).to_crs(3035).dissolve()
    aux["geometry"] = aux.geometry.apply(del_to_poly)
    aux['id_uar'] = id_
    res.append(aux[['geometry', 'id_uar']])

a = pd.concat(res)
a = pd.merge(lze_info, a, on='id_uar', how='right')
a = gpd.GeoDataFrame(a)
a = a.set_crs("EPSG:3035")
a.to_file(r"/Users/nicoforteza/Desktop/pollution_density_lze_europe/data/uar/uar_data_new_euro.geojson")
