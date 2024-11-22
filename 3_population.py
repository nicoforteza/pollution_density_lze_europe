import rioxarray
import numpy as np
from config import *

years = range(2007, 2023)
raw_path = r"data/population/{}/landscan-global-{}-assets/landscan-global-{}.tif"
pol_path = r"data/pollution/{}/pol.tif"
clean_path = r"data/population/{}/pop.tif"
eur = get_europe()
eurproj = eur.to_crs(4326)
minx, miny, maxx, maxy = eurproj.total_bounds
minx = minx - 1,
miny = miny - 1,
maxx = maxx + 1,
maxy = maxy + 1

for year in years:
    print("Cleaning population raster for year {}".format(year))
    pol = rioxarray.open_rasterio(pol_path.format(year)).sel(band=1)
    pop = rioxarray.open_rasterio(
        raw_path.format(year, year, year)
    ).sel(band=1).rio.clip_box(
        minx=minx,
        miny=miny,
        maxx=maxx,
        maxy=maxy,
    )
    clipped_pop = pop.rio.reproject_match(pol).rio.clip(eur.geometry)
    clipped_pop.values[clipped_pop.values < 0] = 0
    clipped_pop.rio.write_nodata(0, inplace=True)
    clipped_pop.values = clipped_pop.values.astype(np.dtype('float32'))
    clipped_pop.rio.to_raster(clean_path.format(year))
    print(clipped_pop.shape)
    del pop, clipped_pop

