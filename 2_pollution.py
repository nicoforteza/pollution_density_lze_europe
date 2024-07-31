from config import *
import rioxarray
from glob import glob
from pathlib import Path

paths = glob(r"data/pollution/*/*/*avg*.tif")
clean_path = r"data/pollution/{}/pol.tif"
eur = get_europe()

for path in paths:
    year = int(Path(path).parent.parent.name)
    print("Cleaning PM2.5 raster for year {}".format(year))
    pol = rioxarray.open_rasterio(path).sel(band=1)
    pol.values[pol.values < 0] = 0
    pol.rio.write_nodata(0, inplace=True)
    pol = pol.rio.clip(eur.geometry)
    pol.rio.to_raster(clean_path.format(year))
