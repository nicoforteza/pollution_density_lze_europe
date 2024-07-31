"""""
import numpy as np
import pandas as pd
from geopy.extra.rate_limiter import RateLimiter
from geopy.geocoders import Photon
import geopandas as gpd

retrieve = False

rawpath = r"data/city_regulations/Urban Access Regulations Extract 7 2 24.xlsx"
raw = pd.read_excel(rawpath).rename(
    columns={
        'Standard: Beginning / End Actual': 'date',
        'Main Scheme': "type",
        'Vehicles affected': 'vehicles',
        'City': 'city'
    }
)
df = raw[['date', 'type', 'vehicles', 'city']]
dates = df.date.str.replace('-', '').str.replace('_', '').str.replace(' ', '').str.split('|').apply(pd.Series)
dates.columns = ['start', 'end']
df = pd.concat([df.drop('date', axis=1), dates], axis=1)
df['type'] = df['type'].replace('', np.nan).fillna(0).astype(int)
df['start'] = pd.to_datetime(df['start'], format='%Y/%m/%d', errors='coerce')
df['end'] = pd.to_datetime(df['end'], format='%Y/%m/%d', errors='coerce')
df['city_name'] = df.city.str.split('-').str[0]
cities_list = df.city_name.unique()[6:]


def geocode_city(city):
    geolocator = Photon(user_agent=my_geocoder)
    try:
        geocode = RateLimiter(geolocator.geocode, max_retries=1, error_wait_seconds=0)
        location = geocode(city)
        if location is not None:
            print(location)
            return location
        else:
            return None
    except:
        return None


if retrieve:
    cities_long_name = [geocode_city(city) for city in cities_list]
    cities_long_name = [
        (location.raw['properties']['name'], location.point.latitude, location.point.longitude)
        if location is not None else (None, None, None) for location in cities_long_name]
    cities_long_name = pd.DataFrame(cities_long_name, columns=['city_name', 'lat', 'long'])
    cities_long_name.to_parquet(r"data/city_regulations/names_cities.pqt")
else:
    cities = pd.read_parquet(r"data/city_regulations/names_cities.pqt")
    cities = cities.rename(columns={'city_name': 'city'}).assign(city_name=cities_list)
    df = pd.merge(df, cities, on='city_name', how='left')
    df['geometry'] = gpd.points_from_xy(df.long, df.lat, crs=4326)
    df = gpd.GeoDataFrame(df, geometry='geometry')
    # clean = gpd.read_file(r"data/between/cities_clean.pqt")
    df.to_file(r"data/city_regulations/names_cities_clean.geojson")

"""""

import pandas as pd
import numpy as np
import os
os.environ['USE_PYGEOS'] = '0'
import geopandas as gpd
import json
from shapely import Point
from bs4 import BeautifulSoup
import pandas as pd
from glob import glob
import os
import re


def parse_htmls():

    parsed = glob(r"../data/city_regulations/*.html")
    results = {}

    for url in parsed:
        id_ = os.path.split(url)[-1].split('.')[0]
        results[id_] = list()
        with open(url) as fp:
            soup = BeautifulSoup(fp, 'html.parser')

        panel = soup.find(id='collapseOne_100')
        if panel is not None:
            text_elements = panel.find_all(text=True)
            pattern = re.compile(r'\b(?:20[0-1][0-9]|202[0-5])\b')

            closest_number = None
            min_distance = float('inf')

            for text in text_elements:
                matches = re.findall(pattern, text)
                for match in matches:
                    from_index = text.find("from")
                    since_index = text.find("since")
                    number_index = text.find(match)
                    distance = abs(from_index - number_index) if from_index != -1 else abs(since_index - number_index)
                    if distance < min_distance:
                        min_distance = distance
                        closest_number = match
                    results[id_].extend([match])
        else:
            results[id_].extend(["1"])

    df = pd.DataFrame.from_dict(results, orient='index')
    df.columns = [str(col) for col in df.columns]
    df.to_parquet(r"../data/city_regulations/dates.pqt")


# paths and configuration
load_years = range(2007, 2022)
city_years = [2007, 2010, 2015, 2020]
between_path = r"data/between/{}"


def other():

    # 1-. load regulation data
    with open(path, "r") as f:
        df = json.load(f)
    scraped = pd.DataFrame(df['data'])
    dates = pd.read_parquet(r"data/city_regulations/dates.pqt")
    df = pd.merge(scraped, dates.reset_index().rename(columns={'index': 'id'})[['0', 'id']],
                  on='id', how='left').rename(columns={'0': 'initreg'})
    # this is the beginning of the regulation year
    df['initreg'] = df['initreg'].astype(float).replace(1, np.nan)
    df['geometry'] = gpd.points_from_xy(df['city_longitude'], df['city_latitude'])
    df = gpd.GeoDataFrame(df.rename(columns={'id': 'reg_id'}), crs=4326)

    # 2-. load between sample
    vector = gpd.read_file(os.path.join(between_path.format(2020), "between_v1.geojson")).to_crs(4326)
    vector = vector[['id', 'geometry', 'country_id']]

    # 3-. do merge with regulations
    db = vector.sjoin(df).dropna(subset='initreg')
    db = db[(db.initreg <= float(2020)) & (db.initreg >= 2007)].drop_duplicates(subset=['id', 'initreg', 'scheme_color'])

    def get_reg(x):

        year = x.initreg.min()

        return pd.DataFrame({
            "year": np.array(list(load_years)),
            "timetotreat": np.array(list(load_years)) - year,
            "treat": ((np.array(list(load_years)) - year) > 0).astype(int),
            "yeartreat": [year] * len(load_years)
        })
    db = db.groupby(['id']).apply(
        lambda x: get_reg(x)).reset_index().drop(
        'level_1', axis=1)

    # 4-. merge with all database
    vector = pd.read_parquet(r"data/between/cities_clean.pqt")
    treat_df = pd.merge(vector, db, on=['id', 'year'], how='left')
    treat_df['yeartreat'].fillna(np.inf, inplace=True)
    treat_df['timetotreat'].fillna(-np.inf, inplace=True)
    treat_df['treat'].fillna(0, inplace=True)
    treat_df.to_parquet(r"data/between/cities_clean_uar.pqt")


path = r"data/mapapi.php"


if __name__ == '__main__':

    other()