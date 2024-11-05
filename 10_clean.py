import numpy as np

from config import *
import warnings
warnings.filterwarnings('ignore')

eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2007, 2022)
between_path = r"data/between/{}"
within_path = r"data/within/{}/"


def read_and_clean_between(year):
    df = gpd.read_file(os.path.join(between_path.format(year), "between_v1.geojson"))
    print(df.crs)
    df = df.drop("geometry", axis=1).dropna()
    return df


def read_and_clean_within(year):
    df = pd.read_parquet(os.path.join(within_path.format(year), "within.pqt")).dropna().assign(year=year)
    df2 = gpd.read_file(os.path.join(between_path.format(year), "between_v1.geojson"))
    df = pd.merge(df, df2.rename(columns={'id': 'city'})[["city", 'year', 'country_id']], on=['city', 'year'])
    print("Within shape for year {}: {}".format(year, df.shape))
    return df


def read_and_clean_within_lze(year):
    df = pd.read_parquet(os.path.join(within_path.format(year), "within_lze.pqt"))#.dropna().assign(year=year)
    df = df[df.city_buffer > 0]
    cities = get_cities(year)
    lzes = gpd.read_file(r"data/uar/uar_data_new_euro.geojson")
    aux = pd.merge(cities.drop('geometry', axis=1),
                   lzes.drop(['geometry', 'id_uar'], axis=1).drop_duplicates(subset='id'),
                   on='id', how='left')
    aux['year'] = year
    aux['timetotreat'] = aux['year'] - aux['yeartreat']
    df['id'] = df['city_buffer'].astype(int)
    df = df.drop(['city_buffer'], axis=1)
    cols = ['city', 'lze', 'lze_1km', 'lze_3km', 'lze_5km', 'lze_10km', 'lze_25km']
    df[cols] = (df[cols] > 0).astype(int).values
    df = pd.merge(df, aux.rename(columns={'city': 'city_name'}), on='id', how='left')

    print("Within shape for year {}: {}".format(year, df.shape))
    return df


def clean_between():
    x = pd.concat([read_and_clean_between(year) for year in years])
    x.to_parquet(r"data/between/cities_clean.pqt")


def clean_within():
    for year in years:
        read_and_clean_within(year).dropna().assign(year=year).to_parquet(
            r"data/within/within_clean_{}.pqt.gzip".format(year)
        )


def clean_within_lze():
    df = pd.concat([read_and_clean_within_lze(year) for year in years], axis=0)

    def get_reg(x):
        year = x.yeartreat.min()
        if np.isnan(year):
            x['yeartreat'] = np.Inf
            x['timetotreat'] = -np.Inf
        else:
            x['yeartreat'] = year
            x['timetotreat'] = x['year'] - year
            x['city_name'] = x['city_name'].bfill().ffill()
        return x

    df = df.groupby(['id']).apply(lambda x: get_reg(x))
    treat_mask = ~df.city_name.isnull()
    df['treat'] = treat_mask.astype(int)
    df[['yeartreat', 'timetotreat']] = df[['yeartreat', 'timetotreat']].fillna(-np.Inf)
    df.to_parquet(
            r"data/within/within_clean_lze.pqt.gzip"
        )


if __name__ == '__main__':

    # 1: clean between for regressions
    # clean_between()
    # 2: clean within for regressions
    # clean_within()
    # 3: clean within lze for regressions
    clean_within_lze()
