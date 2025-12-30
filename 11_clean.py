from scipy.spatial.distance import cdist
import warnings

from scipy.spatial.distance import cdist

from config import *

warnings.filterwarnings('ignore')

eur = get_europe()
eurproj = eur.to_crs(4326)
years = range(2007, 2023)
between_path = r"data/between/{}"
within_path = r"data/within/{}/"
iv_cols_patt = r"cell_id|year|nutrient|soil|mean|id_city|pop|pol|aquif|hist|latitude|longitude|km_to_city|is_city$|dist|country_id|area|prec|wind|temp"
change_year = False
if change_year:
    rev = pd.read_excel(r"data/uar/lez_revise.xlsx")[['id', 'yeartreat2']]
    rev.columns = ['id', 'yeartreat']


def read_and_clean_between(year):
    df = gpd.read_file(os.path.join(between_path.format(year), "between_v1.geojson"))
    df = df.drop("geometry", axis=1).dropna()
    print(year, df.shape)
    return df


def read_and_clean_within(year):
    df = pd.read_parquet(os.path.join(within_path.format(year), "within.pqt")).dropna().assign(year=year)
    df2 = gpd.read_file(os.path.join(between_path.format(year), "between_v1.geojson"))
    df = pd.merge(df, df2.rename(columns={'id': 'city'})[["city", 'year', 'country_id']], on=['city', 'year'])
    print("Within shape for year {}: {}".format(year, df.shape))
    return df


def read_and_clean_within_lze(year):

    df = pd.read_parquet(os.path.join(within_path.format(year), "within_lze.pqt")).assign(year=year)
    cities = get_cities(year)
    lzes = gpd.read_file(r"data/uar/uar_data_new_euro.geojson")

    # merge df with cities by id
    df = pd.merge(cities.drop(['year', 'geometry'], axis=1).rename(
        columns={'id': 'id_city'}), df, how='right', on='id_city')
    # fix country
    geometry = gpd.points_from_xy(df['longitude'], df['latitude'], crs="EPSG:4326")
    geometry = gpd.GeoDataFrame({"cell_id": df.index}, geometry=geometry, crs="EPSG:4326")
    geometry = geometry.sjoin(eurproj)[['CNTR_CODE', 'cell_id']].set_index("cell_id")
    df = pd.merge(geometry, df, left_index=True, right_index=True, how='right').drop(
        ["country_id"], axis=1).rename(
        columns={'CNTR_CODE': 'country_id'}
    )

    # merge df with lezs by uar id
    lzes = lzes.drop(
        ["geometry", "id"], axis=1).rename(
        columns={'id_uar': 'id_lez_5km', 'city': 'city_name'}
    )
    df = pd.merge(lzes, df, on='id_lez_5km', how='right')
    df.index.name = 'cell_id'
    df['timetotreat'] = year - df['yeartreat']
    df = df.replace({'timetotreat': {np.nan: -np.Inf}, 'yeartreat': {np.nan: np.Inf}})

    if change_year:
        lzes = lzes.drop("yeartreat", axis=1)
        lzes = pd.merge(lzes, rev, on='id')

    # has the cell interference?
    df['city_25km_itf'] = (df.is_city_25km_collapse > 1).astype(int)
    df['lez_5km_itf'] = (df.is_lez_5km_collapse > 1).astype(int)

    # some other modifications
    df['coast_dist'] = df['coast_dist'] / 1e4
    df['river_dist'] = df['river_dist'] / 1e4

    # identify the year of the spillover for each cell:
    # extend the year treat to the affected cells, not only the identified by LEZ
    # this applies only to id city cells outside the 10 km perimeter of the LEZ but inside the city

    df['city_affected'] = 0
    t_ids = df[(df.id_lez_5km > 0) & (df.is_city == 1)].id_city.unique()
    for idx in t_ids:
        city_data = df[df.id_city == idx]
        lez_ids = city_data[city_data.id_lez > 0].id_lez.unique()
        if len(lez_ids) == 1:
            print("City {} with {} interference cells and 1 LEZ".format(idx, city_data.lez_5km_itf.sum()))
            city_data[lzes.columns[1:]] = city_data[lzes.columns[1:]].replace(
                {np.Inf: np.nan}).bfill().ffill()
            city_data['timetotreat'] = year - city_data['yeartreat']
            df.loc[city_data.index, :] = city_data
        elif len(lez_ids) == 0:
            lez_ids = city_data[city_data.id_lez_5km > 0].id_lez_5km.unique()
            if len(lez_ids) == 1:
                print("City {} with {} interference cells and "
                      "no LEZ - affected by 1 LEZ (id: {})".format(idx, city_data.lez_5km_itf.sum(), lez_ids))
                city_data[lzes.columns[1:]] = city_data[lzes.columns[1:]].replace(
                    {np.Inf: np.nan}).bfill().ffill()
                city_data['timetotreat'] = year - city_data['yeartreat']
                city_data['city_affected'] = 1
                df.loc[city_data.index, :] = city_data
            else:
                print("City {} with {} interference cells and "
                      "no LEZ - affected by multiple LEZ (id {})".format(idx, city_data.lez_5km_itf.sum(), lez_ids))

                df_with_group = city_data[city_data.id_lez_5km > 0]
                df_without_group = city_data[city_data.id_lez_5km.isin([-1, -2])]

                if len(df_with_group) == 0:
                    raise ValueError("No groups to assign")

                coords_with_group = df_with_group[['latitude', 'longitude']].values
                coords_without_group = df_without_group[['latitude', 'longitude']].values
                distances = cdist(
                    coords_without_group,
                    coords_with_group,
                    metric='euclidean'
                )
                nearest_indices = np.argmin(distances, axis=1)
                nearest_groups = df_with_group.id_lez_5km.values[nearest_indices]
                city_data.loc[df_without_group.index, 'id_lez_5km'] = nearest_groups
                city_data = city_data.drop(lzes.columns[1:], axis=1)
                save_index = city_data.index
                city_data = pd.merge(lzes, city_data, on='id_lez_5km', how='right')
                city_data.index = save_index
                city_data['timetotreat'] = year - city_data['yeartreat']
                city_names = city_data.city_name.unique()
                if len(city_names) > 1:
                    if ("Rotterdam" or "The Hague") in city_names:
                        city_data['city_name'] = 'Rotterdam - Hague'
                    elif ("Düsseldorf" or "Wuppertal" or "Remscheid") in city_names:
                        city_data['city_name'] = 'Düsseldorf Area'
                    elif ('Recklinghausen' or 'Herne' or 'Bottrop' or 'Oberhausen' or
                          'Gelsenkirchen' or 'Essen' or 'Bochum' or 'Dortmund' or 'Duisburg') in city_names:
                        city_data['city_name'] = 'Westfalia'
                    elif ("Stuttgart" or "Ludwigsburg") in city_names:
                        city_data['city_name'] = 'Stuttgart'
                    elif ("Wiesbaden" or "Main") in city_names:
                        city_data['city_name'] = 'Mainz'
                df.loc[city_data.index, :] = city_data

        elif len(lez_ids) > 1:
            print("City {} with {} interference cells and >1 LEZ".format(idx, city_data.lez_5km_itf.sum()))

            df_with_group = city_data[city_data.id_lez_5km > 0]
            df_without_group = city_data[city_data.id_lez_5km.isin([-1, -2])]

            if len(df_with_group) == 0:
                raise ValueError("No groups to assign")

            coords_with_group = df_with_group[['latitude', 'longitude']].values
            coords_without_group = df_without_group[['latitude', 'longitude']].values
            distances = cdist(
                coords_without_group,
                coords_with_group,
                metric='euclidean'
            )
            nearest_indices = np.argmin(distances, axis=1)
            nearest_groups = df_with_group.id_lez_5km.values[nearest_indices]
            city_data.loc[df_without_group.index, 'id_lez_5km'] = nearest_groups
            city_data = city_data.drop(lzes.columns[1:], axis=1)
            save_index = city_data.index
            city_data = pd.merge(lzes, city_data, on='id_lez_5km', how='right')
            city_data.index = save_index
            city_data['timetotreat'] = year - city_data['yeartreat']
            city_names = city_data.city_name.unique()
            if len(city_names) > 1:
                if ("Rotterdam" or "The Hague") in city_names:
                    city_data['city_name'] = 'Rotterdam - Hague'
                elif ("Düsseldorf"  or "Wuppertal" or "Remscheid") in city_names:
                    city_data['city_name'] = 'Düsseldorf Area'
                elif ('Recklinghausen' or 'Herne' or 'Bottrop' or 'Oberhausen' or
                      'Gelsenkirchen' or 'Essen' or 'Bochum' or 'Dortmund' or 'Duisburg') in city_names:
                    city_data['city_name'] = 'Westfalia'
                elif ("Stuttgart" or "Ludwigsburg") in city_names:
                    city_data['city_name'] = 'Stuttgart'
                elif ("Wiesbaden" or "Main") in city_names:
                    city_data['city_name'] = 'Mainz'

            df.loc[city_data.index, :] = city_data

    print("Within shape for year {}: {}".format(year, df.shape))
    return df


def clean_between():
    x = pd.concat([read_and_clean_between(year) for year in years])
    x.to_parquet(r"data/between/cities_clean_v2.pqt")


def clean_within():
    for year in years:
        read_and_clean_within(year).dropna().assign(year=year).to_parquet(
            r"data/within/within_clean_{}.pqt.gzip".format(year)
        )


def clean_within_lze():
    df = pd.concat([read_and_clean_within_lze(year) for year in years], axis=0)
    df = df[~df['pop'].isnull()].reset_index()  # remove water
    df.filter(regex=iv_cols_patt, axis=1).to_parquet(r"data/within/iv.pqt")
    df = df.loc[:, ~df.columns.str.match(r"nutrient|soil|aquif")]  # remove instruments
    # now id_lez_5km is all affected cells within a city, regardless of the distance
    df[df.id_lez_5km > 0].to_parquet(r"data/within/lez.pqt.gzip")


if __name__ == '__main__':

    # 1: between
    # clean_between()
    # 2: within + lze
    clean_within_lze()
