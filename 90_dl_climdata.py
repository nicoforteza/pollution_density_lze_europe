import os
import cdsapi

c = cdsapi.Client()

# 7345ec27-10b8-4a43-8899-6a30bb09db14
# os.environ["CDSAPI_KEY"] = "264918:5cfac463-ddd0-4735-b130-2d4c5027462e"
# os.environ["CDSAPI_URL"] = "https://cds.climate.copernicus.eu/api/v2"

variables = ['total_precipitation', '10m_wind_speed', '2m_temperature', 'total_precipitation']
years = range(2022, 2023)

for var in variables:
    for year in years:
        c.retrieve(
            'reanalysis-era5-single-levels-monthly-means',
            {
                'format': 'grib',
                'product_type': 'monthly_averaged_reanalysis',
                'variable': [
                    var
                ],
                'year': str(year),
                'month': [
                    '01', '02', '03',
                    '04', '05', '06',
                    '07', '08', '09',
                    '10', '11', '12',
                ],
                'time': '00:00',
            },
            r'data/controls/climate/{}_{}.grib'.format(year, var))
        print("Saved {} climate data".format(year))
