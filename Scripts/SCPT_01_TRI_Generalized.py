import numpy
import pandas
import geopandas
import traceback
import datetime

# This is just for debuging display purposes
pandas.set_option('display.max_columns', None)  # or 1000
pandas.set_option('display.max_rows', None)  # or 1000
pandas.set_option('display.max_colwidth', 50)  # or 199

# Create Function Definitions
try:
    print ('Creating Function Definitions')
    def md_str(value):
        if value < 10:
            return '0'+str(value)
        else:
            return str(value)
    def date_process(df, date_field):
        df[date_field] = df[date_field].str.replace(r'/', r'-')
        df[date_field] = df[date_field].str.replace(r'.', r'-')
        df = df[~df[date_field].isin([numpy.NAN])]
        df[['D1', 'D2', 'D3']] = df[date_field].str.split(r'-', expand=True)
        dl = 1
        for part in [df.D1.unique(), df.D2.unique(), df.D3.unique()]:
            for i in range(0, len(part)):
                part[i] = int(part[i])
            part.sort()
            value = part[-1]
            print(value)
            if (value < 13) & (value > 0):
                df['MONTH'] = df.pop('D' + str(dl))
                print('MONTH')
            if (value > 20) & (value < 32):
                df['DAY'] = df.pop('D' + str(dl))
                print('DAY')
            if value > 1000:
                df['YEAR'] = df.pop('D' + str(dl))
                print('YEAR')
            dl += 1
        df['YEAR'] = df['YEAR'].apply(lambda x: str(x))
        df['MONTH'] = df['MONTH'].apply(lambda x: md_str(int(x)))
        df['DAY'] = df['MONTH'].apply(lambda x: md_str(int(x)))
        df[date_field] = df.pop('YEAR') + r'-' + df.pop('MONTH') + r'-' + df.pop('DAY')
        return df
    print('Function Definitions Created')
except Exception:
    print('Function Definitions Failed')
    traceback.print_exc()

# Importing Data
try:
    print ('Importing Data')
    # Importing Data
    df = pandas.read_csv(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Cabo_Verde\Base_Data\Cabo_Verde_clean.csv')
    # Formatting the Date Column and filling NAs with lab res date column
    df.loc[df['report_date'].isin([numpy.NAN]), 'report_date'] = df['Lab_resdate']
    df = date_process(df, 'report_date')
    # calculating Additional Case Age Statistics fields
    df[['Cases_Under30', 'Cases_Over30', 'Cases_Age_NA']] = 0
    df.loc[(df['patinfo_ageonset_years'] <= 30) & (~df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (df['report_classif'].isin(['CONFIRMED'])) & (df['patcourse_status'].isin(['ALIVE', numpy.NAN, 'RECOVERED'])), 'Cases_Under30'] = 1
    df.loc[(df['patinfo_ageonset_years'] > 30) & (~df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (df['report_classif'].isin(['CONFIRMED'])) & (df['patcourse_status'].isin(['ALIVE', numpy.NAN, 'RECOVERED'])), 'Cases_Over30'] = 1
    df.loc[(df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (df['report_classif'].isin(['CONFIRMED'])) & (df['patcourse_status'].isin(['ALIVE', numpy.NAN, 'RECOVERED'])), 'Cases_Age_NA'] = 1
    # calculating Additional Death Age Statistics
    df[['Deaths_Under30', 'Deaths_Over30', 'Deaths_Age_NA']] = 0
    df.loc[(df['patinfo_ageonset_years'] <= 30) & (~df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (
        df['report_classif'].isin(['CONFIRMED'])) & (df['patcourse_status'].isin(['DEAD', 'dead'])), 'Deaths_Under30'] = 1
    df.loc[(df['patinfo_ageonset_years'] > 30) & (~df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (
        df['report_classif'].isin(['CONFIRMED'])) & (df['patcourse_status'].isin(['DEAD', 'dead'])), 'Deaths_Over30'] = 1
    df.loc[(df['patinfo_ageonset_years'].isin([numpy.NAN, 'nan'])) & (df['report_classif'].isin(['CONFIRMED'])) & (
        df['patcourse_status'].isin(['DEAD', 'dead'])), 'Deaths_Age_NA'] = 1
    # Calculating Additional Test positivity statistics
    df[['Confirmed_Cases', 'UnConfirmed_Cases']] = 0
    df.loc[(df['report_classif'].isin(['CONFIRMED'])), 'Confirmed_Cases'] = 1
    df.loc[(~df['report_classif'].isin(['CONFIRMED'])), 'UnConfirmed_Cases'] = 1
    # Calculating Cases and Deaths
    df['Daily_Cases'] = df['Cases_Under30'] + df['Cases_Over30'] + df['Cases_Age_NA']
    df['Daily_Deaths'] = df['Deaths_Under30'] + df['Deaths_Over30'] + df['Deaths_Age_NA']
    df['Daily_Recovered'] = 0
    df.loc[(df['patcourse_status'].isin(['RECOVERED'])) & (df['report_classif'].isin(['CONFIRMED'])), 'Daily_Recovered'] = 1
    # Aggregating by Admin 1 and Date
    df = df.groupby(['patinfo_resadmin1', 'report_date']).sum().reset_index()
    # Select out pertinent columns
    df = df[['patinfo_resadmin1', 'report_date', 'Cases_Under30', 'Cases_Over30', 'Cases_Age_NA',
             'Cases_Under30', 'Cases_Over30', 'Cases_Age_NA', 'Daily_Cases', 'Daily_Deaths', 'Daily_Recovered']]
    # Merge Data With Context
    ctx = pandas.read_csv(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Cabo_Verde\Intermediate_Products\CPV_CTX.csv')
    df = df.merge(ctx, on='patinfo_resadmin1', how='left').drop(['admin1Name', 'patinfo_res1'], 1)
    # admin1Pcod is an unique alphanumeric identifier for the admin level 1
    df = df[['admin1Pcod', 'report_date', 'Daily_Cases', 'Daily_Deaths', 'Daily_Recovered']]
    print(df.head(10))
    # Import population and fill in missing dates
    pop = pandas.read_csv(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Cabo_Verde\Intermediate_Products\CPV_POP.csv')
    pop = pop[['admin1Pcod', 'Population', 'Shape_Area', 'Shape_Leng']]
    pop['del'] = 1
    # Create Time series to fill in missing dates
    start = datetime.datetime(2020, 3, 3)
    end = datetime.datetime(2020, 6, 18)
    d_base = pandas.date_range(start, end).to_frame().rename(columns={0: 'report_date'}).reset_index().drop('index', 1)
    d_base['report_date'] = d_base['report_date'].apply(lambda x: str(x))
    d_base[['report_date', 'time']] = d_base['report_date'].str.split(r' ', expand=True)
    d_base = d_base.drop('time', 1)
    d_base['del'] = 1
    # Merge Time Series with Population Data
    pop = pop.merge(d_base, on='del').drop('del', 1)
    print(pop.head())
    # Merge Population and Case Data
    df = df.merge(pop, on=['admin1Pcod', 'report_date'], how='right').fillna(0)
    # Calculating Cumulative Sums
    df = df.sort_values('report_date', ascending=True)
    df = df.fillna(0)
    print(df.head())
    print(df.info())
    df['Cum_Cases'] = df.groupby('admin1Pcod')['Daily_Cases'].transform(lambda x: x.cumsum())
    df['Cum_Deaths'] = df.groupby('admin1Pcod')['Daily_Deaths'].transform(lambda x: x.cumsum())
    df['Cum_Recovered'] = df.groupby('admin1Pcod')['Daily_Recovered'].transform(lambda x: x.cumsum())
    print (df.head(20))
    print('Import Complete')
except Exception:
    print('Import Failed')
    traceback.print_exc()

# Calculate Transmission Risk Index
#"""
try:
    print('Calculating Transmission Risk Index')
    # Defining TRI Function
    def cal_den(df, case_den, cases, population, area, window=1.0):
        df[case_den] = ((df[cases] / df[population] * 100000) / df[area] * 100).round(4).fillna(0).replace([numpy.inf, -numpy.inf], 0)
        return df

    # Calculating TRI
    adm1_id = 'admin1Pcod'
    df = df[~df[adm1_id].isin([numpy.NAN])]
    df = df.sort_values('report_date', ascending=True)
    # Calculating Raw IDW and Base Normalization
    df = cal_den(df, 'TRI_RIDX', 'Cum_Cases', 'Population', 'Shape_Area')
    df['TRI_IDX'] = df.groupby('report_date')['TRI_RIDX'].transform(lambda x: (x / x.max() * 100).round(2)).fillna(0).replace([numpy.inf, -numpy.inf])
    # This portion is to set outliers to a specific value as to increase the visual difference between the lower TRI values
    df.loc[~df[adm1_id].isin(['CV10']), 'TRI_IDX2'] = \
        df[~df[adm1_id].isin(['CV10'])].groupby('report_date')['TRI_RIDX'].transform(
            lambda x: (x / x.max() * 75).round(2)).fillna(0).replace([numpy.inf, -numpy.inf], 0)
    df.loc[df[adm1_id].isin(['CV10']), 'TRI_IDX2'] = \
        df[df[adm1_id].isin(['CV10'])].groupby('report_date')['TRI_RIDX'].transform(
            lambda x: (x / x.max() * 100).round(2)).fillna(0).replace([numpy.inf, -numpy.inf], 0)
    # set only 1 patinfo_resadmin1 per admin1 ID
    ctx = ctx.groupby('admin1Pcod').first().reset_index()
    # Merge Back with Context
    df = df.merge(ctx, on='admin1Pcod', how='left')
    df.set_index('patinfo_resadmin1').to_csv(r'D:\\Users\Paul\Documents\HSR\Projects\Africa_Project\Cabo_Verde\Final_Products\CPV_adm01_TRI.csv', encoding='utf-8')
    print('Transmission Risk Index Calculated')
except Exception:
    print('Transmission Risk Index Failed')
    traceback.print_exc()
#"""