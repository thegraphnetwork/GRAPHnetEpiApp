#!/usr/bin/env python3
"""
Script for loading the CSVs into PostgreSQL
"""
import pandas as pd
from pandas.errors import ParserError
from sqlalchemy import create_engine, Table, MetaData, exc
from tqdm import tqdm
from psycopg2.extensions import AsIs

import inquirer
import os
import glob
from dotenv import load_dotenv

load_dotenv('../.env_db')
PGUSER = os.getenv('POSTGRES_USER')
PGPASS = os.getenv('POSTGRES_PASSWORD')
PGHOST = 'localhost'
PGDB = os.getenv('POSTGRES_DB')

# Create connection
pg_engine = create_engine(f"postgresql://{PGUSER}:{PGPASS}@{PGHOST}/{PGDB}")

questions = [
    inquirer.Path('data_dir',
                  message="Enter directory where the clean CSVs located(don't forget the trailing '/')",
                  path_type=inquirer.Path.DIRECTORY,
                  ),
    inquirer.Text('table_name',
                  message="Type the name of the table in which to insert the data",
                  default='line_list'
                  )
]
alter_table_questions = [
    inquirer.Confirm(
        'add_column',
        message="Do you want to add this column to the line_list table?",
        default=False,
    )
]


def get_table_info(table_name):
    """
    :return table information from database
    :param table_name:
    :return:
    """
    metadata = MetaData()
    table = Table(table_name, metadata, autoload=True, autoload_with=pg_engine)
    return table


def load_csvs(pth):
    """
    Generator that loads CSV files with pandas reporting any parsing error
    :param pth: Path to directory containing the CSVs
    :return: DataFrame with the contents of the CSVs
    """
    csvs = glob.glob(os.path.join(pth, '*_clean.csv'))
    for csv in csvs:
        cname = os.path.split(csv)[-1]
        print(f'loading {cname}')
        try:
            df = pd.read_csv(csv, encoding='utf8', quoting=1, quotechar='"', low_memory=False).convert_dtypes()
            yield df
        except ParserError as e:
            print(f"Problem parsing {cname}")
            print(e)
            continue
        except UnicodeDecodeError as ue:
            print(f"Encoding of {cname} is not UTF-8")
            print(ue)
            continue
        except Exception as oe:
            print(f"Unclassified error when reading {cname}")
            print(oe)
            continue


def add_new_column(table_name, column_name, column_type):
    with pg_engine.connect() as connection:
        connection.execute(f"ALTER TABLE {table_name} ADD COLUMN {column_name} {column_type};")


def validate_column_types(df):
    """
    Makes sure the types of every column are consistent
    :param df: dataframe
    :return: dataframe
    """
    _null = AsIs('NULL')
    for column in df.columns:
        if 'date' in column:
            df[column] = pd.to_datetime(df[column], errors="coerce")
        if column == 'expo_sourcecaseids':
            df[column] = [s if pd.isnull(s) else s.replace(',','|') for s in df[column]]
        # if '_id' in column:
        #     df[column] = df[column].astype(int,)
    # df.fillna(value=_null, inplace=True)
    df2 = df.where(pd.notnull(df), None)
    return df2


def insert_into_db(csvg, table_name):
    '''
    Pulls dataframes from the load_csvs generator and attempts to insert
    it into the database
    :param csvg: Generator yieldind dataframes
    :return:
    '''
    table = get_table_info(table_name)
    db_cols = [col.name for col in table.columns]
    for df in csvg:
        df.columns = [c.lower() for c in df.columns]  # make sure all column names are lowercase
        for c in df.columns:
            if c not in db_cols:
                print(f"Existing columns: {db_cols}\nMissing column: {c}\n Type: {df[c].dtype}")
                coltype = 'NUMERIC' if str(df[c].dtype) == 'float64' else 'VARCHAR(128)'
                ans = inquirer.prompt(alter_table_questions)
                if ans['add_column']:
                    add_new_column('line_list', column_name=c, column_type=coltype)
                else:
                    print(f'skipping...')
        df = validate_column_types(df)
        insert_sql = F" INSERT INTO {table_name}({','.join(df.columns)}) VALUES({','.join(['%s' for i in df.columns])}) ON CONFLICT DO NOTHING"
        with pg_engine.connect() as connection:
            print(f"Loading {len(df)} cases.")
            # pbar = tqdm(total=len(df), unit=' cases',)
            i=0
            for row in df.iterrows():
                row = row[1]
                try:
                    connection.execute(insert_sql, row.where(pd.notnull(row),None))
                    # pbar.update(i)
                    i += 1
                except exc.ProgrammingError as e:
                    print(row)
                    print(row.where(pd.notnull(row),None))
                    raise(e)
            # pbar.close()
        # try:
        #     df.to_sql(table_name, con=pg_engine, if_exists='append', index=False, method=None)  # 'multi')
        # except exc.IntegrityError as e:
        #     print(e)


answers = inquirer.prompt(questions)
csvgen = load_csvs(answers['data_dir'])
insert_into_db(csvgen, answers['table_name'])
