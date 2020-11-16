#!/usr/bin/env python3
import pandas as pd
import inquirer
import glob
import os
from dotenv import load_dotenv

load_dotenv('../.env_db')
PGUSER = os.getenv('POSTGRES_USER')
PGPASS = os.getenv('POSTGRES_PASSWORD')
PGHOST = 'localhost'
PGDB = os.getenv('POSTGRES_DB')

questions = [
    inquirer.Path('maps_dir',
                  message="Enter directory where the gpkg maps located(don't forget the trailing '/')",
                  path_type=inquirer.Path.DIRECTORY,
                  )
]


def insert_into_postgis(pth):
    maps = glob.glob(os.path.join(pth, '*gadm36_*.gpkg'))

    for m in maps:
        fname = os.path.split(m)[-1]
        print(f"Inserting {fname} into PostGIS.")
        splitname = m.split('_')
        country_name = splitname[0]
        country_ISO_code = splitname[2].split('.')[0]
        os.system(f'ogr2ogr -f PostgreSQL PG:"dbname=\'{PGDB}\' host=\'{PGHOST}\' port=\'5432\' user=\'{PGUSER}\' password=\'{PGPASS}\'" {m}')



answers = inquirer.prompt(questions)
insert_into_postgis(answers['maps_dir'])