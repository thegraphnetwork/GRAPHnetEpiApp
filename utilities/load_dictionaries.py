#!/usr/bin/env python3
"""
Script for loading the admin name dictionaries into PostgreSQL
"""
import pandas as pd
from pandas.errors import ParserError
from sqlalchemy import create_engine, Table, MetaData, exc
from tqdm import tqdm
from psycopg2.extensions import AsIs
import json


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
    inquirer.Path('dict_dir',
                  message="Enter directory where the name dictionary JSON files are(don't forget the trailing '/')",
                  path_type=inquirer.Path.DIRECTORY,
                  )
]

def dict_generate(pth):
    ds = glob.glob(os.path.join(pth, '*_namedict.json'))
    for d in ds:
        cname = os.path.split(d)[-1].split('_namedict.json')[0]
        cname = cname.replace('_', ' ').title()
        dic = json.load(open(d))[0]
        yield dic, cname
    else:
        print("All dictionaries loaded")

def load_into_db(gen, table_name):
    with pg_engine.connect() as connection:
        insert_sql = f"INSERT INTO {table_name}(name, population, admin2_dictionary) VALUES(%s,%s,%s);"
        for dic, cname in gen:
            print(f"Loading {cname} dictionary")
            connection.execute(insert_sql, [cname, 0, json.dumps(dic)])


def main(answers):
    dicgen = dict_generate(answers['dict_dir'])
    load_into_db(dicgen, 'country')


if __name__ == "__main__":
    answers = inquirer.prompt(questions)
    main(answers)