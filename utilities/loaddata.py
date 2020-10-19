#!/usr/bin/env python3
"""
Script for loading the CSVs into Postgre
"""
import ibis
import inquirer
import os
from dotenv import load_dotenv
load_dotenv('../.env_db')

pg_con = ibis.postgres.connect(
    user=os.getenv('SHINYAPP_SUPERUSER_USERNAME'),
    password=os.getenv('SHINYAPP_SUPERUSER_PASSWORD'),
    host='localhost',
    port=5432,
    database=os.getenv('SHINYAPP_DBNAME'),
)


questions = [
    inquirer.Path('data_dir',
                  message='Where are the clean CSVs located?',
                  path_type=inquirer.Path.DIRECTORY,
                  ),
]

answers = inquirer.prompt(questions)