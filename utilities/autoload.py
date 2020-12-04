#!/usr/bin/env python3
"""
Script for automatically loading the data files into the database.
"""

import loaddata, load_gpkg_maps, load_dictionaries


## Loading CSV linelists
params = {
    'data_dir': './csv/',
    'table_name': 'linelist'
}
loaddata.main(params, auto=True)

## Loading Maps
params = {
    'maps_dir': './maps/',
}
load_gpkg_maps.main(params)

## Loading Dictionaries
params = {
    'dict_dir': './dicts/',
}
load_dictionaries(params)