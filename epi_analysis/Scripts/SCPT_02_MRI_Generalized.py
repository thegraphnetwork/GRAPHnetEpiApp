from Projects.COVID_19.Scripts.New_Scripts.COVID_import import *
import sys
sys.path.insert(0, r'D:\Users\Paul\Documents\HSR\Projects\COVID_19\Scripts\New_Scripts\PRI')
from COVID_import import *

# Set Global Variables
#"""
try:
    start_msg('Setting Global Variables')
    vars = {}
    with open(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Faux_LL_Analysis\Scripts\variables.json', 'r') as fp:
        vars = json.load(fp)
    globals().update(vars)
    id_field = 'ISO_3DIGIT'
    com_msg('Global Variables Set')
except Exception:
    fail_msg('Failed to Set Global Variables')
#"""

# Import Base Data
#"""
try:
    start_msg('Importing Data')
    # Import Mortality Risk Factors by geographic region / admin region
    age_final = pd.read_csv(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Faux_LL_Analysis\Intermediate_Products\AFRO_adm00_MRI_Factors_Final.csv'
                            ).rename(columns={'ISO_Code':'ISO_3DIGIT'})
    df_debug(age_final, 'Age Final', 1, 0, 30)
    com_msg('Import Complete')
except Exception:
    fail_msg('Import Failed')
#"""

# Importing weights
#"""
try:
    start_msg('Importing Weights')
    sdoh_weights = pd.read_csv(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\COVID19_Risk_Weights.csv').drop('Full Label', axis=1)
    sdoh_weights = sdoh_weights[~sdoh_weights['Field'].isin(['EP_HOD', 'EP_U25K'])]
    index_fields = sdoh_weights['Field'].tolist()
    index_weights = sdoh_weights['Weight'].tolist()
    df_debug(sdoh_weights, 'SDOH Weights', 1, 0, 30)
    com_msg('Import Complete')
except Exception:
    fail_msg('Import Failed')
#"""

# Calculating area outside 35km
#"""
try:
    start_msg('Calculating gt 35k area')
    # Import geometry of the admin regions
    base_shp = gp.read_file(shp_dir + r'AFRO_adm00.shp')
    base_shp = base_shp.to_crs("EPSG:3857")
    base_shp['Area_1'] = base_shp.geometry.area
    # Import locations of medical facilities with a 35 km buffer around them
    fac_loc = gp.read_file(r'D:\Users\Paul\Documents\HSR\Projects\Africa_Project\Base_Geographies\Africa_Med_Fac_35K.shp')
    # Identify the proportion of the admin area that is outside of the 35km buffer from the medical facilities
    erase = gp.overlay(base_shp, fac_loc, how='difference')
    erase['Area_2'] = erase.geometry.area
    erase = erase[[id_field, 'Area_1', 'Area_2']]
    erase['Gt35K_RTO'] = (erase['Area_2'] / erase['Area_1'])
    df_debug(erase, 'Erase', 1, 0)
    df_debug(base_shp, 'base shp', 1, 0)
    com_msg('35k calculation complete')
except Exception:
    fail_msg('35k calculation failed')
#"""

# Calculating MRI
#"""
try:
    start_msg('Calculating MRI')
    # Merge the Mortality Risk Index Factors with the proportion of the admin area that is outside of the 35km buffer
    age_final = age_final.merge(erase, on=id_field, how='left').fillna(0)
    # Calculate the Mortality Risk Index
    age_final['MRI_IDX'] = ((age_final[index_fields[0]]*index_weights[0])).fillna(0).replace([np.inf, -np.inf], 0)
    for i in range(1, len(index_fields)):
        age_final['MRI_IDX'] = (age_final['MRI_IDX'] + (age_final[index_fields[i]] * index_weights[i])).fillna(0).replace([np.inf, -np.inf], 0)
    age_final['MRI_RIDX'] = (age_final['MRI_IDX'] / 9.47).round(2)
    # Incorporating the distance from medical facilities
    age_final['MRI_RIDX2'] = ((1-age_final['Gt35K_RTO']) * age_final['MRI_RIDX']) + (age_final['Gt35K_RTO'] * age_final['MRI_RIDX'] * 2.00).round(2)
    age_final['MIN2'] = age_final[~age_final['ISO_3DIGIT'].isin(['SSD'])]['MRI_RIDX2'].min()
    # Normalizing the MRI Index between 0 and 100
    age_final.loc[age_final['ISO_3DIGIT'].isin(['SSD']), 'MRI_IDX'] = 0
    age_final.loc[~age_final['ISO_3DIGIT'].isin(['SSD']), 'MRI_IDX'] = (
            (age_final['MRI_RIDX2'] - age_final.MIN2) * (100 / (age_final.MRI_RIDX2.max() - age_final['MIN2']))).replace([np.NAN, np.inf, -np.inf], 0).round(2)
    age_final['Gt35K_RTO'] = age_final['Gt35K_RTO'].round(4)
    age_final = age_final.drop(['Area_1', 'Area_2'], 1).set_index('Region')
    # Exporting MRI
    age_final.to_csv(final_dir + r'AFRO_adm00_MRI.csv')
    df_debug(age_final, 'Age Final', 1, 0)
    com_msg('MRI Complete')
except Exception:
    fail_msg('MRI Failed')
#"""