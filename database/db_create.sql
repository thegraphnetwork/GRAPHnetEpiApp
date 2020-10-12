-- Created by Vertabelo (http://vertabelo.com)
-- Last modification date: 2020-10-08 11:26:59.036

-- tables
-- Table: country
CREATE TABLE country (
    id serial  NOT NULL,
    name varchar(128)  NOT NULL,
    population bigint  NOT NULL,
    admin2_dictionary json  NOT NULL,
    CONSTRAINT country_pk PRIMARY KEY (id)
);

-- Table: line_list
CREATE TABLE line_list (
patinfo_ID VARCHAR(50),
report_date DATE,
patinfo_first_name VARCHAR(50),
patinfo_last_name VARCHAR(50),
patinfo_ageonset_years INT,
patinfo_ageonset_months INT,
patcourse_status VARCHAR(50),
patinfo_sex VARCHAR(4),
patinfo_resadmin1 VARCHAR(50),
patinfo_resadmin2 VARCHAR(50),
patinfo_occus VARCHAR(50),
patinfo_occus_specify VARCHAR(50),
expo_travel VARCHAR(50),
expo_travel_country VARCHAR(50),
expo_date_departure DATE,
pat_symptomatic VARCHAR(50),
pat_contact VARCHAR(50),
patcourse_dateonset DATE,
expo_sourcecaseids VARCHAR(50),
patcourse_severity VARCHAR(50),
report_classif VARCHAR(50),
patcourse_datedeath DATE,
patinfo_resadmin3 VARCHAR(50),
patinfo_resadmin4 VARCHAR(50),
report_orginst VARCHAR(50),
patinfo_idadmin1 VARCHAR(50),
patinfo_idadmin2 VARCHAR(50),
report_pointofentry VARCHAR(50),
report_pointofentry_date DATE,
consultation_dateHF DATE,
patcourse_admit VARCHAR(50),
patcourse_presHCF DATE,
patcourse_comp VARCHAR(50),
patsympt_fever VARCHAR(50),
patsympt_sorethroat VARCHAR(50),
patsympt_cough VARCHAR(50),
patsympt_runnynose VARCHAR(50),
patsympt_short VARCHAR(50),
patsympt_other VARCHAR(50),
Comcond_preexist1 VARCHAR(50),
Comcond_preexist VARCHAR(50),
expo_healthcare VARCHAR(50),
expo_ari VARCHAR(50),
expo_aricontsetting VARCHAR(50),
expo_other VARCHAR(50),
expo_contact_case VARCHAR(50),
expo_ID1 INT,
expo_ID2 INT,
expo_ID3 INT,
expo_ID4 INT,
expo_arisetting VARCHAR(50),
Lab_coll VARCHAR(50),
Lab_type VARCHAR(50),
Lab_datetaken DATE,
Lab_performed VARCHAR(50),
Lab_result VARCHAR(50),
Lab_resdate DATE,
Lab_other VARCHAR(50),
Lab_otherres VARCHAR(50),
patcourse_datedischarge DATE,
name_first VARCHAR(50),
name_last VARCHAR(50),
expo_visit_healthcare VARCHAR(50),
lab_otherres VARCHAR(50),
PRIMARY KEY (patinfo_ID)
);

-- foreign keys
-- Reference: line_list_country (table: line_list)
ALTER TABLE line_list ADD CONSTRAINT line_list_country
    FOREIGN KEY (country_id)
    REFERENCES country (id)
    NOT DEFERRABLE
    INITIALLY IMMEDIATE
;

-- End of file.
