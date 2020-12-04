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
id serial NOT NULL,
patinfo_id VARCHAR(256),
report_date DATE,
--patinfo_first_name VARCHAR(256),
--patinfo_last_name VARCHAR(256),
patinfo_ageonset_years INT,
patinfo_ageonset_months INT,
patcourse_status VARCHAR(256),
patinfo_sex VARCHAR(32),
patinfo_resadmin1 VARCHAR(256),
patinfo_resadmin2 VARCHAR(256),
patinfo_occus VARCHAR(32),
patinfo_occus_specify VARCHAR(256),
expo_travel VARCHAR(32),
expo_travel_country VARCHAR(256),
expo_date_departure DATE,
pat_symptomatic VARCHAR(32),
pat_contact VARCHAR(32),
patcourse_dateonset DATE,
expo_sourcecaseids VARCHAR(256),
patcourse_severity VARCHAR(256),
report_classif VARCHAR(256),
patcourse_datedeath DATE,
patinfo_resadmin3 VARCHAR(256),
patinfo_resadmin4 VARCHAR(256),
report_orginst VARCHAR(256),
patinfo_idadmin1 VARCHAR(256),
patinfo_idadmin2 VARCHAR(256),
report_pointofentry VARCHAR(32),
report_pointofentry_date DATE,
consultation_dateHF DATE,
patcourse_admit VARCHAR(32),
patcourse_presHCF DATE,
patcourse_comp VARCHAR(256),
patsympt_fever VARCHAR(32),
patsympt_sorethroat VARCHAR(32),
patsympt_cough VARCHAR(32),
patsympt_runnynose VARCHAR(32),
patsympt_short VARCHAR(32),
patsympt_other VARCHAR(256),
comcond_preexist1 VARCHAR(32),
comcond_preexist VARCHAR(256),
expo_healthcare VARCHAR(32),
expo_ari VARCHAR(32),
expo_aricontsetting VARCHAR(256),
expo_other VARCHAR(256),
expo_contact_case VARCHAR(32),
expo_ID1 INT,
expo_ID2 INT,
expo_ID3 INT,
expo_ID4 INT,
expo_arisetting VARCHAR(256),
lab_coll VARCHAR(32),
lab_type VARCHAR(256),
lab_datetaken DATE,
lab_performed VARCHAR(256),
lab_result VARCHAR(256),
lab_resdate DATE,
lab_other VARCHAR(256),
patcourse_datedischarge DATE,
--name_first VARCHAR(256),
--name_last VARCHAR(256),
expo_visit_healthcare VARCHAR(256),
lab_otherres VARCHAR(256),
country_id INT,
CONSTRAINT line_list_pk PRIMARY KEY(id),
UNIQUE (patinfo_id, report_date, patinfo_ageonset_years, patinfo_sex, patinfo_resadmin1, patinfo_resadmin2)
);

-- foreign keys
-- Reference: line_list_country (table: line_list)
-- ALTER TABLE line_list ADD CONSTRAINT line_list_country
--     FOREIGN KEY (country_id)
--     REFERENCES country (id)
--     NOT DEFERRABLE
--     INITIALLY IMMEDIATE
-- ;

-- End of file.
