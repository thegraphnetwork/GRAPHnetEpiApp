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
    id int  NOT NULL,
    country_id int  NOT NULL,
    CONSTRAINT line_list_pk PRIMARY KEY (id)
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
