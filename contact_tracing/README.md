<img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/ui_demo_snake_plot.gif" alt="ui_demo_snake_plot" width="590"/>

# R Shiny App for analyzing Contact Tracing Data

This repo contains the code for GoContactR, an R-shiny application built to allow contact tracing teams to generate automated reports that summarize contact follow-up data.

Find a sample live app [HERE](https://gocontactr.shinyapps.io/sample/).

The application has been built to work with two primary data sources: data from KoboCollect-exported csv files, and data from a Go.Data instance (by direct API connection).

At the start of the global.R file, the developer sets the `PARAMS$country_code` variable; this determines whether the Go.Data or KoboCollect versions will be loaded.

## Report generation

The application generates reports in the following formats:

-   HTML slides. See [here](https://drive.google.com/file/d/18NIlb2wtKVXnGwascB9F0DKHT26-Lz8x/view?usp=sharing) (Download and open in browser)
-   Flat HTML page. See [here](https://drive.google.com/file/d/1hAMGwwo5VGDhJDB04JLfj6Oo_tZ-6n3K/view?usp=sharing) (Download and open in browser)
-   Powerpoint document. See [here](https://drive.google.com/file/d/1_J7zp-gPDcdMt5viJC_o3YsJRwG1GGqC/view?usp=sharing)
-   Word document. See [here](https://drive.google.com/file/d/1GL3IS3AeD7S536z-D6rvNXc2rpM3RzgG/view?usp=sharing)

## Deployment

We recommend deploying your application to RStudio's shinyapps.io service, as they have built-in tools that make this process mostly pain-free.

Two important points when doing this:

-   Do not push the DESCRIPTION file up to shinyapps.io. If you push the DESCRIPTION file, shinyapps will assume that your application is package, and it will only load libraries listed in the DESCRIPTION file. (We created the description file to enable documentation via `pkgdown`).

-   Similarly, do not push the .Rprofile file. This will introduce problems related to the `renv` package.

## Folder structure

The application is split into the *global.R*, *server.R* and *ui.R* files. The *server* file sources its functions from the *helper_scripts* folder.

<img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/key_files_and_folders.png" alt="key_files_and_folders" width="580"/>

## Data Flow

The data flow for the application is fairly straightforward. A single dataset is loaded in with the `read_file_raw` function, processed with the `read_file_transformed` function, filtered with the `read_file_filtered` function, then passed on to all the application outputs.

<img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/gocontactr_data_flow.png" alt="gocontactr_data_flow" width="588"/>

## Key outputs

The application calculates and plots:

-   New contacts over time;
-   Total cumulative contacts over time;
-   Contacts under surveillance over time;
-   % follow-up of contacts;
-   Number of contacts per index case;
-   Number of contacts per case type; and
-   Follow-up state among active contacts

among others

#### Load data tab

UI inputs and outputs to load and peruse the contacts data. <img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/ui_demo_load_data_tab.png" alt="ui_demo_load_data_tab" width="541"/>

#### All contacts tab

Outputs related to all contacts.

<img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/ui_demo_all_contacts_tab.png" alt="ui_demo_all_contacts_tab" width="529"/>

#### Active contacts tab

Outputs related to contacts under surveillance on the review date.

<img src="https://raw.githubusercontent.com/kendavidn/GoContactR/master/www/ui_demo_active_contacts_tab.png" alt="ui_demo_active_contacts_tab" width="512"/>
