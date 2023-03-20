# GBIF Issues Explorer

# Archived

This repo is now archived. For a similar set of features, check https://github.com/Smithsonian/GBIF-Dataset-Explorer

## Intro 

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core Archive.

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

Occurrence records in GBIF can be tagged with a number of issues that their system has detected. However, like the processing information page indicates:

> Not all issues indicate bad data. Some are merley flagging the fact that GBIF has altered values during processing.

This tool allows collection and data managers, as well as researchers, to explore issues in GBIF Darwin Core Archive downloads in an easy web-based interface. Just enter a GBIF download key and the tool will download the zip file, create a local database, and display the issues in the data contained. Once provided with the GBIF key, this tool will:

 1. Download the zip archive
 1. Extract the files
 1. Create a local database
 1. Load the data from the occurrence, verbatim, multimedia, and dataset tables to the database
 1. Generate summary statistics of the issues

To use, just provide the key to a Darwin Core Archive from GBIF. The download key can be requested via the GBIF API or on the website. If your download URL is:

`www.gbif.org/occurrence/download/0001419-180824113759888`

Then, the last part, '0001419-180824113759888' is the GBIF key you will need to provide this tool. The first time the app is run, it takes some time to create a local database, in particular for large data files. Afterwards, it uses the local database, so it will be faster. 

As an alternative, you can copy the zip file to the `data` folder and run the `load_from_DwC_zip.R` script. It will run the same steps as above (skipping downloading the file) from the command line. 

Then, you can click the 'Explore Issues' tab to see how many records have been tagged with a particular issue.

Once you select an issue, a table will display the rows that have been tagged with that issue. If you click on a row, more details of the occurrence record will be shown, including a map using Leaflet (if the record has coordinates). You can choose to delete the row from the local database.

The 'Explore Data Fields' will show a summary and top data values in all fields of the `occurrence.txt` file (except for the gbifID field). 

## Features

 * Load a GBIF DwC download from the web or from a local zip file
 * Navigate the issues in the records
   * Spatial issues are shown with relevant fields and a map
 * Explore the data included in each field
   * How many are null or empty, how many distinct values there are?

## Screenshots

Main page, showing the number of records with specific issues:

![gbif1](https://user-images.githubusercontent.com/2302171/76112272-e4288e00-5faf-11ea-8c3d-a204ff75697a.png)

Exploring issues by looking at record details:

![GBIF Issues Explorer2](https://user-images.githubusercontent.com/2302171/76112339-06221080-5fb0-11ea-85af-20b9f9c8b0a7.png)

Explore the data fields, see number of records without data, and distinct values (new in version 0.4):

![GBIF Issues Explorer3](https://user-images.githubusercontent.com/2302171/76112397-205bee80-5fb0-11ea-9a21-4cb07212fb67.png)

## Testing the app in local computer

To test the app locally, without the need of a server, just install R and Shiny. Then, run a command that will download the source files from Github. 

R version 3.3 or better is required. After starting R, copy and paste these commands:

```R
install.packages(
    c("shiny", "DT", "dplyr", 
      "ggplot2", "stringr", "leaflet", 
      "XML", "curl", "data.table", "RSQLite", 
      "jsonlite", "R.utils", "shinyWidgets", 
      "shinycssloaders")
    )

library(shiny)
runGitHub("GBIF-Issues-Explorer", "Smithsonian")
```

Please note that the installation of the required packages may take a few minutes to download and install. Future versions will try to reduce the number of dependencies.

## Winner of 2nd Place Award in GBIF 2018 Ebbe Nielsen Challenge!

The Global Biodiversity Information Facility ([GBIF](https://gbif.org)) Secretariat announced today that the Smithsonian Institution’s Digitization Program Office ([DPO](http://dpo.si.edu)) was selected by an expert jury as a [winner in the 2018 Ebbe Nielsen Challenge](https://www.gbif.org/news/4TuHBNfycgO4GEMOKkMi4u/six-winners-top-the-2018-ebbe-nielsen-challenge). The entry, submitted by DPO Informatics Program Officer Luis J. Villanueva, the GBIF Issues Explorer, won a Second Place award among 23 entries from countries around the world. The Challenge was open to software tools that used GBIF data or tools to promote open science and open biodiversity data.

[More details...](http://dpo.si.edu/blog/smithsonian-institution%E2%80%99s-digitization-program-office-wins-prize-gbif-2018-ebbe-nielsen)

## Install


The app requires R 3.3, or later, and these packages:

 * shiny
 * DT
 * dplyr
 * ggplot2
 * stringr
 * leaflet
 * XML
 * curl
 * data.table
 * RSQLite
 * jsonlite
 * R.utils
 * shinyWidgets
 * shinycssloaders

To install the required packages:

```R
install.packages(
    c("shiny", "DT", "dplyr", 
      "ggplot2", "stringr", "leaflet", 
      "XML", "curl", "data.table", "RSQLite", 
      "jsonlite", "R.utils", "shinyWidgets", 
      "shinycssloaders")
    )
```

Please feel free to submit issues, ideas, suggestions, and pull requests. 
