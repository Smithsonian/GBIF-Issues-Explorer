# GBIF Issues Explorer

## Winner of 2nd Place Award in GBIF 2018 Ebbe Nielsen Challenge!

The Global Biodiversity Information Facility ([GBIF](https://gbif.org)) Secretariat announced today that the Smithsonian Institution’s Digitization Program Office ([DPO](http://dpo.si.edu)) was selected by an expert jury as a [winner in the 2018 Ebbe Nielsen Challenge](https://www.gbif.org/news/4TuHBNfycgO4GEMOKkMi4u/six-winners-top-the-2018-ebbe-nielsen-challenge). The entry, submitted by DPO Informatics Program Officer Luis J. Villanueva, the GBIF Issues Explorer, won a Second Place award among 23 entries from countries around the world. The Challenge was open to software tools that used GBIF data or tools to promote open science and open biodiversity data.

[More details...](http://dpo.si.edu/blog/smithsonian-institution%E2%80%99s-digitization-program-office-wins-prize-gbif-2018-ebbe-nielsen)

![ebbenielsen2018](https://user-images.githubusercontent.com/2302171/47153516-e2243900-d2ad-11e8-9c62-ed6a919f2e69.png)

## Intro 

This [Shiny](https://shiny.rstudio.com/) app allows researchers and data/collection managers to navigate the records with issues in a GBIF Darwin Core Archive.

The app can be used to:

 * Determine the source of issues:
   * Researchers can determine if the data is usable for a particular analysis
   * Collection and data managers can check their own database and figure out the source of the problem and fix it in the next update to GBIF
 * Determine if an issue would affect an analysis:
   * For example, a COUNTRY_COORDINATE_MISMATCH could be because the coordinates fall just outside the country borders. Is this an error in the coordinates or an expected result of an occurrence in water?

## Screenshots

Main page, showing the number of records with specific issues:

![gbif1](https://user-images.githubusercontent.com/2302171/76112272-e4288e00-5faf-11ea-8c3d-a204ff75697a.png)

Exploring issues by looking at record details:

![GBIF Issues Explorer2](https://user-images.githubusercontent.com/2302171/76112339-06221080-5fb0-11ea-85af-20b9f9c8b0a7.png)

Explore the data fields, see number of records without data, and distinct values:

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

## Install

To use, just provide the key to a Darwin Core Archive from GBIF. The download key can be requested via the GBIF API or on the website. If your download URL is:

`www.gbif.org/occurrence/download/0001419-180824113759888`

Then, the last part, '0001419-180824113759888' is the GBIF key you will need to provide this tool.

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

The first time the app is run, it takes some time to create a local database, in particular for large data files. Afterwards, it uses the local database, so it will be faster. 

Please feel free to submit issues, ideas, suggestions, and pull requests. 
