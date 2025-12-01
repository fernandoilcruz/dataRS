# dataRS: an R package for DataRS API database

## Introduction <img src="man/figures/logo.png" align="right" height="150" alt="" />

dataRS is an R package for downloading socioeconomic data for the state of Rio Grande do Sul using DataRS API.
The API documentation can be accessed [here](https://integra.ped.rs.gov.br/api-details#api=dee-dados-hml).

## API Key
Before using the package, you must obtain an API key for private use. Refer to the API section on the [DataRS website](https://data.rs.gov.br/index.php) for instructions.

## About vignettes

If you intend to build the package vignettes with your instalation, you must provide an API key in your global envioronment previously.
Set it in your .Renviron file.
 
## Installation via GitHub

```r
library(devtools)

#If you don't have an API key
devtools::install_github("fernandoilcruz/dataRS")


#If you already have an API key
devtools::install_github("fernandoilcruz/dataRS", build_vignettes = TRUE)

```