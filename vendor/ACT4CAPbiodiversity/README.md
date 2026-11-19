# ACT4CAP-biodiversity-module

This package calculates biodiversity impacts (BII and Csar) from land use change information in AGMIP classification. 
 The code in this repository is adapted from [Leclere et al 2020](https://www.nature.com/articles/s41586-020-2705-y) and https://github.com/iiasa/Biodiversity_Link

 
## Installation
In order to install the package properly, open Rstudio and copy the following commands in the console.\
This should install both ACT4CAPbiodiversity and its dependencies. \

```
install.packages("devtools")
devtools::install_github("sibyllerp/ACT4CAPbiodiversity@main", 
                          dependencies = c("Imports"))
                          
```

## Run
There are 2 main functions in the package, which are run in the following order: \

 * `get_abdlnd()`: Derive abandonment and restoration events from LUC information and export final LU
 * `get_biodiversity()`: calculate biodiversity impact (BII and/or CSAR) from final LU

### Derive abandonment and restoration

We derive abandonment and restoration assuming the following:
- transitions from CRP/GRS/FOR to ONL are abandoned land. 
- transitions to FOR|PRI are restored land.

```
data_lu <- get_abdlnd (data = read.csv("../Inputs/DS_ACT4CAP_AgMIP_GLOBIOM_10km_20261001_luc_base.csv"),
                       LUmapping = "AGMIP",
                       resolution = "EEA_10kmID")
```
 

### Biodiversity Impact

BII and Csar impacts are calculated using the previously computed land use information (including abandoned and restored land)

This function is adapted from Bending the curve calculation with some adjustments, for example taking into consideration the age of restored and abandoned land in the calculations

Restored and abandoned areas are assume to linearly increase in quality with the final state being 80% as good as the undisturbed vegetation when the age is > 70 years.

```
results_biodiversity <- get_biodiversity(data = data_lu,
                                         scen = "base",
                                         get_bii = TRUE,
                                         get_csar = TRUE,
                                         LUmapping = "AGMIP",
                                         resolution = "EEA_10kmID",
                                         path_mapping = "Data-raw/one_kmID_master_mapping_2026-08-12.parquet")


```


