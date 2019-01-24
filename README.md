# [R/UAStools](https://github.com/andersst91/UAStools/wiki) 

### Authors:
Steven L. Anderson II (andersst@tamu.edu)  
Seth C. Murray (sethmurray@tamu.edu)

### Description:
UAStools is a package developed to provide useful tools for working with remote sensing data set, specifically in reference to field-based, agriculture research plots. Currently, the [plotshpcreate.R](https://github.com/andersst91/UAStools/wiki/plotshpcreate.R) function is availible to construct a multi-polygon shapefile (.shp) of a reasearch trial, with individual polygons defining specific research plot.


# Installation  
Make sure you have the latest version of [R](https://cran.r-project.org/). 

[R/UAStools](https://github.com/andersst91/UAStools/wiki) is availible on [GitHub](https://github.com/andersst91/UAStools).

You then need to install the [devtools](https://github.com/r-lib/devtools) package, plus a set of package dependencies: [sp](https://cran.r-project.org/web/packages/sp/index.html) and [rgdal](https://cran.r-project.org/web/packages/rgdal/index.html). (Additional, secondary dependencies will also be installed.)

To install [R/UAStools](https://github.com/andersst91/UAStools/wiki) execute the following commands in R:  

    install.packages("devtools") 
    library("devtools")
    devtools::install_github("andersst91/UAStools")
    library("UAStools")

# Availible Functions
## [plotshpcreate.R](https://github.com/andersst91/UAStools/wiki/plotshpcreate.R)  
Construct a multi-polygon shapefile (.shp) of a reasearch trial, with individual polygons defining specific research field plots.  
Visit the wiki page for more details: https://github.com/andersst91/UAStools/wiki/plotshpcreate.R  

### Note: Make sure to setwd() before executing [plotshpcreate.R](https://github.com/andersst91/UAStools/wiki/plotshpcreate.R)!!!

    ?plotshpcreate
    ?SampleInfile
    
### Citation

To cite R/UAStools in publications, use:  

https://github.com/andersst91/UAStools

### Liscense
[Licensed](LICENSE) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
