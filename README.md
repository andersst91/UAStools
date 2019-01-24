# [R/UAStools](https://github.com/andersst91/UAStools/wiki) 

### Authors:
Steven L. Anderson II (andersst@tamu.edu)  
Seth C. Murray (sethmurray@tamu.edu)

### Description:
UAStools is a package developed to provide useful tools for working with remote sensing data set, specifically in reference to field-based, agriculture research plots. Currently, the [plotshpcreate.R](https://github.com/andersst91/UAStools/wiki/plotshpcreate.R) function is availible to construct a multi-polygon shapefile (.shp) of a reasearch trial, with individual polygons defining specific research plot.


# Installation  
To install UAS tools execute the following commands in R:  

    install.packages("devtools") 
    library("devtools")
    devtools::install_github("andersst91/UAStools")
    library("UAStools")

</br>
####Sample dataset for infile structure is object SampleInfile####</br>
</br>
</br>
?plotshpcreate </br>
?SampleInfile

### Citation

To cite R/UAStools in publications, use:  

https://github.com/andersst91/UAStools

### Liscense
[Licensed](LICENSE) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
