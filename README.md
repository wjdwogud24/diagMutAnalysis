
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diagMutAnalysis

R package that analyzes ICGC data for BCB410H: Applied Bioinformatics

<!-- badges: start -->
<!-- badges: end -->

## Description

`diagMutAnalysis` is an R package that takes data formats as same as
from the ICGC DCC open database. The aim of the package is to analyze
different aspects of the ICGC database analysis and also recreate it in
a user friendly R package\[<https://dcc.icgc.org/>\]. The package
provides function on analyzing observed gene mutation percentage in data
as a recreation of ICGC database providing observed gene mutation
percentage in sample. Another function creates plots not existing in the
ICGC database as a different perspective to the data. Providing
information on frequency and types of cds mutations, and mutation
consequence types in a bar plot and pie chart of respective order.

## Installation

To install the latest version of the package:

``` r
# install.packages("devtools")
require("devtools")
devtools::install_github("wjdwogud24/diagMutAnalysis", build_vignettes = TRUE)
library("diagMutAnalysis")
```

To run the Shiny app: Under construction

## Overview

``` r
ls("package:diagMutAnalysis")
data(package = "diagMutAnalysis") 
browseVignettes("diagMutAnalysis")
```

`diagMutAnalysis` contains 2 functions that demonstrate analysis on ICGC
open data. The mutationPercentage function aims to recreate mutation
percentage data provided on the ICGC browser. Aim is to be able to
recreate meaningful data on ICGC browser with any data if given correct
format. mutationTypePlot creates two plots a bar plot and a pie chart.
Bar plot shows information on substitution types in the sample. Pie
chart gives information on mutation consequence types in the sample.

![](./inst/extdata/JUNG_J_A1.png)

## References

- \[Hadley Wickham (2022). stringr: Simple, Consistent Wrappers for
  Common String Operations. R package version
  1.4.1.https://CRAN.R-project.org/package=stringr\]

- \[H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
  Springer-Verlag New York, 2016\]

- \[Hadley Wickham, Romain Francois, Lionel Henry and Kirill
  Muller(2022). dplyr: A Grammar of Data Manipulation. R package version
  1.0.10. <https://CRAN.R-project.org/package=dplyr>\]

- \[<https://dcc.icgc.org/>\]

## Acknowledgements

This package was developed as part of an assessment for 2022 BCB410H:
Applied Bioinformatics course at the University of Toronto, Toronto,
CANADA. `diagMutAnalysis` welcomes issues, enhancement requests, and
other contributions. To submit an issue, use the [GitHub
issues](https://github.com/wjdwogud24/diagMutAnalysis/issues). Many
thanks to those who provided feedback to improve this package.

## Contributions

The package is created by the author Jae Hyung Jung. Ggplot2, dplyr,
stringr package is used several times throughout functions.
mutationPercentage uses dplyr packages extensively for data
manipulation. mutationTypePlot uses the dplyr, ggplot2, and stringr
package to data manipulate, create bar plot/pie chart and extract
specific strings in data in the respective order. The references are
provided. Thought process of developing analysis is influenced by ICGC
database. Analysis not done in ICGC that are only solely in this package
are created using Author’s intuition on what is useful for the ICGC
data.
