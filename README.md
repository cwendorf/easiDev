# `easiDev` 

## The Developmental Version of EASI

[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.1.0-6666ff.svg)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

### Overview

**easiDev** is an R package that implements features of estimation statistics. It is the developmental version of [**EASI**](https://github.com/cwendorf/EASI/) and lacks certain features (while providing others). It is a proof of concept for future versions of EASI.

### Installation

This package is not currently on CRAN, but can be installed and loaded using these R commands:

``` r
install.packages("remotes")
remotes::install_github("cwendorf/easiDev")
library(easiDev)
```

If you do not wish a full install, the latest functions can be made available using these R commands:

```r
source("http://raw.githubusercontent.com/cwendorf/easiDev/main/source-easiDev.R")
```
