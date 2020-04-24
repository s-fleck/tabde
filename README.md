# tabde

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/s-fleck/tabde.svg?branch=master)](https://travis-ci.org/s-fleck/tabde)

**tabde** enables you to ensure and verify the correct column names and data
types for `data.frames` that are imported/exported from/to databases, .csv
files, fixed with text files, etc. The core piece of tabde are 
*table_designs*: `data.frames` that contain column names, data types,
and optionally the corresponding SQL data types, fixed-with column positions,
and a description of the column. Consequently these *table_designs* also
double as documentation of the dataset.


## Development status

**tabde** is under constant but slow development, as it is tied into some
production workflows that I maintain. As of yet there is no roadmap for when
it will be considered stable.


## Installation

You can install tabde from GitHub with:


``` r
# install.packages("devtools")
devtools::install_github("s-fleck/tabde")
```
