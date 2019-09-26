## Test environments

* local: 3.6.0 (2019-04-26) x86_64-apple-darwin15.6.0 (64-bit) UTF-8
* travis: 
* r-hub: 
* win-builder: 

## R CMD check results

0 errors | 0 warnings | 3 note

* checking installed package size ... NOTE
  installed size is 29.2Mb
  sub-directories of 1Mb or more:
    data  29.1Mb

  This is a data package that will be rarely updated.
  
* Checking dependencies in R code
   Namespaces in Imports field not imported from:
     ‘tibble’ ‘tidyr’
     All declared Imports should be used.
     
  The dataset in this package contains nested tibbles, which come from the `tibble` package. `tidyr` is required to unnest the tibbles. While not technically required to install the package, these are required to access and use the dataset.
  
* checking top-level files ... NOTE
  Non-standard file/directory found at top level:
  ‘README_files’
  
  This directory contains png plots used in README.md.

## revdepcheck results

