## Test environments

* local: x86_64-apple-darwin15.6.0-3.6.0
* r-hub: 
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Debian Linux, R-devel, GCC
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* travis: oldrel, release, devel
* win-builder: x86_64-w64-mingw32 (64-bit) R-devel

## R CMD check results

0 errors | 0 warnings | 5 note (aggregate from all test environments)

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Nathan Chaney <nathan@nathanchaney.com>’
  New submission
  Possibly mis-spelled words in DESCRIPTION:
    tibbles (8:25, 10:39)  
  Size of tarball: 21205189 bytes
  
  First-time submitter. I'm happy to make any necessary changes.

* checking installed package size ... NOTE
  installed size is 28.4Mb
  sub-directories of 1Mb or more:
    data  28.3Mb

  This is a data package that will be rarely updated.
  
* checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    ‘tibble’ ‘tidyr’
    All declared Imports should be used.
     
  The dataset in this package contains nested tibbles, which come from the `tibble` package. `tidyr` is required to unnest the tibbles. While not technically required to install the package, these are required to access and use the dataset.
  
* checking top-level files ... NOTE
  Non-standard file/directory found at top level:
  ‘README_files’
  
  This directory contains png plots used in README.md.
  
* checking data for non-ASCII characters ... NOTE
  Note: found 12761 marked UTF-8 strings

  This dataset contains the text of 17k+ individual acts of legislation, plus their titles and subtitles, for natural language processing. The text encodings are from the original source documents available on the Arkansas legislature's website.
