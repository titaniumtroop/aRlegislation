## v0.2.0:
--------------------
* Stored LDA, lawmaker, and lawmaker-session models individually in order to meet Github maximum size requirements
  - Created function to load LDA models (calculations take forever, so this isn't something that will be generated frequently)
  - Created functionss to load derivative models based on dot product of LDA models and sparse lawmaker matrices (both on a career and by-session basis)
  - Excluded most files in these directories from builds to keep package size down (only topics 200-203 are included -- it was easiest to exclude 100-199); perhaps a download function should be written
* Created demo folder for machine learning analyses
  - Excluded from Github sync for now pending possible publication of masters thesis
  - Currently these are Rmd files that are used to run the analyses and generate images for use in thesis/presentations
  - These could stand to be annotated/documented as standalone vignettes
* data-raw/results folder houses results of analyses; excluded from git due to large size of some of the files
  - In order to run corruption_NN analyses, SVM analyses must be run first (the search grid is duplicated so some of the parameters carry over and can be compared)
  - These analyses will be saved to the data-raw/results directory, which may need to be created

## v0.1.8:
--------------------
* Add corruption flag to lawmakers dataset (defaults to FALSE; set to TRUE only with comment showing link to news source)
* Considering whether to deprecate using sponsor short name as assigned during each session, and instead use a legislator_id based on the entity-resolved lawmakers

## v0.1.7:
--------------------
* Bug fixes for sponsor names
* Create nested dataset for entity-resolved lawmakers that nests their details for each session

## v0.1.6:
--------------------
* Manually updated missing sponsor full names and some other missing details

## v0.1.5:
--------------------
* Added legislation_corpus dataset
* Change seniority to match listed number (was multiplied by -1 so #1 seniority was highest; however, not using that field for anything currently, and shouldn't be confusing to future users)
* Further cleaning of sponsor details

## v0.1.4:
--------------------
* Moved README examples to vignettes

## v0.1.3:
--------------------
* Fix munging error that left footers in some act text

## v0.1.2:
--------------------
* Run all CRAN build checks to confirm that package works across multiple systems

## v0.1.1:
--------------------
* change column names in dataset to be tidy-compliant
* first README draft added

## v0.1.0:
--------------------
* Initial commit of data package that passes R CMD build checks
