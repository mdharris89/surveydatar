# CRAN Submission Comments

## Test environments

* Local macOS (darwin25.2.0, R 4.4.1)
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest), R-release and R-devel
* win-builder (R-devel and R-release)
* R-hub (to be confirmed before submission)

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 1 note ✗

### NOTE: New submission

This is the first submission of surveydatar to CRAN.

## Downstream dependencies

This is a new package submission, so there are no downstream dependencies to check.

## Additional notes

### Package Description

surveydatar provides comprehensive tools for processing and analyzing market research survey data in R, including:

* Metadata management via the survey_data and dpdict framework
* Flexible cross-tabulation with composable expressions
* Statistical significance testing with multiple comparison adjustments
* Weighting with quadratic calibration
* Export functions for reporting and visualization tools

### Dependencies

All package dependencies are justified:

* **CVXR**: Required for quadratic calibration weighting (`run_unified_weighting()`)
* **dplyr, purrr, rlang, tidyselect**: Core data manipulation and tidy evaluation
* **haven, sjlabelled**: Import and work with labelled data from SPSS/Stata
* **stringr, stringdist**: String processing and fuzzy matching for variable names
* **Matrix**: Sparse matrix operations for efficient computation
* **stats, utils**: Standard R utilities

Optional dependencies in Suggests:

* **DataEditR**: Interactive table editing (optional feature in `create_questions_dict()`)
* **reactable, htmltools, htmlwidgets**: Interactive HTML table output
* **flourishcharts**: Export to Flourish visualization platform
* **clipr**: Clipboard operations for Excel/PowerPoint export
* **tidyr**: Wide-to-long reshaping utilities
* **knitr, rmarkdown**: Vignette building
* **testthat, withr**: Testing framework

### Documentation

The package includes:

* 200+ documented exported functions
* 6 comprehensive vignettes
* Extensive examples throughout
* NEWS.md documenting initial release features

### Testing

The package has a comprehensive test suite covering:

* Core tabulation functionality
* Significance testing
* Weighting algorithms
* Metadata management
* Data processing functions
* Base calculators and statistics

All tests pass on multiple platforms.

