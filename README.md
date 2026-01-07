# surveydatar

Tools for processing and analysing market research survey data in R.

## Quick start

```r
library(surveydatar)

# Load data (e.g. from SPSS)
raw_data <- haven::read_sav("survey.sav")

# Create a metadata dictionary and make edits
dpdict <- create_dict(raw_data)
dpdict$variable_labels[1] <- "Respondent ID"
dpdict$variable_names[2] <- "satisfaction"

# Apply edits and wrap as survey_data
dat <- update_dat_from_dpdict(raw_data, dpdict)
survey <- create_survey_data(dat, dpdict)

# Run a crosstab
tab(survey, satisfaction, weight = "wgt")
```

## What's in the package

### Data processing and metadata

The package provides tools for managing survey metadata via the `dpdict` (data processing dictionary) and `survey_data` objects:

- `create_dict()` / `create_dict_with_metadata()` — build a dpdict from labelled data
- `update_dat_from_dpdict()` — apply dpdict edits back to data
- `create_survey_data()` — wrap data + dpdict into a single object
- `update_dict_with_metadata()` — infer question groups, types, and structure
- `datamap()` / `datamap_questions()` — view variable and question-level metadata
- `validate_survey_data()` — check alignment between data and dpdict
- `collapse_wide_question()` — reshape wide question blocks
- `convert_integers_to_double()` — prepare data for SPSS export
- `find_and_replace_in_var_labels()`, `reorder_substrings()` — bulk label editing
- `labelled_case_when()`, `lv()` — create labelled variables with recoding

### Tabulation

`tab()` produces crosstabs from survey data with support for:
- Composable row/column expressions using helpers like `top_box()`, `bottom_box()`, `total()`
- Question-group expansion (pass a question group name to add all items)
- Fuzzy variable matching by name or label
- Weighted tabulation via `weight = "column_name"`
- Clipboard export with `copy_tab()`

### Post-tab calculations

Use `derive()` to add calculated rows/columns to a `tab_result`:
- `delta_vs()` — difference from a reference column
- `index_vs()` — index relative to a reference column
- `share_of_sum()` — row/column as share of a total
- `sum_if()` — conditional aggregation

### Significance testing

- `add_sig()` — pairwise significance tests between columns (z-test, t-test, Mann-Whitney)
- `add_sig_all()` — apply significance testing across all relevant statistics
- Multiple comparison adjustments (Bonferroni, Benjamini-Hochberg, etc.)

### Exports and outputs

- `pivot_to_grid()` — reshape tab results for charting tools
- `tab_to_flourish()` — format for Flourish
- `tab_to_reactable()` — interactive HTML tables

### Weighting

`run_unified_weighting()` provides quadratic calibration with constraint helpers for demographic and occasion-based weighting. See `?run_unified_weighting` for details.

## Read next

Vignettes provide detailed workflows:

- [Package overview](vignettes/surveydatar-overview-vignette.Rmd) — `survey_data`, `dpdict`, and metadata management
- [Tabulation guide](vignettes/tab-vignette.Rmd) — `tab()` usage and options
- [Significance testing](vignettes/sig-testing-vignette.Rmd) — `add_sig()` and statistical tests
- [Built-in statistics and helpers](vignettes/guide-to-built-in-statistics-and-helpers.Rmd) — extending `tab()`
- [Flourish export](vignettes/tabtoflourish-vignette.Rmd) — `tab_to_flourish()`
- [Interactive tables](vignettes/tabtoreactable_vignette.Rmd) — `tab_to_reactable()`

Or start with the help pages: `?tab`, `?create_survey_data`, `?add_sig`, `?run_unified_weighting`.

## Status

The package is under active development. This README describes functionality that is currently implemented.
