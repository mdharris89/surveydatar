# surveydatar

Tools for processing and analysing market research survey data in R.

## Who this is for

This package is designed for **market research analysts** working with:
- Brand tracking and ad-hoc consumer studies
- SPSS/Qualtrics data with variable and value labels
- Quota samples and non-probability sampling
- Iterative analysis producing client deliverables (dashboards, reports, presentations)

**Not designed for:** Academic survey research with complex probability samples. For design-based inference with stratification and clustering, use the [survey](https://cran.r-project.org/package=survey) package instead.

## Why use surveydatar

The core problem: when you import SPSS/Stata data into R, variable labels and value labels are attached but **easily lost during analysis**. After filtering, recoding, or creating derived variables, you're left with unlabeled output and have to manually reconstruct what each column means.

surveydatar solves this by keeping data and metadata together through your entire analytical pipeline, from import to client-ready output.

## Quick start

```r
library(surveydatar)

# Import SPSS data with labels
survey <- haven::read_sav("survey.sav") %>% create_survey_data()

# Crosstab with top-2-box and significance testing
# Labels preserved automatically: "Overall Satisfaction - Satisfied/Very Satisfied"
tab(survey, top_box(satisfaction, 2), region, weight = "wgt") %>%
  derive(delta_vs("North")) %>%        # Add difference from North region
  add_sig() %>%                         # Add statistical significance markers
  tab_to_reactable()                    # Interactive HTML table

# Multiple related tables at once
multi_tab(survey, satisfaction, by = gender, direction = "cols") %>%
  add_sig() %>%
  copy_tab()  # Formatted paste to Excel/PowerPoint
```

**Key workflow advantages:**
- Variable and value labels flow through the entire pipeline
- Domain-specific helpers (`top_box`, `among`, `response_match`) encode common market research patterns
- Composable operations (`derive`, `add_sig`, `arrange_rows`) avoid manual calculations
- Outputs integrate with business tools (Flourish, PowerPoint, interactive dashboards)

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

**Note:** Tests assume simple random sampling within each cell. For complex survey designs with stratification/clustering, use the [survey](https://cran.r-project.org/package=survey) package for design-based variance estimation.

### Exports and outputs

- `pivot_to_grid()` — reshape tab results for charting tools
- `tab_to_flourish()` — format for Flourish
- `tab_to_reactable()` — interactive HTML tables

### Weighting

`run_unified_weighting()` provides quadratic calibration with constraint helpers for demographic and occasion-based weighting. See `?run_unified_weighting` for details.

## Comparison with other packages

**When to use surveydatar:**
- You work with SPSS/Stata/Qualtrics data and need labels preserved through analysis
- You produce multiple similar tables (brand tracking, wave-over-wave) and want reproducible workflows
- Your outputs go to PowerPoint decks, Flourish dashboards, or interactive HTML reports
- You use quota sampling or non-probability samples

**When to use alternatives:**
- **Simple one-off crosstabs:** `questionr::ltabs()` is simpler
- **Academic publications:** `gtsummary::tbl_cross()` offers better formatting for Word/LaTeX
- **Complex probability samples:** `survey` + `srvyr` packages provide design-based inference
- **Quick exploration:** Base R `table()` + `prop.table()` is more transparent

surveydatar prioritizes **workflow efficiency** and **metadata preservation** for market research practitioners over statistical rigor for complex sample designs.

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
