# surveydatar 0.1.0

## Initial CRAN release

This is the first release of surveydatar, a comprehensive toolkit for processing and analyzing market research survey data in R.

### Core Features

#### Data Processing and Metadata Management

* `create_survey_data()`, `create_dict()`, `create_dict_with_metadata()` - Create and manage survey data objects with integrated metadata dictionaries (dpdict)
* `update_dat_from_dpdict()` - Apply metadata changes back to survey data
* `update_dict_with_metadata()` - Automatically infer question groups, types, and structure
* `datamap()`, `datamap_questions()` - View variable and question-level metadata
* `validate_survey_data()` - Validate alignment between data and metadata
* `collapse_wide_question()` - Reshape wide question blocks
* `split_into_question_groups()` - Automatically group related variables
* Comprehensive metadata utilities: `find_and_replace_in_var_labels()`, `reorder_substrings()`, `standardise_survey_separators()`

#### Tabulation System

* `tab()` - Flexible cross-tabulation with composable row/column expressions
* Built-in helpers: `top_box()`, `bottom_box()`, `total()`, `percentile()`, `among()`
* Question-group expansion for analyzing multi-item questions
* Fuzzy variable matching by name or label
* Weighted tabulation with `weight` parameter
* `multi_tab()`, `multi_tab_rows()`, `multi_tab_cols()` - Create multiple related tables
* `banner()` - Define reusable column structures

#### Post-Tabulation Calculations

* `derive()` - Add calculated rows/columns to tab results
* `delta_vs()` - Calculate differences from reference columns
* `index_vs()` - Calculate indices relative to reference columns
* `share_of_sum()` - Calculate shares of totals
* `sum_if()` - Conditional aggregation

#### Statistical Significance Testing

* `add_sig()` - Pairwise significance testing between columns
* `add_sig_all()` - Apply significance testing across all relevant statistics
* Support for z-tests (proportions), t-tests (means), and Mann-Whitney U tests (medians)
* Multiple comparison adjustments: Bonferroni, Benjamini-Hochberg, and others
* Cell-based significance storage preserving pipeline continuity

#### Table Manipulation and Formatting

* Row/column operations: `hide_rows()`, `hide_cols()`, `select_rows()`, `select_cols()`, `arrange_rows()`, `arrange_cols()`
* `group_rows()`, `group_cols()` - Visually group related rows/columns
* `move_row()`, `move_col()` - Reposition table elements
* `format_cells()`, `format_row()`, `format_col()` - Apply formatting to specific cells or regions
* `hide_base()`, `show_base()`, `hide_summary()`, `show_summary()` - Control visibility of metadata rows

#### Export and Visualization

* `pivot_to_grid()` - Reshape tab results for charting tools
* `tab_to_flourish()` - Export formatted tables to Flourish
* `tab_to_reactable()` - Create interactive HTML tables with reactable
* `copy_tab()` - Copy tables to clipboard for pasting into Excel/PowerPoint
* `glue_tab()` - Combine multiple tables

#### Weighting

* `run_unified_weighting()` - Quadratic calibration weighting
* `create_weighting_config()` - Define weighting specifications
* `make_constraint()` - Create demographic and occasion-based constraints
* Comprehensive constraint validation and diagnostics

#### Extensibility

* `create_helper()`, `create_statistic()`, `create_macro()` - Register custom tab components
* `create_significance_test()` - Add custom statistical tests
* `create_derive_operation()` - Define custom derived calculations
* Registry system for managing extensions

### Additional Utilities

* Data manipulation helpers: `labelled_case_when()`, `lv()`, `realiselabelled()`
* String utilities: `closest_matching_string()`, `get_longest_common_substring()`, `any_grepl()`, `any_gsub()`
* Survey-specific tools: `convert_integers_to_double()`, `fix_for_spss_export()`
* Test data generators for development and examples

### Documentation

* Six comprehensive vignettes covering package overview, tabulation, significance testing, built-in statistics, and export functions
* Extensive function documentation with examples
* 200+ exported functions with consistent API design

