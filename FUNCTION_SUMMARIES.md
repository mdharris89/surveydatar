# surveydatar: Key Function Summaries

This document provides 2-3 sentence summaries of every key function in the surveydatar package and how they're implemented.

---

## Core Tab Creation Functions

### `tab()`
**Location:** R/tab.R:184

Creates cross-tabulation tables using a cell-based architecture with position-independent cells. Implements a complete parse → expand → compute → layout pipeline that processes macro detection, variable resolution (with fuzzy matching), formula parsing, variable expansion, cell-native computation via `compute_cells_as_bundle()`, and layout grid construction. Returns a `tab_cell_collection` object containing a cell store with sequential IDs, an explicit layout grid, and full metadata for each cell.

### `multi_tab_rows()` / `multi_tab_cols()`
**Location:** R/tab.R:1610, R/tab.R:1598

Convenience wrappers around `multi_tab()` that create multiple tab() results filtered by different groups and combine them row-wise or column-wise. Each function calls `multi_tab()` with the appropriate direction parameter ("rows" or "cols") and supports filtering by variable values, named expression lists, or subsetting expressions.

### `multi_tab()`
**Location:** R/tab.R:1480

Runs `tab()` multiple times with different filters based on the `by` parameter and combines results using `glue_tab()`. Parses the `by` parameter to create filter groups (supporting variable names, named lists, or subsetting expressions), generates a separate tab for each group, and optionally includes a total (unfiltered) table before gluing all results together in the specified direction.

### `glue_tab()`
**Location:** R/tab.R:1157

Combines two `tab_cell_collection` objects by merging their cell stores and layouts either side-by-side (cols) or vertically (rows). Uses `merge_cell_stores()` to combine cells with ID remapping for the second tab, finds common rows/columns, builds a new grid matrix by copying cell IDs (with remapping), and combines layout definitions while preserving metadata.

---

## Computation Engine Functions

### `compute_cells_as_bundle()`
**Location:** R/computation.R:157

Core cell-native computation function that creates cells directly from array intersections without intermediate matrix structures. Iterates through row and column array combinations, computes value and base for each intersection using the statistic's processor and base_calculator, builds full cell specifications with metadata, stores cells immediately in the cell store, and handles summary row/column computation when applicable.

### `build_cell_specification()`
**Location:** R/computation.R:494

Constructs the hierarchical specification component for a cell object by extracting metadata from arrays and parsed specifications. Combines row/column/base expressions, normalizes DSL expressions with data awareness, includes computation context (statistic ID, values variable), and creates a complete specification with syntactic identity (expressions) and semantic identity (normalized DSL).

### `sync_base_matrix()`
**Location:** R/computation.R:26

Recalculates the base matrix whenever dimensions change to ensure consistency between arrays and bases. Iterates through all row/column combinations and uses the statistic's `base_calculator` function to compute the base value for each cell intersection.

### `compute_cells_vectorized()`
**Location:** R/computation.R:108

Computes values for multiple cells using vectorized operations by iterating through row and column array combinations. Applies the statistic's processor function to each intersection and returns a matrix of computed cell values.

---

## Cell Store Functions

### `new_cell_store()`
**Location:** R/cell_store.R:13

Creates an empty cell store with dual storage: a hash table for O(1) access and columnar storage for fast bulk operations. Initializes an environment containing the cells hash table, columnar data vectors (cell_ids, values, bases, specifications, derivations), a cell index for position tracking, and a counter for sequential ID generation.

### `add_cell()`
**Location:** R/cell_store.R:61

Adds a new cell to both the hash table and columnar storage with a sequential ID (c_000001, c_000002, etc.). Generates a unique cell ID, creates a cell object with value/base/specification/computation/derivation, adds it to the hash table for O(1) access, appends to columnar vectors for bulk operations, and updates the position index.

### `get_cell()` / `get_cells()`
**Location:** R/cell_store.R:99, R/cell_store.R:113

Retrieves cell(s) by ID from the hash table with O(1) access time. `get_cell()` returns a single cell object or NULL if not found; `get_cells()` applies `get_cell()` to a vector of IDs using `lapply()`.

### `filter_cells()`
**Location:** R/cell_store.R:155

Returns cell IDs matching a predicate function by iterating over positions in columnar storage. Reconstructs each cell object using `get_cell()`, applies the predicate function, and collects matching positions.

### `filter_cells_by_value()`
**Location:** R/cell_store.R:181

Optimized filtering for simple value/base conditions using columnar access. Evaluates a condition expression against the columnar `values` and `bases` vectors directly without reconstructing full cell objects.

---

## Parsing and Formula Conversion Functions

### `parse_table_formula()`
**Location:** R/parsing.R:147

Parses NSE expressions into structured specifications using quosures and expression analysis. Resolves external variables to values via `resolve_external_variables()`, identifies expression type (simple variable, multiplication, helper function, general expression), and returns a specification list with type, components/args, and label.

### `resolve_external_variables()`
**Location:** R/parsing.R:28

Walks the expression tree replacing external variables with their values while preserving data column names and registered helper calls. Implements priority resolution (data columns → external variables → unknown symbols), errors on unregistered function calls with helpful guidance to register as helpers, and recursively processes call arguments while preserving helper function calls.

### `expand_variables()`
**Location:** R/parsing.R:286

Expands categorical variables and question groups into individual component specifications. Recursively processes complex expressions (multiplication, subtraction), expands categorical variables using labels or factor levels from dpdict metadata, handles helper functions without expansion, and applies label mode (smart/full/suffix) for display formatting.

### `formula_to_array()`
**Location:** R/parsing.R (referenced but not in excerpt)

Converts parsed specifications to numeric arrays for computation. Evaluates expressions against data, processes helper functions via their processor functions, and returns numeric arrays with metadata attributes.

---

## Weighting Functions

### `run_unified_weighting()`
**Location:** R/weighting.R:62

Unified survey weighting using bias-corrected constrained quadratic optimization. Processes stages with constraints and helpers, builds a sparse constraint matrix, solves the quadratic program min Σ(w_i - w*_i)² subject to linear constraints with tolerances using CVXR, and returns weighted data with diagnostics (ESS, efficiency, CV, solver status, constraint report).

### `make_constraint()`
**Location:** R/weighting.R:216

Creates a self-describing constraint object carrying all metadata to eliminate pattern matching. Validates inputs (formula, type, country, metadata, tolerance), auto-extracts variables from formula using `all.vars()`, and returns a `weighting_constraint` object with class attribute.

### `create_weighting_config()`
**Location:** R/weighting_config.R (referenced)

Creates configuration object specifying column names, constraint type patterns, demographic mappings, and legacy mappings. Enables customization of strata column, ID column, constraint naming conventions, and demographic variable mappings for user-friendly constraint specification.

### `extract_constraint_info()`
**Location:** R/weighting.R:301

Centralizes metadata extraction from constraint objects into a data frame, eliminating regex parsing. Extracts standard fields (formula, type, country, description, tolerance) and common metadata fields (doc_num, category, is_core, demo_type), returning a structured data frame.

### `%+-%` (tolerance operator)
**Location:** R/weighting.R:349

Infix operator for attaching tolerances to constraint formulas in concise notation. Validates tolerance (numeric, length 1 or 2), adds it as an attribute to the formula, and sets class to `formula_with_tolerance`.

---

## Significance Testing Functions

### `add_sig()`
**Location:** R/significance.R:41

Adds statistical significance testing to tab results by attaching metadata directly to cells. Validates input is a `tab_cell_collection`, calls `add_sig_cell_native()` for cell-based implementation, and preserves significance through derive operations without requiring materialization.

### `add_sig_all()`
**Location:** R/significance.R:70

Performs pairwise significance testing for all columns versus each other. Identifies and excludes summary/base columns from testing, iterates through remaining columns calling `add_sig()` for each with error handling, and uses column names as test names for identification.

### `compute_significance()`
**Location:** R/significance.R:149

Core significance testing engine that performs statistical tests (z-test, t-test, Mann-Whitney, etc.). Auto-detects test type based on statistic (column_pct→z_test, mean→t_test), determines comparison base column from `versus` parameter, filters to data rows/columns (excluding base/summary), and computes p-values and significance levels with optional multiple comparison adjustment.

---

## Derive Operations Functions

### `derive()`
**Location:** R/derive.R:66

Applies a derive operation to a tab_result as an additive post-compute transformation. Validates tab_result object, retrieves operation from registry or spec object, executes the operation's processor function with parameters, records the operation in derive_operations attribute with timestamp, and returns the modified result.

### `sum_if()`
**Location:** R/derive.R:156

Creates a derive specification for conditional sum by metadata field. Returns a `derive_spec` object with operation "sum_if", metadata_field parameter (e.g., "ival", "ivar"), dimension parameter ("rows" or "cols"), and optional label_fn for custom group labels.

### `process_sum_if()`
**Location:** R/derive.R:166

Processes the sum_if operation by grouping cells and creating derived sum cells. Groups rows or columns by extracting metadata field from first cell in each dimension, iterates through groups creating derived cells by summing values from source cells, creates layout_defs for new derived dimensions with appropriate matchers, and calls `refresh_layout()` to reallocate the grid.

### `create_derive_operation()`
**Location:** R/derive.R:36

Registers a custom derive operation in the global registry. Creates a `derive_operation` object with id/processor/description, adds it to `.derive_registry$operations`, and warns if overwriting existing operation.

---

## Layout and Arrangement Functions

### `arrange_rows()` / `arrange_cols()`
**Location:** R/layout.R:37, R/layout.R:207

Reorders rows or columns using flexible strategies: explicit label list, cell values in a dimension, or custom ordering function. For explicit ordering, matches labels and optionally preserves unlisted items; for value-based sorting, extracts cell values from specified column/row and orders by those values; for custom functions, applies function to first cell of each dimension and sorts by returned keys. Modifies layout_defs and calls `refresh_layout()`.

### `hide_rows()` / `hide_cols()` / `hide_rows_except()` / `hide_cols_except()`
**Location:** R/layout.R (referenced)

Controls visibility of specific rows or columns by label pattern matching. For "except" variants, shows only matching labels; for standard variants, hides matching labels. Modifies layout to filter dimensions while preserving base/summary rows/columns.

### `hide_base()` / `show_base()`
**Location:** R/layout.R (referenced)

Controls visibility of base rows and columns. Modifies the `show_base` attribute in the tab_result which is respected during materialization.

### `hide_summary()` / `show_summary()`
**Location:** R/layout.R (referenced)

Controls visibility of summary rows (NET, Avg, etc.) and summary columns (Total, NET, etc.). Updates visibility filters to exclude or include summary dimensions identified by layout_def matchers.

### `group_rows()` / `group_cols()`
**Location:** R/layout.R (referenced)

Groups rows or columns by adding visual grouping to layout metadata. Adds group labels and formatting information to layout_defs for display purposes.

---

## Export and Visualization Functions

### `tab_to_reactable()`
**Location:** R/tabtoreactable.R:91

Converts a tab_result into a reactable-ready object with styling options. Extracts metadata before modifications, applies native operations (hide_base, hide_summary) if cell-based, materializes to data frame, identifies special rows/columns, stores settings for color modes (none/heatmap/top_n/significance), and returns a `reactable_tab` object with data, metadata, and settings.

### `display_reactable()`
**Location:** R/tabtoreactable.R (referenced)

Renders a `reactable_tab` object as an interactive HTML table. Applies color formatting based on mode, creates column definitions with tooltips, handles significance symbols if present, and displays using the reactable package with configured options (freeze headers, pagination, sorting, etc.).

### `tab_to_flourish()`
**Location:** R/tabtoflourish.R (referenced)

Converts tab results to Flourish visualization format for data visualization platform. Transforms data to Flourish's expected structure, handles metadata mapping, and supports preview functionality.

### `copy_tab()` / `copy_df()`
**Location:** R/export.R (referenced)

Copies formatted table to clipboard for pasting into Excel or other applications. Formats numeric values, preserves row/column labels, and uses `clipr` package for cross-platform clipboard access.

---

## Data Dictionary Functions

### `datamap()`
**Location:** R/surveymetadatafunctions.R:78

Creates an SPSS-style data summary data frame with variable names, labels, classes, and metadata as rows. Extracts variable labels, classes, missing counts, unique counts, value labels, and first values for each column. For `survey_data` objects, adds question type and alias_with_suffix from dpdict.

### `datamap_questions()`
**Location:** R/surveymetadatafunctions.R:162

Provides a view by question group instead of individual variables, showing aggregated metadata. Groups variables by question_group from dpdict, counts variables per group, extracts first variable's metadata (type, label, name, class, values), and returns or displays the summary.

### `create_dict()`
**Location:** R/surveymetadatafunctions.R (referenced)

Creates a simple dpdict (data processing dictionary) for bulk updates to variable names and labels. Returns a data frame with columns for variable_names and variable_labels, optionally populated from existing data.

### `create_dict_with_metadata()`
**Location:** R/surveymetadatafunctions.R (referenced)

Creates a comprehensive dpdict with full metadata including question types, question groups, and aliases. Calls `split_into_question_groups()` to identify question groupings, generates question_alias and alias_with_suffix, and includes questiontype column.

### `update_dict_with_metadata()`
**Location:** R/surveymetadatafunctions.R (referenced)

Adds metadata columns to an existing simple dpdict. Calls helper functions to identify question groups, generate aliases, and add questiontype information.

### `validate_dat_dpdict_alignment()`
**Location:** R/surveymetadatafunctions.R (referenced)

Performs simple checks that dat and dpdict are aligned and in suitable format. Checks that variable names in dpdict match columns in dat, dpdict has required columns, and no duplicate entries exist.

### `validate_no_dpdict_duplicates()`
**Location:** R/surveymetadatafunctions.R (referenced)

Checks for unique values in variable names, variable labels, and alias_with_suffix in dpdict. Reports any duplicates that would cause issues in tab operations or exports.

### `split_into_question_groups()`
**Location:** R/surveymetadatafunctions.R (referenced)

Attempts to find sensible groupings of variables into question_groups based on common prefixes/suffixes and naming patterns. Uses longest common substring matching, analyzes affixes with `get_affix_df()`, and groups variables with shared stems.

### `get_questions_dict()`
**Location:** R/surveymetadatafunctions.R (referenced)

Extracts a questions dictionary (one row per unique question) from a dpdict. Filters to unique question_alias values and returns relevant question-level metadata.

### `update_aliases()`
**Location:** R/surveymetadatafunctions.R (referenced)

Updates question_alias and alias_with_suffix in dpdict after manual edits to questions_dict. Propagates changes from the questions dictionary back to the full variable-level dpdict.

---

## Survey Data Creation Functions

### `create_survey_data()`
**Location:** R/dataprocessingfunctions.R (referenced)

Creates a `survey_data` object combining data and metadata (dpdict). Validates inputs, ensures alignment between dat and dpdict, and returns a structured list with class `survey_data` containing dat and dpdict components.

### `validate_survey_data()`
**Location:** R/dataprocessingfunctions.R (referenced)

Validates the structure and alignment of a `survey_data` object. Checks that required components exist, dat is a data frame, dpdict has required columns, and variable names align.

---

## Data Type Conversion Functions

### `character_to_labelled_via_dict()`
**Location:** R/dataprocessingfunctions.R:28

Converts character vectors to sjlabelled/haven labelled numeric vectors using a lookup dictionary. Creates lookup automatically based on value frequency if not provided, partitions values by frequency threshold, maps character values to numeric codes, creates the labels attribute as a named numeric vector, and returns a haven_labelled object.

### `realiselabelled_vec()`
**Location:** R/dataprocessingfunctions.R:163

Converts a single atomic vector (logical/character/factor/numeric) to labelled format. Handles logicals as 0/1 with multi-response labels, converts characters using `character_to_labelled_via_dict()`, converts factors by mapping levels to numeric indices, auto-detects binary 0/1 numerics as multi-response, and preserves or sets variable labels.

### `realiselabelled()`
**Location:** R/dataprocessingfunctions.R (referenced)

Vectorized version that applies `realiselabelled_vec()` to selected columns in a data frame. Supports tidyselect expressions for column selection, applies value_label_lookup if provided, and returns the modified data frame with labelled columns.

### `labelled_case_when()`
**Location:** R/dataprocessingfunctions.R (referenced)

Conditional value assignment for labelled data that preserves labels. Implements case_when logic while maintaining haven_labelled class and label attributes.

### `lv()` (labelled value)
**Location:** R/dataprocessingfunctions.R (referenced)

Helper function to create a single labelled value. Used within `labelled_case_when()` to construct labelled results.

---

## Helper and Macro Functions

### `create_helper()`
**Location:** R/builtins.R (referenced)

Registers a custom helper function in the tab system for use in formulas. Validates the processor function signature, creates a `tab_helper` object with id and processor, adds it to `.tab_registry$helpers`, and enables the helper to be used in `tab()` expressions.

### `create_macro()`
**Location:** R/builtins.R (referenced)

Registers a custom macro that can transform tab parameters before processing. Creates a `tab_macro` object with processor function, adds to `.tab_registry$macros`, and enables macro detection in `tab()` via `detect_tab_macro()`.

### `create_statistic()`
**Location:** R/builtins.R (referenced)

Registers a custom statistic for use in `tab()`. Defines processor function (computes cell value), base_calculator function (computes base), summary_row/col calculators if applicable, and metadata (label, requires_values, etc.).

### `list_tab_helpers()` / `list_tab_macros()` / `list_tab_statistics()`
**Location:** R/builtins.R (referenced)

Lists all registered helpers, macros, or statistics from the global registry. Returns character vectors of names for inspection.

### `clear_tab_registry()`
**Location:** R/builtins.R (referenced)

Clears all registered helpers, macros, and statistics from the global registry. Useful for testing or resetting to default state.

---

## Built-in Helper Functions

### `top_box()` / `bottom_box()`
**Location:** R/builtins.R (referenced)

Selects top N or bottom N response options from a categorical variable for analysis. Identifies the highest/lowest N value codes, creates a logical array where those values are TRUE, and adds metadata for label generation.

### `any_positive()`
**Location:** R/builtins.R (referenced)

Checks for any positive value in a group of variables. Returns TRUE if any value is positive, used for multi-response "any mention" analysis.

### `total()`
**Location:** R/builtins.R (referenced)

Creates a total/base row or column that includes all respondents. Returns an array of all 1s with appropriate metadata.

### `base_cell_count()` / `base_cell_count_valid()`
**Location:** R/builtins.R (referenced)

Returns the base count for a specific cell intersection. `base_cell_count()` returns sum of base_array * row_array * col_array; `_valid()` excludes NA values.

### `base_column_total()` / `base_row_total()` / `base_grand_total()`
**Location:** R/builtins.R (referenced)

Returns base counts for column totals, row totals, or grand total. Used in base row/column display and for significance testing denominators.

### `percentile()`
**Location:** R/builtins.R (referenced)

Calculates a specific percentile of values in a distribution. Used for median (50th percentile) and other quantile statistics.

---

## Pattern Matching (DSL) Functions

### `pattern()`
**Location:** R/dsl.R (referenced)

Creates a pattern object for semantic cell matching using the domain-specific language. Wraps expressions in a pattern matcher that normalizes and compares cell specifications semantically rather than syntactically.

### `and_matcher()` / `or_matcher()` / `not_matcher()`
**Location:** R/dsl.R (referenced)

Logical combinators for pattern matching. Combines multiple matchers with AND, OR, or NOT logic for complex cell selection criteria.

### `base_matcher()` / `expr_matcher()` / `label_matcher()` / `value_matcher()` / `statistic_matcher()`
**Location:** R/dsl.R (referenced)

Specialized matchers for different cell components. Match cells based on base threshold, expression content, label text/pattern, value range/condition, or statistic type.

### `response_match()` / `value_range()`
**Location:** R/dsl.R (referenced)

Helpers for matching specific response patterns or value ranges in cells. Used for filtering and selecting cells in derive and hide operations.

---

## String Utility Functions

### `any_grepl()` / `any_gsub()`
**Location:** R/conveniencefunctions.R (referenced)

Pattern matching and replacement across vectors. `any_grepl()` returns TRUE if any pattern matches; `any_gsub()` applies gsub sequentially for multiple pattern replacements.

### `remove_trailing_characters()` / `insert_into_str()` / `sub_char_by_index()`
**Location:** R/conveniencefunctions.R (referenced)

String manipulation utilities for label processing. Remove trailing chars, insert strings at positions, or replace characters by index.

### `gsub_by_dict()`
**Location:** R/conveniencefunctions.R (referenced)

Bulk find-and-replace using a dictionary data frame. Applies multiple substitutions from a mapping table.

### `closest_matching_string()`
**Location:** R/conveniencefunctions.R (referenced)

Fuzzy string matching using string distance algorithms. Finds the closest match from a set of candidate strings, used in variable name resolution.

### `get_longest_common_substring()`
**Location:** R/surveymetadatafunctions.R (referenced)

Finds the longest common substring between two strings using dynamic programming. Used in question grouping to identify common stems.

---

## Validation and Utility Functions

### `check_seps()` / `standardise_survey_separators()`
**Location:** R/dataprocessingfunctions.R (referenced)

Validates and standardizes field separators used in variable naming. Ensures consistent separator patterns across variable names for reliable parsing.

### `check_key_consistency()` / `validate_variable_names()`
**Location:** R/dataprocessingfunctions.R (referenced)

Validation functions for data integrity. Checks that key columns are unique, variable names follow expected patterns, and no duplicates exist.

### `all_duplicated()` / `all_matching()`
**Location:** R/dataprocessingfunctions.R (referenced)

Utilities for finding duplicate or matching rows. `all_duplicated()` returns all rows involved in duplicates (not just first occurrence); `all_matching()` finds all rows matching a condition.

### `conditionally_replace_NAs_in_multiresponse()`
**Location:** R/dataprocessingfunctions.R (referenced)

Handles NA replacement in multi-response variables based on routing logic. Replaces NAs with 0 when respondent was exposed to question but didn't select option.

### `sum_if()` (utility variant)
**Location:** R/dataprocessingfunctions.R (referenced)

Conditional sum utility function (different from derive operation). Sums values where a condition is TRUE.

### `number_to_word_rank()` / `word_rank_to_number()`
**Location:** R/dataprocessingfunctions.R (referenced)

Converts between numeric ranks and word representations. "1" ↔ "first", "2" ↔ "second", etc., for natural language label generation.

---

## S3 Methods for tab_result

### `print.tab_result()`
**Location:** R/compatibility_methods.R (referenced)

Print method that materializes tab_result to data.frame and displays with significance indicators. Shows Base row if show_base = TRUE, formats cells with appropriate decimal places, and includes significance symbols if testing was applied.

### `as.data.frame.tab_result()`
**Location:** R/compatibility_methods.R (referenced)

Materializes a `tab_cell_collection` to a standard data.frame. Allocates grid based on current layout_defs, populates cells by matching against layout_defs, extracts values and significance data, and constructs data.frame with row_label column and numeric columns for each layout column.

### `nrow()` / `ncol()` / `dim()` / `names()` / `dimnames()`
**Location:** R/compatibility_methods.R (referenced)

Data.frame compatibility methods for tab_result. Return dimensions and names from the layout grid structure without requiring materialization.

### `head()` / `tail()`
**Location:** R/compatibility_methods.R (referenced)

Return first or last N rows of the tab result. Materializes to data.frame and applies head/tail.

---

## S3 Methods for survey_data

### `filter.survey_data()` / `select.survey_data()` / `mutate.survey_data()`
**Location:** R/compatibility_methods.R (referenced)

Dplyr verb methods that preserve survey_data structure. Apply operations to the dat component while maintaining dpdict alignment and returning a survey_data object.

### `left_join.survey_data()` / `bind_rows()` / `bind_cols()`
**Location:** R/compatibility_methods.R (referenced)

Join and binding methods for survey_data objects. Handle metadata merging, update dpdict when combining datasets, and preserve survey_data class.

### `[.survey_data` / `[[.survey_data`
**Location:** R/compatibility_methods.R (referenced)

Subsetting methods for survey_data. Enable `survey_obj[rows, cols]` and `survey_obj$column` syntax while maintaining dpdict alignment.

---

## Test Data Generators

### `get_basic_test_dat()` / `get_big_test_dat()`
**Location:** R/testdatagenerators.R (referenced)

Generate synthetic survey data for testing and examples. `get_basic_test_dat()` creates a small dataset with common variable types; `get_big_test_dat()` creates larger dataset with prefixes and question grids.

### `get_minimal_unlabelled_test_dat()` / `get_minimal_labelled_test_dat()`
**Location:** R/testdatagenerators.R (referenced)

Generate minimal test datasets with or without haven labels. Used for unit testing basic functionality.

### `get_routing_test_dat()` / `get_problematic_metadata_test_dat()`
**Location:** R/testdatagenerators.R (referenced)

Generate datasets with specific test scenarios: routing/skip logic patterns or edge cases for metadata handling. Used for testing complex scenarios and error handling.

---

## Additional Utilities

### `%>%` (pipe operator)
**Location:** R/utils-pipe.R

Re-exports the magrittr pipe operator for use with surveydatar functions. Enables chaining: `data %>% tab(q1, q2) %>% add_sig() %>% arrange_rows()`.

### `%||%` (null coalescing)
**Location:** R/utilities.R (referenced)

Returns left side if not NULL, otherwise right side. Used throughout codebase for default value assignment: `value %||% default`.

### `view_base_matrix()`
**Location:** R/utilities.R (referenced)

Visualizes the base count matrix for a tab result. Helpful for debugging and understanding sample sizes across intersections.

### `my_view()`
**Location:** R/surveymetadatafunctions.R:52

Displays a data frame in RStudio viewer when available, avoiding XQuartz issues on macOS. Uses RStudio's View if available, otherwise falls back to utils::View().

### `query_dict()`
**Location:** R/utilities.R (referenced)

Interactive search function for exploring a dpdict. Filters by variable name or label patterns for quick lookup.

---

## Architecture Notes

### Cell Store Architecture
The package uses a dual-storage cell store with:
- **Hash table**: O(1) access by cell ID (c_000001, c_000002, ...)
- **Columnar storage**: Fast bulk operations on values/bases
- **Position independence**: Cells exist independently of layout
- **Full metadata**: Each cell stores value, base, specification, computation, derivation

### Layout System
Layout uses a definitional system:
- **layout_defs**: Define what should appear in rows/columns using matchers
- **Allocation**: Cells allocated to grid by matching specifications against defs
- **Matchers**: Semantic matching (expr_matcher, label_matcher, base_matcher, etc.)
- **Filter rules**: Applied before allocation to restrict cell pool

### Pipeline Architecture
The core pipeline follows: **Parse → Expand → Compute → Layout**
1. **Parse**: NSE expressions → structured specifications
2. **Expand**: Variables → individual components (e.g., categorical values)
3. **Compute**: Specifications → numeric arrays → cells with metadata
4. **Layout**: Cells → 2D grid via semantic matching

### Derive System
Derive operations are **additive**:
- Add new cells, never modify existing ones
- Track source cells in derivation metadata
- Composable: multiple derives can chain
- New layout_defs added for derived dimensions

---

## Summary Statistics

- **Total Functions**: ~171 exported functions
- **Core Files**: 24 R source files
- **Architecture**: Cell-based with position-independent cells
- **Storage**: Dual (hash + columnar) for O(1) access and fast bulk ops
- **Pipeline**: Parse → Expand → Compute → Layout
- **Key Paradigms**: Immutable cells, additive derives, semantic matching
