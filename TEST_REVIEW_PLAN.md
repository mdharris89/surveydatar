# Test Coverage Review Plan - surveydatar Package

**Goal:** Systematically review all 350+ functions across 24 R files against 7 test files to identify gaps, insufficient coverage, and poorly specified tests.

**Package Stats:**
- **R Source Files:** 24 files (~29,154 lines)
- **Test Files:** 7 files (~9,904 lines)
- **Exported Functions:** 212
- **Total Functions:** 350+
- **Current Test Cases:** ~500+

---

## Review Methodology

For each R source file, we will:

1. **Enumerate all functions** (exported and internal)
2. **Map to test files** - identify which test file(s) cover each function
3. **Assess test quality:**
   - ✅ **Comprehensive** - Multiple test cases, edge cases covered
   - ⚠️ **Partial** - Basic functionality tested, missing edge cases
   - ❌ **Missing** - No tests found
   - 🔍 **Unclear** - Tests exist but coverage unclear
4. **Document gaps** - specific scenarios/edge cases not tested
5. **Prioritize improvements** - Critical > High > Medium > Low

---

## Priority-Based Review Order

### 🔴 **PRIORITY 1: Core Architecture** (Must be bulletproof)

These components form the foundation - failures cascade everywhere.

#### 1.1 cell_store.R (14 functions)
**Purpose:** Core cell storage with hash table + columnar storage

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `new_cell_store()` | ? | 🔍 | - Empty store initialization<br>- Large-scale stores (10K+ cells) |
| `generate_cell_id()` | ? | 🔍 | - ID uniqueness guarantees<br>- Sequential numbering |
| `add_cell()` | ? | 🔍 | - Duplicate cell IDs<br>- Invalid cell structure<br>- Performance with bulk adds |
| `get_cell()` | ? | 🔍 | - Non-existent cell IDs<br>- NULL returns |
| `get_cells()` | ? | 🔍 | - Empty ID vectors<br>- Partial matches |
| `all_cell_ids()` | ? | 🔍 | - Empty stores<br>- Large stores |
| `filter_cells()` | ? | 🔍 | - Complex filter expressions<br>- No matches<br>- All matches |
| `filter_cells_by_value()` | ? | 🔍 | - NA handling<br>- Numeric vs character values |
| `validate_cell_store()` | ? | 🔍 | - Invalid store structures<br>- Corrupted data |

**Tests Needed:**
- [ ] Cell lifecycle (create → add → retrieve → filter)
- [ ] Hash table collision handling
- [ ] Performance benchmarks (O(1) access verification)
- [ ] Memory efficiency with large stores
- [ ] Concurrent access patterns (if applicable)

---

#### 1.2 computation.R (5 functions)
**Purpose:** Cell-native computation engine

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `calculate_base_array()` | test-tab-core.R? | 🔍 | - Empty data<br>- NA handling<br>- Weighted vs unweighted |
| `sync_base_matrix()` | ? | 🔍 | - Matrix dimension mismatches<br>- Sparse matrices |
| `compute_cells_vectorized()` | ? | 🔍 | - Edge case array intersections<br>- Performance validation |
| `compute_cells_as_bundle()` | ? | 🔍 | - Empty bundles<br>- Large bundles |
| `build_cell_specification()` | ? | 🔍 | - Invalid specifications<br>- Partial specifications |

**Tests Needed:**
- [ ] Array intersection calculations (2D, 3D)
- [ ] Base calculation correctness (unweighted, weighted, valid base)
- [ ] Vectorized vs bundle computation equivalence
- [ ] NA propagation through computations
- [ ] Extreme values (very large/small bases)

---

#### 1.3 tab.R (13 functions)
**Purpose:** Main tab() interface and pipeline orchestration

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `tab()` | test-tab-core.R | ⚠️ | - Complex nested formulas<br>- All parameter combinations<br>- Error messages |
| `multi_tab()` | ? | ❌ | - Not found in test files |
| `multi_tab_cols()` | ? | ❌ | - Not found in test files |
| `multi_tab_rows()` | ? | ❌ | - Not found in test files |

**Tests Needed:**
- [ ] All parameter combinations for `tab()`
- [ ] Parse → expand → compute pipeline integrity
- [ ] Multi-tab functions (rows, cols, full)
- [ ] Error handling and user-friendly messages
- [ ] Memory efficiency with large datasets
- [ ] Formula validation edge cases

---

#### 1.4 constructors.R (26 functions)
**Purpose:** Registry system and object constructors

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `new_tab_helper()` | test-tab-helpers.R? | ⚠️ | - Invalid helper structures |
| `new_tab_stat()` | test-tab-helpers.R? | ⚠️ | - Invalid statistic structures |
| `new_tab_macro()` | ? | 🔍 | - Invalid macro structures |
| `new_tab_sig_test()` | ? | 🔍 | - Invalid test structures |
| `create_helper()` | test-tab-helpers.R | ⚠️ | - Name collisions<br>- Invalid function signatures |
| `create_statistic()` | test-tab-helpers.R | ⚠️ | - Name collisions<br>- Invalid function signatures |
| `create_macro()` | test-tab-helpers.R | ⚠️ | - Recursive macros<br>- Circular dependencies |
| `create_macro_call()` | ? | 🔍 | - Invalid macro names |
| `create_significance_test()` | ? | 🔍 | - Invalid test specifications |
| Registry functions | test-tab-core.R | ⚠️ | - Registry corruption<br>- Clear/reset behavior |
| Base calculators (7 functions) | ? | 🔍 | - NA handling<br>- Zero bases<br>- Negative weights |

**Tests Needed:**
- [ ] Registry isolation between tests
- [ ] Custom object creation validation
- [ ] Base calculator edge cases (NA, zero, negative)
- [ ] Name collision handling
- [ ] Registry clear/reset functionality

---

### 🟠 **PRIORITY 2: Parsing & DSL** (Data flows through here)

#### 2.1 parsing.R (16 functions)
**Purpose:** Formula parsing and variable expansion

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `resolve_external_variables()` | ? | 🔍 | - Nested lists<br>- Non-standard evaluation edge cases |
| `parse_table_formula()` | ? | 🔍 | - Malformed formulas<br>- Complex nested expressions |
| `expand_variables()` | ? | 🔍 | - Missing variables<br>- Fuzzy matching failures |
| `formula_to_array()` | ? | 🔍 | - Empty formulas<br>- Large categorical variables |
| `process_helper()` | ? | 🔍 | - Nested helpers<br>- Helper errors |
| `process_helper_for_specs()` | ? | 🔍 | - Specification edge cases |

**Tests Needed:**
- [ ] NSE → structured specs conversion
- [ ] External variable resolution (all types)
- [ ] Formula validation and error messages
- [ ] Variable expansion (categorical, question groups)
- [ ] Helper processing (nested, chained)
- [ ] Fuzzy matching behavior

---

#### 2.2 dsl.R (26 functions)
**Purpose:** Domain-specific language for semantic cell matching

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `validate_dsl_expr()` | ? | 🔍 | - All allowed operations<br>- Disallowed operations caught |
| `eval_dsl()` | ? | 🔍 | - Complex logical expressions<br>- between() function |
| `normalize_dsl()` | ? | 🔍 | - Normalization edge cases |
| `derive_label_from_dsl()` | ? | 🔍 | - Label generation for complex expressions |

**Tests Needed:**
- [ ] DSL validation (allowed/disallowed operations)
- [ ] DSL evaluation correctness
- [ ] Semantic matching edge cases
- [ ] Label derivation from expressions
- [ ] Performance with complex DSL

---

### 🟡 **PRIORITY 3: Helpers, Statistics & Significance**

#### 3.1 builtins.R (16 functions)
**Purpose:** Built-in helpers and statistics

| Helper/Statistic | Test Location | Coverage | Gaps Identified |
|------------------|--------------|----------|-----------------|
| `top_box()` | test-tab-helpers.R | ✅ | (Review for edge cases) |
| `bottom_box()` | test-tab-helpers.R | ✅ | (Review for edge cases) |
| `value_range()` | test-tab-helpers.R | ✅ | - Invalid ranges |
| `pattern()` | test-tab-helpers.R | ✅ | - Complex regex patterns |
| `percentile()` | test-tab-helpers.R | ✅ | - Edge percentiles (0, 100) |
| `all_matching()` | test-tab-helpers.R | ✅ | - No matches |
| `any_positive()` | test-tab-helpers.R | ✅ | - All zero/negative |
| `banner()` | test-tab-helpers.R | ✅ | - Empty banners |
| `response_match()` | test-tab-helpers.R | ✅ | - Complex matching |
| `total()` | test-tab-helpers.R | ✅ | - Grand totals vs row/col totals |
| Statistics (count, %, mean, etc.) | test-tab-core.R | ⚠️ | - NA handling<br>- Weighted variants<br>- Zero bases |
| Significance tests (6 types) | test-tab-helpers.R? | ⚠️ | - Statistical correctness<br>- Edge cases (small n, ties) |

**Tests Needed:**
- [ ] Statistical correctness validation (against known results)
- [ ] Weighted vs unweighted equivalence when weights = 1
- [ ] NA handling in all statistics
- [ ] Zero/small base behavior
- [ ] Significance test assumptions and warnings

---

#### 3.2 significance.R (8 functions)
**Purpose:** Significance test execution and result handling

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `add_sig()` | test-tab-helpers.R? | ⚠️ | - Multiple comparison correction<br>- Edge cases |
| `add_sig_all()` | ? | 🔍 | - Large tables<br>- Performance |

**Tests Needed:**
- [ ] All 6 significance test types
- [ ] Multiple comparison scenarios
- [ ] Weighted vs unweighted tests
- [ ] Small sample warnings
- [ ] Result notation formatting

---

### 🟢 **PRIORITY 4: Layout & Display**

#### 4.1 layout.R (52 functions - LARGEST MODULE)
**Purpose:** Layout operations and grid manipulation

**Key Function Groups:**

**Arrangement (2 functions):**
- `arrange_rows()`, `arrange_cols()` - test-tab-helpers.R (partial)

**Selection (2 functions):**
- `select_rows()`, `select_cols()` - test-tab-helpers.R (partial)

**Grouping (2 functions):**
- `group_rows()`, `group_cols()` - test-tab-helpers.R (partial)

**Movement (2 functions):**
- `move_row()`, `move_col()` - ? (likely missing)

**Hiding/Showing (8 functions):**
- `hide_rows()`, `hide_cols()`, `hide_rows_except()`, `hide_cols_except()`
- `hide_base()`, `show_base()`, `hide_summary()`, `show_summary()`
- Coverage: Partial in test-tab-helpers.R

**Conditional Hiding (1 function):**
- `hide_if()` - ? (likely missing)

**Formatting (3 functions):**
- `format_row()`, `format_col()`, `format_cells()` - ? (coverage unclear)

**Grid Manipulation (30+ functions):**
- Internal functions for grid allocation, ordering, etc.

**Tests Needed:**
- [ ] All arrangement operations (rows, cols)
- [ ] Selection operations (positive, negative indices)
- [ ] Grouping operations (nested groups)
- [ ] Movement operations (boundary conditions)
- [ ] Visibility operations (hide, show, conditional)
- [ ] Formatting operations (all format types)
- [ ] Operation chaining (multiple operations)
- [ ] Non-destructive metadata verification

---

#### 4.2 Export & Display (3 files: export.R, tabtoreactable.R, tabtoflourish.R)

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `print.tab_result()` | test-tab-export.R | ⚠️ | - Large tables<br>- Unicode handling |
| `copy_tab()` | test-tab-export.R | ⚠️ | - Clipboard edge cases<br>- Platform differences |
| `copy_df()` | test-tab-export.R | ⚠️ | - Large data frames |
| `display_reactable()` | test-tab-export.R | ⚠️ | - Interactive features |
| `tab_to_reactable()` | test-tab-export.R | ⚠️ | - All formatting options<br>- Color scales |
| `tab_to_flourish()` | test-tab-export.R | ⚠️ | - All chart types<br>- Data transformation |

**Tests Needed:**
- [ ] Print formatting (all output modes)
- [ ] Clipboard functionality (cross-platform)
- [ ] Reactable generation (all options)
- [ ] Flourish export (all chart types)
- [ ] Significance notation in displays
- [ ] Large table handling

---

### 🔵 **PRIORITY 5: Data Management**

#### 5.1 surveymetadatafunctions.R (40 functions)
**Purpose:** Data dictionary and metadata management

**Key Function Groups:**

**Dictionary Creation (2 functions):**
- `create_dict()`, `create_dict_with_metadata()` - test-surveymetadatafunctions.R (good coverage)

**Dictionary Querying (5+ functions):**
- `query_dict()`, `get_questions_dict()`, etc. - test-surveymetadatafunctions.R (partial)

**Dictionary Updates (5+ functions):**
- `update_dict()`, `modify_labels()`, etc. - test-surveymetadatafunctions.R (partial)

**Validation (5+ functions):**
- `validate_survey_data()`, `validate_no_dpdict_duplicates()`, etc. - test-surveymetadatafunctions.R (partial)

**Label Operations (10+ functions):**
- `split_grid_labels()`, `update_labelled_values()`, etc. - Coverage unclear

**Metadata Display (2 functions):**
- `datamap()`, `datamap_questions()` - test-surveymetadatafunctions.R (basic)

**Tests Needed:**
- [ ] All validation functions (edge cases)
- [ ] Label manipulation (all operations)
- [ ] Metadata consistency checks
- [ ] Large dictionary performance
- [ ] Question group handling
- [ ] Grid variable splitting

---

#### 5.2 weighting.R + weighting_config.R (33 functions total)

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `create_weighting_config()` | test-weighting-config.R | ⚠️ | - Complex configurations |
| `calculate_target_weights()` | test-weighting.R | ⚠️ | - Extreme weights<br>- Non-convergence |
| `run_unified_weighting()` | test-weighting.R | ⚠️ | - Multiple variables<br>- Edge cases |
| `make_constraint()` | test-weighting-config.R | ⚠️ | - Invalid constraints |
| Constraint validation | test-weighting-config.R | ⚠️ | - Conflicting constraints |

**Tests Needed:**
- [ ] Weighting algorithm correctness
- [ ] Constraint handling (valid, invalid, conflicting)
- [ ] Alpha blending edge cases
- [ ] Effective sample size calculations
- [ ] Weight trimming/capping
- [ ] Non-convergence handling
- [ ] Integration with statistics

---

### ⚪ **PRIORITY 6: Utilities & Compatibility**

#### 6.1 utilities.R + conveniencefunctions.R (40 functions)

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `any_grepl()` | test-conveniencefunctions.R | ✅ | (Review edge cases) |
| `any_gsub()` | test-conveniencefunctions.R | ✅ | (Review edge cases) |
| `bind_cols()` | test-conveniencefunctions.R | ⚠️ | - Dimension mismatches |
| `bind_rows()` | test-conveniencefunctions.R | ⚠️ | - Column mismatches |
| `all_duplicated()` | test-conveniencefunctions.R | ⚠️ | - Empty vectors |
| String utilities (10+ functions) | Partial in test-conveniencefunctions.R | ⚠️ | - Unicode, special characters |

**Tests Needed:**
- [ ] All string manipulation edge cases
- [ ] Data binding edge cases
- [ ] Pattern matching (complex patterns)
- [ ] Performance benchmarks for utilities

---

#### 6.2 compatibility_methods.R (10 S3 methods)

| Method | Test Location | Coverage | Gaps Identified |
|--------|--------------|----------|-----------------|
| `dim.tab_result()` | ? | 🔍 | - Empty tab results |
| `dimnames.tab_result()` | ? | 🔍 | - NULL names |
| `names.tab_result()` | ? | 🔍 | - Name assignment |
| `[.tab_result()` | ? | 🔍 | - Out of bounds<br>- Negative indices |
| `[[.tab_result()` | ? | 🔍 | - Single element access |
| `$.tab_result()` | ? | 🔍 | - Invalid names |
| `head.tab_result()`, `tail.tab_result()` | ? | 🔍 | - Small tables |
| `str.tab_result()` | ? | 🔍 | - Structure display |

**Tests Needed:**
- [ ] All S3 method behaviors
- [ ] Compatibility with base R expectations
- [ ] Edge cases (empty, single element, large)

---

#### 6.3 derive.R (15 functions)

| Function | Test Location | Coverage | Gaps Identified |
|----------|--------------|----------|-----------------|
| `derive()` | test-tab-helpers.R | ⚠️ | - Complex derive chains |
| `create_derive_operation()` | ? | 🔍 | - Invalid operations |
| Operation registry | ? | 🔍 | - Registry management |

**Tests Needed:**
- [ ] All derive operations
- [ ] Derive operation chaining
- [ ] Cell metadata preservation
- [ ] Custom derive operations

---

## Test Quality Assessment Framework

For each function, assess:

### 1. **Functionality Coverage**
- [ ] Happy path tested
- [ ] Edge cases tested (empty, NULL, NA, extreme values)
- [ ] Error cases tested (invalid inputs, type mismatches)
- [ ] Boundary conditions tested

### 2. **Integration Coverage**
- [ ] Function works in isolation
- [ ] Function works in pipeline/workflow
- [ ] Function works with all parameter combinations
- [ ] Function works with different data types

### 3. **Performance Coverage**
- [ ] Function tested with small data
- [ ] Function tested with large data (scalability)
- [ ] Function tested for memory efficiency
- [ ] Function tested for speed (benchmarks if critical)

### 4. **Test Quality**
- [ ] Tests have clear, descriptive names
- [ ] Tests are independent (no test interdependencies)
- [ ] Tests use appropriate assertions
- [ ] Tests document expected behavior
- [ ] Tests are maintainable

---

## Gap Analysis Template

For each identified gap, document:

```markdown
### Gap: [Function Name] - [Specific Scenario]

**Priority:** Critical / High / Medium / Low
**Current State:** No test / Partial test / Inadequate test
**Missing Coverage:** [Describe what's not tested]
**Risk:** [What could break? Impact?]
**Suggested Tests:**
1. [Test description]
2. [Test description]
**Effort Estimate:** Small / Medium / Large
```

---

## Review Process

### Phase 1: Mapping (2-3 days)
1. For each R file, enumerate all functions
2. Search test files for function references
3. Create initial coverage matrix
4. Identify obvious gaps (functions with no tests)

### Phase 2: Quality Assessment (3-5 days)
1. For each tested function, review test quality
2. Identify edge cases not covered
3. Assess integration test coverage
4. Document specific gaps

### Phase 3: Prioritization (1 day)
1. Rank gaps by priority (Critical → Low)
2. Estimate effort to close gaps
3. Create implementation roadmap

### Phase 4: Implementation (Ongoing)
1. Write missing tests (highest priority first)
2. Improve inadequate tests
3. Add integration tests
4. Benchmark critical paths

---

## Automated Tools to Support Review

```r
# Use these R packages for coverage analysis:
library(covr)
library(testthat)

# Generate coverage report
cov <- package_coverage()
report(cov)

# Identify untested functions
zero_coverage(cov)

# Function-by-function coverage
function_coverage(cov)

# Visual coverage report
covr::codecov()
```

---

## Success Criteria

Test suite is sufficient when:

✅ **Coverage:** >90% line coverage, >80% branch coverage
✅ **Stability:** All tests pass consistently
✅ **Clarity:** Tests document expected behavior
✅ **Completeness:** All exported functions have tests
✅ **Edge Cases:** Critical functions test edge cases
✅ **Integration:** Key workflows tested end-to-end
✅ **Performance:** Critical paths benchmarked
✅ **Regression:** Past bugs have regression tests

---

## Next Steps

1. **Install coverage tools:** `install.packages("covr")`
2. **Generate initial coverage report:** `covr::package_coverage()`
3. **Begin Phase 1 mapping** using this document as template
4. **Create detailed gap analysis** for Priority 1 components
5. **Develop test improvement roadmap**

---

**Document Version:** 1.0
**Last Updated:** 2026-01-02
**Owner:** Package Maintainer
