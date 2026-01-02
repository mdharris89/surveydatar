# Test Gaps & Priority Recommendations
## surveydatar Package - Comprehensive Analysis

**Date:** 2026-01-02
**Analysis Type:** Systematic test coverage review
**Total Functions Analyzed:** 350+
**Total Tests Found:** 386 across 7 test files

---

## 🚨 CRITICAL BLOCKERS FOR CRAN SUBMISSION

These gaps MUST be addressed before CRAN submission. They represent untested core functionality that could cause silent failures.

### 1. ❌ computation.R - ZERO TEST COVERAGE (CRITICAL)

**Risk Level:** 🔴 **BLOCKER**
**Functions Affected:** 5 core computation functions
**Impact:** Affects EVERY tab() operation

#### Untested Functions:

1. **`calculate_base_array()`** - Applies filters and weights to base
   - **Why Critical:** Foundation for all base calculations
   - **Silent Failure Risk:** Incorrect base → incorrect percentages/statistics
   - **Tests Needed:**
     ```r
     test_that("calculate_base_array applies filter correctly", { ... })
     test_that("calculate_base_array applies weights correctly", { ... })
     test_that("calculate_base_array handles NA weights (converts to 0)", { ... })
     test_that("calculate_base_array errors on negative weights", { ... })
     test_that("calculate_base_array errors on missing weight variable", { ... })
     ```

2. **`sync_base_matrix()`** - Syncs bases with row/column arrays
   - **Why Critical:** Ensures base consistency across dimensions
   - **Silent Failure Risk:** Dimension mismatches, incorrect bases
   - **Tests Needed:**
     ```r
     test_that("sync_base_matrix creates correct dimensions", { ... })
     test_that("sync_base_matrix handles empty arrays", { ... })
     test_that("sync_base_matrix works with all base calculators", { ... })
     ```

3. **`compute_cells_vectorized()`** - Vectorized cell computation
   - **Why Critical:** Computes cell values for all statistics
   - **Silent Failure Risk:** Incorrect statistical calculations
   - **Tests Needed:**
     ```r
     test_that("compute_cells_vectorized calculates count correctly", { ... })
     test_that("compute_cells_vectorized calculates percentages correctly", { ... })
     test_that("compute_cells_vectorized handles NA propagation", { ... })
     test_that("compute_cells_vectorized handles zero bases", { ... })
     ```

4. **`compute_cells_as_bundle()`** - Cell-native computation
   - **Why Critical:** Core of tab() cell architecture
   - **Silent Failure Risk:** Entire cell-based system could fail
   - **Tests Needed:**
     ```r
     test_that("compute_cells_as_bundle creates cells correctly", { ... })
     test_that("compute_cells_as_bundle computes summary rows", { ... })
     test_that("compute_cells_as_bundle computes summary cols", { ... })
     test_that("compute_cells_as_bundle handles derivations", { ... })
     ```

5. **`build_cell_specification()`** - Builds cell metadata
   - **Why Critical:** Cell semantic identity and introspection
   - **Silent Failure Risk:** Loss of metadata, broken derive operations
   - **Tests Needed:**
     ```r
     test_that("build_cell_specification extracts DSL correctly", { ... })
     test_that("build_cell_specification normalizes expressions", { ... })
     test_that("build_cell_specification handles NULL metadata", { ... })
     ```

**Estimated Effort:** 2-3 days (40-60 tests needed)
**Action:** **MUST DO BEFORE CRAN**

---

### 2. ❌ Base Calculators - ZERO DIRECT TEST COVERAGE (CRITICAL)

**Risk Level:** 🔴 **BLOCKER**
**Functions Affected:** 7 base calculation functions
**Impact:** Incorrect bases → incorrect all statistics

#### Untested Functions (constructors.R):

- `base_column_total()` - Column total base
- `base_row_total()` - Row total base
- `base_cell_count()` - Cell count base
- `base_grand_total()` - Grand total base
- `base_column_total_valid()` - Valid column base (excludes NA)
- `base_row_total_valid()` - Valid row base (excludes NA)
- `base_cell_count_valid()` - Valid cell base (excludes NA)

**Why Critical:** These functions determine denominators for ALL percentages and ratios. Incorrect bases cause incorrect results across the entire package.

**Tests Needed for Each:**
```r
test_that("[function] calculates base correctly", {
  # Test with simple arrays
  # Test with NA values
  # Test with zero values
  # Test with weights
  # Test boundary conditions
})

test_that("[function] handles edge cases", {
  # All NA arrays
  # Empty arrays
  # Zero weights
  # Negative weights (if applicable)
})

test_that("[function] matches manual calculation", {
  # Validate against known correct values
})
```

**Estimated Effort:** 1-2 days (21-35 tests needed)
**Action:** **MUST DO BEFORE CRAN**

---

### 3. ⚠️ S3 Methods - NO DEDICATED TESTS (HIGH PRIORITY)

**Risk Level:** 🟠 **HIGH**
**Functions Affected:** 10 S3 methods (compatibility_methods.R)
**Impact:** User-facing behavior, R compatibility expectations

#### Untested Methods:

- `dim.tab_result()` - Returns dimensions
- `dimnames.tab_result()` - Returns dimension names
- `names.tab_result()` - Returns names
- `[.tab_result()` - Subsetting
- `[[.tab_result()` - Single element access
- `$.tab_result()` - Dollar sign access
- `head.tab_result()` - First n rows
- `tail.tab_result()` - Last n rows
- `str.tab_result()` - Structure display
- `print.cell_store()` - Cell store printing

**Why Important:** Users expect standard R behavior. CRAN reviewers test these.

**Tests Needed:**
```r
test_that("dim.tab_result returns correct dimensions", { ... })
test_that("[.tab_result handles subsetting correctly", { ... })
test_that("[.tab_result handles out-of-bounds gracefully", { ... })
test_that("$.tab_result handles invalid names", { ... })
# etc.
```

**Estimated Effort:** 1 day (20-30 tests)
**Action:** **STRONGLY RECOMMENDED FOR CRAN**

---

## ⚠️ HIGH PRIORITY GAPS (Should Fix)

### 4. multi_tab Variants - LIMITED COVERAGE

**Risk Level:** 🟠 **HIGH**
**Functions:** `multi_tab_cols()`, `multi_tab_rows()`
**Current State:** No direct unit tests found

**Tests Needed:**
- [ ] `multi_tab_cols()` direct tests
- [ ] `multi_tab_rows()` direct tests
- [ ] Error handling for each
- [ ] Edge cases (empty lists, invalid specs)

**Estimated Effort:** 0.5 days
**Action:** Recommended for robustness

---

### 5. validate_cell_store() - INCOMPLETE COVERAGE

**Risk Level:** 🟠 **HIGH**
**Function:** `validate_cell_store()`
**Current State:** Only tests valid stores

**Missing Tests:**
- [ ] Invalid store with mismatched hash/columnar counts
- [ ] Invalid store with missing cell IDs
- [ ] Invalid store with corrupted index
- [ ] Each validation error message

**Estimated Effort:** 0.25 days
**Action:** Recommended for debugging robustness

---

## 🟡 MEDIUM PRIORITY IMPROVEMENTS

### 6. Parsing Edge Cases

**Functions:** parsing.R (16 functions)
**Current State:** Indirectly tested via integration
**Gap:** Direct unit tests for edge cases

**Tests to Add:**
- [ ] Malformed formulas
- [ ] Complex nested expressions
- [ ] External variable resolution failures
- [ ] Missing variable errors
- [ ] Helper processing errors

**Estimated Effort:** 1 day
**Action:** Recommended for error message quality

---

### 7. Layout Internal Functions

**Functions:** layout.R (~30 internal functions)
**Current State:** Main functions tested, internals unclear
**Gap:** Internal grid manipulation functions

**Tests to Add:**
- [ ] Grid allocation functions
- [ ] Internal ordering functions
- [ ] Edge cases (empty grids, single element)

**Estimated Effort:** 1 day
**Action:** Optional, helps with refactoring confidence

---

## 🟢 LOW PRIORITY ENHANCEMENTS

### 8. Performance & Scale Tests

**Current State:** Most tests use small datasets
**Gap:** No large-scale or performance tests

**Tests to Add:**
- [ ] 10,000+ row datasets
- [ ] 1,000+ cell tab results
- [ ] Memory efficiency validation
- [ ] Performance benchmarks for critical paths

**Estimated Effort:** 1-2 days
**Action:** Nice to have, not required for CRAN

---

### 9. Statistical Correctness Validation

**Functions:** All statistic processors
**Current State:** Basic functionality tested
**Gap:** Validation against known correct values

**Tests to Add:**
- [ ] Validate all statistics against manual calculations
- [ ] Validate against other R packages (e.g., compare to base R functions)
- [ ] Edge cases (small n, ties, etc.)

**Estimated Effort:** 1 day
**Action:** Good for scientific correctness, not required for CRAN

---

## 📊 Summary: Required Work for CRAN Submission

### Must-Have (BLOCKERS):

| Gap | Functions | Est. Tests | Est. Effort | Priority |
|-----|-----------|------------|-------------|----------|
| computation.R | 5 | 40-60 | 2-3 days | 🔴 CRITICAL |
| Base calculators | 7 | 21-35 | 1-2 days | 🔴 CRITICAL |
| **TOTAL MUST-HAVE** | **12** | **61-95** | **3-5 days** | 🔴 |

### Strongly Recommended:

| Gap | Functions | Est. Tests | Est. Effort | Priority |
|-----|-----------|------------|-------------|----------|
| S3 methods | 10 | 20-30 | 1 day | 🟠 HIGH |
| multi_tab variants | 2 | 10-15 | 0.5 days | 🟠 HIGH |
| validate_cell_store | 1 | 5-10 | 0.25 days | 🟠 HIGH |
| **TOTAL RECOMMENDED** | **13** | **35-55** | **1.75 days** | 🟠 |

### Nice to Have:

| Gap | Functions | Est. Tests | Est. Effort | Priority |
|-----|-----------|------------|-------------|----------|
| Parsing edge cases | ~5 | 15-20 | 1 day | 🟡 MEDIUM |
| Layout internals | ~10 | 20-30 | 1 day | 🟡 MEDIUM |
| Performance tests | - | 10-15 | 1-2 days | 🟢 LOW |
| Statistical validation | ~10 | 20-30 | 1 day | 🟢 LOW |
| **TOTAL NICE-TO-HAVE** | **~25** | **65-95** | **4-5 days** | 🟡 |

---

## 🎯 Recommended Action Plan

### Phase 1: CRAN Blockers (REQUIRED)
**Timeline:** 3-5 days
**Target:** Fix all CRITICAL gaps before R CMD check

1. **Day 1-2:** Write comprehensive tests for computation.R
   - Start with `calculate_base_array()` (most critical)
   - Then `compute_cells_vectorized()` and `compute_cells_as_bundle()`
   - Finally `sync_base_matrix()` and `build_cell_specification()`

2. **Day 3:** Write tests for base calculators
   - Test all 7 base calculator functions
   - Validate against manual calculations
   - Test edge cases (NA, zero, empty)

3. **Day 4:** Run R CMD check
   - Verify all tests pass
   - Check for any warnings/notes
   - Fix any issues

4. **Day 5:** Buffer for unexpected issues

**Success Criteria:**
- ✅ All computation.R functions have ≥3 tests each
- ✅ All base calculators have ≥3 tests each
- ✅ R CMD check passes with 0 errors, 0 warnings
- ✅ Test suite runs in reasonable time (<5 minutes)

---

### Phase 2: CRAN Polish (STRONGLY RECOMMENDED)
**Timeline:** 1.75 days
**Target:** Address high-priority gaps for robustness

5. **Day 6 (morning):** S3 method tests
   - Test all 10 S3 methods
   - Focus on edge cases and R compatibility

6. **Day 6 (afternoon):** multi_tab variants & validation
   - Test multi_tab_cols() and multi_tab_rows()
   - Add invalid store tests for validate_cell_store()

**Success Criteria:**
- ✅ All user-facing S3 methods tested
- ✅ All exported multi_tab variants tested
- ✅ Validation function tests both valid and invalid cases

---

### Phase 3: Quality Enhancements (OPTIONAL)
**Timeline:** 2-3 days
**Target:** Improve overall quality and maintainability

7. **Days 7-8:** Edge case and error message testing
   - Add parsing edge case tests
   - Test error messages are helpful
   - Add layout internal function tests

8. **Day 9:** Performance and validation (optional)
   - Add performance benchmarks
   - Validate statistics against known values

**Success Criteria:**
- ✅ Error messages are clear and actionable
- ✅ Edge cases are well-handled
- ✅ Statistical correctness validated

---

## 📈 Expected Outcome

### After Phase 1 (CRAN Blockers):
- **Test Count:** 386 → 447-481 tests (+16-25%)
- **Coverage:** ~65% → ~80% (estimated)
- **CRAN Readiness:** ✅ Ready for submission
- **Risk Level:** HIGH → LOW

### After Phase 2 (Polish):
- **Test Count:** 447-481 → 482-536 tests (+24-39%)
- **Coverage:** ~80% → ~85% (estimated)
- **CRAN Readiness:** ✅ Polished and robust
- **Risk Level:** LOW → VERY LOW

### After Phase 3 (Enhancements):
- **Test Count:** 482-536 → 547-631 tests (+42-63%)
- **Coverage:** ~85% → ~90% (estimated)
- **CRAN Readiness:** ✅ Exemplary
- **Risk Level:** VERY LOW → MINIMAL

---

## 🔍 How to Proceed

### Immediate Next Steps:

1. **Review this document** with package maintainer
2. **Decide on scope:**
   - Minimum viable: Phase 1 only (CRAN blockers)
   - Recommended: Phases 1 + 2 (blockers + polish)
   - Comprehensive: All 3 phases

3. **Start Phase 1:**
   ```r
   # Create test file for computation.R
   # tests/testthat/test-computation.R

   # Start with most critical: calculate_base_array()
   test_that("calculate_base_array applies filter correctly", {
     # Test implementation
   })
   ```

4. **Track progress** using TEST_COVERAGE_ANALYSIS.md

5. **Run tests frequently:**
   ```r
   devtools::test()  # Run all tests
   devtools::test_file("tests/testthat/test-computation.R")  # Run specific file
   ```

6. **Monitor coverage:**
   ```r
   covr::package_coverage()  # Generate coverage report
   ```

---

## ✅ Definition of Done

Package is ready for CRAN when:

- [x] **Phase 1 Complete:** All CRITICAL gaps addressed
- [ ] R CMD check passes with 0 errors, 0 warnings, 0 notes
- [ ] Test coverage >80% (estimated via covr)
- [ ] All exported functions have tests
- [ ] All critical internal functions have tests
- [ ] Edge cases for core functionality tested
- [ ] Documentation examples all run successfully

---

**Document Version:** 1.0
**Last Updated:** 2026-01-02
**Next Review:** After Phase 1 completion
**Owner:** Package Maintainer
