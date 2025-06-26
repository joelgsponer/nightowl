# GitHub Issue #27: Parameter Naming Consistency Implementation Plan

## 1. Analysis Summary

**Current State:**
- **Consistency Score: 6/10** - Mixed parameter naming conventions found across the codebase
- **25+ inconsistent parameters** across 98+ functions identified
- **Root Cause:** Organic growth without consistent naming guidelines, mixing camelCase, snake_case, and abbreviated conventions

**Key Issues Found:**
- Grouping parameters: `group_by` vs `split_by` vs `group.by` vs `groupBy`
- Axis limits: `xlim`/`ylim` vs `x_lim`/`y_lim`
- Color parameters: `add_colors` vs `addColors`
- Data references: `.data` vs `data` (partially resolved)
- Function parameter inconsistencies: `fun.data` vs `fun_data`

**Impact:** Breaking changes required for 15+ exported functions

## 2. Strategic Approach

**Enhance vs Build Decision:** **ENHANCE** - Update existing functions with deprecation warnings for backward compatibility

**Phased Approach Rationale:**
- Phase 1: Non-breaking changes (internal functions, documentation)
- Phase 2: Breaking changes with deprecation warnings (6-month deprecation period)
- Phase 3: Remove deprecated parameters

## 3. Step-by-Step Solution

### Phase 1: Non-Breaking Changes (Week 1-2)
1. **Update internal functions** in `R/utils.r`, `R/validation.R`
2. **Standardize documentation** parameter naming in all roxygen2 comments
3. **Update test files** to use consistent parameter names
4. **Create parameter naming guide** in `CLAUDE.md`

### Phase 2: Breaking Changes with Deprecation (Week 3-4)
1. **Update exported functions** with dual parameter support
2. **Add deprecation warnings** using `lifecycle::deprecate_warn()`
3. **Update all examples** in documentation
4. **Update vignettes** to use new parameter names

### Phase 3: Cleanup (After 6 months)
1. **Remove deprecated parameters**
2. **Clean up conditional logic**
3. **Update NAMESPACE** and documentation

## 4. Files to Modify

**R Files (23 files):**
- `R/plot.R` - `options_svg` structure
- `R/Summary.r` - `group_by` consistency
- `R/summaries.R` - `add_colors` → `add_color`
- `R/forest.R` - `xlim` → `x_lim`
- `R/inline_plots.R` - `fun.data` → `fun_data`, `xlim`/`ylim` → `x_lim`/`y_lim`
- `R/km.R` - `break_width` → `break_width`
- `R/grouped_chisq.R` - `split_by` → `group_by`
- `R/coxph.R` - parameter consistency
- `R/add_plots.R` - `xlim`/`ylim` → `x_lim`/`y_lim`
- 14 additional files with minor parameter updates

**Documentation Files:**
- All `man/*.Rd` files (auto-generated)
- `vignettes/*.Rmd` - update parameter examples

**Test Files (31 files):**
- All `tests/testthat/test-*.R` files
- Focus on `test-inline-plots.R`, `test-forestplot.R`, `test-grouped_chisq.R`

## 5. Testing Strategy

1. **Parameter Compatibility Tests:**
   ```r
   test_that("deprecated parameters still work with warnings", {
     expect_warning(forestplot(x=1, xlim=c(0,2)))
     expect_no_warning(forestplot(x=1, x_lim=c(0,2)))
   })
   ```

2. **Integration Tests:**
   - Method chaining compatibility
   - R6 class parameter validation

3. **Documentation Tests:**
   - All examples run without errors
   - Vignette code execution

## 6. Metric-Driven Outcomes

**Before (Current State):**
- Consistency Score: 6/10
- 25 inconsistent parameters across 98 functions
- Mixed naming conventions (camelCase, snake_case, abbreviated)
- No formal parameter naming guidelines

**After (Target State):**
- Consistency Score: 9/10
- 95%+ tidyverse-compliant naming for exported functions
- Comprehensive parameter naming guidelines
- Backward compatibility maintained for 6 months
- Clear deprecation warnings for old parameter names

## 7. Performance & Security Considerations

- **Performance:** Minimal impact - parameter aliasing adds negligible overhead
- **Security:** No security implications - maintaining existing validation patterns
- **Memory:** No memory impact - same parameter values, different names

## 8. Specific Parameter Changes

**High Priority Changes:**
1. `group_by` standardization across all functions
2. `x_lim`/`y_lim` instead of `xlim`/`ylim`
3. `fun_data` instead of `fun.data`
4. `add_color` instead of `add_colors`
5. `group_by` instead of `split_by` in grouped functions

This plan ensures systematic adoption of tidyverse naming conventions while maintaining backward compatibility and minimizing disruption to existing users.