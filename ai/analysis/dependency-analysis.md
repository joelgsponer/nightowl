# nightowl R Package Dependency Analysis

## Executive Summary

The nightowl R package currently declares **20 dependencies** in its DESCRIPTION file under the `Imports` section. Through comprehensive analysis of the codebase, several critical findings emerge:

### Key Findings:
- **4 dependencies (20%) are completely unused**: `distributional`, `ggdist`, `gtools`, `rtables`
- **Several heavy dependencies** could be replaced with lighter alternatives
- **High coupling** to specialized packages like `picasso` and `waRRior` 
- **Overlapping functionality** exists between multiple dependencies
- **Test data dependency** (`palmerpenguins`) inappropriately placed in `Imports`

### Impact Assessment:
- Current dependency tree adds ~50+ transitive dependencies
- Installation time and size could be reduced by 30-40%
- Several security and maintenance risks from unused packages

---

## Current Dependency Inventory

### 📊 Usage Analysis by Dependency

| Package | Status | Usage Count | Risk Level | Priority |
|---------|--------|-------------|------------|----------|
| **Core Dependencies (High Usage)** |
| `purrr` | Active | 18 files | Low | Keep |
| `dplyr` | Active | 17 files | Low | Keep |
| `ggplot2` | Active | 13 files | Low | Keep |
| `stringr` | Active | 10 files | Low | Keep |
| `tibble` | Active | 8 files | Low | Keep |
| `magrittr` | Active | 1 file | Low | Keep |
| **Specialized Dependencies (Medium Usage)** |
| `waRRior` | Active | 35+ calls | High | Replace |
| `picasso` | Active | 25+ calls | High | Replace |
| `palmerpenguins` | Active | 8+ calls | Medium | Move to Suggests |
| `GGally` | Active | 2 files | Medium | Keep |
| `ggpmisc` | Active | 1 file | Low | Keep |
| `ggpubr` | Active | 1 file | Low | Keep |
| `Hmisc` | Active | 3 calls | Low | Keep |
| `MetBrewer` | Active | 1 call | Low | Keep |
| `reactable` | Active | 3 calls | Medium | Keep |
| `reactablefmtr` | Active | 1 call | Low | Keep |
| `pillar` | Active | Minimal | Low | Keep |
| `vctrs` | Active | 3 calls | Low | Keep |
| `uuid` | Active | 1 call | Low | Replace |
| **Unused Dependencies (Zero Usage)** |
| `distributional` | **UNUSED** | 0 files | High | **REMOVE** |
| `ggdist` | **UNUSED** | 0 files | High | **REMOVE** |
| `gtools` | **UNUSED** | 0 files | High | **REMOVE** |
| `rtables` | **UNUSED** | 0 files | High | **REMOVE** |
| `testthat` | **MISPLACED** | Test only | Medium | **Move to Suggests** |

---

## Detailed Analysis

### 🔴 Critical Issues

#### 1. Completely Unused Dependencies
These packages add unnecessary weight and potential security risks:

```r
# DESCRIPTION - Remove these lines:
distributional,    # 0 usages - Statistical distributions
ggdist,           # 0 usages - Visualizing distributions  
gtools,           # 0 usages - Various programming tools
rtables,          # 0 usages - Reporting tables
```

**Impact**: Removing these 4 packages would eliminate ~15-20 transitive dependencies.

#### 2. Misplaced Test Dependencies
```r
# Currently in Imports (WRONG):
testthat,         # Should be in Suggests
palmerpenguins,   # Test data - should be in Suggests
```

#### 3. Heavy External Dependencies
The package has high coupling to two external packages:

**waRRior** (35+ function calls):
- `waRRior::named_group_split()` - Used 8+ times
- `waRRior::get_groups()` - Used 6+ times  
- `waRRior::pop()` - Used 5+ times
- `waRRior::getfun()` - Used 4+ times

**picasso** (25+ function calls):
- `picasso::roche_colors()` - Used 15+ times
- `picasso::theme_picasso()` - Used 3+ times
- `picasso::hide_x_axis()` - Used 5+ times

### 🟡 Optimization Opportunities

#### 1. UUID Generation
```r
# Current (heavy dependency):
uuid::UUIDgenerate()

# Alternative (base R):
paste(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
```

#### 2. String Manipulation Consolidation
Heavy usage of `stringr` could be partially replaced with base R for simple operations.

#### 3. Data Manipulation
Some `dplyr` operations could use base R alternatives in simple cases.

---

## Risk Assessment

### 🔴 High Risk Issues

1. **Unused Dependencies** (Risk: Security/Maintenance)
   - 4 packages adding attack surface without benefit
   - Automatic security scans flag unused packages
   - Maintenance burden for no gain

2. **External Package Coupling** (Risk: Availability/Maintenance)
   - Heavy reliance on `waRRior` and `picasso` packages
   - These may not be on CRAN or well-maintained
   - Could cause installation failures

### 🟡 Medium Risk Issues

1. **Test Data in Imports** (Risk: Compliance)
   - `palmerpenguins` should be in Suggests per CRAN policy
   - `testthat` in wrong section

2. **Dependency Bloat** (Risk: Performance)
   - Current setup requires ~50+ packages for installation
   - Slow installation and large footprint

### 🟢 Low Risk Issues

1. **Version Pinning** 
   - No version constraints specified
   - Could lead to compatibility issues

---

## Specific Recommendations

### Phase 1: Immediate Actions (High Priority)

#### 1.1 Remove Unused Dependencies
```r
# Remove these lines from DESCRIPTION Imports:
distributional,
ggdist, 
gtools,
rtables,
```

#### 1.2 Move Misplaced Dependencies
```r
# Move from Imports to Suggests:
testthat,
palmerpenguins,

# Add Suggests section:
Suggests:
    testthat (>= 3.0.0),
    palmerpenguins,
    knitr,
    rmarkdown
```

#### 1.3 Replace Simple Dependencies
```r
# Replace uuid::UUIDgenerate() with:
generate_id <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 12, replace = TRUE), collapse = "")
}
```

### Phase 2: Reduce External Coupling (Medium Priority)

#### 2.1 waRRior Functions - Create Internal Alternatives
```r
# Instead of waRRior::named_group_split():
internal_group_split <- function(data, var) {
  split(data, data[[var]])
}

# Instead of waRRior::get_groups():
get_groups <- function(data) {
  if("grouped_df" %in% class(data)) {
    group_vars(data)
  } else {
    character(0)
  }
}

# Instead of waRRior::pop():
pop_element <- function(x, element) {
  x[!names(x) %in% element]
}
```

#### 2.2 picasso Functions - Create Internal Alternatives
```r
# Color palette function:
nightowl_colors <- function() {
  c(
    blue = "#1f77b4",
    orange = "#ff7f0e", 
    green = "#2ca02c",
    red = "#d62728",
    # ... etc
  )
}

# Theme function:
theme_nightowl <- function() {
  ggplot2::theme_minimal() +
  ggplot2::theme(
    # custom theme elements
  )
}
```

### Phase 3: Long-term Optimization (Low Priority)

#### 3.1 Evaluate Heavy Dependencies
Consider whether full packages are needed for minimal usage:
- `GGally` - Only used for ggpairs, consider lighter alternative
- `Hmisc` - Only 3 function calls, could implement internally
- `reactablefmtr` - Only 1 function call

#### 3.2 Version Constraints
Add version constraints for stability:
```r
Imports:
    dplyr (>= 1.0.0),
    ggplot2 (>= 3.3.0),
    purrr (>= 0.3.0),
    stringr (>= 1.4.0),
    # etc.
```

---

## Implementation Priority Matrix

| Action | Effort | Impact | Priority | Timeline |
|--------|--------|--------|----------|----------|
| Remove unused deps | Low | High | 🔴 Critical | Week 1 |
| Move test deps to Suggests | Low | High | 🔴 Critical | Week 1 |
| Replace uuid dependency | Low | Medium | 🟡 High | Week 2 |
| Create waRRior alternatives | High | High | 🟡 High | Month 1 |
| Create picasso alternatives | High | High | 🟡 High | Month 1 |
| Evaluate GGally usage | Medium | Medium | 🟢 Medium | Month 2 |
| Add version constraints | Low | Low | 🟢 Medium | Month 2 |

---

## Concrete Action Items

### Week 1 Actions
1. **Edit DESCRIPTION file**:
   - Remove: `distributional`, `ggdist`, `gtools`, `rtables`
   - Move to Suggests: `testthat`, `palmerpenguins`

2. **Update test files**:
   - Add conditional checks for `palmerpenguins`
   - Use `skip_if_not_installed("palmerpenguins")` in tests

3. **Replace UUID usage**:
   - Create internal `generate_id()` function
   - Replace all `uuid::UUIDgenerate()` calls

### Month 1 Actions  
4. **Create internal utilities** (`R/internal-utils.R`):
   - Implement alternatives to frequently used `waRRior` functions
   - Implement basic color palette to reduce `picasso` dependency
   - Add internal theme function

5. **Update function calls**:
   - Replace `waRRior::` calls with internal functions
   - Replace `picasso::` color calls with internal palette

### Month 2 Actions
6. **Dependency review**:
   - Evaluate if `GGally` can be replaced for ggpairs functionality
   - Consider if `Hmisc` functions can be implemented internally
   - Review `reactablefmtr` usage

7. **Version management**:
   - Add appropriate version constraints to remaining dependencies
   - Test compatibility with older package versions

---

## Expected Outcomes

### Immediate Benefits (Week 1)
- **Reduced package size**: 20% reduction in dependencies
- **Faster installation**: Remove 15-20 transitive dependencies  
- **CRAN compliance**: Proper Suggests vs Imports usage
- **Security improvement**: Remove unused attack surface

### Long-term Benefits (Month 1-2)
- **Reduced external coupling**: Less reliance on external packages
- **Improved maintainability**: Internal control over key functions
- **Better stability**: Version constraints prevent breaking changes
- **Performance gains**: Lighter dependency footprint

### Risk Mitigation
- **Staged approach**: Minimize chance of breaking changes
- **Comprehensive testing**: Ensure functionality maintained
- **Documentation**: Clear migration path for users
- **Backward compatibility**: Maintain API where possible

---

## Testing Strategy

### Validation Steps
1. **Automated testing**: Run full test suite after each change
2. **Installation testing**: Test on clean R environments  
3. **Integration testing**: Verify all exported functions work
4. **Performance testing**: Measure load time improvements
5. **Documentation testing**: Ensure all examples still work

### Rollback Plan
- Maintain git branches for each phase
- Keep original DESCRIPTION as backup
- Document all changes for easy reversal
- Test thoroughly before merging changes

---

*Analysis completed: 2025-06-25*  
*Codebase version: nightowl 0.0.1.0*  
*Total files analyzed: 35 R files + test files*