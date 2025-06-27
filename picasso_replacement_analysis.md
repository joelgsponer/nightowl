# Picasso Package Replacement Analysis for nightowl

## Summary of Picasso Usage

After analyzing the nightowl codebase, I found that picasso is used for the following functions:

### 1. Color Functions
- `picasso::roche_colors()` - Returns Roche brand colors
- `picasso::roche_color()` - Returns a single Roche brand color
- `picasso::colors_ibm()` - IBM color palette (not found in usage)
- `picasso::roche_palette_discrete()` - Discrete color palette for ggplot2

### 2. Theme Functions  
- `picasso::theme_picasso()` - Custom ggplot2 theme
- `picasso::theme_void()` - Void theme (minimal theme)

### 3. Plot Modification Functions
- `picasso::hide_legend()` - Removes legend from plot
- `picasso::hide_title()` - Removes title from plot  
- `picasso::hide_x_axis()` - Removes x-axis from plot
- `picasso::hide_y_axis()` - Removes y-axis from plot
- `picasso::add_x_axis()` - Adds x-axis to plot

### 4. Utility Functions
- `picasso::is_dark()` - Checks if a color is dark

## Usage by File

1. **R/options.R**: Uses `picasso::roche_colors()` for default color initialization
2. **R/donut_plot.R**: Uses `picasso::is_dark()` and `picasso::hide_x_axis()`
3. **R/km.R**: Uses `picasso::roche_colors()` for default colors
4. **R/forest.R**: Extensive use of `picasso::roche_colors()` and axis hiding functions
5. **R/coxph.R**: Uses `picasso::roche_colors()` for plot colors
6. **R/add_plots.R**: Uses `picasso::roche_palette_discrete()`
7. **R/inline_plots.R**: Extensive use of colors and axis hiding functions
8. **R/correlation_matrix.R**: Uses `picasso::theme_picasso()` and `picasso::roche_color()`
9. **R/summaries.R**: Uses `picasso::theme_void()` and `picasso::is_dark()`

## Replacement Strategies

### 1. Color Functions Replacement

#### Option A: Create Internal Color Palette
```r
# R/colors.R
nightowl_colors <- function() {
  c(
    blue = "#007DBA",      # Roche blue
    red = "#E40046",       # Roche red  
    green = "#00847A",     # Roche green
    black = "#000000",
    grey = "#BEBEBE",
    lightblue = "#B3D9F2",
    apple = "#8FD14F"      # Apple green
  )
}

nightowl_color <- function(name, alpha = 1) {
  colors <- nightowl_colors()
  if (!name %in% names(colors)) {
    stop(paste("Color", name, "not found"))
  }
  col <- colors[[name]]
  if (alpha < 1) {
    rgb_val <- col2rgb(col)
    col <- rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha * 255, maxColorValue = 255)
  }
  col
}

nightowl_palette_discrete <- function(n = 1) {
  colors <- nightowl_colors()
  function(n) {
    if (n <= length(colors)) {
      unname(colors[1:n])
    } else {
      # Use colorRampPalette to generate more colors if needed
      colorRampPalette(unname(colors))(n)
    }
  }
}
```

#### Option B: Use Existing Package (RColorBrewer/viridis)
```r
# Use RColorBrewer which is already available
nightowl_colors <- function() {
  # Use a professional palette from RColorBrewer
  colors <- RColorBrewer::brewer.pal(9, "Set1")
  names(colors) <- c("blue", "red", "green", "purple", "orange", 
                     "yellow", "brown", "pink", "grey")
  colors["black"] <- "#000000"
  colors["lightblue"] <- "#B3D9F2"
  colors["apple"] <- "#8FD14F"
  colors
}
```

### 2. Theme Functions Replacement

```r
# R/themes.R
theme_nightowl <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      axis.ticks = ggplot2::element_line(color = "grey20"),
      axis.text = ggplot2::element_text(color = "grey20"),
      axis.title = ggplot2::element_text(color = "grey20"),
      legend.position = "top"
    )
}

theme_nightowl_void <- function() {
  ggplot2::theme_void()
}
```

### 3. Plot Modification Functions Replacement

```r
# R/plot_helpers.R
hide_legend <- function() {
  ggplot2::theme(legend.position = "none")
}

hide_title <- function() {
  ggplot2::theme(plot.title = ggplot2::element_blank())
}

hide_x_axis <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank()
  )
}

hide_y_axis <- function() {
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank()
  )
}

add_x_axis <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(),
    axis.title.x = ggplot2::element_text(),
    axis.ticks.x = ggplot2::element_line(),
    axis.line.x = ggplot2::element_line()
  )
}
```

### 4. Utility Functions Replacement

```r
# R/color_utils.R
is_dark <- function(color) {
  # Convert color to RGB
  rgb_val <- col2rgb(color)
  # Calculate luminance using standard formula
  luminance <- (0.299 * rgb_val[1] + 0.587 * rgb_val[2] + 0.114 * rgb_val[3]) / 255
  return(luminance < 0.5)
}
```

## Implementation Plan

1. **Phase 1: Create Internal Functions**
   - Create `R/colors.R` with color definitions
   - Create `R/themes.R` with theme functions
   - Create `R/plot_helpers.R` with axis/legend helpers
   - Create `R/color_utils.R` with utility functions

2. **Phase 2: Update Code**
   - Replace all `picasso::roche_colors()` with `nightowl_colors()`
   - Replace all `picasso::roche_color()` with `nightowl_color()`
   - Replace all `picasso::theme_*` with `theme_nightowl*`
   - Replace all `picasso::hide_*` with `hide_*`
   - Replace all `picasso::is_dark()` with `is_dark()`

3. **Phase 3: Testing**
   - Update tests to use new functions
   - Ensure visual output matches expectations
   - Test backward compatibility if needed

## Dependencies Analysis

The replacement would NOT require any new dependencies because:
- Color manipulation: Base R functions (`col2rgb`, `rgb`) are sufficient
- Themes: `ggplot2` (already imported) provides all needed functionality
- Color palettes: `RColorBrewer` or `MetBrewer` (already imported) can be used
- Utility functions: Can be implemented with base R

## Backward Compatibility Considerations

1. **Color Values**: If exact Roche brand colors are important, they should be preserved
2. **Function Names**: Consider creating aliases for smooth transition
3. **API Compatibility**: Maintain same function signatures where possible

## Recommended Approach

1. **Use Option A** for colors (internal palette) to maintain brand consistency
2. **Implement all helper functions** internally for full control
3. **Add deprecation warnings** if maintaining picasso functions temporarily
4. **Document color choices** for future reference

## Example Migration

```r
# Before
plot + picasso::hide_x_axis() + 
  scale_color_manual(values = picasso::roche_colors())

# After  
plot + hide_x_axis() + 
  scale_color_manual(values = nightowl_colors())
```

## Benefits of Removing Picasso

1. **Reduced Dependencies**: One less external package to maintain
2. **Better Control**: Full control over color schemes and themes
3. **Easier Installation**: No need to install picasso (which might not be on CRAN)
4. **Customization**: Easier to adapt colors/themes for different use cases
5. **Portability**: Package becomes more self-contained

## Risks and Mitigation

1. **Risk**: Breaking existing user code that expects picasso functions
   - **Mitigation**: Provide transition period with both APIs

2. **Risk**: Color consistency across different outputs
   - **Mitigation**: Thoroughly document and test color values

3. **Risk**: Missing some picasso functionality
   - **Mitigation**: Comprehensive testing of all replaced functions