# nightowl <img src="man/figures/logo.png" align="right" height="104" />

> Statistical visualization and analysis toolkit for clinical research and data science

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml)

## Overview

nightowl is a comprehensive R package designed for statistical visualization and analysis, with a particular focus on clinical research applications. Built on ggplot2 with extensive customization capabilities, nightowl provides an intuitive interface for creating publication-ready plots, conducting statistical analyses, and generating interactive visualizations.

### Key Features

- **🌲 Forest Plots**: Specialized forest plot functions with confidence intervals for meta-analyses
- **📊 Statistical Visualization**: Comprehensive plotting system including boxplots, violin plots, donut charts, and more
- **⏱️ Survival Analysis**: Cox proportional hazards models and Kaplan-Meier survival curves  
- **🎯 Interactive Elements**: SVG rendering with interactive capabilities and hover effects
- **📋 Smart Tables**: Reactable-based interactive tables with built-in styling
- **🚀 Performance Optimized**: Built-in caching system for large dataset handling
- **🎨 Declarative Plotting**: R6 class-based architecture for consistent, reproducible visualizations
- **📈 Statistical Testing**: Integrated statistical tests (chi-square, t-tests, Kruskal-Wallis)

## Installation

### Development Version (Recommended)

Install the latest development version from GitHub:

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("joelgsponer/nightowl")
```

### Dependencies

nightowl builds on the excellent work of many R packages. Key dependencies include:

- Core: `ggplot2`, `dplyr`, `magrittr`
- Visualization: `ggpubr`, `patchwork`, `reactable`
- Statistics: `survival`, `broom`
- Interactivity: `ggiraph`, `htmltools`

## Quick Start

### Basic Forest Plot

```r
library(nightowl)

# Create a simple forest plot
forestplot(estimate = 0.5, lower = 0.2, upper = 0.8)
```

### Statistical Summary with Visualization

```r
library(dplyr)

# Create grouped summary with visualization
mtcars %>%
  Summary$new(x = "mpg", by = "cyl", method = summarise_numeric_pointrange) %>%
  reactable()
```

### Survival Analysis

```r
# Cox proportional hazards model with forest plot
data %>%
  plot_coxph(
    time = "time",
    event = "event", 
    treatment = "treatment",
    covariates = c("age", "stage")
  )
```

### Declarative Plotting

```r
# Advanced plotting with R6 classes
plot_obj <- nightowl::plot(
  data = iris,
  mapping = list(x = "Species", y = "Sepal.Length", color = "Species"),
  layers = list(
    list(type = "boxplot"),
    list(type = "generic", geom = "ggplot2::geom_jitter", alpha = 0.6)
  )
)

# Convert to different formats
plot_obj %>% as_ggplot()  # ggplot2 object
plot_obj %>% as_html()    # Interactive HTML
```

## Documentation

### Getting Started

- **[Getting Started Vignette](articles/getting-started.html)**: Learn the basics of nightowl
- **[Statistical Visualization Guide](articles/statistical-visualization.html)**: Comprehensive plotting tutorial
- **[Advanced Features](articles/advanced-features.html)**: R6 classes and customization
- **[Performance Tips](articles/performance-optimization.html)**: Optimizing for large datasets

### Function Reference

Browse all available functions in the [Reference](reference/index.html) section, organized by category:

- **Core Plotting**: `forestplot()`, `donut_plot()`, `plot()`
- **Statistical Analysis**: `fit_coxph()`, `plot_km()`, `grouped_chisq()`
- **Summaries**: `Summary$new()`, `summarise_*()` functions
- **Tables**: `render_reactable()`, `render_kable()`
- **R6 Classes**: `Plot`, `Summary`, `Coxph`

## Examples

### Clinical Research Workflow

```r
library(nightowl)
library(dplyr)

# Analyze clinical trial data
clinical_data %>%
  # Create survival analysis
  plot_km(time = "survival_time", event = "death", treatment = "arm") %>%
  
  # Add risk tables
  km_table() %>%
  
  # Render as interactive plot
  as_html()
```

### Publication-Ready Forest Plot

```r
# Multi-study meta-analysis visualization
studies_data %>%
  group_by(study) %>%
  summarise(
    estimate = mean(effect),
    lower = quantile(effect, 0.025),
    upper = quantile(effect, 0.975)
  ) %>%
  forestplot(estimate = estimate, lower = lower, upper = upper) +
  theme_minimal() +
  labs(title = "Treatment Effect Across Studies")
```

## Performance Features

nightowl includes several performance optimizations:

- **Intelligent Caching**: Automatic plot caching for repeated operations
- **Memory Efficiency**: Optimized data structures for large datasets
- **Parallel Processing**: Support for multi-core operations where applicable

```r
# Enable performance monitoring
options(nightowl.cache = TRUE)
options(nightowl.performance_tracking = TRUE)
```

## Integration

nightowl works seamlessly with the tidyverse and other popular R packages:

```r
library(tidyverse)
library(nightowl)

# Tidyverse integration
iris %>%
  group_by(Species) %>%
  summarise(
    mean_length = mean(Sepal.Length),
    plots = list(forestplot(mean_length, mean_length - sd, mean_length + sd))
  ) %>%
  pull(plots) %>%
  patchwork::wrap_plots()
```

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development Setup

```r
# Clone the repository
git clone https://github.com/joelgsponer/nightowl.git
cd nightowl

# Install development dependencies
devtools::install_dev_deps()

# Run tests
devtools::test()

# Build documentation
devtools::document()
pkgdown::build_site()
```

## Getting Help

- **Bug Reports**: [GitHub Issues](https://github.com/joelgsponer/nightowl/issues)
- **Feature Requests**: [GitHub Discussions](https://github.com/joelgsponer/nightowl/discussions)
- **Documentation**: [Package Website](https://joelgsponer.github.io/nightowl/)

## Citation

If you use nightowl in your research, please cite:

```r
citation("nightowl")
```

## License

MIT License. See [LICENSE.md](LICENSE.md) for details.

---

**nightowl** - Making statistical visualization intuitive and beautiful 🦉✨