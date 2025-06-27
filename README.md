# nightowl <img src="man/figures/logo.png" align="right" height="104" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/joelgsponer/nightowl/branch/dev/graph/badge.svg)](https://app.codecov.io/gh/joelgsponer/nightowl?branch=dev)
[![Security Scan Status](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/security-scan.yaml)
[![pkgdown](https://github.com/joelgsponer/nightowl/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/joelgsponer/nightowl/actions/workflows/pkgdown.yml)
<!-- badges: end -->

Statistical visualization and analysis for clinical research applications.

## Documentation

Complete documentation is available at: **https://joelgsponer.github.io/nightowl/**

The documentation site includes:
- **Function reference** with examples and usage details
- **Getting started guide** for new users
- **Statistical summaries** and analysis workflows  
- **Survival analysis** tutorials and examples

## Installation

```r
remotes::install_github(
  repo = "federerj/nightowl",
  host = "https://github.roche.com/api/v3",
  update = "never"
)
```

## Security

This package includes comprehensive security vulnerability scanning for R dependencies:

- **Automated scanning** via GitHub Actions on every push and weekly schedules
- **Local security checks** using `source("inst/scripts/security-check.R")`
- **CI/CD integration** that fails builds on critical vulnerabilities
- **Multiple scanning tools**: oysteR, OSV Scanner, and riskmetric

See [`inst/SECURITY.md`](inst/SECURITY.md) for detailed security policy and scanning procedures.
