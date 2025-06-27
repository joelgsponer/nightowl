# nightowl <img src="man/figures/logo.png" align="right" height="104" />

Statistical visualization and analysis for clinical research applications.

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
