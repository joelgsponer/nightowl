# Security Policy

## Security Vulnerability Scanning

The `nightowl` package implements comprehensive security scanning for R dependencies to identify and address potential vulnerabilities.

### Automated Security Scanning

#### GitHub Actions Workflow

The package includes an automated security scanning workflow (`.github/workflows/security-scan.yaml`) that:

- **Runs on every push and pull request** to main branches
- **Performs weekly scheduled scans** every Monday at 2 AM UTC
- **Uses multiple security scanning tools**:
  - **oysteR**: R-specific vulnerability scanner using OSS Index
  - **OSV Scanner**: Cross-language vulnerability scanner using OSV.dev database
  - **riskmetric**: R package quality assessment
- **Fails builds on critical vulnerabilities** (CVSS ≥ 9.0)
- **Generates detailed security reports** as workflow artifacts
- **Comments on pull requests** with security scan results

#### Manual Security Scanning

For local development and testing, use the included security check script:

```r
# Run security scan
source("inst/scripts/security-check.R")

# Results will be saved to security-output/ directory
```

The script generates:
- Console output with vulnerability summary
- `security-report.md` - Human-readable report
- `scan-summary.json` - Machine-readable results
- `*-vulnerabilities.json` - Detailed vulnerability data

### Security Testing

Security tests are integrated into the package test suite:

```r
# Run all tests including security checks
devtools::test()

# Run only security tests
devtools::test(filter = "security")
```

**Note**: Security tests are skipped on CRAN and can be disabled by setting `SKIP_SECURITY_TESTS=true`.

### Vulnerability Severity Levels

| Severity | CVSS Score | Action Required |
|----------|------------|----------------|
| **Critical** | ≥ 9.0 | Immediate action - blocks CI/CD |
| **High** | 7.0 - 8.9 | Address in next release |
| **Medium** | 4.0 - 6.9 | Monitor and update when convenient |
| **Low** | < 4.0 | Optional updates |

### Security Tools and Databases

#### oysteR
- **Purpose**: R-specific vulnerability scanning
- **Database**: OSS Index by Sonatype
- **Coverage**: R packages and their dependencies
- **Installation**: `install.packages("oysteR")`

#### OSV Scanner
- **Purpose**: Cross-language vulnerability detection
- **Database**: OSV.dev (aggregates GitHub Security Advisories, etc.)
- **Coverage**: 11+ programming languages including R
- **GitHub Action**: `google/osv-scanner-action`

#### riskmetric
- **Purpose**: R package quality assessment
- **Metrics**: Community usage, test coverage, documentation
- **Installation**: `install.packages("riskmetric")`

### Reporting Security Issues

If you discover a security vulnerability in the `nightowl` package itself:

1. **Do not open a public issue**
2. Email the maintainers privately with details
3. Allow time for assessment and patching before public disclosure

### Dependency Management

#### Monitoring Dependencies

- **GitHub Dependabot**: Not currently supported for R packages
- **Manual monitoring**: Use oysteR regularly
- **Automated alerts**: Configured via GitHub Actions

#### Updating Dependencies

```r
# Check for package updates
old.packages()

# Update specific packages
update.packages("package_name")

# Update all packages
update.packages()

# Re-run security scan after updates
source("inst/scripts/security-check.R")
```

### Best Practices

1. **Regular Scanning**
   - Run security scans before releases
   - Monitor weekly automated scan results
   - Address critical vulnerabilities immediately

2. **Dependency Hygiene**
   - Minimize dependencies where possible
   - Keep dependencies up to date
   - Review new dependencies for security issues

3. **Development Process**
   - Security checks are part of CI/CD pipeline
   - Pull requests include security scan results
   - Critical vulnerabilities block merges

4. **Documentation**
   - Security scan results are archived as artifacts
   - Vulnerability reports include remediation guidance
   - Security policy is kept up to date

### Limitations

1. **R Ecosystem Coverage**
   - R vulnerabilities are less commonly reported than other languages
   - Many R package vulnerabilities involve embedded JavaScript libraries
   - Security databases may have limited R-specific entries

2. **Network Dependencies**
   - Security scans require internet access
   - May be rate-limited by vulnerability databases
   - Could fail in restricted network environments

3. **False Positives**
   - Vulnerability scanners may report false positives
   - Manual review may be required for some findings
   - Context-specific risk assessment needed

### Resources

- [OSS Index by Sonatype](https://ossindex.sonatype.org/)
- [OSV.dev Vulnerability Database](https://osv.dev/)
- [GitHub Security Features](https://docs.github.com/en/code-security)
- [R Package Security Best Practices](https://r-pkgs.org/security.html)

### Contact

For security-related questions or concerns, contact the package maintainers through the appropriate channels listed in the DESCRIPTION file.