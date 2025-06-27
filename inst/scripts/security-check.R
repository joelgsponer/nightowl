#!/usr/bin/env Rscript

#' Security Vulnerability Check Script
#' 
#' This script performs local security scanning of R package dependencies
#' using the oysteR package and provides formatted output.
#' 
#' Usage:
#'   Rscript inst/scripts/security-check.R
#'   
#' Or from R console:
#'   source("inst/scripts/security-check.R")

# Load required packages
if (!require("oysteR", quietly = TRUE)) {
  cat("Installing oysteR package...\n")
  install.packages("oysteR")
  library(oysteR)
}

if (!require("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
  library(jsonlite)
}

#' Perform security scan and generate report
security_scan <- function(output_dir = "security-output") {
  
  cat("🔒 nightowl Package Security Scanner\n")
  cat("====================================\n\n")
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize results
  scan_results <- list(
    timestamp = Sys.time(),
    total_vulnerabilities = 0,
    critical_vulnerabilities = 0,
    high_vulnerabilities = 0,
    medium_vulnerabilities = 0,
    description_scan = NULL,
    installed_scan = NULL
  )
  
  cat("📋 Scanning DESCRIPTION file dependencies...\n")
  
  # Scan dependencies from DESCRIPTION file
  description_audit <- tryCatch({
    oysteR::audit_description()
  }, error = function(e) {
    cat("⚠️  Error scanning DESCRIPTION dependencies:", e$message, "\n")
    data.frame()
  })
  
  if (nrow(description_audit) > 0) {
    cat("❌ VULNERABILITIES FOUND IN DESCRIPTION DEPENDENCIES:\n")
    print(description_audit)
    cat("\n")
    
    scan_results$description_scan <- description_audit
    scan_results$total_vulnerabilities <- scan_results$total_vulnerabilities + nrow(description_audit)
    
    # Categorize by severity if CVSS scores available
    if ("cvss_score" %in% names(description_audit)) {
      scan_results$critical_vulnerabilities <- scan_results$critical_vulnerabilities + 
        sum(description_audit$cvss_score >= 9.0, na.rm = TRUE)
      scan_results$high_vulnerabilities <- scan_results$high_vulnerabilities + 
        sum(description_audit$cvss_score >= 7.0 & description_audit$cvss_score < 9.0, na.rm = TRUE)
      scan_results$medium_vulnerabilities <- scan_results$medium_vulnerabilities + 
        sum(description_audit$cvss_score >= 4.0 & description_audit$cvss_score < 7.0, na.rm = TRUE)
    }
    
    # Save detailed results
    write(toJSON(description_audit, pretty = TRUE), 
          file.path(output_dir, "description-vulnerabilities.json"))
    
  } else {
    cat("✅ No vulnerabilities found in DESCRIPTION dependencies\n\n")
  }
  
  cat("📦 Scanning currently installed packages...\n")
  
  # Scan currently installed packages
  installed_audit <- tryCatch({
    oysteR::audit_installed_r_pkgs()
  }, error = function(e) {
    cat("⚠️  Error scanning installed packages:", e$message, "\n")
    data.frame()
  })
  
  if (nrow(installed_audit) > 0) {
    cat("❌ VULNERABILITIES FOUND IN INSTALLED PACKAGES:\n")
    print(installed_audit)
    cat("\n")
    
    scan_results$installed_scan <- installed_audit
    scan_results$total_vulnerabilities <- scan_results$total_vulnerabilities + nrow(installed_audit)
    
    # Categorize by severity
    if ("cvss_score" %in% names(installed_audit)) {
      scan_results$critical_vulnerabilities <- scan_results$critical_vulnerabilities + 
        sum(installed_audit$cvss_score >= 9.0, na.rm = TRUE)
      scan_results$high_vulnerabilities <- scan_results$high_vulnerabilities + 
        sum(installed_audit$cvss_score >= 7.0 & installed_audit$cvss_score < 9.0, na.rm = TRUE)
      scan_results$medium_vulnerabilities <- scan_results$medium_vulnerabilities + 
        sum(installed_audit$cvss_score >= 4.0 & installed_audit$cvss_score < 7.0, na.rm = TRUE)
    }
    
    # Save detailed results
    write(toJSON(installed_audit, pretty = TRUE), 
          file.path(output_dir, "installed-vulnerabilities.json"))
    
  } else {
    cat("✅ No vulnerabilities found in installed packages\n\n")
  }
  
  # Generate summary report
  cat("📊 SECURITY SCAN SUMMARY\n")
  cat("========================\n")
  cat("Scan completed at:", format(scan_results$timestamp), "\n")
  cat("Total vulnerabilities found:", scan_results$total_vulnerabilities, "\n")
  cat("Critical (CVSS ≥ 9.0):", scan_results$critical_vulnerabilities, "\n")
  cat("High (CVSS 7.0-8.9):", scan_results$high_vulnerabilities, "\n")
  cat("Medium (CVSS 4.0-6.9):", scan_results$medium_vulnerabilities, "\n\n")
  
  # Generate recommendations
  if (scan_results$critical_vulnerabilities > 0) {
    cat("🚨 CRITICAL ACTION REQUIRED\n")
    cat("Critical vulnerabilities detected. Immediate action required:\n")
    cat("- Review critical vulnerabilities in security report\n")
    cat("- Update affected packages immediately\n")
    cat("- Consider alternative packages if updates unavailable\n\n")
  } else if (scan_results$high_vulnerabilities > 0) {
    cat("⚠️  HIGH PRIORITY UPDATES RECOMMENDED\n")
    cat("High severity vulnerabilities detected:\n")
    cat("- Plan package updates for next release\n")
    cat("- Monitor for security patches\n\n")
  } else if (scan_results$total_vulnerabilities > 0) {
    cat("ℹ️  MEDIUM PRIORITY UPDATES AVAILABLE\n")
    cat("Some vulnerabilities detected with medium severity\n")
    cat("- Consider updating packages when convenient\n")
    cat("- Monitor for security advisories\n\n")
  } else {
    cat("🎉 NO VULNERABILITIES DETECTED\n")
    cat("All scanned packages appear to be secure\n\n")
  }
  
  # Generate markdown report
  generate_markdown_report(scan_results, output_dir)
  
  # Save scan results as JSON
  write(toJSON(scan_results, pretty = TRUE), 
        file.path(output_dir, "scan-summary.json"))
  
  cat("📁 Results saved to:", output_dir, "\n")
  cat("📄 View detailed report:", file.path(output_dir, "security-report.md"), "\n")
  
  # Return results for programmatic access
  invisible(scan_results)
}

#' Generate markdown security report
generate_markdown_report <- function(scan_results, output_dir) {
  
  report_file <- file.path(output_dir, "security-report.md")
  
  report_content <- sprintf("# nightowl Package Security Report

## Scan Summary

**Scan Date:** %s  
**Total Vulnerabilities:** %d  
**Critical (CVSS ≥ 9.0):** %d  
**High (CVSS 7.0-8.9):** %d  
**Medium (CVSS 4.0-6.9):** %d  

## Risk Assessment

%s

## Detailed Results

### DESCRIPTION Dependencies
%s

### Installed Packages
%s

## Recommendations

1. **Immediate Actions (Critical)**
   - Address any critical vulnerabilities immediately
   - Consider package alternatives if no patches available

2. **Short-term Actions (High)**
   - Plan updates for high-severity vulnerabilities
   - Monitor security advisories

3. **Medium-term Actions (Medium)**
   - Schedule routine package updates
   - Implement dependency monitoring

## Security Tools Used

- **oysteR**: R package vulnerability scanner using OSS Index by Sonatype
- **Database**: OSS Index vulnerability catalog

## Next Steps

1. Review detailed vulnerability information in JSON files
2. Update affected packages using `update.packages()`
3. Re-run security scan to verify fixes
4. Consider implementing automated dependency monitoring

---
*Report generated by nightowl security scanner*
",
    format(scan_results$timestamp),
    scan_results$total_vulnerabilities,
    scan_results$critical_vulnerabilities,
    scan_results$high_vulnerabilities,
    scan_results$medium_vulnerabilities,
    if (scan_results$critical_vulnerabilities > 0) {
      "🚨 **CRITICAL**: Immediate action required"
    } else if (scan_results$high_vulnerabilities > 0) {
      "⚠️ **HIGH**: Action recommended"
    } else if (scan_results$total_vulnerabilities > 0) {
      "ℹ️ **MEDIUM**: Monitor and update when convenient"
    } else {
      "✅ **LOW**: No vulnerabilities detected"
    },
    if (!is.null(scan_results$description_scan) && nrow(scan_results$description_scan) > 0) {
      paste("Vulnerabilities found in", nrow(scan_results$description_scan), "DESCRIPTION dependencies")
    } else {
      "✅ No vulnerabilities found in DESCRIPTION dependencies"
    },
    if (!is.null(scan_results$installed_scan) && nrow(scan_results$installed_scan) > 0) {
      paste("Vulnerabilities found in", nrow(scan_results$installed_scan), "installed packages")
    } else {
      "✅ No vulnerabilities found in installed packages"
    }
  )
  
  writeLines(report_content, report_file)
}

# Run scan if script is executed directly
if (!interactive()) {
  security_scan()
}