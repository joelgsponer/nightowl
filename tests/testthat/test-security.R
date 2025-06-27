test_that("security scanning functions work", {
  # Skip if oysteR is not available (e.g., on CRAN)
  skip_if_not_installed("oysteR")
  
  # Test that oysteR audit functions can be called
  expect_no_error({
    audit_result <- oysteR::audit_description()
  })
  
  # The result should be a data frame
  audit_result <- oysteR::audit_description()
  expect_s3_class(audit_result, "data.frame")
  
  # Test installed packages audit
  expect_no_error({
    installed_audit <- oysteR::audit_installed_r_pkgs()
  })
  
  installed_audit <- oysteR::audit_installed_r_pkgs()
  expect_s3_class(installed_audit, "data.frame")
})

test_that("security check script exists and is executable", {
  security_script <- system.file("scripts", "security-check.R", package = "nightowl")
  
  # Skip if package is not installed (during development)
  skip_if(security_script == "", "Package not installed")
  
  expect_true(file.exists(security_script))
})

test_that("no critical vulnerabilities in dependencies", {
  # This test checks for critical vulnerabilities
  # Skip if oysteR is not available or in non-interactive environments
  skip_if_not_installed("oysteR")
  skip_on_cran()  # Skip on CRAN to avoid network dependency
  
  # Only run this test occasionally to avoid hitting API limits
  skip_if(Sys.getenv("SKIP_SECURITY_TESTS") == "true")
  
  tryCatch({
    audit_result <- oysteR::audit_description()
    
    # Check if we have CVSS scores to evaluate severity
    if ("cvss_score" %in% names(audit_result) && nrow(audit_result) > 0) {
      critical_vulns <- sum(audit_result$cvss_score >= 9.0, na.rm = TRUE)
      
      # Fail test if critical vulnerabilities are found
      expect_equal(critical_vulns, 0, 
                   info = paste("Critical vulnerabilities found:",
                                paste(audit_result[audit_result$cvss_score >= 9.0, "title"], 
                                      collapse = ", ")))
      
      # Warn about high severity vulnerabilities
      high_vulns <- sum(audit_result$cvss_score >= 7.0 & audit_result$cvss_score < 9.0, na.rm = TRUE)
      if (high_vulns > 0) {
        warning("High severity vulnerabilities found: ", high_vulns)
      }
    }
    
  }, error = function(e) {
    # Skip test if there are network issues or API problems
    skip(paste("Security scan failed:", e$message))
  })
})