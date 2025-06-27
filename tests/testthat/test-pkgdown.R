test_that("pkgdown configuration is valid", {
  # Test that _pkgdown.yml exists and is valid YAML (relative to package root)
  pkgdown_file <- file.path("..", "..", "_pkgdown.yml")
  if (!file.exists(pkgdown_file)) {
    pkgdown_file <- "_pkgdown.yml"  # fallback for different test environments
  }
  expect_true(file.exists(pkgdown_file))
  
  if (requireNamespace("yaml", quietly = TRUE)) {
    config <- yaml::read_yaml(pkgdown_file)
    
    # Test required configuration elements
    expect_true("url" %in% names(config))
    expect_true("template" %in% names(config))
    expect_true("home" %in% names(config))
    expect_true("navbar" %in% names(config))
    expect_true("reference" %in% names(config))
    
    # Test URL format
    expect_match(config$url, "^https://")
    expect_match(config$url, "github\\.io")
    
    # Test template configuration
    expect_equal(config$template$bootstrap, 5)
    
    # Test reference organization
    expect_true(length(config$reference) > 0)
    expect_true(all(sapply(config$reference, function(x) "title" %in% names(x))))
  }
})

test_that("pkgdown can build documentation", {
  skip_if_not_installed("pkgdown")
  skip_on_ci() # Skip on CI as it requires Pandoc and full build environment
  
  # Test that pkgdown can initialize without errors
  pkg_root <- if (file.exists("DESCRIPTION")) "." else file.path("..", "..")
  expect_no_error({
    pkg <- pkgdown::as_pkgdown(pkg_root)
  })
})

test_that("package metadata is pkgdown-ready", {
  desc_file <- if (file.exists("DESCRIPTION")) "DESCRIPTION" else file.path("..", "..", "DESCRIPTION")
  desc <- read.dcf(desc_file)
  
  # Test that required fields for pkgdown are present
  expect_true("URL" %in% colnames(desc))
  expect_true("BugReports" %in% colnames(desc))
  
  # Test URL format
  url <- desc[1, "URL"]
  expect_match(url, "^https://")
  expect_match(url, "github\\.io")
  
  # Test BugReports format
  bugs <- desc[1, "BugReports"]
  expect_match(bugs, "^https://github\\.com/")
  expect_match(bugs, "/issues$")
})

test_that("documentation files exist for pkgdown", {
  # Determine package root
  pkg_root <- if (file.exists("DESCRIPTION")) "." else file.path("..", "..")
  
  # Test that essential documentation files exist
  expect_true(file.exists(file.path(pkg_root, "README.md")))
  expect_true(file.exists(file.path(pkg_root, "DESCRIPTION")))
  expect_true(dir.exists(file.path(pkg_root, "man")))
  
  # Test that man directory has documentation files
  man_files <- list.files(file.path(pkg_root, "man"), pattern = "\\.Rd$")
  expect_true(length(man_files) > 0)
  
  # Test for package logo if it exists
  logo_path <- file.path(pkg_root, "man", "figures", "logo.png")
  if (file.exists(logo_path)) {
    expect_true(file.info(logo_path)$size > 0)
  }
})

test_that("pkgdown workflow file is valid", {
  pkg_root <- if (file.exists("DESCRIPTION")) "." else file.path("..", "..")
  workflow_file <- file.path(pkg_root, ".github", "workflows", "pkgdown.yml")
  expect_true(file.exists(workflow_file))
  
  if (requireNamespace("yaml", quietly = TRUE)) {
    workflow <- yaml::read_yaml(workflow_file)
    
    # Test workflow structure
    expect_true("name" %in% names(workflow))
    expect_true("TRUE" %in% names(workflow))  # "on" gets parsed as TRUE by yaml library
    expect_true("jobs" %in% names(workflow))
    
    # Test that required permissions are set
    expect_true("permissions" %in% names(workflow))
    expect_true("pages" %in% names(workflow$permissions))
    expect_true("id-token" %in% names(workflow$permissions))
    
    # Test that jobs include build and deploy
    expect_true("build" %in% names(workflow$jobs))
    expect_true("deploy" %in% names(workflow$jobs))
  }
})