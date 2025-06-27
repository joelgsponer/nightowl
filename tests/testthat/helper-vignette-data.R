# Helper functions and shared data for vignette testing

# Create standardized test datasets that mirror vignette examples
create_vignette_test_data <- function() {
  # Standard clinical trial dataset for survival analysis
  clinical_survival_test <- data.frame(
    patient_id = 1:50,  # Smaller for testing
    time = rexp(50, rate = 0.02),
    event = rbinom(50, 1, 0.7),
    treatment = sample(c("Control", "Treatment A", "Treatment B"), 50, replace = TRUE),
    age = round(rnorm(50, 65, 12)),
    sex = sample(c("Male", "Female"), 50, replace = TRUE),
    stage = sample(c("I", "II", "III", "IV"), 50, replace = TRUE),
    biomarker_high = sample(c("High", "Low"), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Standard statistical summary dataset
  summary_test_data <- data.frame(
    group = sample(c("A", "B", "C"), 100, replace = TRUE),
    numeric_var = rnorm(100, 50, 15),
    categorical_var = sample(c("Yes", "No", "Maybe"), 100, replace = TRUE),
    binary_var = sample(c("Yes", "No"), 100, replace = TRUE),
    continuous_score = runif(100, 0, 100),
    stringsAsFactors = FALSE
  )
  
  # Standard plotting dataset (ChickWeight subset)
  plotting_test_data <- ChickWeight[ChickWeight$Time < 10, ]
  
  list(
    clinical_survival = clinical_survival_test,
    summary_data = summary_test_data,
    plotting_data = plotting_test_data
  )
}

# Check if optional packages are available for testing
check_optional_packages <- function() {
  optional_pkgs <- c("palmerpenguins", "survival", "patchwork")
  available_pkgs <- sapply(optional_pkgs, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })
  return(available_pkgs)
}

# Create mock penguins data if palmerpenguins is not available
create_mock_penguins <- function() {
  if (requireNamespace("palmerpenguins", quietly = TRUE)) {
    return(palmerpenguins::penguins)
  } else {
    # Create synthetic penguins-like data
    mock_penguins <- data.frame(
      species = sample(c("Adelie", "Chinstrap", "Gentoo"), 100, replace = TRUE),
      island = sample(c("Biscoe", "Dream", "Torgersen"), 100, replace = TRUE),
      bill_length_mm = rnorm(100, 44, 6),
      bill_depth_mm = rnorm(100, 17, 2),
      flipper_length_mm = rnorm(100, 200, 15),
      body_mass_g = rnorm(100, 4200, 800),
      sex = sample(c("male", "female"), 100, replace = TRUE),
      stringsAsFactors = FALSE
    )
    return(mock_penguins)
  }
}

# Validate vignette code execution environment
setup_vignette_test_environment <- function() {
  # Ensure required packages are loaded
  required_packages <- c("nightowl", "dplyr", "ggplot2")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      skip(paste("Required package", pkg, "not available for vignette testing"))
    }
  }
  
  # Set up test data
  test_data <- create_vignette_test_data()
  
  # Set global options for testing
  old_options <- options(
    warn = 1,  # Show warnings immediately
    stringsAsFactors = FALSE
  )
  
  # Return cleanup function
  return(function() {
    options(old_options)
  })
}

# Extract and validate R code from vignette chunks
extract_vignette_code <- function(vignette_path) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    skip("knitr package required for vignette code extraction")
  }
  
  # Create temporary file for extracted code
  temp_file <- tempfile(fileext = ".R")
  
  # Extract code
  knitr::purl(vignette_path, output = temp_file, quiet = TRUE)
  
  if (!file.exists(temp_file)) {
    return(NULL)
  }
  
  # Read and return code
  code_lines <- readLines(temp_file)
  unlink(temp_file)
  
  return(code_lines)
}

# Test helper for checking plot objects
validate_nightowl_plot <- function(plot_obj, expected_class = "Plot") {
  expect_true(inherits(plot_obj, expected_class))
  
  if (expected_class == "Plot") {
    # Validate Plot R6 class
    expect_true(is.function(plot_obj$svg))
    expect_true(is.function(plot_obj$html))
  }
}

# Test helper for checking summary objects  
validate_nightowl_summary <- function(summary_obj) {
  expect_true(inherits(summary_obj, "Summary"))
  expect_true(is.function(summary_obj$reactable))
  expect_true(is.function(summary_obj$html))
  expect_true(is.function(summary_obj$raw))
}

# Test helper for checking survival objects
validate_nightowl_coxph <- function(coxph_obj) {
  expect_true(inherits(coxph_obj, "Coxph"))
  expect_true(!is.null(coxph_obj$data))
  expect_true(!is.null(coxph_obj$time))
  expect_true(!is.null(coxph_obj$event))
}

# Performance testing helper
time_vignette_execution <- function(vignette_path, max_time_seconds = 30) {
  start_time <- Sys.time()
  
  # Extract and execute code
  code_lines <- extract_vignette_code(vignette_path)
  
  if (is.null(code_lines) || length(code_lines) == 0) {
    return(list(success = TRUE, time = 0, message = "No code to execute"))
  }
  
  # Execute in isolated environment
  test_env <- new.env()
  eval(quote(library(nightowl)), envir = test_env)
  eval(quote(library(dplyr)), envir = test_env)
  eval(quote(library(ggplot2)), envir = test_env)
  
  tryCatch({
    # Execute code line by line with timeout check
    for (i in seq_along(code_lines)) {
      if (as.numeric(Sys.time() - start_time) > max_time_seconds) {
        return(list(
          success = FALSE, 
          time = as.numeric(Sys.time() - start_time),
          message = paste("Execution exceeded", max_time_seconds, "seconds at line", i)
        ))
      }
      
      eval(parse(text = code_lines[i]), envir = test_env)
    }
    
    end_time <- Sys.time()
    return(list(
      success = TRUE,
      time = as.numeric(end_time - start_time),
      message = "All code executed successfully"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      time = as.numeric(Sys.time() - start_time),
      message = paste("Error:", e$message)
    ))
  })
}

# Cleanup function for test artifacts
cleanup_vignette_test_artifacts <- function() {
  # Remove any temporary files created during testing
  temp_files <- list.files(tempdir(), pattern = "vignette.*\\.(html|pdf|R)$", full.names = TRUE)
  unlink(temp_files)
  
  # Clear any large objects from global environment
  rm(list = ls(pattern = "test_.*", envir = .GlobalEnv), envir = .GlobalEnv)
  
  # Force garbage collection
  gc()
}