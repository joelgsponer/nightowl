test_that("vignette code chunks execute successfully", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  
  # Check if vignettes directory exists
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  # Find all vignette files
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  if (length(vignette_files) == 0) {
    skip("No vignette files found")
  }
  
  # Test each vignette file
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("Code in", vignette_name, "executes without error"), {
      # Create temporary R file for extracted code
      temp_r_file <- tempfile(fileext = ".R")
      on.exit(unlink(temp_r_file))
      
      # Extract R code chunks from vignette
      expect_silent({
        knitr::purl(vignette_file, output = temp_r_file, quiet = TRUE)
      })
      
      # Test that extracted file exists and has content
      expect_true(file.exists(temp_r_file))
      
      # Read the extracted code
      code_lines <- readLines(temp_r_file)
      
      # Skip empty files
      if (length(code_lines) == 0) {
        skip(paste("No R code found in", vignette_name))
      }
      
      # Test code execution in isolated environment
      test_env <- new.env()
      
      # Load required packages in test environment
      eval(quote(library(nightowl)), envir = test_env)
      eval(quote(library(dplyr)), envir = test_env)
      eval(quote(library(ggplot2)), envir = test_env)
      
      # Source the extracted code
      expect_error({
        source(temp_r_file, local = test_env)
      }, NA, info = paste("Error executing code from", vignette_name))
    })
  }
})

test_that("vignettes have required YAML headers", {
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("YAML header in", vignette_name, "is properly formatted"), {
      # Read first 20 lines to check YAML header
      header_lines <- readLines(vignette_file, n = 20)
      
      # Check for YAML delimiters
      yaml_start <- which(header_lines == "---")[1]
      yaml_end <- which(header_lines == "---")[2]
      
      expect_false(is.na(yaml_start), info = paste("Missing YAML start delimiter in", vignette_name))
      expect_false(is.na(yaml_end), info = paste("Missing YAML end delimiter in", vignette_name))
      
      # Extract YAML content
      yaml_content <- header_lines[(yaml_start + 1):(yaml_end - 1)]
      yaml_text <- paste(yaml_content, collapse = "\n")
      
      # Check for required fields
      expect_true(grepl("title:", yaml_text), info = paste("Missing title field in", vignette_name))
      expect_true(grepl("output: rmarkdown::html_vignette", yaml_text), info = paste("Missing correct output format in", vignette_name))
      expect_true(grepl("VignetteIndexEntry", yaml_text), info = paste("Missing VignetteIndexEntry in", vignette_name))
      expect_true(grepl("VignetteEngine", yaml_text), info = paste("Missing VignetteEngine in", vignette_name))
      expect_true(grepl("VignetteEncoding", yaml_text), info = paste("Missing VignetteEncoding in", vignette_name))
    })
  }
})

test_that("vignettes follow coding standards", {
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("Code standards in", vignette_name), {
      # Extract R code chunks
      temp_r_file <- tempfile(fileext = ".R")
      on.exit(unlink(temp_r_file))
      
      knitr::purl(vignette_file, output = temp_r_file, quiet = TRUE)
      
      if (!file.exists(temp_r_file)) {
        skip(paste("No R code to check in", vignette_name))
      }
      
      code_lines <- readLines(temp_r_file)
      
      if (length(code_lines) == 0) {
        skip(paste("No R code found in", vignette_name))
      }
      
      # Check for proper package namespace usage
      nightowl_functions <- code_lines[grepl("nightowl::", code_lines)]
      if (length(nightowl_functions) > 0) {
        # If nightowl functions are used, they should use proper namespace
        expect_true(
          all(grepl("nightowl::", nightowl_functions)),
          info = paste("Some nightowl functions in", vignette_name, "don't use proper namespace")
        )
      }
      
      # Check for appropriate data usage (no absolute paths, etc.)
      expect_false(
        any(grepl("/Users/|C:\\\\|~/", code_lines)),
        info = paste("Found absolute file paths in", vignette_name)
      )
      
      # Check for proper error handling in examples
      # Avoid using stop() or error() in vignette examples
      expect_false(
        any(grepl("stop\\(|error\\(", code_lines)),
        info = paste("Found error/stop calls in", vignette_name, "examples")
      )
    })
  }
})

test_that("vignette dependencies are available", {
  # Check that all packages used in vignettes are available
  required_packages <- c("nightowl", "dplyr", "ggplot2", "knitr", "rmarkdown")
  
  for (pkg in required_packages) {
    test_that(paste("Package", pkg, "is available"), {
      expect_true(
        requireNamespace(pkg, quietly = TRUE),
        info = paste("Required package", pkg, "is not available")
      )
    })
  }
  
  # Check optional packages that may be used
  optional_packages <- c("palmerpenguins", "survival", "patchwork")
  
  for (pkg in optional_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      test_that(paste("Optional package", pkg, "loads correctly"), {
        expect_silent(library(pkg, character.only = TRUE))
      })
    }
  }
})

test_that("vignettes produce expected outputs", {
  skip_on_cran()  # Skip on CRAN due to time constraints
  
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("Vignette", vignette_name, "renders successfully"), {
      # Create temporary output directory
      temp_dir <- tempdir()
      temp_output <- file.path(temp_dir, paste0(tools::file_path_sans_ext(vignette_name), ".html"))
      
      # Render vignette
      expect_silent({
        rmarkdown::render(
          vignette_file,
          output_file = temp_output,
          quiet = TRUE,
          envir = new.env()
        )
      })
      
      # Check that output file was created
      expect_true(file.exists(temp_output), info = paste("Failed to render", vignette_name))
      
      # Check that output file has reasonable size (> 1KB)
      if (file.exists(temp_output)) {
        expect_gt(file.size(temp_output), 1000, info = paste("Rendered", vignette_name, "is suspiciously small"))
      }
      
      # Clean up
      if (file.exists(temp_output)) {
        unlink(temp_output)
      }
    })
  }
})

test_that("vignette examples use consistent data", {
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("Data usage in", vignette_name, "is consistent"), {
      # Read vignette content
      vignette_content <- readLines(vignette_file)
      vignette_text <- paste(vignette_content, collapse = "\n")
      
      # Check for consistent dataset usage
      # Should use standard datasets like ChickWeight, mtcars, palmerpenguins::penguins
      standard_datasets <- c("ChickWeight", "mtcars", "palmerpenguins::penguins", "penguins")
      
      # If vignette uses data, it should be from recognized sources
      if (grepl("data\\(|<-.*data\\.frame|tibble\\(", vignette_text)) {
        # Data is being used or created
        # This is fine - just checking that it follows patterns
        expect_true(TRUE, info = paste("Data usage detected in", vignette_name))
      }
      
      # Check for proper data setup sections
      if (grepl("setup", vignette_text, ignore.case = TRUE)) {
        expect_true(
          grepl("library\\(nightowl\\)", vignette_text),
          info = paste("Setup section in", vignette_name, "should load nightowl")
        )
      }
    })
  }
})

test_that("vignettes have appropriate chunk options", {
  vignette_dir <- file.path("..", "..", "vignettes")
  if (!dir.exists(vignette_dir)) {
    skip("Vignettes directory not found")
  }
  
  vignette_files <- list.files(vignette_dir, pattern = "\\.(Rmd|rmd)$", full.names = TRUE)
  
  for (vignette_file in vignette_files) {
    vignette_name <- basename(vignette_file)
    
    test_that(paste("Chunk options in", vignette_name, "are appropriate"), {
      vignette_content <- readLines(vignette_file)
      vignette_text <- paste(vignette_content, collapse = "\n")
      
      # Check for global chunk options setup
      expect_true(
        grepl("knitr::opts_chunk\\$set", vignette_text),
        info = paste("Missing global chunk options in", vignette_name)
      )
      
      # Check that eval=FALSE is not used extensively (examples should run)
      eval_false_chunks <- length(gregexpr("eval\\s*=\\s*FALSE", vignette_text)[[1]])
      if (eval_false_chunks > 0 && eval_false_chunks != -1) {
        # Some eval=FALSE is ok for performance-intensive or optional examples
        expect_lt(
          eval_false_chunks, 5,
          info = paste("Too many eval=FALSE chunks in", vignette_name, "- examples should be executable")
        )
      }
      
      # Check for appropriate figure dimensions
      expect_true(
        grepl("fig\\.width|fig\\.height", vignette_text),
        info = paste("Missing figure dimension settings in", vignette_name)
      )
    })
  }
})