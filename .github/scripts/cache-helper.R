# Cache Helper Functions for nightowl R Package
# Provides utility functions for dependency caching and optimization
# Addresses GitHub Issue #53

#' Generate cache key for current environment
#' 
#' Creates a standardized cache key based on R version, OS, and dependencies
#' 
#' @param prefix Character prefix for the cache key (e.g., "nightowl", "security")
#' @param r_version R version string
#' @param description_hash Hash of DESCRIPTION file
#' @param extra_files Additional files to include in hash
#' @return Character string representing the cache key
generate_cache_key <- function(prefix = "nightowl", 
                               r_version = paste0(R.version$major, ".", R.version$minor),
                               description_hash = tools::md5sum("DESCRIPTION"),
                               extra_files = NULL) {
  
  # Get OS information
  os_info <- Sys.info()["sysname"]
  
  # Base key components
  key_parts <- c(
    prefix,
    os_info,
    r_version,
    description_hash
  )
  
  # Add extra file hashes if provided
  if (!is.null(extra_files)) {
    extra_hashes <- sapply(extra_files, function(f) {
      if (file.exists(f)) tools::md5sum(f) else "missing"
    })
    key_parts <- c(key_parts, extra_hashes)
  }
  
  # Create cache key
  cache_key <- paste(key_parts, collapse = "-")
  
  # Sanitize for GitHub Actions cache
  cache_key <- gsub("[^a-zA-Z0-9.-]", "-", cache_key)
  cache_key <- gsub("-+", "-", cache_key)
  cache_key <- gsub("^-|-$", "", cache_key)
  
  return(cache_key)
}

#' Check cache effectiveness
#' 
#' Analyzes build times and cache hit rates to assess caching effectiveness
#' 
#' @param baseline_file Path to baseline metrics file
#' @return List containing cache performance metrics
analyze_cache_performance <- function(baseline_file = ".baseline_metrics") {
  
  cat("=== Cache Performance Analysis ===\\n")
  
  # Initialize metrics
  metrics <- list(
    cache_enabled = TRUE,
    baseline_available = file.exists(baseline_file),
    r_libs_path = .libPaths()[1],
    cached_packages = 0,
    cache_size_mb = NA
  )
  
  # Check if R libraries directory exists and has packages
  if (dir.exists(metrics$r_libs_path)) {
    installed_pkgs <- list.dirs(metrics$r_libs_path, full.names = FALSE, recursive = FALSE)
    metrics$cached_packages <- length(installed_pkgs[installed_pkgs != ""])
    
    # Calculate cache size
    if (metrics$cached_packages > 0) {
      cache_size_bytes <- sum(sapply(list.dirs(metrics$r_libs_path, full.names = TRUE, recursive = FALSE), 
                                   function(d) sum(file.size(list.files(d, recursive = TRUE, full.names = TRUE)), na.rm = TRUE)))
      metrics$cache_size_mb <- round(cache_size_bytes / 1024^2, 2)
    }
  }
  
  # Read baseline if available
  if (metrics$baseline_available) {
    baseline_content <- readLines(baseline_file)
    cat("Baseline information:\\n")
    cat(paste(baseline_content, collapse = "\\n"), "\\n")
  }
  
  # Cache effectiveness assessment
  cat("\\n=== Cache Status ===\\n")
  cat("R library path:", metrics$r_libs_path, "\\n")
  cat("Cached packages:", metrics$cached_packages, "\\n")
  
  if (!is.na(metrics$cache_size_mb)) {
    cat("Cache size:", metrics$cache_size_mb, "MB\\n")
    
    # Performance assessment
    if (metrics$cache_size_mb > 100) {
      cat("✅ Substantial cache populated - expect significant build time reduction\\n")
    } else if (metrics$cache_size_mb > 10) {
      cat("⚠️  Moderate cache populated - expect moderate build time reduction\\n")
    } else {
      cat("❌ Small cache - limited build time reduction expected\\n")
    }
  } else {
    cat("❌ No cached packages found - cache miss or first run\\n")
  }
  
  return(metrics)
}

#' Handle internal package dependencies
#' 
#' Special handling for internal packages (picasso, waRRior) that may not be on CRAN
#' 
#' @param packages Character vector of package names to check
#' @return List indicating which packages are internal and their status
handle_internal_packages <- function(packages = c("picasso", "waRRior")) {
  
  cat("=== Internal Package Handling ===\\n")
  
  internal_status <- list()
  
  for (pkg in packages) {
    cat("Checking internal package:", pkg, "\\n")
    
    status <- list(
      package = pkg,
      installed = pkg %in% rownames(installed.packages()),
      available_on_cran = pkg %in% available.packages()[, "Package"],
      needs_special_handling = FALSE
    )
    
    # Check if package is available
    if (!status$installed && !status$available_on_cran) {
      status$needs_special_handling <- TRUE
      cat("⚠️ ", pkg, "not found - may require private repository access\\n")
    } else if (status$installed) {
      cat("✅", pkg, "is installed\\n")
    } else if (status$available_on_cran) {
      cat("📦", pkg, "available on CRAN\\n")
    }
    
    internal_status[[pkg]] <- status
  }
  
  # Provide recommendations
  packages_needing_attention <- sapply(internal_status, function(x) x$needs_special_handling)
  
  if (any(packages_needing_attention)) {
    cat("\\n🔧 Recommendations for internal packages:\\n")
    cat("1. Ensure GitHub PAT is configured for private repositories\\n")
    cat("2. Consider using remotes::install_github() for internal packages\\n")
    cat("3. Add internal packages to separate cache key for version tracking\\n")
  }
  
  return(internal_status)
}

#' Validate dependency installation
#' 
#' Checks that all required dependencies are properly installed and accessible
#' 
#' @param description_file Path to DESCRIPTION file
#' @return Boolean indicating if all dependencies are satisfied
validate_dependencies <- function(description_file = "DESCRIPTION") {
  
  cat("=== Dependency Validation ===\\n")
  
  if (!file.exists(description_file)) {
    cat("❌ DESCRIPTION file not found\\n")
    return(FALSE)
  }
  
  # Read DESCRIPTION file
  desc <- read.dcf(description_file)
  
  # Extract dependencies
  imports <- if ("Imports" %in% colnames(desc)) {
    trimws(strsplit(desc[, "Imports"], ",")[[1]])
  } else {
    character(0)
  }
  
  depends <- if ("Depends" %in% colnames(desc)) {
    trimws(strsplit(desc[, "Depends"], ",")[[1]])
  } else {
    character(0)
  }
  
  suggests <- if ("Suggests" %in% colnames(desc)) {
    trimws(strsplit(desc[, "Suggests"], ",")[[1]])
  } else {
    character(0)
  }
  
  # Clean package names (remove version requirements)
  clean_pkg_name <- function(pkg_strings) {
    pkg_strings <- pkg_strings[pkg_strings != ""]
    pkg_strings <- sapply(pkg_strings, function(x) {
      # Remove version specifications like (>= 1.0.0)
      gsub("\\s*\\([^)]*\\)", "", x)
    }, USE.NAMES = FALSE)
    pkg_strings[pkg_strings != "R"]  # Remove R itself
  }
  
  imports <- clean_pkg_name(imports)
  depends <- clean_pkg_name(depends)
  suggests <- clean_pkg_name(suggests)
  
  all_required <- unique(c(imports, depends))
  all_packages <- unique(c(imports, depends, suggests))
  
  cat("Required packages (Imports + Depends):", length(all_required), "\\n")
  cat("Total packages (including Suggests):", length(all_packages), "\\n")
  
  # Check installation status
  installed <- rownames(installed.packages())
  
  missing_required <- setdiff(all_required, installed)
  missing_suggests <- setdiff(suggests, installed)
  
  # Report results
  if (length(missing_required) == 0) {
    cat("✅ All required dependencies are installed\\n")
    dependencies_satisfied <- TRUE
  } else {
    cat("❌ Missing required dependencies:", paste(missing_required, collapse = ", "), "\\n")
    dependencies_satisfied <- FALSE
  }
  
  if (length(missing_suggests) > 0) {
    cat("⚠️  Missing suggested dependencies:", paste(missing_suggests, collapse = ", "), "\\n")
  }
  
  # Cache status for dependencies
  cat("\\n📊 Dependency Cache Status:\\n")
  cat("- Installed packages:", length(installed), "\\n")
  cat("- Required for nightowl:", length(all_required), "\\n")
  cat("- Cache hit rate:", round((length(all_required) - length(missing_required)) / length(all_required) * 100, 1), "%\\n")
  
  return(dependencies_satisfied)
}

# Main execution if script is run directly
if (!interactive()) {
  cat("nightowl Cache Helper Script\\n")
  cat("============================\\n\\n")
  
  # Run all validation functions
  cache_perf <- analyze_cache_performance()
  internal_pkgs <- handle_internal_packages()
  deps_valid <- validate_dependencies()
  
  # Generate cache key example
  example_key <- generate_cache_key("nightowl-example")
  cat("\\n📝 Example cache key:", example_key, "\\n")
  
  # Summary
  cat("\\n=== Summary ===\\n")
  if (deps_valid && cache_perf$cached_packages > 0) {
    cat("✅ Caching is working effectively\\n")
  } else {
    cat("⚠️  Caching may need attention - check logs above\\n")
  }
}