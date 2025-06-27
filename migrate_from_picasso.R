#!/usr/bin/env Rscript

#' Migration Script: Replace picasso with nightowl functions
#'
#' This script demonstrates how to replace picasso:: calls with nightowl:: equivalents
#' Run this script to see the replacements that need to be made

# Define the replacements
replacements <- list(
  # Color functions
  "picasso::roche_colors\\(\\)" = "nightowl::nightowl_colors()",
  "picasso::roche_colors\\(" = "nightowl::nightowl_colors()[",
  "picasso::roche_color\\(" = "nightowl::nightowl_color(",
  "picasso::roche_palette_discrete\\(" = "nightowl::nightowl_palette_discrete(",
  "picasso::colors_ibm\\(\\)" = "nightowl::nightowl_colors()",
  
  # Theme functions
  "picasso::theme_picasso\\(" = "nightowl::theme_nightowl(",
  "picasso::theme_void\\(" = "nightowl::theme_nightowl_void(",
  
  # Plot modification functions
  "picasso::hide_legend\\(" = "nightowl::hide_legend(",
  "picasso::hide_title\\(" = "nightowl::hide_title(",
  "picasso::hide_x_axis\\(" = "nightowl::hide_x_axis(",
  "picasso::hide_y_axis\\(" = "nightowl::hide_y_axis(",
  "picasso::add_x_axis\\(" = "nightowl::add_x_axis(",
  
  # Utility functions
  "picasso::is_dark\\(" = "nightowl::is_dark("
)

# Files to update
files_to_update <- c(
  "R/options.R",
  "R/donut_plot.R", 
  "R/km.R",
  "R/forest.R",
  "R/coxph.R",
  "R/add_plots.R",
  "R/inline_plots.R",
  "R/correlation_matrix.R",
  "R/summaries.R",
  "tests/testthat/test-donut_plot.R",
  "tests/testthat/test-summary.R"
)

# Function to perform replacements
update_file <- function(file_path, replacements, dry_run = TRUE) {
  if (!file.exists(file_path)) {
    cat("File not found:", file_path, "\n")
    return(invisible(NULL))
  }
  
  # Read file
  content <- readLines(file_path)
  original_content <- content
  
  # Apply replacements
  for (pattern in names(replacements)) {
    replacement <- replacements[[pattern]]
    content <- gsub(pattern, replacement, content)
  }
  
  # Check if changes were made
  if (!identical(original_content, content)) {
    if (dry_run) {
      cat("\n--- Changes for", file_path, "---\n")
      # Show first few changes
      changes <- which(original_content != content)
      for (i in head(changes, 5)) {
        cat("Line", i, ":\n")
        cat("  OLD:", original_content[i], "\n")
        cat("  NEW:", content[i], "\n")
      }
      if (length(changes) > 5) {
        cat("  ... and", length(changes) - 5, "more changes\n")
      }
    } else {
      writeLines(content, file_path)
      cat("Updated:", file_path, "\n")
    }
  } else {
    cat("No changes needed for:", file_path, "\n")
  }
}

# Example usage
cat("=== DRY RUN MODE ===\n")
cat("This will show what changes would be made.\n")
cat("To actually update files, set dry_run = FALSE\n\n")

for (file in files_to_update) {
  update_file(file, replacements, dry_run = TRUE)
}

# Special handling for specific patterns
cat("\n\n=== Special Cases to Handle Manually ===\n")
cat("1. In options.R, update the default initialization:\n")
cat("   initialize = function(colors = nightowl::nightowl_colors(),\n")
cat("                        missingColor = nightowl::nightowl_colors()[\"grey\"])\n\n")

cat("2. Color name access patterns need adjustment:\n")
cat("   OLD: picasso::roche_colors(\"red\")\n")
cat("   NEW: nightowl::nightowl_color(\"red\")\n")
cat("   OR:  nightowl::nightowl_colors()[\"red\"]\n\n")

cat("3. Update DESCRIPTION file to remove picasso dependency\n\n")

cat("4. Add proper documentation with roxygen2 for new functions\n\n")

# Create a summary report
cat("\n=== Summary ===\n")
cat("Files that use picasso:", length(files_to_update), "\n")
cat("Total replacements defined:", length(replacements), "\n")
cat("\nRun the following to add new files to git:\n")
cat("git add R/colors.R R/themes.R R/plot_helpers.R\n")
cat("\nThen update existing files and remove picasso dependency from DESCRIPTION\n")