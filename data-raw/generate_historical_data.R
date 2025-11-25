# Generate historical data files for each region mapping
# This script creates data_historical_values_{reg_id}.gdx files
# for each region mapping available in witchtools

library(witchtools)
library(gdxtools)
library(data.table)

# Get region and time mappings from witchtools
region_mappings <- witchtools::region_mappings
time_mappings <- witchtools::time_mappings

# Only process a subset of region mappings
# These are the most commonly used mappings for WITCH models and IIASA databases
mappings_to_process <- c(
  # WITCH standard mappings
  "witch17",
  # RICE mappings
  "maxiso3", "ed58",
  # IIASA/generic mappings
  "global", "r5"
)

cat("Will process", length(mappings_to_process), "region mappings:\n")
cat(paste(mappings_to_process, collapse = ", "), "\n\n")

# Determine the correct path to the source GDX file
if (file.exists("data-raw/data_historical_values.gdx")) {
  source_gdx <- normalizePath("data-raw/data_historical_values.gdx", winslash = "/")
  data_dir <- "data"
} else {
  stop("Source GDX file not found.\n",
       "Please place the source file at: data-raw/data_historical_values.gdx\n",
       "This file should contain the full (unreleased) historical data.")
}

cat("Using source file:", source_gdx, "\n")
cat("Output directory:", data_dir, "\n\n")

# Create data directory if it doesn't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Load the source GDX file once
cat("Loading source GDX file...\n")
source_gdx_obj <- gdx(source_gdx)

# Manual conversion function
convert_historical_data <- function(source_gdx_obj, reg_id, region_mapping) {
  cat("  Converting to", reg_id, "regions...\n")

  converted_params <- list()

  # Process each parameter
  for (param_name in source_gdx_obj$parameters$name) {
    param_data <- gdxtools::extract(source_gdx_obj, param_name)

    # Identify the region column (iso3 or any region mapping name)
    col_names <- names(param_data)[names(param_data) != "value"]
    region_col <- NULL

    if ("iso3" %in% col_names) {
      region_col <- "iso3"
    } else {
      # Check if any column matches a region mapping name
      for (col in col_names) {
        if (col %in% names(region_mappings)) {
          region_col <- col
          break
        }
      }
    }

    if (!is.null(region_col)) {
      # This parameter has a region dimension - aggregate it
      param_data <- as.data.table(param_data)

      # Uppercase the region column to match mapping (iso3 codes are uppercase)
      if (region_col == "iso3") {
        param_data[[region_col]] <- toupper(param_data[[region_col]])
      }

      # Get the appropriate mapping
      if (region_col == "iso3") {
        # Map from iso3 to target region
        mapping <- region_mapping
        setnames(mapping, c("target_region", "source_region"))
        param_data <- merge(param_data, mapping,
                           by.x = region_col, by.y = "source_region",
                           all.x = TRUE, allow.cartesian = TRUE)

        # Remove rows where mapping failed
        param_data <- param_data[!is.na(target_region)]

        # Remove the original region column
        param_data[[region_col]] <- NULL

        # Aggregate by target region and other dimensions
        group_cols <- c("target_region", setdiff(col_names, region_col))
        param_data <- param_data[, .(value = sum(value, na.rm = TRUE)), by = group_cols]

        # Rename target_region to 'n' and preserve original column order
        # Original columns had region_col at a specific position, we need to put 'n' there
        setnames(param_data, "target_region", "n")

        # Reorder columns to match original order (with 'n' replacing region_col position)
        # Original order was: col_names (which includes region_col) + "value"
        # New order should be: col_names with region_col replaced by 'n' + "value"
        original_order <- col_names
        original_order[original_order == region_col] <- "n"
        param_data <- param_data[, c(original_order, "value"), with = FALSE]
      } else {
        # Already in a region mapping format, just rename to 'n'
        # Preserve column order
        old_names <- names(param_data)
        setnames(param_data, region_col, "n")
        # Ensure value is last
        new_order <- setdiff(names(param_data), "value")
        param_data <- param_data[, c(new_order, "value"), with = FALSE]
      }
    } else {
      # No region dimension, keep as-is (e.g., global parameters)
      param_data <- as.data.table(param_data)
    }

    # Handle V1 column that might actually be year
    if ("V1" %in% names(param_data) && !("year" %in% names(param_data))) {
      # Check if V1 contains year-like values (4-digit numbers >= 1900)
      v1_vals <- param_data[[1]]  # V1 is first column
      v1_numeric <- suppressWarnings(as.numeric(as.character(v1_vals)))
      if (!all(is.na(v1_numeric)) && all(v1_numeric[!is.na(v1_numeric)] >= 1900 & v1_numeric[!is.na(v1_numeric)] <= 2200)) {
        # V1 contains years, rename it
        setnames(param_data, "V1", "year")
      }
    }

    # Ensure year column is numeric if it exists
    if ("year" %in% names(param_data)) {
      param_data[, year := as.numeric(as.character(year))]
    }

    # Store the converted parameter
    converted_params[[param_name]] <- param_data
  }

  return(converted_params)
}

# Loop through selected region mappings and create converted files
cat("\nGenerating historical data files for selected region mappings...\n\n")

for (.reg_id in mappings_to_process) {
  # Check if this mapping exists in witchtools
  if (!(.reg_id %in% names(region_mappings))) {
    warning("Region mapping '", .reg_id, "' not found in witchtools. Skipping.")
    next
  }

  cat("Processing region mapping:", .reg_id, "\n")

  tryCatch({
    # Get the region mapping
    region_mapping <- as.data.table(region_mappings[[.reg_id]])

    # The mapping has two columns: target (reg_id name) and source (iso3)
    # Rename columns for clarity
    col1 <- names(region_mapping)[1]  # target region (e.g., "witch17", "global")
    col2 <- names(region_mapping)[2]  # source region (usually "iso3")

    # Convert historical data
    converted_params <- convert_historical_data(source_gdx_obj, .reg_id, region_mapping)

    # Write to GDX file
    target_file <- file.path(data_dir, paste0("data_historical_values_", .reg_id, ".gdx"))
    cat("  Writing GDX file...\n")
    gdxtools::write.gdx(target_file, params = converted_params)

    cat("  Created:", target_file, "\n")

  }, error = function(e) {
    warning("Failed to process region mapping '", .reg_id, "': ", e$message)
  })
}

cat("\nHistorical data generation complete!\n")
cat("Generated files:\n")
print(list.files(data_dir, pattern = "^data_historical_values_.*\\.gdx$"))

# Extract set dependencies from the ORIGINAL source GDX file
cat("\nExtracting set dependencies from original historical data file...\n")

# Read the original source file to get true set dependencies
cat("Reading:", source_gdx, "\n")

# Get all region mapping names to replace with 'n'
region_set_names <- names(region_mappings)

# Create a named list where each element is a parameter
# and contains a vector of set names with regions replaced by 'n'
hist_set_deps <- list()

# Get all parameters from the GDX file
params <- source_gdx_obj$parameters

if (nrow(params) > 0) {
  for (i in 1:nrow(params)) {
    param_name <- params$name[i]
    param_dim <- params$dim[i]

    if (param_dim > 0) {
      # Get the actual data to see column names
      param_data <- tryCatch({
        gdxtools::extract(source_gdx_obj, param_name)
      }, error = function(e) {
        NULL
      })

      if (!is.null(param_data) && ncol(param_data) > 1) {
        # Get column names (excluding 'value')
        col_names <- names(param_data)
        col_names <- col_names[col_names != "value"]

        # Replace column names to match WITCH conventions:
        # - Any region mapping name or 'iso3' becomes 'n'
        # - Keep everything else as is (including 'year')
        col_names_standardized <- sapply(col_names, function(name) {
         if (name == "iso3" || name %in% region_set_names) {
            return("n")
          } else {
            return(name)
          }
        }, USE.NAMES = FALSE)

        # Store as a vector
        hist_set_deps[[param_name]] <- col_names_standardized

        cat("  ", param_name, ": ", paste(col_names_standardized, collapse = ", "), "\n", sep = "")
      }
    }
  }
}

# Save as RDS file (preserves list structure)
if (length(hist_set_deps) > 0) {
  rds_file <- file.path(data_dir, "historical_data_set_dependencies.rds")
  saveRDS(hist_set_deps, rds_file)
  cat("\nSet dependencies saved to:", rds_file, "\n")
  cat("Total parameters:", length(hist_set_deps), "\n")

  cat("\nExample set dependencies:\n")
  print(head(hist_set_deps, 10))
} else {
  warning("No set dependencies extracted!")
}
