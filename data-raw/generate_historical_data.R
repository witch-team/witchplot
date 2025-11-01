# Generate historical data files for each region mapping
# This script creates data_historical_values_{reg_id}.gdx files
# for each region mapping available in witchtools

library(witchtools)

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
# The source file should be in data-raw/ (not released with package)
# The processed files will be in data/ (released with package)
if (file.exists("data-raw/data_historical_values.gdx")) {
  source_gdx <- normalizePath("data-raw/data_historical_values.gdx", winslash = "/")
  data_raw_dir <- "data-raw"
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

# Create a temporary working directory for conversions
temp_dir <- file.path(data_dir, "temp_conversion")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)
}

# Copy source file to temp directory
temp_source <- file.path(temp_dir, "data_historical_values.gdx")
file.copy(source_gdx, temp_source, overwrite = TRUE)

# Loop through selected region mappings and create converted files
cat("Generating historical data files for selected region mappings...\n\n")

for (.reg_id in mappings_to_process) {
  # Check if this mapping exists in witchtools
  if (!(.reg_id %in% names(region_mappings))) {
    warning("Region mapping '", .reg_id, "' not found in witchtools. Skipping.")
    next
  }

  cat("Processing region mapping:", .reg_id, "\n")

  tryCatch({
    # Convert the GDX file for this specific region mapping
    # This will aggregate regions and rename the region column to 'n'
    # All other set names (year, ghg, jreal, etc.) are preserved
    witchtools::convert_gdx(
      gdxfile = temp_source,
      output_directory = temp_dir,
      region_mappings = region_mappings,
      time_mappings = time_mappings,
      reg_id = .reg_id,
      time_id = "year"  # Keep year as year, don't convert to t
    )

         # The file is saved as data_historical_values.gdx in the output directory
    # Move it to the final location with the proper name
    temp_output <- file.path(temp_dir, "data_historical_values.gdx")
    target_file <- file.path(data_dir, paste0("data_historical_values_", .reg_id, ".gdx"))

    if (file.exists(temp_output)) {
      #file.copy(temp_output, target_file, overwrite = TRUE)
      cat("  Created:", target_file, "\n")
      # Restore the source file for next iteration
      file.copy(source_gdx, temp_source, overwrite = TRUE)
    } else {
      warning("Expected output file not found: ", temp_output)
    }

  }, error = function(e) {
    warning("Failed to process region mapping '", .reg_id, "': ", e$message)
    # Restore the source file even on error
    file.copy(source_gdx, temp_source, overwrite = TRUE)
  })
}

# Clean up temporary directory
unlink(temp_dir, recursive = TRUE)

cat("\nHistorical data generation complete!\n")
cat("Generated files:\n")
print(list.files(data_dir, pattern = "^data_historical_values_.*\\.gdx$"))

# Extract set dependencies from the ORIGINAL source GDX file
cat("\nExtracting set dependencies from original historical data file...\n")

library(gdxtools)

# Read the original source file to get true set dependencies
cat("Reading:", source_gdx, "\n")
mygdx <- gdx(source_gdx)

# Get all region mapping names to replace with 'n'
region_set_names <- names(region_mappings)

# Create a named list where each element is a parameter
# and contains a vector of set names with regions replaced by 'n'
hist_set_deps <- list()

# Get all parameters from the GDX file
params <- mygdx$parameters

if (nrow(params) > 0) {
  for (i in 1:nrow(params)) {
    param_name <- params$name[i]
    param_dim <- params$dim[i]

    if (param_dim > 0) {
      # Get the actual data to see column names
      param_data <- tryCatch({
        mygdx[param_name]
      }, error = function(e) {
        NULL
      })

      if (!is.null(param_data) && ncol(param_data) > 1) {
        # Get column names (excluding 'value')
        col_names <- names(param_data)
        col_names <- col_names[col_names != "value"]

        # Replace column names to match WITCH conventions:
        # - Any region mapping name (witch17, witch20, etc.) or 'iso3' becomes 'n'
        # - 'e' (emissions in historical data) becomes 'ghg' (WITCH name)
        # - Keep everything else as is (including 'year')
        col_names_standardized <- sapply(col_names, function(name) {
         if (name == "iso3" || name %in% region_set_names) {
            return("n")
          } else {
            return(name)  # Keep original name including 'year', 'jreal', 'fuel', etc.
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
