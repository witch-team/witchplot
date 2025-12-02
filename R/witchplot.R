#' @keywords internal
.onLoad <- function(libname, pkgname) {
ggplot2::theme_set(ggplot2::theme_bw())
options(
results_dir="./",
restrict_files="results_",
exclude_files="",
removepattern="",
year0=2005,
tstep=5,
yearmin=1980,
yearmax=2100,
reg_id=NULL,
deploy_online=FALSE,
figure_format="png",
add_historical=TRUE,
write_plotdata_csv=FALSE
)

# Try to automatically initialize GDX library
tryCatch({
  if(requireNamespace("gdxtools", quietly = TRUE)) {
    # Silently try to initialize GDX - don't show messages during package load
    suppressMessages(gdxtools::igdx())
  }
}, error = function(e) {
  # Silently fail - GDX will be initialized later if needed
  # User will get helpful error message from setup_gdx() if they try to use it
})
}

#' Clean up global environment from previous witchplot sessions
#' @keywords internal
.cleanup_witchplot_globals <- function() {
  # List of all global variables created by witchplot
  witchplot_globals <- c(
    "results_dir", "restrict_files", "exclude_files", "removepattern",
    "deploy_online", "figure_format", "add_historical", "write_plotdata_csv",
    "reg_id", "year0", "tstep", "yearmin", "yearmax",
    "filelist", "scenlist", "file_group_columns",
    "witch_regions", "display_regions", "region_palette",
    "region_palette_specific_short", "region_palette_longnames",
    "stochastic_files", "all_var_descriptions",
    "graphdir", "map_var_hist", "iamc_filename", "iamc_databasename",
    "iiasadb_snapshot", "iiasadb_historical",
    "varlist_combine_old_new_j", "file_separate", "nice_region_names", "restrict_regions"
  )

  # Remove all witchplot globals that exist
  for(var in witchplot_globals) {
    if(exists(var, envir = .GlobalEnv)) {
      rm(list = var, envir = .GlobalEnv)
    }
  }

  invisible(NULL)
}

#' Launch WITCH Model Interactive Visualization
#'
#' Loads WITCH model GDX result files and launches an interactive Shiny application
#' for scenario comparison and visualization.
#'
#' @param results_dir Path(s) to results directory containing GDX files. Can be a vector for multiple directories (default: "./")
#' @param restrict_files Pattern to filter GDX files (default: "results_")
#' @param exclude_files Pattern to exclude GDX files (default: "")
#' @param removepattern Pattern to remove from scenario names (default: "results_")
#' @param add_historical Logical, add historical data where available (default: TRUE)
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures: "png", "pdf", "svg" (default: "png")
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param launch Logical, launch Shiny app immediately (default: TRUE). Set FALSE to load data only.
#' @param ... Additional options passed to session configuration. Useful options include:
#'   \itemize{
#'     \item \code{file_separate}: Vector to split scenario names into multiple columns. Format: c("type", "separator", "col1", "col2", ...).
#'           Type can be "separate" (split all), "first" (first element), or "last" (last element).
#'           Example: \code{file_separate = c("separate", "_", "model", "scenario", "carbon_price")} splits "SSP2_1p5C_high" into three columns.
#'     \item \code{nice_region_names}: Named vector to rename regions for display. Example: \code{c("usa_te" = "USA", "eur" = "Europe")}
#'     \item \code{restrict_regions}: Character vector of regions to display (filters out others)
#'   }
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Basic usage with defaults
#'   run_witch()
#'
#'   # Disable historical data
#'   run_witch(add_historical = FALSE)
#'
#'   # Specify custom paths
#'   run_witch(results_dir = "results")
#'
#'   # Compare multiple result directories
#'   run_witch(results_dir = c("results_bau", "results_policy"))
#'
#'   # Load data without launching UI (for scripting)
#'   run_witch(launch = FALSE)
#'
#'   # Split scenario names into separate columns
#'   # If files are named like "SSP2_BAU_low", "SSP2_1p5C_high", etc.
#'   run_witch(file_separate = c("separate", "_", "SSP", "policy", "sensitivity"))
#'
#'   # Extract only last part of scenario name
#'   run_witch(file_separate = c("last", "_", "sensitivity"))
#'
#'   # Rename regions and restrict display
#'   run_witch(
#'     F = c("usa_te" = "USA", "eur" = "Europe"),
#'     restrict_regions = c("USA", "Europe", "China")
#'   )
#' }
#'
#' @export
run_witch <- function(results_dir="./", restrict_files="results_", exclude_files="", removepattern="results_",
                      add_historical=TRUE, deploy_online=FALSE, figure_format="png", write_plotdata_csv=FALSE,
                      launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Set all options
opts <- list(results_dir=results_dir, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("results_dir", results_dir, envir=.GlobalEnv)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("removepattern", removepattern, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
# Clear memoise cache for get_witch when add_historical changes
if(exists("get_witch")) {
  memoise::forget(get_witch)
}
.initialize_witchplot_session()
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "witch", package="witchplot"))
}

#' Launch RICE50+ Model Interactive Visualization
#'
#' Loads RICE50+ model GDX result files and launches an interactive Shiny application
#' for scenario comparison and visualization with regional disaggregation.
#'
#' @param results_dir Path(s) to results directory containing GDX files (default: "./")
#' @param reg_id Regional aggregation ID, e.g., "ed58" for 58 regions (default: "ed58")
#' @param year0 Base year for the model (default: 2015)
#' @param tstep Time step in years (default: 5)
#' @param restrict_files Pattern to filter GDX files (default: "results_")
#' @param exclude_files Pattern to exclude GDX files (default: "")
#' @param removepattern Pattern to remove from scenario names (default: "")
#' @param add_historical Logical, add historical data where available (default: TRUE)
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures: "png", "pdf", "svg" (default: "png")
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param launch Logical, launch Shiny app immediately (default: TRUE)
#' @param ... Additional options passed to session configuration. Useful options include:
#'   \itemize{
#'     \item \code{file_separate}: Vector to split scenario names into multiple columns. Format: c("type", "separator", "col1", "col2", ...).
#'           Type can be "separate" (split all), "first" (first element), or "last" (last element).
#'           Example: \code{file_separate = c("separate", "_", "model", "scenario", "carbon_price")} splits "SSP2_1p5C_high" into three columns.
#'     \item \code{nice_region_names}: Named vector to rename regions for display. Example: \code{c("usa_te" = "USA", "eur" = "Europe")}
#'     \item \code{restrict_regions}: Character vector of regions to display (filters out others)
#'   }
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   run_rice()
#'
#'   # Disable historical data
#'   run_rice(add_historical = FALSE)
#'
#'   # Custom regional aggregation
#'   run_rice(reg_id = "ed57", year0 = 2020, tstep = 10)
#'
#'   # Specify custom paths
#'   run_rice(results_dir = "results")
#' }
#'
#' @export
run_rice <- function(results_dir="./", reg_id="ed58", year0=2015, tstep=5, restrict_files="results_", exclude_files="", removepattern="results_",
                     add_historical=TRUE, deploy_online=FALSE, figure_format="png", write_plotdata_csv=FALSE,
                     launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Set all options
opts <- list(results_dir=results_dir, reg_id=reg_id, year0=year0, tstep=tstep, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("results_dir", results_dir, envir=.GlobalEnv)
assign("reg_id", reg_id, envir=.GlobalEnv)
assign("year0", year0, envir=.GlobalEnv)
assign("tstep", tstep, envir=.GlobalEnv)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("removepattern", removepattern, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
# Clear memoise cache for get_witch when add_historical changes
if(exists("get_witch")) {
  memoise::forget(get_witch)
}
# Load map_var_hist from CSV file
map_var_hist_file <- system.file("config", "map_var_hist_rice.csv", package="witchplot")
if(file.exists(map_var_hist_file)) {
  map_var_hist <- data.table::fread(map_var_hist_file)
  map_var_hist <- map_var_hist %>% dplyr::rowwise() %>% dplyr::mutate(conv=eval(parse(text=conv))) %>% data.table::as.data.table()
} else {
  warning("map_var_hist_rice.csv not found, historical data mapping disabled")
  map_var_hist <- data.table::data.table()
}
assign("map_var_hist", map_var_hist, envir=.GlobalEnv)
.initialize_witchplot_session()
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "rice", package="witchplot"))
}

#' Launch FIDELIO Model Interactive Visualization
#'
#' Loads FIDELIO model GDX result files and launches an interactive Shiny application
#' for analyzing economic impacts and input-output model results.
#'
#' @param results_dir Path(s) to results directory containing GDX files (default: "./")
#' @param restrict_files Pattern to filter GDX files (default: "results_")
#' @param exclude_files Pattern to exclude GDX files (default: "")
#' @param removepattern Pattern to remove from scenario names (default: "")
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures (default: "png")
#' @param add_historical Logical, add historical data where available (default: TRUE)
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param launch Logical, launch Shiny app immediately (default: TRUE)
#' @param ... Additional options passed to session configuration
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   run_fidelio()
#'   run_fidelio(results_dir = "results")
#' }
#'
#' @export
run_fidelio <- function(results_dir="./", restrict_files="results_", exclude_files="", removepattern="results_",
                        add_historical=TRUE, deploy_online=FALSE, figure_format="png", write_plotdata_csv=FALSE,
                        launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Set all options
opts <- list(results_dir=results_dir, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("results_dir", results_dir, envir=.GlobalEnv)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("removepattern", removepattern, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
# Clear memoise cache for get_witch when add_historical changes
if(exists("get_witch")) {
  memoise::forget(get_witch)
}
.initialize_witchplot_session()
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "fidelio", package="witchplot"))
}

#' Launch IIASA Database Comparison Viewer
#'
#' Loads IAM scenario data in IAMC format (CSV/XLSX files or IIASA database connection)
#' and launches an interactive Shiny application for comparing scenarios.
#'
#' By default (iamc_filename=NULL), automatically discovers and loads all CSV and XLSX files
#' in the results_dir. Files are combined into a single dataset for comparison.
#' Supports multiple directories - pass as a vector to load and compare across directories.
#'
#' @param results_dir Path(s) to director(ies) containing IAMC format files. Can be a vector for multiple directories (default: "./")
#' @param reg_id Regional aggregation(s) to display, e.g., c("witch20", "global") (default: c("witch20", "global"))
#' @param iamc_filename Specific IAMC file to load (CSV, XLSX, or CSV.ZIP). If NULL, loads all CSV/XLSX files in results_dir (default: NULL)
#' @param iamc_databasename Name of IIASA database to connect to (e.g., "ENGAGE"). Alternative to iamc_filename (default: NULL)
#' @param year0 Base year for the model (default: 2005)
#' @param tstep Time step in years (default: 5)
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures (default: "png")
#' @param add_historical Logical, add historical data where available (default: TRUE)
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param map_var_hist Data frame mapping IAMC variables to historical data sources. If NULL, uses default mapping.
#' @param launch Logical, launch Shiny app immediately (default: TRUE)
#' @param ... Additional options passed to session configuration
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Auto-load all CSV/XLSX files in current directory
#'   run_iiasadb()
#'
#'   # Load specific file
#'   run_iiasadb(iamc_filename = "scenarios.csv")
#'
#'   # Load from custom directory
#'   run_iiasadb(results_dir = "EIEE-MIP")
#'
#'   # Compare across multiple directories
#'   run_iiasadb(results_dir = c("results_v1", "results_v2"))
#'
#'   # Connect to IIASA database
#'   run_iiasadb(iamc_databasename = "ENGAGE")
#' }
#'
#' @export
run_iiasadb <- function(results_dir="./", reg_id=c("r5"), iamc_filename=NULL, iamc_databasename=NULL,
                        add_historical=TRUE, deploy_online=FALSE, figure_format="png", write_plotdata_csv=FALSE,
                        launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Set all options
opts <- list(results_dir=results_dir, reg_id=reg_id, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("results_dir", results_dir, envir=.GlobalEnv)
assign("reg_id", reg_id, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
# Clear memoise cache for get_witch when add_historical changes
if(exists("get_witch")) {
  memoise::forget(get_witch)
}
if(!is.null(iamc_filename)) assign("iamc_filename", iamc_filename, envir=.GlobalEnv)
if(!is.null(iamc_databasename)) assign("iamc_databasename", iamc_databasename, envir=.GlobalEnv)
# Load map_var_hist from CSV file
map_var_hist_file <- system.file("config", "map_var_hist_iiasadb.csv", package="witchplot")
if(file.exists(map_var_hist_file)) {
  map_var_hist <- data.table::fread(map_var_hist_file)
  map_var_hist <- map_var_hist %>% dplyr::rowwise() %>% dplyr::mutate(conv=eval(parse(text=conv))) %>% data.table::as.data.table()
} else {
  warning("map_var_hist_iiasadb.csv not found, historical data mapping disabled")
  map_var_hist <- data.table::data.table()
}
assign("map_var_hist", map_var_hist, envir=.GlobalEnv)
# IIASADB doesn't use GDX files, so don't initialize GDX session
if(!is.null(iamc_databasename)) {
# Try to find snapshot in results_dir first, then fall back to package
snapshot_file <- NULL
if(exists("results_dir") && length(results_dir) > 0) {
  results_snapshot <- file.path(results_dir[1], "iiasadb_snapshot.Rdata")
  if(file.exists(results_snapshot)) {
    snapshot_file <- results_snapshot
  }
}
# Fall back to package location if not found in results_dir
if(is.null(snapshot_file)) {
  pkg_snapshot <- system.file("gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata", package="witchplot")
  if(file.exists(pkg_snapshot)) {
    snapshot_file <- pkg_snapshot
  }
}

load_from_db <- TRUE
snapshot_loaded_from_file <- FALSE
if(!is.null(snapshot_file)) {
input <- menu(c("Yes", "No"), title="There is a snapshot available. Do you want to load it?")
if(input==1) {
  load(snapshot_file, envir=.GlobalEnv)
  message("Loaded snapshot from: ", snapshot_file)
  load_from_db <- FALSE
  snapshot_loaded_from_file <- TRUE
}
}
if(load_from_db) {
  message("Fetching data from IIASA database: ", iamc_databasename)
  iiasadb_snapshot <- download_iiasadb(database=iamc_databasename, varlist="*", region="World", modlist="*", scenlist="*", add_metadata=FALSE)
  names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
  iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, YEAR, VALUE) %>% dplyr::rename(value=VALUE) %>% dplyr::filter(!is.na(value))
  assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
}
} else {
# Load files from all directories
file_list <- list()
total_files <- 0

for(results_path in results_dir) {
  pathdir_label <- basename(results_path)
  message("\nLoading from directory: ", results_path)

  # If iamc_filename is NULL, automatically find and combine all CSV and XLSX files
  if(is.null(iamc_filename)) {
    csv_files <- list.files(results_path, pattern="\\.csv$", full.names=FALSE, ignore.case=TRUE)
    csv_files <- csv_files[!stringr::str_detect(csv_files, "\\.zip$")]  # Exclude .csv.zip files
    xlsx_files <- list.files(results_path, pattern="\\.xlsx$", full.names=FALSE, ignore.case=TRUE)
    csvzip_files <- list.files(results_path, pattern="\\.csv\\.zip$", full.names=FALSE, ignore.case=TRUE)
    all_files <- c(csv_files, xlsx_files, csvzip_files)

    if(length(all_files)==0) {
      warning("No CSV or XLSX files found in: ", results_path)
      next
    }

    message("Found ", length(all_files), " file(s): ", paste(all_files, collapse=", "))

    # Load and combine all files from this directory
    for(fname in all_files) {
      message("  Loading: ", fname)

      # Try to load file with error handling
      file_data <- tryCatch({
        if(stringr::str_detect(fname, "\\.xlsx$")) {
          openxlsx::read.xlsx(file.path(results_path, fname), sheet=1)
        } else if(stringr::str_detect(fname, "\\.csv\\.zip$")) {
          data.table::fread(cmd=paste0('unzip -cq "', file.path(results_path, fname), '" ', gsub(".zip", "", basename(fname))), header=TRUE, quote="\"", sep=",", check.names=FALSE, fill=TRUE)
        } else {
          data.table::fread(file.path(results_path, fname), header=TRUE, quote="\"", sep=",", check.names=FALSE, fill=TRUE)
        }
      }, error = function(e) {
        warning("  Failed to load ", fname, ": ", e$message, ". Skipping this file.")
        return(NULL)
      })

      # Skip if file failed to load
      if(is.null(file_data)) next

      # Check if file has required IAMC columns
      names(file_data) <- toupper(names(file_data))
      required_cols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT")
      if(!all(required_cols %in% names(file_data))) {
        warning("  ", fname, " does not appear to be IAMC format (missing required columns). Skipping.")
        next
      }

      # Add pathdir column if multiple directories
      if(length(results_dir) > 1) {
        file_data$PATHDIR <- pathdir_label
      }

      # Use unique key for file_list to avoid overwrites across directories
      file_key <- paste0(pathdir_label, "___", fname)
      file_list[[file_key]] <- file_data
      total_files <- total_files + 1
    }
  } else {
    # Load specific file if iamc_filename is provided
    if(!file.exists(file.path(results_path, iamc_filename))) {
      warning("File not found: ", file.path(results_path, iamc_filename))
      next
    }

    message("  Loading: ", iamc_filename)
    if(stringr::str_detect(iamc_filename, "\\.xlsx$")) {
      file_data <- openxlsx::read.xlsx(file.path(results_path, iamc_filename), sheet=1)
      names(file_data) <- toupper(names(file_data))
    } else if(stringr::str_detect(iamc_filename, "\\.csv\\.zip$")) {
      file_data <- data.table::fread(cmd=paste0('unzip -cq "', file.path(results_path, iamc_filename), '" ', gsub(".zip", "", basename(iamc_filename))), header=TRUE, quote="\"", sep=",", check.names=FALSE)
      names(file_data) <- toupper(names(file_data))
    } else if(stringr::str_detect(iamc_filename, "\\.csv$") && !stringr::str_detect(iamc_filename, "\\.csv\\.zip$")) {
      file_data <- data.table::fread(file.path(results_path, iamc_filename), header=TRUE, quote="\"", sep=",", check.names=FALSE)
      names(file_data) <- toupper(names(file_data))
    }

    # Add pathdir column if multiple directories
    if(length(results_dir) > 1) {
      file_data$PATHDIR <- pathdir_label
    }

    file_key <- paste0(pathdir_label, "___", iamc_filename)
    file_list[[file_key]] <- file_data
    total_files <- total_files + 1
  }
}

if(length(file_list)==0) stop("No IAMC files found in any of the specified directories")

iiasadb_snapshot <- data.table::rbindlist(file_list, fill=TRUE)
message("\nCombined ", total_files, " file(s) from ", length(results_dir), " director(ies) with ", nrow(iiasadb_snapshot), " total rows")

# Convert year columns to numeric and pivot longer
iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::mutate(dplyr::across(matches("^\\d{4}$"), ~suppressWarnings(as.numeric(.x))))

# Determine which columns to keep (not year columns)
if(length(results_dir) > 1) {
  non_year_cols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT", "PATHDIR")
} else {
  non_year_cols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT")
}

iiasadb_snapshot <- iiasadb_snapshot %>%
  tidyr::pivot_longer(cols=-dplyr::all_of(non_year_cols), names_to="YEAR") %>%
  dplyr::mutate(YEAR=as.integer(YEAR)) %>%
  as.data.frame()

assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
}
iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::mutate(REGION=toupper(REGION))
if(!exists("iiasadb_snapshot")) stop("Please check you specified a correct iiasadb file or connection.")

# Also assign to iiasadb_data for use with get_iiasadb() function
assign("iiasadb_data", iiasadb_snapshot, envir=.GlobalEnv)

# Pre-load historical data if add_historical is enabled
if(add_historical) {
  iiasadb_with_historical <- list()
  for(varname in map_var_hist$varname_model) {
    if(nrow(iiasadb_snapshot %>% dplyr::filter(VARIABLE==varname))>0) {
      iiasadb_with_historical[[varname]] <- add_historical_values(iiasadb_snapshot %>% dplyr::filter(VARIABLE==varname), varname=varname, iiasadb=TRUE, verbose=FALSE)
    }
  }
  iiasadb_historical <- data.table::rbindlist(iiasadb_with_historical) %>% dplyr::filter(stringr::str_detect(SCENARIO, "historical")) %>% as.data.frame()
} else {
  # Create empty historical data frame
  iiasadb_historical <- data.frame()
}

assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
assign("iiasadb_historical", iiasadb_historical, envir=.GlobalEnv)

# Save the snapshot only if we fetched new data (not if we loaded from existing snapshot)
# For iamc_databasename: only save if we downloaded from DB
# For iamc_filename/files: always save since we loaded from files
should_save <- (!exists("snapshot_loaded_from_file") || !snapshot_loaded_from_file)

if(should_save) {
  save_path <- NULL
  if(exists("results_dir") && length(results_dir) > 0) {
    # Save to results_dir
    save_path <- file.path(results_dir[1], "iiasadb_snapshot.Rdata")
    save(iiasadb_snapshot, iiasadb_historical, file=save_path)
    message("Saved snapshot to: ", save_path)
  } else {
    # Fall back to package location if results_dir doesn't exist
    pkg_save_path <- system.file("gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata", package="witchplot")
    if(pkg_save_path != "" && dir.exists(dirname(pkg_save_path))) {
      save_path <- pkg_save_path
      save(iiasadb_snapshot, iiasadb_historical, file=save_path)
      message("Saved snapshot to: ", save_path)
    } else {
      # Try inst/ directory if package location doesn't work
      inst_path <- file.path("inst", "gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata")
      if(dir.exists(dirname(inst_path))) {
        save_path <- inst_path
        save(iiasadb_snapshot, iiasadb_historical, file=save_path)
        message("Saved snapshot to: ", save_path)
      }
    }
  }
}
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "iiasadb", package="witchplot"))
}
