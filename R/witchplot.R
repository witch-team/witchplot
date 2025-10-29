#' @keywords internal
.onLoad <- function(libname, pkgname) {
ggplot2::theme_set(ggplot2::theme_bw())
options(
model_dir="../",
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
}

#' Clean up global environment from previous witchplot sessions
#' @keywords internal
.cleanup_witchplot_globals <- function() {
  # List of all global variables created by witchplot
  witchplot_globals <- c(
    "model_dir", "subdir", "fullpathdir",
    "restrict_files", "exclude_files", "removepattern",
    "deploy_online", "figure_format", "add_historical", "write_plotdata_csv",
    "reg_id", "year0", "tstep", "yearmin", "yearmax",
    "filelist", "scenlist", "file_group_columns",
    "witch_regions", "display_regions", "region_palette",
    "region_palette_specific_short", "region_palette_longnames",
    "flexible_timestep", "stochastic_files", "all_var_descriptions",
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
#' @param model_dir Path to WITCH model source directory (default: "../")
#' @param results_dir Path(s) to results directory containing GDX files. Can be a vector for multiple directories (default: "./")
#' @param restrict_files Pattern to filter GDX files (default: "results_")
#' @param exclude_files Pattern to exclude GDX files (default: "")
#' @param removepattern Pattern to remove from scenario names (default: "results_")
#' @param launch Logical, launch Shiny app immediately (default: TRUE). Set FALSE to load data only.
#' @param ... Additional options passed to session configuration
#'
#' @section Global Options:
#' The following options should be set via \code{options()} before calling this function:
#' \itemize{
#'   \item \code{deploy_online}: Logical, whether to deploy online (default: FALSE)
#'   \item \code{figure_format}: Output format for figures: "png", "pdf", etc. (default: "png")
#'   \item \code{add_historical}: Logical, add historical data where available (default: TRUE)
#'   \item \code{write_plotdata_csv}: Logical, save plot data as CSV files (default: FALSE)
#' }
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Basic usage with defaults
#'   run_witch()
#'
#'   # Specify custom paths
#'   run_witch(model_dir = "../witch", results_dir = "results")
#'
#'   # Compare multiple result directories
#'   run_witch(results_dir = c("results_bau", "results_policy"))
#'
#'   # Load data without launching UI (for scripting)
#'   run_witch(launch = FALSE)
#' }
#'
#' @export
run_witch <- function(model_dir="../", results_dir="./", restrict_files="results_", exclude_files="", removepattern="results_", launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Get global-only options from options() with defaults
deploy_online <- getOption("deploy_online", FALSE)
figure_format <- getOption("figure_format", "png")
add_historical <- getOption("add_historical", TRUE)
write_plotdata_csv <- getOption("write_plotdata_csv", FALSE)
# Set all options
opts <- list(model_dir=model_dir, results_dir=results_dir, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("model_dir", model_dir, envir=.GlobalEnv)
assign("subdir", basename(results_dir), envir=.GlobalEnv)
assign("fullpathdir", results_dir, envir=.GlobalEnv)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("removepattern", removepattern, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
.initialize_witchplot_session()
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "witch", package="witchplot"))
}

#' Launch RICE50+ Model Interactive Visualization
#'
#' Loads RICE50+ model GDX result files and launches an interactive Shiny application
#' for scenario comparison and visualization with regional disaggregation.
#'
#' @param model_dir Path to RICE model source directory (default: "../")
#' @param results_dir Path(s) to results directory containing GDX files (default: "./")
#' @param reg_id Regional aggregation ID, e.g., "ed58" for 58 regions (default: "ed58")
#' @param year0 Base year for the model (default: 2015)
#' @param tstep Time step in years (default: 5)
#' @param restrict_files Pattern to filter GDX files (default: "results_")
#' @param exclude_files Pattern to exclude GDX files (default: "")
#' @param removepattern Pattern to remove from scenario names (default: "")
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures (default: "png")
#' @param add_historical Logical, add historical data where available (default: TRUE)
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param map_var_hist Data frame mapping model variables to historical data. If NULL, uses default mapping.
#' @param launch Logical, launch Shiny app immediately (default: TRUE)
#' @param ... Additional options passed to session configuration
#'
#' @return Invisibly returns NULL. Launches Shiny application if launch=TRUE.
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   run_rice()
#'
#'   # Custom regional aggregation
#'   run_rice(reg_id = "ed57", year0 = 2020, tstep = 10)
#'
#'   # Specify custom paths
#'   run_rice(model_dir = "../RICE50x", results_dir = "results")
#' }
#'
#' @export
run_rice <- function(model_dir="../", results_dir="./", reg_id="ed58", year0=2015, tstep=5, restrict_files="results_", exclude_files="", removepattern="results_", launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Get global-only options from options() with defaults
deploy_online <- getOption("deploy_online", FALSE)
figure_format <- getOption("figure_format", "png")
add_historical <- getOption("add_historical", TRUE)
write_plotdata_csv <- getOption("write_plotdata_csv", FALSE)
# Set all options
opts <- list(model_dir=model_dir, results_dir=results_dir, reg_id=reg_id, year0=year0, tstep=tstep, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("model_dir", model_dir, envir=.GlobalEnv)
assign("subdir", basename(results_dir), envir=.GlobalEnv)
assign("fullpathdir", results_dir, envir=.GlobalEnv)
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
#' @param model_dir Path to FIDELIO model source directory (default: "../")
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
#'   run_fidelio(model_dir = "../fidelio", results_dir = "results")
#' }
#'
#' @export
run_fidelio <- function(model_dir="../", results_dir="./", restrict_files="results_", exclude_files="", removepattern="results_", launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Get global-only options from options() with defaults
deploy_online <- getOption("deploy_online", FALSE)
figure_format <- getOption("figure_format", "png")
add_historical <- getOption("add_historical", TRUE)
write_plotdata_csv <- getOption("write_plotdata_csv", FALSE)
# Set all options
opts <- list(model_dir=model_dir, results_dir=results_dir, restrict_files=restrict_files, exclude_files=exclude_files, removepattern=removepattern, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("model_dir", model_dir, envir=.GlobalEnv)
assign("subdir", basename(results_dir), envir=.GlobalEnv)
assign("fullpathdir", results_dir, envir=.GlobalEnv)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("removepattern", removepattern, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
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
#'
#' @param model_dir Path to model source directory (default: "../")
#' @param results_dir Path to directory containing IAMC format files (default: "./")
#' @param reg_id Regional aggregation(s) to display, e.g., c("witch20", "global") (default: c("witch20", "global"))
#' @param iamc_filename Specific IAMC file to load (CSV, XLSX, or CSV.ZIP). If NULL, loads all CSV/XLSX files in results_dir (default: NULL)
#' @param iamc_databasename Name of IIASA database to connect to (e.g., "ENGAGE"). Alternative to iamc_filename (default: NULL)
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
#'   # Connect to IIASA database
#'   run_iiasadb(iamc_databasename = "ENGAGE")
#' }
#'
#' @export
run_iiasadb <- function(model_dir="../", results_dir="./", reg_id=c("witch20", "global"), iamc_filename=NULL, iamc_databasename=NULL, launch=TRUE, ...) {
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Get global-only options from options() with defaults
deploy_online <- getOption("deploy_online", FALSE)
figure_format <- getOption("figure_format", "png")
add_historical <- getOption("add_historical", TRUE)
write_plotdata_csv <- getOption("write_plotdata_csv", FALSE)
# Set all options
opts <- list(model_dir=model_dir, results_dir=results_dir, reg_id=reg_id, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("model_dir", model_dir, envir=.GlobalEnv)
assign("subdir", basename(results_dir), envir=.GlobalEnv)
assign("fullpathdir", results_dir, envir=.GlobalEnv)
assign("reg_id", reg_id, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
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
snapshot_file <- system.file("gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata", package="witchplot")
if(file.exists(snapshot_file)) {
input <- menu(c("Yes", "No"), title="There is a snapshot saved. Do you want to load it locally?")
if(input==1) load(snapshot_file, envir=.GlobalEnv)
} else {
iiasadb_snapshot <- get_iiasadb(database=iamc_databasename, varlist="*", region="World", modlist="*", scenlist="*", add_metadata=FALSE)
names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, YEAR, VALUE) %>% dplyr::rename(value=VALUE) %>% dplyr::filter(!is.na(value))
assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
}
} else {
results_path <- if(length(results_dir)==1) results_dir else results_dir[1]
# If iamc_filename is NULL, automatically find and combine all CSV and XLSX files
if(is.null(iamc_filename)) {
csv_files <- list.files(results_path, pattern="\\.csv$", full.names=FALSE, ignore.case=TRUE)
csv_files <- csv_files[!stringr::str_detect(csv_files, "\\.zip$")]  # Exclude .csv.zip files
xlsx_files <- list.files(results_path, pattern="\\.xlsx$", full.names=FALSE, ignore.case=TRUE)
csvzip_files <- list.files(results_path, pattern="\\.csv\\.zip$", full.names=FALSE, ignore.case=TRUE)
all_files <- c(csv_files, xlsx_files, csvzip_files)
if(length(all_files)==0) stop("No CSV or XLSX files found in results_dir: ", results_path)
message("Found ", length(all_files), " file(s) to load: ", paste(all_files, collapse=", "))
# Load and combine all files
file_list <- list()
for(fname in all_files) {
message("Loading: ", fname)
if(stringr::str_detect(fname, "\\.xlsx$")) {
file_data <- openxlsx::read.xlsx(file.path(results_path, fname), sheet=1)
} else if(stringr::str_detect(fname, "\\.csv\\.zip$")) {
file_data <- data.table::fread(cmd=paste0('unzip -cq "', file.path(results_path, fname), '" ', gsub(".zip", "", basename(fname))), header=TRUE, quote="\"", sep=",", check.names=FALSE)
} else {
file_data <- data.table::fread(file.path(results_path, fname), header=TRUE, quote="\"", sep=",", check.names=FALSE)
}
names(file_data) <- toupper(names(file_data))
file_list[[fname]] <- file_data
}
iiasadb_snapshot <- data.table::rbindlist(file_list, fill=TRUE)
message("Combined ", length(file_list), " files with ", nrow(iiasadb_snapshot), " total rows")
} else {
# Load specific file if iamc_filename is provided
if(stringr::str_detect(iamc_filename, "\\.xlsx$")) {
iiasadb_snapshot <- openxlsx::read.xlsx(file.path(results_path, iamc_filename), sheet=1)
names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
}
if(stringr::str_detect(iamc_filename, "\\.csv\\.zip$")) {
iiasadb_snapshot <- data.table::fread(cmd=paste0('unzip -cq "', file.path(results_path, iamc_filename), '" ', gsub(".zip", "", basename(iamc_filename))), header=TRUE, quote="\"", sep=",", check.names=FALSE)
names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
}
if(stringr::str_detect(iamc_filename, "\\.csv$") && !stringr::str_detect(iamc_filename, "\\.csv\\.zip$")) {
iiasadb_snapshot <- data.table::fread(file.path(results_path, iamc_filename), header=TRUE, quote="\"", sep=",", check.names=FALSE)
names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
}
}
iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::mutate(dplyr::across(matches("^\\d{4}$"), ~suppressWarnings(as.numeric(.x))))
iiasadb_snapshot <- iiasadb_snapshot %>% tidyr::pivot_longer(cols=-c(MODEL, SCENARIO, REGION, VARIABLE, UNIT), names_to="YEAR") %>% dplyr::mutate(YEAR=as.integer(YEAR)) %>% as.data.frame()
assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
}
iiasadb_snapshot <- iiasadb_snapshot %>% dplyr::mutate(REGION=toupper(REGION))
if(!exists("iiasadb_snapshot")) stop("Please check you specified a correct iiasadb file or connection.")
iiasadb_with_historical <- list()
for(varname in map_var_hist$varname_model) {
if(nrow(iiasadb_snapshot %>% dplyr::filter(VARIABLE==varname))>0) {
iiasadb_with_historical[[varname]] <- add_historical_values(iiasadb_snapshot %>% dplyr::filter(VARIABLE==varname), varname=varname, iiasadb=TRUE, verbose=FALSE)
}
}
iiasadb_historical <- data.table::rbindlist(iiasadb_with_historical) %>% dplyr::filter(stringr::str_detect(SCENARIO, "historical")) %>% as.data.frame()
assign("iiasadb_snapshot", iiasadb_snapshot, envir=.GlobalEnv)
assign("iiasadb_historical", iiasadb_historical, envir=.GlobalEnv)

# Save the snapshot - try installed package location first, fall back to inst/ directory
save_path <- system.file("gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata", package="witchplot")
if(save_path == "" || !dir.exists(dirname(save_path))) {
  # Package not installed or path doesn't exist, save to inst/ directory instead
  inst_path <- file.path("inst", "gdxcompaR", "iiasadb", "iiasadb_snapshot.Rdata")
  if(dir.exists(dirname(inst_path))) {
    save_path <- inst_path
    message("Saved snapshot to: ", save_path)
    save(iiasadb_snapshot, iiasadb_historical, file=save_path)
  } else {
    # Silently skip save if neither location exists
    save_path <- NULL
  }
} else {
  message("Saved snapshot to: ", save_path)
  save(iiasadb_snapshot, iiasadb_historical, file=save_path)
}
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "iiasadb", package="witchplot"))
}
