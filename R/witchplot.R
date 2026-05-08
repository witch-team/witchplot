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

# Show a friendly startup message if GAMS cannot be found.
# IMPORTANT: never call gdxtools::igdx() here — it segfaults when GAMS is missing.
# Instead, detect GAMS by looking for the executable.
if (!.gams_available()) {
  packageStartupMessage(
    "Note: GAMS could not be found on this system.\n",
    "  run_witch(), run_rice() and run_fidelio() need GAMS to read GDX files.\n",
    "  Download the free GAMS Community Edition: https://gams.com/download/\n",
    "  run_iiasadb() works without GAMS."
  )
}
}

# Internal: detect GAMS without calling igdx() (which segfaults when GAMS is absent).
.gams_available <- function() {
  # Check PATH for gams executable
  if (nchar(Sys.which("gams")) > 0) return(TRUE)
  # Check GAMSDIR environment variable
  gams_dir <- Sys.getenv("GAMSDIR")
  if (nchar(gams_dir) > 0 && dir.exists(gams_dir)) return(TRUE)
  # Check common Windows installation directory C:/GAMS/<version>/
  if (.Platform$OS.type == "windows") {
    gams_base <- "C:/GAMS"
    if (dir.exists(gams_base) && length(list.dirs(gams_base, recursive=FALSE)) > 0) return(TRUE)
  }
  # Check common Linux/Mac paths
  if (.Platform$OS.type == "unix") {
    if (any(dir.exists(c("/opt/gams", "/usr/local/gams", path.expand("~/gams"))))) return(TRUE)
  }
  FALSE
}

# Internal helper: stop with a friendly message if GAMS is not available
.require_gams <- function() {
  if (!.gams_available()) {
    stop(
      "GAMS is not installed or could not be found on this system.\n",
      "  run_witch(), run_rice() and run_fidelio() require GAMS to read GDX files.\n",
      "  Download the free GAMS Community Edition: https://gams.com/download/\n",
      "  After installing GAMS, restart R and try again.\n",
      "  Tip: run_iiasadb() works without GAMS.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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
    "iiasadb_data", "iiasadb_historical",
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
.require_gams()
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
.require_gams()
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
.require_gams()
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
#' Loads IAM scenario data in IAMC format (CSV/XLSX files or IIASA database
#' connection) and launches an interactive Shiny application for comparing
#' scenarios. This function provides a simple R interface to the
#' \href{https://pyam-iamc.readthedocs.io/}{pyam} Python library (via
#' \code{reticulate}), which is the standard toolkit for working with IAMC
#' scenario data.
#'
#' By default (\code{iamc_filename=NULL}), automatically discovers and loads all
#' CSV and XLSX files in the \code{results_dir}. Files are combined into a
#' single dataset for comparison. Supports multiple directories - pass as a
#' vector to load and compare across directories.
#'
#' @param results_dir Path(s) to director(ies) containing IAMC format files.
#'   Can be a vector for multiple directories (default: "./")
#' @param reg_id Regional aggregation(s) to display (default: \code{c("r5")})
#' @param iamc_filename Specific IAMC file to load (CSV, XLSX, or CSV.ZIP).
#'   If NULL, loads all CSV/XLSX files in \code{results_dir} (default: NULL)
#' @param iamc_databasename Name of IIASA database/platform to connect to
#'   (e.g., \code{"ar6-public"}, \code{"ENGAGE"}). Alternative to
#'   \code{iamc_filename} (default: NULL). Old-format instances show a grey
#'   icon in the IIASA web UI; new ixmp4-format instances show a blue icon.
#' @param restrict_files Pattern to restrict which files are loaded
#'   (default: \code{""} = load all). Only files matching this pattern are
#'   included.
#' @param exclude_files Pattern to exclude files from loading
#'   (default: \code{""} = exclude none). Files matching this pattern are
#'   skipped.
#' @param deploy_online Logical, whether to deploy online (default: FALSE)
#' @param figure_format Output format for figures: "png", "pdf", "svg"
#'   (default: "png")
#' @param add_historical Logical, add historical data where available
#'   (default: TRUE)
#' @param write_plotdata_csv Logical, save plot data as CSV (default: FALSE)
#' @param map_var_hist Data frame mapping IAMC variables to historical data
#'   sources. If NULL, uses default mapping from
#'   \code{inst/config/map_var_hist_iiasadb.csv}.
#' @param launch Logical, launch Shiny app immediately (default: TRUE). Set
#'   FALSE to load data only (for scripting with \code{get_iiasadb()}).
#' @param run_pyam If non-NULL, run a pyam/ixmp4 API query instead of
#'   launching the Shiny app. Requires \code{reticulate} and a working Python
#'   environment with \code{pyam} installed (\code{pip install pyam-iamc}).
#'   Supported values: \cr
#'   \emph{Old-format instances (grey icon in IIASA web UI):} \cr
#'   \itemize{
#'     \item \code{"list_platforms"} - list all available IIASA platforms
#'       (via \code{pyam.iiasa.platforms()}, no database needed)
#'     \item \code{"valid_connections"} - list platforms accessible with your
#'       credentials (no database needed; old-format only)
#'     \item \code{"list_models"} - list all models in \code{iamc_databasename}
#'       (via \code{pyam.iiasa.Connection(db).models()})
#'     \item \code{"list_scenarios"} - list all scenarios in the database
#'       (via \code{Connection(db).scenarios()})
#'     \item \code{"list_variables"} - list all variables in the database
#'       (via \code{Connection(db).variables()})
#'     \item \code{"list_regions"} - list all regions in the database
#'       (via \code{Connection(db).regions()})
#'     \item \code{"meta"} - get model/scenario metadata as a data frame
#'       (via \code{Connection(db).meta()})
#'     \item \code{"meta_columns"} - list available metadata columns
#'       (via \code{Connection(db).meta_columns})
#'     \item \code{"index"} - get model-scenario index as a data frame
#'       (via \code{Connection(db).index()})
#'   }
#'   \emph{New ixmp4-format instances (blue icon in IIASA web UI):} \cr
#'   \itemize{
#'     \item \code{"ixmp4_list_runs"} - list all model/scenario runs
#'       (via \code{ixmp4.Platform(db).runs.tabulate()})
#'     \item \code{"ixmp4_variables"} - list all variables
#'       (via \code{Platform(db).iamc.variables.tabulate()})
#'     \item \code{"ixmp4_regions"} - list all regions
#'       (via \code{Platform(db).regions.tabulate()})
#'     \item \code{"ixmp4_units"} - list all units
#'       (via \code{Platform(db).units.tabulate()})
#'   }
#' @param creds Optional credentials for private IIASA databases. Pass as a
#'   named list: \code{list(username="user@email.com", password="secret")}.
#'   For persistent login use \code{iiasa_login()} instead.
#' @param reglist Regions to download when using \code{iamc_databasename}.
#'   Default \code{"common"} automatically resolves to aggregate regions
#'   (R5, R10, World, etc.) via the ixmp4 API. Pass a character vector for
#'   custom region lists, or \code{"World"} for global totals only.
#' @param varlist Variables to download when using \code{iamc_databasename}.
#'   Default \code{"*"} downloads all variables. Pass a character vector to
#'   restrict (e.g., \code{c("Emissions|CO2", "GDP|PPP")}).
#' @param modlist Models to download when using \code{iamc_databasename}.
#'   Default \code{"*"} downloads all models.
#' @param scenlist Scenarios to download when using \code{iamc_databasename}.
#'   Default \code{"*"} downloads all scenarios. Pass a character vector to
#'   restrict (e.g., \code{c("SSP2-Baseline", "SSP2-1p5C")}). Use
#'   \code{run_iiasadb(run_pyam="list_scenarios")} first to see available names.
#' @param ... Additional options passed to session configuration
#'
#' @return Invisibly returns NULL. Launches Shiny application if
#'   \code{launch=TRUE}. If \code{run_pyam} is set, returns the query result
#'   invisibly and does not launch the app.
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
#'   # Connect to IIASA database (ixmp4 / blue-icon platform)
#'   run_iiasadb(iamc_databasename = "ENGAGE")
#'
#'   # Load only files matching a pattern
#'   run_iiasadb(restrict_files = "SPARCCLE")
#'
#'   # Exclude files matching a pattern
#'   run_iiasadb(exclude_files = "template")
#'
#'   # List all available IIASA platforms (no database needed)
#'   run_iiasadb(run_pyam = "list_platforms")
#'
#'   # List platforms accessible with your credentials (old-format only)
#'   run_iiasadb(run_pyam = "valid_connections")
#'
#'   # List models in a database
#'   run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "list_models")
#'
#'   # List variables available in a database
#'   run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "list_variables")
#'
#'   # List regions in a database
#'   run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "list_regions")
#'
#'   # Get model/scenario metadata
#'   meta <- run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "meta")
#'
#'   # Get model-scenario index
#'   idx <- run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "index")
#'
#'   # Query an ixmp4-format (blue icon) platform
#'   run_iiasadb(iamc_databasename = "ENGAGE", run_pyam = "ixmp4_list_runs")
#'   run_iiasadb(iamc_databasename = "ENGAGE", run_pyam = "ixmp4_variables")
#'
#'   # Login to access private databases
#'   iiasa_login("your@email.com")
#'   run_iiasadb(iamc_databasename = "private-db")
#'
#'   # Or pass credentials directly
#'   run_iiasadb(
#'     iamc_databasename = "private-db",
#'     creds = list(username = "user@email.com", password = "secret")
#'   )
#'
#'   # Download only specific variables, regions and scenarios
#'   run_iiasadb(
#'     iamc_databasename = "ar6-public",
#'     varlist  = c("Emissions|CO2", "GDP|PPP", "Population"),
#'     reglist  = c("World", "R5ASIA", "R5LAM"),
#'     scenlist = c("SSP2-Baseline", "SSP2-1p5C")
#'   )
#'
#'   # List available scenarios first, then download a subset
#'   run_iiasadb(iamc_databasename = "ar6-public", run_pyam = "list_scenarios")
#'   run_iiasadb(iamc_databasename = "ar6-public", scenlist = c("SSP2-Baseline"))
#' }
#'
#' @export
run_iiasadb <- function(results_dir="./", reg_id=c("r5"), iamc_filename=NULL, iamc_databasename=NULL,
                        restrict_files="", exclude_files="",
                        add_historical=TRUE, deploy_online=FALSE, figure_format="png", write_plotdata_csv=FALSE,
                        launch=TRUE, run_pyam=NULL, creds=NULL, reglist="common", varlist="*", modlist="*", scenlist="*", ...) {
# Handle run_pyam operations: query the IIASA database without loading data
# or launching Shiny. Delegates to pyam_iiasa() via .run_pyam_iiasa().
# Supported values: "list_platforms", "list_models", "list_scenarios",
#   "list_variables", "list_regions", "meta", "meta_columns", "index"
if (!is.null(run_pyam)) {
  pyam <- .ensure_pyam()
  .check_ixmp4_auth()
  return(invisible(.run_pyam_iiasa(pyam, iamc_databasename, run_pyam, creds=creds)))
}
# Clean up any global variables from previous sessions
.cleanup_witchplot_globals()
if(!is.vector(results_dir)) results_dir <- c(results_dir)
# Normalize results_dir to avoid double slashes in file paths
results_dir <- normalizePath(results_dir, winslash="/", mustWork=FALSE)
# Set all options
opts <- list(results_dir=results_dir, reg_id=reg_id, restrict_files=restrict_files, exclude_files=exclude_files, deploy_online=deploy_online, figure_format=figure_format, add_historical=add_historical, write_plotdata_csv=write_plotdata_csv, ...)
options(opts)
assign("restrict_files", restrict_files, envir=.GlobalEnv)
assign("exclude_files", exclude_files, envir=.GlobalEnv)
assign("results_dir", results_dir, envir=.GlobalEnv)
assign("reg_id", reg_id, envir=.GlobalEnv)
assign("deploy_online", deploy_online, envir=.GlobalEnv)
assign("figure_format", figure_format, envir=.GlobalEnv)
assign("add_historical", add_historical, envir=.GlobalEnv)
assign("write_plotdata_csv", write_plotdata_csv, envir=.GlobalEnv)
assign("graphdir", file.path(results_dir[1], "graphs"), envir=.GlobalEnv)
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
# Try to find cached parquet in results_dir first, then fall back to package
cache_file <- NULL
if(exists("results_dir") && length(results_dir) > 0) {
  results_cache <- file.path(results_dir[1], "iiasadb_data.parquet")
  if(file.exists(results_cache)) {
    cache_file <- results_cache
  }
}
# Fall back to package location if not found in results_dir
if(is.null(cache_file)) {
  pkg_cache <- system.file("gdxcompaR", "iiasadb", "iiasadb_data.parquet", package="witchplot")
  if(file.exists(pkg_cache)) {
    cache_file <- pkg_cache
  }
}

load_from_db <- TRUE
snapshot_loaded_from_file <- FALSE
if(!is.null(cache_file)) {
input <- menu(c("Yes", "No"), title="There is a cached dataset available. Do you want to load it?")
if(input==1) {
  iiasadb_data <- arrow::read_parquet(cache_file)
  assign("iiasadb_data", iiasadb_data, envir=.GlobalEnv)
  hist_cache <- sub("iiasadb_data\\.parquet$", "iiasadb_historical.parquet", cache_file)
  if(file.exists(hist_cache)) {
    assign("iiasadb_historical", arrow::read_parquet(hist_cache), envir=.GlobalEnv)
  } else {
    assign("iiasadb_historical", data.frame(), envir=.GlobalEnv)
  }
  message("Loaded cached data from: ", cache_file)
  load_from_db <- FALSE
  snapshot_loaded_from_file <- TRUE
}
}
if(load_from_db) {
  # Inject creds into ixmp4 credential store early so both region resolution
  # and Platform creation (inside download_iiasadb) can authenticate.
  if (!is.null(creds) && !is.null(creds$username) && !is.null(creds$password)) {
    tryCatch({
      ixmp4_tmp <- reticulate::import("ixmp4", convert=FALSE)
      s <- ixmp4_tmp$conf$settings
      manager_url_str <- as.character(reticulate::py_to_r(s$manager_url))
      s$credentials$set(manager_url_str, creds$username, creds$password)
      s$credentials$dump()
    }, error = function(e) NULL)
  }

  # Resolve reglist="common": fetch aggregate hierarchy regions, with two-stage fallback.
  # Stage 1: ixmp4 Platform (works for ixmp4-format / blue-icon databases).
  # Stage 2: pyam Connection (works for old-format / grey-icon databases).
  # Stage 3: fall back to "*" (all regions via pyam wildcard).
  if(identical(reglist, "common") && !is.null(iamc_databasename)) {
    resolved <- FALSE
    # Stage 1: ixmp4 region hierarchy (ixmp4-format databases)
    tryCatch({
      ixmp4 <- reticulate::import("ixmp4", convert=FALSE)
      reg_df <- as.data.frame(reticulate::py_to_r(ixmp4$Platform(iamc_databasename)$regions$tabulate()))
      common_hierarchies <- c("common", "R5", "R9", "R10", "Regional Organizations")
      reglist <- c(reg_df$name[reg_df$hierarchy %in% common_hierarchies],
                   c("France", "Germany", "Spain", "Italy"))
      reglist <- unique(reglist[!is.na(reglist)])
      message("Resolved 'common' to ", length(reglist), " regions (via ixmp4).")
      resolved <- TRUE
    }, error = function(e) {
      message("ixmp4 region lookup failed (", conditionMessage(e), ") — trying pyam connection...")
    })
    # Stage 2: pyam Connection regions + heuristic for aggregates (old-format databases)
    if (!resolved) {
      tryCatch({
        pyam_tmp <- .ensure_pyam()
        conn <- if (!is.null(creds)) pyam_tmp$iiasa$Connection(iamc_databasename, creds=creds)
                else pyam_tmp$iiasa$Connection(iamc_databasename)
        all_regions <- as.character(reticulate::py_to_r(conn$regions()))
        # Keep known aggregate patterns; if heuristic yields nothing, keep all
        agg_pattern <- "^World$|^R5|^R10|^R9|OECD|LAM|ASIA|MAF|REF|\\bEU\\b|^Global$"
        common_regs <- all_regions[grepl(agg_pattern, all_regions, ignore.case=TRUE)]
        reglist <<- if (length(common_regs) > 0) common_regs else all_regions
        message("Resolved 'common' to ", length(reglist), " regions (via pyam connection).")
        resolved <<- TRUE
      }, error = function(e) {
        message("pyam region lookup also failed (", conditionMessage(e), ").")
      })
    }
    # Stage 3: wildcard fallback — download all regions
    if (!resolved) {
      message("Could not resolve 'common' regions. Falling back to reglist='*' (all regions).\n",
              "Tip: specify reglist= manually or run run_iiasadb(run_pyam='list_regions') first.\n",
              "For auth errors, store credentials with: iiasa_login('your@email.com')")
      reglist <- "*"
    }
  }
  message("Fetching data from IIASA database: ", iamc_databasename)
  partial_path <- file.path(results_dir[1], "iiasadb_partial.Rdata")
  iiasadb_data <- tryCatch(
    download_iiasadb(database=iamc_databasename, varlist=varlist, reglist=reglist, modlist=modlist, scenlist=scenlist, add_metadata=FALSE, autosave_path=partial_path, creds=creds),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("401|403|Unauthorized|forbidden|authentication|credentials|permission|access.denied|login|insufficient.permissions|denied", msg, ignore.case=TRUE)) {
        stop("Access denied to '", iamc_databasename, "'.\n",
             "This database requires authentication. Store credentials once with:\n",
             "  iiasa_login('your@email.com')\n",
             "Or pass directly: run_iiasadb(iamc_databasename='", iamc_databasename,
             "', creds=list(username='...', password='...'))\n",
             "Original error: ", msg, call.=FALSE)
      }
      stop(msg, call.=FALSE)
    }
  )
  names(iiasadb_data) <- toupper(names(iiasadb_data))
  iiasadb_data <- iiasadb_data %>% dplyr::select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, YEAR, VALUE) %>% dplyr::rename(value=VALUE) %>% dplyr::filter(!is.na(value))
  assign("iiasadb_data", iiasadb_data, envir=.GlobalEnv)
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

    # Exclude Excel/Office lock files (e.g. ~$filename.xlsx)
    all_files <- all_files[!stringr::str_detect(all_files, "^~\\$")]

    # Apply restrict_files filter (keep only files matching pattern)
    if(restrict_files != "") {
      all_files <- all_files[stringr::str_detect(all_files, restrict_files)]
    }
    # Apply exclude_files filter (remove files matching pattern)
    if(exclude_files != "") {
      all_files <- all_files[!stringr::str_detect(all_files, exclude_files)]
    }

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

iiasadb_data <- data.table::rbindlist(file_list, fill=TRUE)
message("\nCombined ", total_files, " file(s) from ", length(results_dir), " director(ies) with ", nrow(iiasadb_data), " total rows")

# Convert year columns to numeric and pivot longer
iiasadb_data <- iiasadb_data %>% dplyr::mutate(dplyr::across(matches("^\\d{4}$"), ~suppressWarnings(as.numeric(.x))))

# Determine which columns to keep (not year columns)
if(length(results_dir) > 1) {
  non_year_cols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT", "PATHDIR")
} else {
  non_year_cols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT")
}

iiasadb_data <- iiasadb_data %>%
  tidyr::pivot_longer(cols=-dplyr::all_of(non_year_cols), names_to="YEAR") %>%
  dplyr::mutate(YEAR=as.integer(YEAR)) %>%
  as.data.frame()

assign("iiasadb_data", iiasadb_data, envir=.GlobalEnv)
}
iiasadb_data <- iiasadb_data %>% dplyr::mutate(REGION=toupper(REGION))
if(!exists("iiasadb_data")) stop("Please check you specified a correct iiasadb file or connection.")

# Pre-load historical data if add_historical is enabled
if(add_historical) {
  iiasadb_with_historical <- list()
  for(varname in map_var_hist$varname_model) {
    if(nrow(iiasadb_data %>% dplyr::filter(VARIABLE==varname))>0) {
      iiasadb_with_historical[[varname]] <- add_historical_values(iiasadb_data %>% dplyr::filter(VARIABLE==varname), varname=varname, iiasadb=TRUE, verbose=FALSE)
    }
  }
  if (length(iiasadb_with_historical) > 0) {
    iiasadb_historical <- data.table::rbindlist(iiasadb_with_historical) %>% dplyr::filter(stringr::str_detect(SCENARIO, "historical")) %>% as.data.frame()
  } else {
    iiasadb_historical <- data.frame()
  }
} else {
  iiasadb_historical <- data.frame()
}

assign("iiasadb_data", iiasadb_data, envir=.GlobalEnv)
assign("iiasadb_historical", iiasadb_historical, envir=.GlobalEnv)

# Save the snapshot only if we fetched new data (not if we loaded from existing snapshot)
# For iamc_databasename: only save if we downloaded from DB
# For iamc_filename/files: always save since we loaded from files
should_save <- (!exists("snapshot_loaded_from_file") || !snapshot_loaded_from_file)

if(should_save) {
  save_dir <- if(exists("results_dir") && length(results_dir) > 0) results_dir[1] else NULL
  if(is.null(save_dir)) {
    # Fall back to inst/ when called from the source tree
    inst_dir <- file.path("inst", "gdxcompaR", "iiasadb")
    if(dir.exists(inst_dir)) save_dir <- inst_dir
  }
  if(!is.null(save_dir)) {
    data_path <- file.path(save_dir, "iiasadb_data.parquet")
    hist_path <- file.path(save_dir, "iiasadb_historical.parquet")
    arrow::write_parquet(iiasadb_data, data_path)
    if(nrow(iiasadb_historical) > 0)
      arrow::write_parquet(iiasadb_historical, hist_path)
    message("Saved data to: ", data_path)
  }
}
if(launch) shiny::runApp(appDir=system.file("gdxcompaR", "iiasadb", package="witchplot"))
}
