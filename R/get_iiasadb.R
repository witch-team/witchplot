#First install these packages if not yet available (for standalone mode)
#require(tidyverse)
#require(reticulate)
#require(yaml)

# Helper: Ensure pyam is available, auto-installing if needed.
# Supports both reticulate ephemeral environments (uv, py_require) and
# traditional environments (py_install).
.ensure_pyam <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("Package 'reticulate' is required to connect to IIASA databases.\n",
         "Install with: install.packages('reticulate')\n",
         "Then install pyam in Python: pip install pyam-iamc", call. = FALSE)

  pyam <- tryCatch(
    reticulate::import("pyam", convert = FALSE),
    error = function(e) NULL
  )

  if (is.null(pyam)) {
    message("Python package 'pyam' not found. Installing pyam-iamc...")
    # reticulate >= 1.40 with uv uses ephemeral environments: py_require() is
    # the correct installer. Older reticulate uses py_install(pip=TRUE).
    has_py_require <- exists("py_require", envir = asNamespace("reticulate"),
                             mode = "function", inherits = FALSE)
    tryCatch(
      if (has_py_require) reticulate::py_require("pyam-iamc")
      else reticulate::py_install("pyam-iamc", pip = TRUE),
      error = function(e)
        stop("Failed to install pyam-iamc: ", conditionMessage(e), "\n",
             "Please install manually:\n",
             "  In R:      reticulate::py_require('pyam-iamc')   # uv / ephemeral env\n",
             "  In Python: pip install pyam-iamc                 # traditional env\n",
             "Tip: check your Python setup with reticulate::py_config()", call. = FALSE)
    )
    message("pyam-iamc installed. Importing pyam...")
    pyam <- tryCatch(
      reticulate::import("pyam", convert = FALSE),
      error = function(e)
        stop("pyam was installed but cannot be imported: ", conditionMessage(e), "\n",
             "Try restarting R and calling the function again.", call. = FALSE)
    )
    message("pyam loaded successfully.")
  }

  pyam
}

# Helper: Check ixmp4 authentication across platforms (Windows, Linux, Mac)
.check_ixmp4_auth <- function() {
  # On Windows, R's ~ expands to Documents, not the actual home dir.
  # Use USERPROFILE to get C:\Users\Username (where ixmp4 actually stores files).
  userprofile <- Sys.getenv("USERPROFILE")
  config_paths <- c(
    path.expand("~/.config/ixmp4"),                                   # Linux/Mac XDG config
    path.expand("~/.local/share/ixmp4"),                              # Linux XDG data (and R ~ on Linux)
    if (nchar(userprofile) > 0) file.path(userprofile, ".local", "share", "ixmp4"),  # Windows actual home
    file.path(Sys.getenv("APPDATA"), "ixmp4"),                        # Windows Roaming AppData
    file.path(Sys.getenv("LOCALAPPDATA"), "ixmp4", "ixmp4"),          # Windows Local AppData (platformdirs)
    file.path(Sys.getenv("LOCALAPPDATA"), "ixmp4")                    # Windows Local AppData (alt)
  )
  # Drop NULL/empty paths
  config_paths <- config_paths[!is.null(config_paths) & nchar(config_paths) > 10]
  # Check for directory OR credentials.toml file
  has_login <- any(sapply(config_paths, dir.exists)) ||
               any(sapply(config_paths, function(p) file.exists(file.path(p, "credentials.toml"))))
  if (!has_login) {
    message("No stored IIASA credentials found. Public databases are accessible without login.\n",
            "For private databases, use iiasa_login('username') or pass creds= argument.")
  }
  invisible(has_login)
}

# Helper: Dispatch pyam IIASA Connection API functions
# See: https://pyam-iamc.readthedocs.io/en/stable/api/iiasa.html
#
# OLD-FORMAT instances (grey icon in IIASA web UI) - use pyam.iiasa.Connection:
#   "list_platforms"      - pyam.iiasa.platforms()         (no database needed)
#   "valid_connections"   - Connection().valid_connections  (no database needed; shows OLD-format platforms)
#   "list_models"         - Connection(db).models()
#   "list_scenarios"      - Connection(db).scenarios()
#   "list_variables"      - Connection(db).variables()
#   "list_regions"        - Connection(db).regions()
#   "meta"                - Connection(db).meta()
#   "meta_columns"        - Connection(db).meta_columns
#   "index"               - Connection(db).index()
#
# IXMP4-FORMAT instances (blue icon in IIASA web UI) - use ixmp4.Platform directly:
#   "ixmp4_list_runs"     - Platform(db).runs.tabulate()
#   "ixmp4_variables"     - Platform(db).iamc.variables.tabulate()
#   "ixmp4_regions"       - Platform(db).regions.tabulate()
#   "ixmp4_units"         - Platform(db).units.tabulate()
.run_pyam_iiasa <- function(pyam, database=NULL, operation, creds=NULL) {
  needs_db <- !operation %in% c("list_platforms", "valid_connections")
  if (needs_db && is.null(database)) {
    stop("'iamc_databasename' is required for run_pyam='", operation, "'.\n",
         "Use run_iiasadb(iamc_databasename='mydb', run_pyam='", operation, "')")
  }
  # Helper to create ixmp4 Platform (for ixmp4-format instances, blue icon in web UI)
  .ixmp4_platform <- function() {
    ixmp4 <- import("ixmp4", convert=FALSE)
    ixmp4$Platform(database)
  }
  # Helper to create Connection, passing creds if provided
  .conn <- function() {
    if (!is.null(creds)) pyam$iiasa$Connection(database, creds=creds)
    else pyam$iiasa$Connection(database)
  }
  switch(operation,
    "list_platforms" = {
      message("Fetching available IIASA platforms (via ixmp4)...")
      # Widen pandas display to prevent truncation of Name/Notice columns
      pd <- import("pandas", convert=FALSE)
      old_width <- tryCatch(py_to_r(pd$get_option("display.max_colwidth")), error=function(e) 50)
      pd$set_option("display.max_colwidth", 200L)
      pd$set_option("display.width", 300L)
      result <- tryCatch(
        py_to_r(pyam$iiasa$platforms()),
        error = function(e) { message("Error fetching platforms: ", e$message); NULL }
      )
      pd$set_option("display.max_colwidth", old_width)
      if (!is.null(result)) print(result)
      return(invisible(result))
    },
    "valid_connections" = {
      message("Fetching platforms accessible with your credentials...")
      conn <- pyam$iiasa$Connection()
      result <- as.character(py_to_r(conn$valid_connections))
      cat("Accessible platforms (", length(result), " total):\n", sep="")
      print(result)
      message("Note: Only old-format instances appear above. ixmp4-format instances (blue icon in IIASA web UI)\n",
              "are not listed here. Use run_pyam='list_platforms' to see ixmp4 databases.")
      return(invisible(result))
    },
    "list_models" = {
      result <- as.character(py_to_r(.conn()$models()))
      cat("Models in '", database, "' (", length(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "list_scenarios" = {
      result <- as.character(py_to_r(.conn()$scenarios()))
      cat("Scenarios in '", database, "' (", length(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "list_variables" = {
      result <- as.character(py_to_r(.conn()$variables()))
      cat("Variables in '", database, "' (", length(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "list_regions" = {
      result <- as.character(py_to_r(.conn()$regions()))
      cat("Regions in '", database, "' (", length(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "meta" = {
      result <- as.data.frame(py_to_r(.conn()$meta()))
      cat("Metadata for '", database, "' (", nrow(result), " scenarios):\n", sep="")
      print(head(result, 20))
      if (nrow(result) > 20) message("... showing first 20 rows. Full result returned invisibly.")
      return(invisible(result))
    },
    "meta_columns" = {
      result <- as.character(py_to_r(.conn()$meta_columns))
      cat("Available meta columns in '", database, "':\n", sep="")
      print(result)
      return(invisible(result))
    },
    "index" = {
      result <- as.data.frame(py_to_r(.conn()$index()))
      cat("Model-Scenario index for '", database, "' (", nrow(result), " entries):\n", sep="")
      print(result)
      return(invisible(result))
    },
    # --- ixmp4-format instances (blue icon in IIASA web UI) ---
    "ixmp4_list_runs" = {
      result <- as.data.frame(py_to_r(.ixmp4_platform()$runs$tabulate()))
      cat("Runs (model/scenario) in ixmp4 platform '", database, "' (", nrow(result), " entries):\n", sep="")
      print(head(result, 30))
      if (nrow(result) > 30) message("... showing first 30 rows. Full result returned invisibly.")
      return(invisible(result))
    },
    "ixmp4_variables" = {
      result <- as.data.frame(py_to_r(.ixmp4_platform()$iamc$variables$tabulate()))
      cat("Variables in ixmp4 platform '", database, "' (", nrow(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "ixmp4_regions" = {
      result <- as.data.frame(py_to_r(.ixmp4_platform()$regions$tabulate()))
      cat("Regions in ixmp4 platform '", database, "' (", nrow(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    "ixmp4_units" = {
      result <- as.data.frame(py_to_r(.ixmp4_platform()$units$tabulate()))
      cat("Units in ixmp4 platform '", database, "' (", nrow(result), " total):\n", sep="")
      print(result)
      return(invisible(result))
    },
    stop("Unknown run_pyam operation: '", operation, "'.\n",
         "OLD-format instances: 'list_platforms', 'valid_connections', 'list_models', 'list_scenarios', ",
         "'list_variables', 'list_regions', 'meta', 'meta_columns', 'index'\n",
         "IXMP4-format instances (blue icon): 'ixmp4_list_runs', 'ixmp4_variables', 'ixmp4_regions', 'ixmp4_units'")
  )
}

#' Run pyam IIASA API Functions from R
#'
#' Provides direct access to pyam IIASA Connection API functions, exposing
#' key database query capabilities in R via reticulate.
#'
#' @param operation One of: \code{"list_platforms"}, \code{"valid_connections"},
#'   \code{"list_models"}, \code{"list_scenarios"}, \code{"list_variables"},
#'   \code{"list_regions"}, \code{"meta"}, \code{"meta_columns"}, \code{"index"}
#' @param database Name of the IIASA database/platform (not needed for
#'   \code{"list_platforms"})
#'
#' @return Character vector for list_* / meta_columns operations; data.frame
#'   for meta/index operations; NULL for list_platforms (prints only).
#' @examples
#' \dontrun{
#'   pyam_iiasa("list_platforms")
#'   pyam_iiasa("list_models", database = "ar6-public")
#'   pyam_iiasa("list_variables", database = "ar6-public")
#'   meta <- pyam_iiasa("meta", database = "ar6-public")
#' }
#' @export
pyam_iiasa <- function(operation, database=NULL, creds=NULL) {
  require(reticulate)
  pyam <- import("pyam", convert=FALSE)
  .check_ixmp4_auth()
  .run_pyam_iiasa(pyam, database, operation, creds=creds)
}

#' Login to IIASA database from R
#'
#' Stores an ixmp4 authentication token (equivalent to running
#' \code{ixmp4 login <username>} in the terminal).
#' Only needed for *private* databases; public ones work without login.
#'
#' @param username Your IIASA username (email)
#' @param password Your IIASA password (prompted interactively if NULL)
#' @export
iiasa_login <- function(username, password=NULL) {
  require(reticulate)
  if (is.null(password)) {
    password <- readline(prompt=paste0("IIASA password for '", username, "': "))
  }
  tryCatch({
    ixmp4 <- import("ixmp4")
    settings <- ixmp4$conf$settings
    # credentials.set(key: str, username, password) - key is the manager URL string
    auth_key <- as.character(py_to_r(settings$manager_url))
    settings$credentials$set(auth_key, username, password)
    settings$credentials$dump()  # ixmp4 uses dump() not save() to persist credentials
    message("Successfully logged in as '", username, "'. Credentials stored for future sessions.")
    message("You can now call run_iiasadb() without passing creds=.")
  }, error = function(e) {
    message("Could not store credentials: ", conditionMessage(e),
            "\nNote: IIASA server may be temporarily down.",
            "\nWorkaround: pass creds=list(username='", username,
            "', password='...') directly to run_iiasadb() or pyam_iiasa().")
  })
  invisible(NULL)
}

#Function to download data from IIASA database
download_iiasadb <- function(database="iamc15", varlist="Emissions|CO2", varname=NULL, modlist="*", scenlist="*", reglist="World", show_variables=FALSE, add_metadata=TRUE, run_pyam=NULL, creds=NULL, autosave_path=NULL) {
  pyam <- .ensure_pyam()

  .check_ixmp4_auth()

  # Handle run_pyam operations - early return without downloading data
  if (!is.null(run_pyam)) {
    return(.run_pyam_iiasa(pyam, database, run_pyam, creds=creds))
  }

  #show variables in case
  if(show_variables) {
    conn <- if (!is.null(creds)) pyam$iiasa$Connection(database, creds=creds) else pyam$iiasa$Connection(database)
    result <- py_to_r(conn$variables())
    print(result)
    assign("iiasadb_variables_available", as.data.frame(result), envir=.GlobalEnv)
  }
  # Download with per-model and/or per-region progress when multiple values specified
  # meta=TRUE required (not 1) — pyam checks `meta is True` strictly for ixmp4 platforms
  .fetch_one <- function(mod, reg) {
    args <- list(database, model=mod, scenario=scenlist, variable=varlist, region=reg, meta=TRUE)
    if (!is.null(creds)) args$creds <- creds
    do.call(pyam$read_iiasa, args)
  }
  models_list <- if (length(modlist) > 1) as.list(modlist) else list(modlist)
  region_list <- if (length(reglist) > 1) as.list(reglist) else list(reglist)
  if (length(models_list) > 1 || length(region_list) > 1) {
    total <- length(models_list) * length(region_list)
    # Resume from partial snapshot if available
    partial_results <- list()
    completed_batches <- character(0)
    if (!is.null(autosave_path) && file.exists(autosave_path)) {
      saved_env <- new.env()
      load(autosave_path, envir=saved_env)
      partial_results <- saved_env$partial_results
      completed_batches <- saved_env$completed_batches
      message("Resuming from partial snapshot: ", length(completed_batches), "/", total, " batches already done.")
    }
    t0 <- proc.time()[["elapsed"]]
    k <- 0
    for (mod in models_list) {
      for (reg in region_list) {
        k <- k + 1
        batch_key <- paste0(mod, "|||", reg)
        label <- if (length(models_list) > 1 && length(region_list) > 1)
          paste0(mod, " / ", reg)
        else if (length(models_list) > 1) mod else reg
        if (batch_key %in% completed_batches) {
          message(sprintf("[%d/%d] Skipping (cached): %s", k, total, label))
          next
        }
        message(sprintf("[%d/%d] Downloading: %s  (%.0fs elapsed)",
                        k, total, label, proc.time()[["elapsed"]] - t0))
        res <- tryCatch(
          .fetch_one(mod, reg),
          error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("No scenario data|no data|No data", msg, ignore.case=TRUE))
              message("  -> No data for: ", label, " — skipping.")
            else
              message("  -> Error for: ", label, " — ", msg, " — skipping.")
            NULL
          }
        )
        if (!is.null(res) && !inherits(res, "python.builtin.NoneType")) {
          partial_results[[batch_key]] <- as.data.frame(py_to_r(res$as_pandas()))
          completed_batches <- c(completed_batches, batch_key)
          if (!is.null(autosave_path)) {
            save(partial_results, completed_batches, file=autosave_path)
            message("  -> Partial snapshot saved (", length(completed_batches), "/", total, " batches).")
          }
        } else if (!is.null(res)) {
          # None result (no data) — mark as visited so we don't retry
          completed_batches <- c(completed_batches, batch_key)
        }
      }
    }
    results <- unname(partial_results)
    if (length(results) == 0)
      stop("No data returned from '", database, "'. Check access and filter arguments.")
    # Clean up partial file on successful completion
    if (!is.null(autosave_path) && file.exists(autosave_path)) file.remove(autosave_path)
    iiasadb_df <- data.table::rbindlist(results, fill=TRUE)
    if (!is.null(varname))
      iiasadb_df <- iiasadb_df %>% dplyr::mutate(variable = dplyr::recode(variable, !!!setNames(varname, varlist)))
    return(as.data.frame(iiasadb_df))
  } else {
    iiasadb_data <- .fetch_one(modlist, reglist)
  }

  if (is.null(iiasadb_data) || inherits(iiasadb_data, "python.builtin.NoneType")) {
    stop("No data returned from '", database, "'. Check that you have access to this platform ",
         "and that the specified filters return results.\n",
         "Use run_iiasadb(run_pyam='valid_connections') to see accessible platforms.")
  }
  #If AR6, also add meta categories and other meta data
  if(database == "ar6-public" & add_metadata){
     #as_pandas concatenates data and meta into a pandas DF (meta_cols = TRUE adds all meta data)
    iiasadb_df <- iiasadb_data$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category", "IMP_marker"))
    #pandas to R data frame
    iiasadb_df <- py_to_r(iiasadb_df)
    #all categories are lists, convert to simple vectors
    Policy_category <- data.frame(Policy_category=unlist(iiasadb_df$Policy_category))
    Policy_category_name <- data.frame(Policy_category_name=unlist(iiasadb_df$Policy_category_name))
    Category <- data.frame(Category=unlist(iiasadb_df$Category))
    iiasadb_df <- iiasadb_df %>% select(-c("Policy_category", "Policy_category_name", "Category"))
    iiasadb_df <- cbind(iiasadb_df, Policy_category, Policy_category_name, Category)
    iiasadb_df <- iiasadb_df %>% dplyr::filter(Category!="failed-vetting" & Category!="NaN" & Category!="no-climate-assessment")
  } 
  else
  {
    iiasadb_df <- py_to_r(iiasadb_data$as_pandas())
  }
  if(!is.null(varname)) iiasadb_df <- iiasadb_df %>% mutate(variable = dplyr::recode(variable, !!!setNames(varname, varlist)))
  return(iiasadb_df)
}

#' Load IIASADB Variable from Files
#'
#' Similar to get_witch(), loads IAMC format data from CSV/XLSX files and returns a dataframe
#' in standard WITCH format (n, year, value, file).
#' Data should be loaded at startup using run_iiasadb(launch=FALSE).
#'
#' @param variable_name Name of the IAMC variable to load (e.g., "Population", "GDP|PPP")
#' @param scenplot Vector of scenarios to include (default: all loaded scenarios)
#' @param add_historical Whether to add historical data (default: from global add_historical setting)
#'
#' @return Data frame with columns: n, year, value, file, MODEL, SCENARIO, VARIABLE, UNIT
#' @export
get_iiasadb <- function(variable_name, scenplot = NULL, add_historical = NULL) {
  # Check if IIASADB data is loaded
  if(!exists("iiasadb_data", envir = .GlobalEnv)) {
    stop("IIASADB data not loaded. Please run run_iiasadb(launch=FALSE) first to load data.")
  }

  # Get add_historical setting
  if(is.null(add_historical)) {
    add_historical <- if(exists("add_historical", envir = .GlobalEnv)) get("add_historical", envir = .GlobalEnv) else getOption("add_historical", TRUE)
  }

  # Get data from global environment
  all_data <- get("iiasadb_data", envir = .GlobalEnv)

  # Filter by variable name (exact match, case-insensitive)
  variable_data <- all_data %>%
    filter(toupper(VARIABLE) == toupper(variable_name))

  if(nrow(variable_data) == 0) {
    warning(sprintf("Variable '%s' not found in loaded data", variable_name))
    return(data.frame())
  }

  # Filter by scenarios if specified (before format conversion)
  if(!is.null(scenplot)) {
    variable_data <- variable_data %>% filter(SCENARIO %in% scenplot)
  }

  # Convert from IAMC format to standard WITCH format
  # IAMC: MODEL, SCENARIO, REGION, VARIABLE, UNIT, YEAR, value
  # WITCH: n, year, value, file, (keep MODEL, SCENARIO, VARIABLE, UNIT for reference)
  variable_data <- variable_data %>%
    rename(n = REGION, year = YEAR) %>%
    mutate(
      n = tolower(n),  # Ensure lowercase for consistency
      file = paste(MODEL, SCENARIO, sep = "_")  # Combine MODEL and SCENARIO
    ) %>%
    select(n, year, value, file, MODEL, SCENARIO, VARIABLE, UNIT, everything())

  # Add historical data if requested
  if(add_historical) {
    variable_data <- add_historical_values(variable_data,
                                           varname = variable_name,
                                           verbose = FALSE,
                                           iiasadb = TRUE)
  }

  # Ensure result is a standard data.frame (not tibble or data.table)
  variable_data <- as.data.frame(variable_data)

  return(variable_data)
}
