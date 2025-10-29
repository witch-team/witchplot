## Session Initialization
## This file contains the initialization function called by run_*() functions

#' Initialize witchplot session
#'
#' Sets up the session environment with default options and validates paths.
#' Called internally by run_witch(), run_rice(), run_fidelio(), and run_iiasadb().
#'
#' @return NULL (modifies global environment)
#' @keywords internal
.initialize_witchplot_session <- function() {
  ## Set default options ##
  deploy_online <<- FALSE  # Save graphs if not deployed online
  figure_format <<- "png"
  add_historical <<- TRUE  # Add historical data where available
  ggplot2::theme_set(ggplot2::theme_bw())  # Set default theme
  write_plotdata_csv <<- FALSE  # Save plot data as CSV
  varlist_combine_old_new_j <<- c("Q_EN", "K_EN", "I_EN", "Q_IN")  # Variables to combine old/new j technologies

  # Set default time parameters
  if(!exists("year0", envir=.GlobalEnv)) {
    assign("year0", 2005, envir=.GlobalEnv)
    assign("tstep", 5, envir=.GlobalEnv)
  }
  if(!exists("yearmin", envir=.GlobalEnv)) {
    assign("yearmin", getOption("yearmin", 1980), envir=.GlobalEnv)
  }
  if(!exists("yearmax", envir=.GlobalEnv)) {
    assign("yearmax", getOption("yearmax", 2100), envir=.GlobalEnv)
  }

  ## Validate and normalize model_dir ##
  if(exists("model_dir", envir=.GlobalEnv) && !is.null(get("model_dir", envir=.GlobalEnv))) {
    model_dir <- get("model_dir", envir=.GlobalEnv)

    if(!dir.exists(model_dir)) {
      warning(sprintf("model_dir does not exist: '%s'\nSkipping model directory initialization. Historical data and region mappings will not be available.", model_dir))
      assign("model_dir", NULL, envir=.GlobalEnv)
    } else {
      assign("model_dir", normalizePath(model_dir), envir=.GlobalEnv)
    }
  }

  ## Set up graphs directory ##
  # Always use first fullpathdir for graphs
  if(exists("fullpathdir", envir=.GlobalEnv) && !is.null(get("fullpathdir", envir=.GlobalEnv))) {
    fullpathdir <- get("fullpathdir", envir=.GlobalEnv)

    # Always use first fullpathdir for graphs
    graphdir_val <- file.path(fullpathdir[1], "graphs")
    assign("graphdir", graphdir_val, envir=.GlobalEnv)

    # Validate that directory exists
    if(any(!dir.exists(fullpathdir))) {
      stop(sprintf("Results directory does not exist: '%s'\nPlease check the results_dir parameter.",
                   fullpathdir[!dir.exists(fullpathdir)][1]))
    }
  } else {
    assign("graphdir", NULL, envir=.GlobalEnv)
  }

  # Load GDX files if applicable (only for GDX-based models, not IIASADB)
  if (exists("fullpathdir", envir = .GlobalEnv) &&
      !is.null(get("fullpathdir", envir = .GlobalEnv)) &&
      !exists("iamc_filename", envir = .GlobalEnv) &&
      !exists("iamc_databasename", envir = .GlobalEnv)) {

    fullpathdir <- get("fullpathdir", envir = .GlobalEnv)
    model_dir <- get("model_dir", envir = .GlobalEnv)

    # Get parameters
    restrict_files <- if (exists("restrict_files", envir = .GlobalEnv)) {
      get("restrict_files", envir = .GlobalEnv)
    } else {
      "results_"
    }

    exclude_files <- if (exists("exclude_files", envir = .GlobalEnv)) {
      get("exclude_files", envir = .GlobalEnv)
    } else {
      ""
    }

    removepattern <- if (exists("removepattern", envir = .GlobalEnv)) {
      get("removepattern", envir = .GlobalEnv)
    } else {
      "results_"
    }

    # Don't use global scenlist as custom - it's created by previous runs
    # Only use scenlist_custom if explicitly passed via ... argument
    scenlist_custom <- NULL

    reg_id <- if (exists("reg_id", envir = .GlobalEnv)) {
      get("reg_id", envir = .GlobalEnv)
    } else {
      NULL
    }

    # Load GDX session data using new clean approach
    session_data <- .load_gdx_session(
      results_dir = fullpathdir[1],
      model_dir = model_dir,
      restrict_files = restrict_files,
      exclude_files = exclude_files,
      removepattern = removepattern,
      scenlist_custom = scenlist_custom,
      reg_id = reg_id
    )

    # Set global variables for backward compatibility with existing code
    .set_global_session_vars(session_data)
  }

  invisible(NULL)
}
