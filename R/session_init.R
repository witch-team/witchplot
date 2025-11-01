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
  ## Set default theme ##
  ggplot2::theme_set(ggplot2::theme_bw())

  ## Set variables for combining old/new j technologies ##
  varlist_combine_old_new_j <<- c("Q_EN", "K_EN", "I_EN", "Q_IN")

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

  ## Set up graphs directory ##
  # Always use first results_dir for graphs
  if(exists("results_dir", envir=.GlobalEnv) && !is.null(get("results_dir", envir=.GlobalEnv))) {
    results_dir <- get("results_dir", envir=.GlobalEnv)

    # Always use first results_dir for graphs
    graphdir_val <- file.path(results_dir[1], "graphs")
    assign("graphdir", graphdir_val, envir=.GlobalEnv)

    # Validate that directory exists
    if(any(!dir.exists(results_dir))) {
      stop(sprintf("Results directory does not exist: '%s'\nPlease check the results_dir parameter.",
                   results_dir[!dir.exists(results_dir)][1]))
    }
  } else {
    assign("graphdir", NULL, envir=.GlobalEnv)
  }

  # Load GDX files if applicable (only for GDX-based models, not IIASADB)
  if (exists("results_dir", envir = .GlobalEnv) &&
      !is.null(get("results_dir", envir = .GlobalEnv)) &&
      !exists("iamc_filename", envir = .GlobalEnv) &&
      !exists("iamc_databasename", envir = .GlobalEnv)) {

    results_dir <- get("results_dir", envir = .GlobalEnv)

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
    # Use only first directory for discovering files and metadata
    # The actual loading from all directories happens in get_witch()
    session_data <- .load_gdx_session(
      results_dir = results_dir[1],
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
