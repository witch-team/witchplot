## Modern GDX File Loading
## Refactored to avoid global variables and improve clarity

#' Discover GDX files in directory
#'
#' @param results_dir Path to results directory
#' @param restrict_files Pattern to filter files (e.g., "results_")
#' @param exclude_files Pattern to exclude files
#' @return Character vector of GDX filenames (without .gdx extension)
#' @keywords internal
.discover_gdx_files <- function(results_dir, restrict_files = "results_", exclude_files = "") {
  message("Searching for GDX files in: ", results_dir)

  # Find all GDX files
  all_files <- gsub("\\.gdx$", "", list.files(
    path = results_dir,
    pattern = "\\.gdx$",
    full.names = FALSE,
    recursive = FALSE
  ))

  if (length(all_files) == 0) {
    stop("No GDX files found in: ", results_dir)
  }

  # Apply inclusion filter
  if (!is.null(restrict_files) && restrict_files != "") {
    patterns <- if (is.character(restrict_files)) restrict_files else unlist(restrict_files)
    filtered <- all_files
    for (pattern in patterns) {
      filtered <- filtered[stringr::str_detect(filtered, pattern)]
    }
    all_files <- unique(filtered)
  }

  # Apply exclusion filter
  if (!is.null(exclude_files) && exclude_files != "") {
    all_files <- all_files[!stringr::str_detect(all_files, paste(exclude_files, collapse = "|"))]
  }

  if (length(all_files) == 0) {
    stop("No GDX files found after applying filters")
  }

  message("Found ", length(all_files), " GDX files")
  all_files
}

#' Create scenario list from filenames
#'
#' @param filelist Character vector of GDX filenames
#' @param removepattern Pattern to remove from scenario names
#' @param scenlist_custom Optional pre-defined scenario list
#' @return Named character vector (names=filenames, values=scenario names)
#' @keywords internal
.create_scenlist <- function(filelist, removepattern = "results_", scenlist_custom = NULL) {
  if (!is.null(scenlist_custom)) {
    # User provided scenario list - validate and filter
    missing <- setdiff(names(scenlist_custom), filelist)
    if (length(missing) > 0) {
      warning("Missing scenarios in GDX files: ", paste(missing, collapse = ", "))
    }

    # Keep only scenarios that exist
    valid_files <- intersect(names(scenlist_custom), filelist)
    scenlist <- scenlist_custom[valid_files]
  } else {
    # Auto-generate scenario names from filenames
    scenario_names <- filelist
    if (!is.null(removepattern) && removepattern != "") {
      scenario_names <- gsub(paste(removepattern, collapse = "|"), "", filelist)
    }
    scenlist <- setNames(scenario_names, filelist)
  }

  scenlist
}

#' Load GDX session data
#'
#' Main function that discovers files, creates scenario list, and loads metadata.
#' Returns a list with all session data instead of using global variables.
#'
#' @param results_dir Path to results directory
#' @param model_dir Path to model directory
#' @param restrict_files Pattern to filter GDX files
#' @param exclude_files Pattern to exclude GDX files
#' @param removepattern Pattern to remove from scenario names
#' @param scenlist_custom Optional custom scenario list
#' @param reg_id Regional aggregation ID
#' @return List containing: filelist, scenlist, regions, palettes, metadata
#' @keywords internal
.load_gdx_session <- function(results_dir,
                              model_dir = NULL,
                              restrict_files = "results_",
                              exclude_files = "",
                              removepattern = "results_",
                              scenlist_custom = NULL,
                              reg_id = NULL) {

  # Discover GDX files
  filelist <- .discover_gdx_files(results_dir, restrict_files, exclude_files)

  # Create scenario list
  scenlist <- .create_scenlist(filelist, removepattern, scenlist_custom)

  # Update filelist to only include scenarios in scenlist
  filelist <- names(scenlist)

  # Check if any files remain after filtering
  if (length(filelist) == 0) {
    stop("No GDX files found after applying filters and scenario list matching.\n",
         "Check restrict_files, exclude_files, and scenlist_custom parameters.")
  }

  # Set up file grouping columns
  file_group_columns <- if(exists("file_separate", envir=.GlobalEnv)) {
    file_separate <- get("file_separate", envir=.GlobalEnv)
    c("file", unname(file_separate[3:length(file_separate)]))
  } else {
    "file"
  }

  # Print summary
  print(data.frame(
    file = filelist,
    scenario = as.character(scenlist),
    row.names = NULL
  ))

  # Set filelist globally early so get_witch() can access it
  # This is needed because get_witch() depends on filelist being global
  assign("filelist", filelist, envir = .GlobalEnv)
  assign("fullpathdir", results_dir, envir = .GlobalEnv)

  # Get metadata from first file
  first_gdx_path <- file.path(results_dir, paste0(filelist[1], ".gdx"))
  metadata <- .extract_gdx_metadata(first_gdx_path, filelist, results_dir)

  # Get region information (suppress join messages)
  region_info <- suppressMessages(.extract_region_info(filelist, results_dir, reg_id, model_dir))

  # Return everything as a list
  list(
    filelist = filelist,
    scenlist = scenlist,
    results_dir = results_dir,
    model_dir = model_dir,
    file_group_columns = file_group_columns,
    regions = region_info$regions,
    reg_id = region_info$reg_id,
    region_palette = region_info$palette,
    region_palette_short = region_info$palette_short,
    region_palette_long = region_info$palette_long,
    flexible_timestep = metadata$flexible_timestep,
    stochastic_files = metadata$stochastic_files,
    var_descriptions = metadata$var_descriptions
  )
}

#' Extract metadata from GDX files
#'
#' @param first_gdx_path Path to first GDX file
#' @param filelist All GDX filenames
#' @param results_dir Results directory
#' @return List with metadata
#' @keywords internal
.extract_gdx_metadata <- function(first_gdx_path, filelist, results_dir) {
  # Variable descriptions
  mygdx <- gdxtools::gdx(first_gdx_path)
  var_descriptions <- rbind(
    data.frame(name = mygdx$variables$name, description = mygdx$variables$text),
    data.frame(name = mygdx$parameters$name, description = mygdx$parameters$text)
  )

  # Check for flexible timestep
  flexible_timestep <- FALSE
  if (requireNamespace("gdxtools", quietly = TRUE) &&
      exists("batch_extract", where = asNamespace("gdxtools"), mode = "function")) {
    tlen_values <- suppressWarnings(gdxtools::batch_extract(
      "tlen",
      files = file.path(results_dir, paste0(filelist, ".gdx"))
    ))
    flexible_timestep <- length(unique(tlen_values$tlen$value)) > 1
  }

  # Check for stochastic runs
  stochastic_files <- NULL
  tset <- tryCatch({
    get_witch("t")
  }, error = function(e) NULL)

  if (!is.null(tset) && "t" %in% names(tset)) {
    if (any(stringr::str_detect((tset %>% dplyr::select(t) %>% unique())$t, "_"))) {
      stochastic_files <- tset %>%
        dplyr::filter(stringr::str_detect(t, "_")) %>%
        dplyr::mutate(numeric_t = as.numeric(sub(".*_(\\d+)$", "\\1", t))) %>%
        dplyr::group_by(file) %>%
        dplyr::summarise(num_branches = max(numeric_t, na.rm = TRUE))
    }
  }

  list(
    var_descriptions = var_descriptions,
    flexible_timestep = flexible_timestep,
    stochastic_files = stochastic_files
  )
}

#' Extract region information
#'
#' @param filelist GDX filenames
#' @param results_dir Results directory
#' @param reg_id Regional aggregation ID
#' @param model_dir Model directory
#' @return List with region info and palettes
#' @keywords internal
.extract_region_info <- function(filelist, results_dir, reg_id = NULL, model_dir = NULL) {
  # Determine reg_id if not provided
  if (is.null(reg_id)) {
    # Try to read conf directly from the first GDX file
    conf <- tryCatch({
      first_gdx_path <- file.path(results_dir[1], paste0(filelist[1], ".gdx"))
      if (file.exists(first_gdx_path)) {
        mygdx <- gdxtools::gdx(first_gdx_path)
        if ("conf" %in% gdxtools::all_items(mygdx)$sets) {
          conf_data <- data.table::data.table(mygdx["conf"])
          conf_data
        } else {
          NULL
        }
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(conf)) {
      reg_id <- subset(conf, V1 == "regions")$V2
      if (length(reg_id) == 0) reg_id <- "default"
      if (length(unique(subset(conf, V1 == "regions")$V2)) > 1) {
        message("Note: Multiple regional aggregations detected in files")
      }
    } else {
      reg_id <- "default"
    }
  }

  # Get regions from GDX files
  regions <- .get_regions_from_gdx(filelist, results_dir)

  # Check for historical data directory - try to find actual directory
  if (!is.null(model_dir) && length(model_dir) > 0 && !is.null(reg_id) && length(reg_id) > 0) {
    data_dir <- file.path(model_dir, paste0("data_", reg_id[1]))
    if (!dir.exists(data_dir)) {
      # Try to find a directory that starts with data_<reg_id>
      potential_dirs <- list.dirs(model_dir, full.names = FALSE, recursive = FALSE)
      matching_dirs <- grep(paste0("^data_", reg_id[1]), potential_dirs, value = TRUE)
      if (length(matching_dirs) > 0) {
        data_dir <- file.path(model_dir, matching_dirs[1])
        message("Found data directory: ", matching_dirs[1])
        # Update reg_id to match actual directory
        reg_id <- gsub("^data_", "", matching_dirs[1])
      } else {
        warning("Historical data directory not found: ", data_dir,
                "\nHistorical data features will be disabled.")
      }
    }
  }

  # Create color palettes
  palette <- get_region_palette(regions, reg_id)
  palette_short <- palette
  names(palette_short) <- witch_name_short(names(palette))

  palette_long <- palette
  names(palette_long) <- dplyr::recode(
    names(palette),
    !!!setNames(
      paste0(witch_region_longnames[names(witch_region_longnames)], " (", names(witch_region_longnames), ")"),
      names(witch_region_longnames)
    )
  )

  message(length(filelist), " scenarios and ", length(regions), " regions loaded")

  list(
    regions = regions,
    reg_id = reg_id,
    palette = palette,
    palette_short = palette_short,
    palette_long = palette_long
  )
}

#' Get regions from GDX files
#'
#' @param filelist GDX filenames
#' @param results_dir Results directory
#' @return Character vector of region names
#' @keywords internal
.get_regions_from_gdx <- function(filelist, results_dir) {
  if (requireNamespace("gdxtools", quietly = TRUE) &&
      exists("batch_extract", where = asNamespace("gdxtools"), mode = "function")) {
    n <- suppressWarnings(gdxtools::batch_extract(
      "n",
      files = file.path(results_dir, paste0(filelist, ".gdx"))
    ))
    if (!is.null(n$n)) {
      return(unique(n$n$V1))
    }
  }

  # Fallback: get from first file
  tryCatch({
    first_gdx <- gdxtools::gdx(file.path(results_dir, paste0(filelist[1], ".gdx")))
    if ("n" %in% names(first_gdx$sets)) {
      return(unique(first_gdx$sets$n$V1))
    }
  }, error = function(e) {})

  # Default
  "World"
}

#' Set global environment variables from session data
#'
#' This is a compatibility function for legacy code that expects global variables.
#' New code should use the session list directly.
#'
#' @param session_data List returned by .load_gdx_session()
#' @keywords internal
.set_global_session_vars <- function(session_data) {
  assign("filelist", session_data$filelist, envir = .GlobalEnv)
  assign("scenlist", session_data$scenlist, envir = .GlobalEnv)
  assign("file_group_columns", session_data$file_group_columns, envir = .GlobalEnv)
  assign("reg_id", session_data$reg_id, envir = .GlobalEnv)
  assign("witch_regions", session_data$regions, envir = .GlobalEnv)
  assign("display_regions", session_data$regions, envir = .GlobalEnv)
  assign("region_palette", session_data$region_palette, envir = .GlobalEnv)
  assign("region_palette_specific_short", session_data$region_palette_short, envir = .GlobalEnv)
  assign("region_palette_longnames", session_data$region_palette_long, envir = .GlobalEnv)
  assign("flexible_timestep", session_data$flexible_timestep, envir = .GlobalEnv)
  assign("stochastic_files", session_data$stochastic_files, envir = .GlobalEnv)
  assign("all_var_descriptions", session_data$var_descriptions, envir = .GlobalEnv)
  invisible(NULL)
}
