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

  # Helper: list GDX basenames in dir, return relative paths (with optional prefix)
  .find_gdx_in_dir <- function(dir, prefix = "") {
    if (!dir.exists(dir)) return(character(0))
    found <- gsub("\\.gdx$", "", list.files(
      path = dir, pattern = "\\.gdx$", full.names = FALSE, recursive = FALSE
    ))
    if (length(found) > 0 && nchar(prefix) > 0) found <- file.path(prefix, found)
    found
  }

  # Files directly in results_dir
  all_files <- .find_gdx_in_dir(results_dir)

  # Also search results_dir/results/ and its immediate subdirectories
  results_subdir <- file.path(results_dir, "results")
  if (dir.exists(results_subdir)) {
    all_files <- c(all_files, .find_gdx_in_dir(results_subdir, "results"))
    sub_dirs <- list.dirs(results_subdir, full.names = FALSE, recursive = FALSE)
    for (sub_dir in sub_dirs[nchar(sub_dirs) > 0]) {
      all_files <- c(all_files, .find_gdx_in_dir(
        file.path(results_subdir, sub_dir), file.path("results", sub_dir)
      ))
    }
  }

  if (length(all_files) == 0) {
    stop("No GDX files found in: ", results_dir)
  }

  # Filter on basename: must contain "results_" (matches old str_detect behaviour)
  all_files <- all_files[stringr::str_detect(basename(all_files), "results_")]

  if (length(all_files) == 0) {
    stop("No GDX files starting with 'results_' found in: ", results_dir)
  }

  # Apply additional inclusion filters on basename (if restrict_files is not "results_")
  if (!is.null(restrict_files) && restrict_files != "" && restrict_files != "results_") {
    patterns <- if (is.character(restrict_files)) restrict_files else unlist(restrict_files)
    filtered <- all_files
    for (pattern in patterns) {
      filtered <- filtered[stringr::str_detect(basename(filtered), pattern)]
    }
    all_files <- unique(filtered)
  }

  # Apply exclusion filter on basename
  if (!is.null(exclude_files) && exclude_files != "") {
    all_files <- all_files[!stringr::str_detect(basename(all_files), paste(exclude_files, collapse = "|"))]
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
    # Auto-generate scenario names from basename only (strip path prefix first)
    basenames <- basename(filelist)
    scenario_names <- basenames
    if (!is.null(removepattern) && removepattern != "") {
      scenario_names <- gsub(paste(removepattern, collapse = "|"), "", basenames)
    }
    # Disambiguate duplicate names by prepending the subfolder group label
    dups <- duplicated(scenario_names) | duplicated(scenario_names, fromLast = TRUE)
    if (any(dups)) {
      group_labels <- sapply(filelist, function(f) {
        parts <- strsplit(f, "/", fixed = TRUE)[[1]]
        if (length(parts) >= 3) parts[length(parts) - 1L]
        else if (length(parts) == 2) parts[1L]
        else ""
      })
      for (i in which(dups)) {
        if (nchar(group_labels[i]) > 0)
          scenario_names[i] <- paste0(scenario_names[i], " [", group_labels[i], "]")
      }
    }
    scenlist <- setNames(scenario_names, filelist)
  }

  scenlist
}

#' Extract subfolder group labels for each file in filelist
#'
#' Files directly in results_dir or in results_dir/results/ get group label "".
#' Files in results_dir/results/subdir/ get group label "subdir".
#'
#' @param filelist Character vector of GDX file IDs (as returned by .discover_gdx_files)
#' @return Named character vector: fileid -> group label
#' @keywords internal
.get_scenlist_groups <- function(filelist) {
  group_labels <- sapply(filelist, function(f) {
    parts <- strsplit(f, "/", fixed = TRUE)[[1]]
    # 1 part:  "results_foo"             → top-level root → ""
    # 2 parts: "results/results_foo"     → directly in results/ → "results"
    # 3 parts: "results/bar/results_foo" → in subdir → "bar"
    if (length(parts) >= 3) parts[length(parts) - 1L]
    else if (length(parts) == 2) parts[1L]
    else ""
  })
  setNames(group_labels, filelist)
}

#' Load GDX session data
#'
#' Main function that discovers files, creates scenario list, and loads metadata.
#' Returns a list with all session data instead of using global variables.
#'
#' @param results_dir Path to results directory
#' @param restrict_files Pattern to filter GDX files
#' @param exclude_files Pattern to exclude GDX files
#' @param removepattern Pattern to remove from scenario names
#' @param scenlist_custom Optional custom scenario list
#' @param reg_id Regional aggregation ID
#' @return List containing: filelist, scenlist, regions, palettes, metadata
#' @keywords internal
.load_gdx_session <- function(results_dir,
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
  # This is needed because get_witch() depends on this being global
  # Note: results_dir is already assigned by the caller (run_witch, etc.)
  assign("filelist", filelist, envir = .GlobalEnv)

  # Get metadata from first file
  first_gdx_path <- file.path(results_dir, paste0(filelist[1], ".gdx"))
  metadata <- .extract_gdx_metadata(first_gdx_path, filelist, results_dir)

  # Get region information (suppress join messages)
  region_info <- suppressMessages(.extract_region_info(filelist, results_dir, reg_id))

  # Return everything as a list
  list(
    filelist = filelist,
    scenlist = scenlist,
    results_dir = results_dir,
    file_group_columns = file_group_columns,
    regions = region_info$regions,
    reg_id = region_info$reg_id,
    region_palette = region_info$palette,
    region_palette_short = region_info$palette_short,
    region_palette_long = region_info$palette_long,
    rice_region_names = region_info$rice_region_names,
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

  # Note: We always assume flexible timestep - tlen will be loaded in get_witch()
  # No longer need to detect flexible_timestep

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
    stochastic_files = stochastic_files
  )
}

#' Extract region information
#'
#' @param filelist GDX filenames
#' @param results_dir Results directory
#' @param reg_id Regional aggregation ID
#' @return List with region info and palettes
#' @keywords internal
.extract_region_info <- function(filelist, results_dir, reg_id = NULL) {
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

  # Try to read region long names from data_{reg_id}/n.inc element text
  # (gdxtools does not expose set element text from GDX files)
  rice_region_names <- tryCatch({
    n_inc_path <- file.path(results_dir[1], paste0("data_", reg_id), "n.inc")
    if(file.exists(n_inc_path)) {
      lines   <- readLines(n_inc_path, warn=FALSE)
      matches <- regmatches(lines, regexec("^\\s*(\\w+)\\s+'(.+)'", lines))
      valid   <- Filter(function(x) length(x) == 3, matches)
      if(length(valid) > 0)
        setNames(sapply(valid, `[`, 3), sapply(valid, `[`, 2))
      else NULL
    } else NULL
  }, error = function(e) NULL)

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
    palette_long = palette_long,
    rice_region_names = rice_region_names
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
  assign("scenlist_groups", .get_scenlist_groups(session_data$filelist), envir = .GlobalEnv)
  assign("file_group_columns", session_data$file_group_columns, envir = .GlobalEnv)
  assign("reg_id", session_data$reg_id, envir = .GlobalEnv)
  assign("witch_regions", session_data$regions, envir = .GlobalEnv)
  assign("display_regions", session_data$regions, envir = .GlobalEnv)
  assign("region_palette", session_data$region_palette, envir = .GlobalEnv)
  assign("region_palette_specific_short", session_data$region_palette_short, envir = .GlobalEnv)
  assign("region_palette_longnames", session_data$region_palette_long, envir = .GlobalEnv)
  assign("stochastic_files", session_data$stochastic_files, envir = .GlobalEnv)
  assign("all_var_descriptions", session_data$var_descriptions, envir = .GlobalEnv)
  if(!is.null(session_data$rice_region_names))
    assign("rice_region_names", session_data$rice_region_names, envir = .GlobalEnv)
  invisible(NULL)
}
