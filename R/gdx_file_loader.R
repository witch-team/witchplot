## GDX File Discovery and Loading
## This file contains functions for discovering and loading GDX files

#' Load GDX files from results directory
#'
#' Discovers GDX files in the results directory, applies filters, and sets up
#' scenario list and region information. This function is called internally
#' during session initialization for WITCH/RICE models.
#'
#' @return NULL (modifies global environment with filelist, scenlist, region info)
#' @keywords internal
.load_gdx_files <- function() {
  # Check if we should load GDX files
  if(!exists("results_dir", envir=.GlobalEnv) ||
     is.null(get("results_dir", envir=.GlobalEnv)) ||
     exists("iamc_filename", envir=.GlobalEnv) ||
     exists("iamc_databasename", envir=.GlobalEnv)) {
    return(invisible(NULL))
  }

  results_dir <- get("results_dir", envir=.GlobalEnv)

  # Discover GDX files
  filelist <- gsub(".gdx", "", list.files(
    path = results_dir[1],
    full.names = FALSE,
    pattern = "*.gdx",
    recursive = FALSE
  ))

  # Apply file filters
  if(!exists("restrict_files", envir=.GlobalEnv)) {
    assign("restrict_files", "results_", envir=.GlobalEnv)
  }
  restrict_files <- get("restrict_files", envir=.GlobalEnv)

  if(restrict_files[1] != "") {
    for(i in 1:length(restrict_files)) {
      .filelist_res <- filelist[apply(outer(filelist, restrict_files[i], stringr::str_detect), 1, all)]
      if(i == 1) {
        .filelist_res_all <- .filelist_res
      } else {
        .filelist_res_all <- c(.filelist_res_all, .filelist_res)
      }
    }
    filelist <- unique(.filelist_res_all)
  }

  # Apply exclusion filters
  if(exists("exclude_files", envir=.GlobalEnv)) {
    exclude_files <- get("exclude_files", envir=.GlobalEnv)
    if(exclude_files[1] != "") {
      filelist <- filelist[!stringr::str_detect(filelist, paste(exclude_files, collapse = '|'))]
    }
  }

  if(length(filelist) == 0) {
    stop("No GDX files found.")
  }

  # Assign filelist early so other functions can access it
  assign("filelist", filelist, envir=.GlobalEnv)

  # Set up scenario list
  if(exists("scenlist", envir=.GlobalEnv)) {
    scenlist <- get("scenlist", envir=.GlobalEnv)
    # Check for missing scenarios
    if(length(names(scenlist[!(names(scenlist) %in% filelist)])) > 0) {
      print("Missing Scenarios:")
      print(cat(names(scenlist[!(names(scenlist) %in% filelist)]), sep = ", "))
    }
    filelist <- intersect(names(scenlist), filelist)
    scenlist <- scenlist[filelist]
    assign("scenlist", scenlist, envir=.GlobalEnv)
    assign("filelist", filelist, envir=.GlobalEnv)  # Update after filtering
  } else {
    if(!exists("removepattern", envir=.GlobalEnv)) {
      assign("removepattern", "results_", envir=.GlobalEnv)
    }
    removepattern <- get("removepattern", envir=.GlobalEnv)
    scenlist <- gsub(paste(removepattern, collapse = "|"), "", filelist)
    names(scenlist) <- filelist
    assign("scenlist", scenlist, envir=.GlobalEnv)
  }

  print(data.frame(scenlist = scenlist))

  # Set up file grouping
  if(exists("file_separate", envir=.GlobalEnv)) {
    file_separate <- get("file_separate", envir=.GlobalEnv)
    file_group_columns <- c("file", unname(file_separate[3:length(file_separate)]))
  } else {
    file_group_columns <- "file"
  }
  assign("file_group_columns", file_group_columns, envir=.GlobalEnv)

  # Check for flexible timestep
  if (requireNamespace("gdxtools", quietly = TRUE) &&
      exists("batch_extract", where = asNamespace("gdxtools"), mode = "function")) {
    tlen_values <- suppressWarnings(gdxtools::batch_extract(
      "tlen",
      files = file.path(results_dir, paste0(filelist, ".gdx"))
    ))
    flexible_timestep <- length(unique(tlen_values$tlen$value)) > 1
  } else {
    flexible_timestep <- FALSE
  }
  assign("flexible_timestep", flexible_timestep, envir=.GlobalEnv)

  # Check for stochastic runs
  tset <- get_witch("t")
  if("t" %in% names(tset)) {
    if(any(stringr::str_detect((tset %>% dplyr::select(t) %>% unique())$t, "_"))) {
      stochastic_files <- tset %>%
        dplyr::filter(stringr::str_detect(t, "_")) %>%
        dplyr::mutate(numeric_t = as.numeric(sub(".*_(\\d+)$", "\\1", t))) %>%
        dplyr::group_by(file) %>%
        dplyr::summarise(num_branches = max(numeric_t, na.rm = TRUE))
      assign("stochastic_files", stochastic_files, envir=.GlobalEnv)
    } else {
      assign("stochastic_files", NULL, envir=.GlobalEnv)
    }
  } else {
    assign("stochastic_files", NULL, envir=.GlobalEnv)
  }

  # Get variable descriptions from first file
  mygdx <- gdxtools::gdx(file.path(results_dir[1], paste0(filelist[1], ".gdx")))
  all_var_descriptions <- rbind(
    data.frame(name = mygdx$variables$name, description = mygdx$variables$text),
    data.frame(name = mygdx$parameters$name, description = mygdx$parameters$text)
  )
  assign("all_var_descriptions", all_var_descriptions, envir=.GlobalEnv)

  # Set up region information
  .setup_region_info(filelist, results_dir)

  # filelist already assigned earlier
  invisible(NULL)
}

#' Setup region information
#'
#' @param filelist Character vector of GDX filenames
#' @param results_dir Path to results directory
#' @keywords internal
.setup_region_info <- function(filelist, results_dir) {
  # Get region ID
  if(!exists("reg_id", envir=.GlobalEnv)) {
    conf <- get_witch("conf")
    if(!exists("conf")) {
      stop("No conf set found. Please specify reg_id manually!")
    }
    if(length(unique(subset(conf, V1 == "regions")$V2)) > 1) {
      print("Be careful: not all results files were run with the same regional aggregation!")
    }
    scenlist <- get("scenlist", envir=.GlobalEnv)
    reg_id <- subset(conf, file == scenlist[1] & pathdir == basename(results_dir[1]) & V1 == "regions")$V2
    assign("reg_id", reg_id, envir=.GlobalEnv)
  } else {
    reg_id <- get("reg_id", envir=.GlobalEnv)
  }

  # Get regions list
  if (requireNamespace("gdxtools", quietly = TRUE) &&
      exists("batch_extract", where = asNamespace("gdxtools"), mode = "function")) {
    n <- suppressWarnings(gdxtools::batch_extract("n", files = file.path(results_dir, paste0(filelist, ".gdx"))))
    if(is.null(n$n)) {
      witch_regions <- "World"
    } else {
      witch_regions <- unique(n$n$V1)
    }
  } else {
    # Fallback: try to get regions from first file
    tryCatch({
      first_gdx <- gdxtools::gdx(file.path(results_dir[1], paste0(filelist[1], ".gdx")))
      if("n" %in% names(first_gdx$sets)) {
        witch_regions <- first_gdx$sets$n$V1
      } else {
        witch_regions <- "World"
      }
    }, error = function(e) {
      witch_regions <- "World"
    })
  }

  # Apply nice region names if they exist
  if(exists("nice_region_names", envir=.GlobalEnv)) {
    nice_region_names <- get("nice_region_names", envir=.GlobalEnv)
    witch_regions <- dplyr::recode(witch_regions, !!!nice_region_names)
  }

  display_regions <- witch_regions
  assign("display_regions", display_regions, envir=.GlobalEnv)
  assign("witch_regions", witch_regions, envir=.GlobalEnv)

  # Set up color palettes
  region_palette <- get_region_palette(witch_regions, reg_id)
  if(exists("restrict_regions", envir=.GlobalEnv)) {
    restrict_regions <- get("restrict_regions", envir=.GlobalEnv)
    region_palette <- region_palette[restrict_regions]
  }
  assign("region_palette", region_palette, envir=.GlobalEnv)

  # Short names palette
  region_palette_specific_short <- region_palette
  names(region_palette_specific_short) <- witch_name_short(names(region_palette))
  assign("region_palette_specific_short", region_palette_specific_short, envir=.GlobalEnv)

  # Long names palette
  region_palette_longnames <- region_palette
  names(region_palette_longnames) <- dplyr::recode(
    names(region_palette),
    !!!setNames(
      paste0(witch_region_longnames[names(witch_region_longnames)], " (", names(witch_region_longnames), ")"),
      names(witch_region_longnames)
    )
  )
  assign("region_palette_longnames", region_palette_longnames, envir=.GlobalEnv)

  print(paste(length(get("scenlist", envir=.GlobalEnv)), "Scenarios and", length(witch_regions), "regions considered."))
}
