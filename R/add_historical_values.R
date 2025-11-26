
add_historical_values <- function(variable, varname=deparse(substitute(variable)), verbose=TRUE, iiasadb = F){
  # Determine reg_id - use "witch17" as default if not set
  current_reg_id <- if(exists("reg_id") && !is.null(reg_id) && length(reg_id) > 0) {
    reg_id[1]
  } else {
    "witch17"  # Default for WITCH
  }

  hist_file <- system.file("data", paste0("data_historical_values_", current_reg_id, ".gdx"), package = "witchplot")
  if(hist_file == "") {
    # If not found in package, check results_dir for data_historical_values.gdx
    if(exists("results_dir") && length(results_dir) > 0) {
      local_hist_file <- file.path(results_dir[1], "data_historical_values.gdx")
      if(file.exists(local_hist_file)) {
        hist_file <- local_hist_file
        if(verbose) message(sprintf("Using historical data from results_dir: %s", hist_file))
      } else {
        if(verbose) message(sprintf("No file data_historical_values.gdx found. Consider storing it from your model data folder in your results_dir, then it will be loaded."))
        return(as.data.table(variable))
      }
    } else {
      if(verbose) message(sprintf("No historical data file found for reg_id '%s'. Package data should contain: data/data_historical_values_%s.gdx", current_reg_id, current_reg_id))
      return(as.data.table(variable))
    }
  } else {
    if(verbose) message(sprintf("Using historical data from package: %s", hist_file))
  }
  
  
  if(exists("map_var_hist")) if(!(varname %in% map_var_hist$varname_model)) return(as.data.table(variable))
  #from here process the historical data files
  variable_loaded_original <- variable
  if(iiasadb){
    #for IIASAdb rename relevant set columns (YEAR column already contains actual years, not time periods)
    # Only rename if REGION and YEAR columns exist (they may already be renamed to n and year)
    if("REGION" %in% names(variable) && "YEAR" %in% names(variable)) {
      variable <- variable %>% dplyr::rename(n=REGION, year=YEAR) %>% mutate(VARIABLE=gsub("\\|","_",VARIABLE))
    } else {
      # Already in standard format, just clean VARIABLE name
      if("VARIABLE" %in% names(variable)) {
        variable <- variable %>% mutate(VARIABLE=gsub("\\|","_",VARIABLE))
      }
    }
    varname <- gsub("\\|","_",varname)
  }
  if(exists("map_var_hist")){
    map_var_hist$varname_model <- gsub("\\|","_",map_var_hist$varname_model) #just locally in this function
    
    if(varname %in% map_var_hist$varname_model){
      if(map_var_hist[varname_model==varname]$set_witch[1]!="" & map_var_hist[varname_model==varname]$set_model[1]==""){ #only if no model set given
        variable <- cbind(tempset=map_var_hist[varname_model==varname]$element_witch, variable)
        setnames(variable, "tempset", map_var_hist[varname_model==varname]$set_witch)
      }
      #rename varname to WITCH one
      varname_original <- varname
      varname <- map_var_hist[varname_model==varname]$var_witch[1] #for now only one witch variable for a model variable can be used
    }
  }
  
  valid_suffix <- "_valid"
  #if(varname=="Q_EMI"){valid_suffix <- "_valid_primap"}
  if(varname=="Q"){valid_suffix <- c("_valid_wdi", "_valid_weo")}
  #if(varname=="SOCECON"){valid_suffix <- "_valid_wdi_sum"}
  if(varname=="Q_IN"){valid_suffix <- "_valid_weo"}
  if(varname=="sigma"){valid_suffix <- "_valid_notcompatible"}
  if(varname=="quantiles"){valid_suffix <- "_valid_swiid"} #for quantiles
  if(varname=="K_EN"){valid_suffix <- c("_valid_platts_tot", "_valid_irena", "_valid_iaea", "_valid_gcpt")} #for quantiles, set it to 
  
  #treat special varnames
  if(str_detect(varname, "MAGICC")) varname <- gsub("MAGICC", "", varname)
  if(str_detect(varname, "HECTOR")) varname <- gsub("HECTOR", "", varname)

  # Load historical GDX file(s) from package data
  # For IIASADB, combine global and r5 historical data
  if(iiasadb) {
    # Ensure required packages are loaded for reading GDX files
    require(gdxtools)
    require(data.table)

    # For IIASADB, load ALL available historical data files (global, r5, witch17, ed58, etc.)
    # This ensures maximum regional coverage

    # Get all data_historical_values_*.gdx files from package data folder
    data_dir <- system.file("data", package = "witchplot")
    all_hist_files <- list.files(data_dir, pattern = "^data_historical_values_.*\\.gdx$", full.names = TRUE)

    # Also check results_dir if available
    if(exists("results_dir") && length(results_dir) > 0 && dir.exists(results_dir[1])) {
      local_hist_files <- list.files(results_dir[1], pattern = "^data_historical_values_.*\\.gdx$", full.names = TRUE)
      all_hist_files <- c(all_hist_files, local_hist_files)
    }

    # Remove duplicates (prefer package files over local)
    all_hist_files <- unique(all_hist_files)

    # Load all available files
    gdx_list <- list()
    for(hf in all_hist_files) {
      if(file.exists(hf)) {
        gdx_list[[length(gdx_list) + 1]] <- gdx(hf)
        if(verbose) message(sprintf("Loaded historical data from: %s", basename(hf)))
      }
    }

    if(length(gdx_list) == 0) {
      if(verbose) message("No IIASADB historical data files found")
      return(as.data.table(variable_loaded_original))
    }

    # Use first gdx for initial checks, will combine data from all files later
    .gdx <- gdx_list[[1]]
  } else {
    # For non-IIASADB, load single file
    .gdx <- gdx(hist_file)
  }
  
  #now checking if for the precise variable historical data is there
  if(length(grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE))==0) return(as.data.table(variable_loaded_original)) 
  
  ####### #here continue only if we're sure data will be merged ########
  if(verbose) print(paste0("Historical values added for '", varname, "'."))
  item <- grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE) #use grep with ^ to have them start by varname

  # Remove all valid_xxx where xxx contains "mean"
  item <- item[!grepl("valid_.*mean", item)]

  if(length(item) == 0) {
    if(verbose) message("No historical data items remaining after filtering (all contained 'mean')")
    return(as.data.table(variable_loaded_original))
  }

  # Always use all data sources (check_calibration is always TRUE)
  # For IIASADB, combine data from both global and r5 files
  if(iiasadb && exists("gdx_list")) {
    .hist <- NULL
    for(gdx_idx in 1:length(gdx_list)) {
      .gdx_temp <- gdx_list[[gdx_idx]]
      # Check which items exist in this gdx file
      items_in_gdx <- grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx_temp$parameters$name, value = TRUE)
      items_in_gdx <- items_in_gdx[!grepl("valid_.*mean", items_in_gdx)]

      for(.item in items_in_gdx){
        .hist_single <- as.data.table(.gdx_temp[.item])
        .hist_single$file <- gsub(paste0(tolower(varname), "_"), "", .item)
        if(is.null(.hist)){.hist <- .hist_single}else{.hist <- data.table::rbindlist(list(.hist, .hist_single), fill=TRUE)}
      }
    }
  } else {
    # For non-IIASADB, load from single gdx
    for(.item in item){
      .hist_single <- as.data.table(.gdx[.item])
      .hist_single$file <- gsub(paste0(tolower(varname), "_"), "", .item)
      if(.item==item[1]){.hist <- .hist_single}else{.hist <- data.table::rbindlist(list(.hist, .hist_single), fill=TRUE)}
    }
  }
  
  # Load set dependencies from RDS file (always use RDS for set dependencies)
  # First try package data, then check results_dir
  hist_set_dep_file <- system.file("data", "historical_data_set_dependencies.rds", package = "witchplot")
  if(hist_set_dep_file == "" || !file.exists(hist_set_dep_file)) {
    # If not in package, check results_dir
    if(exists("results_dir") && length(results_dir) > 0) {
      local_rds_file <- file.path(results_dir[1], "historical_data_set_dependencies.rds")
      if(file.exists(local_rds_file)) {
        hist_set_dep_file <- local_rds_file
        if(verbose) message("Using set dependencies from results_dir")
      }
    }
  }

  # Apply set dependencies from RDS file
  if(hist_set_dep_file != "" && file.exists(hist_set_dep_file)) {
    hist_set_dependencies <- readRDS(hist_set_dep_file)
    # Get set dependencies for this historical parameter
    if(.item %in% names(hist_set_dependencies)) {
      setdep_hist <- hist_set_dependencies[[.item]]
      # Rename columns: first columns get the set names from RDS, then value, then file
      names(.hist) <- c(setdep_hist, "value", "file")
      if(verbose) message("  Applied set dependencies from RDS: ", paste(setdep_hist, collapse=", "))
    } else {
      if(verbose) message("  Warning: No set dependencies found for parameter: ", .item)
    }
  } else {
    if(verbose) message("  Warning: No historical_data_set_dependencies.rds file found")
  }
  
  # Ensure n column exists (for global/world data)
  if(!("n" %in% colnames(.hist))) .hist$n = "World"
  
  #adjust time unit to model
  if(iiasadb) {
    # For IIASADB, historical data has 'year' column, just keep as is
    t_historical <- unique(.hist$year)
  } else {
    # For WITCH/RICE, historical data has 'year', convert to time periods 't'
    if("year" %in% names(.hist)) {
      .hist$t <- yeartot(.hist$year)
      .hist <- .hist %>% select(-year)  # Remove year column after conversion
    }
    t_historical <- unique(.hist$t)
  }
  #adjust scenario names
  if(exists("witch_regions")) .hist$n  <- dplyr::recode(.hist$n, !!!setNames(witch_regions, display_regions))

  #Apply map_var_hist transformations
  if(exists("map_var_hist") && exists("varname_original")){
    if(varname_original %in% map_var_hist$varname_model){
      # Determine transformation direction based on what columns exist
      set_witch <- map_var_hist[varname_model==varname_original]$set_witch[1]
      set_model <- map_var_hist[varname_model==varname_original]$set_model[1]

      if(set_model != "" && set_witch != "") {
        # Check which direction to transform:
        # If variable has set_model column, transform .hist FROM set_witch TO set_model (e.g., RICE: e -> ghg)
        # If variable has set_witch column, transform .hist FROM set_model TO set_witch (e.g., WITCH: ghg -> e)

        if(set_model %in% names(variable) && set_witch %in% names(.hist)) {
          # Transform historical FROM WITCH TO MODEL format (e.g., RICE: rename e -> ghg)
          setnames(.hist, set_witch, set_model)
          if(verbose) message("  Renamed set column: ", set_witch, " -> ", set_model)

          # Filter and recode elements (inverse direction: element_witch -> element_model)
          if(set_model %in% names(.hist)) {
            .hist <- .hist %>% filter(c_across(all_of(set_model)) %in% map_var_hist[varname_model==varname_original]$element_witch)
            # Create inverse mapping: element_witch -> element_model
            set_map <- map_var_hist[varname_model==varname_original]$element_model
            names(set_map) <- map_var_hist[varname_model==varname_original]$element_witch
            .hist <- .hist %>% mutate(across(all_of(set_model), ~ dplyr::recode(., !!!set_map)))
            if(verbose) message("  Recoded ", nrow(.hist), " rows in set column: ", set_model, " (WITCH->MODEL)")
          }
          # Store which direction we transformed for unit conversion
          transform_direction <- "WITCH_TO_MODEL"
        } else if(set_witch %in% names(variable) && set_model %in% names(.hist)) {
          # Transform historical FROM MODEL TO WITCH format (standard direction: ghg -> e)
          setnames(.hist, set_model, set_witch)
          if(verbose) message("  Renamed set column: ", set_model, " -> ", set_witch)

          # Filter and recode elements (standard direction: element_model -> element_witch)
          if(set_witch %in% names(.hist)) {
            .hist <- .hist %>% filter(c_across(all_of(set_witch)) %in% map_var_hist[varname_model==varname_original]$element_witch)
            set_map <- map_var_hist[varname_model==varname_original]$element_model
            names(set_map) <- map_var_hist[varname_model==varname_original]$element_witch
            .hist <- .hist %>% mutate(across(all_of(set_witch), ~ dplyr::recode(., !!!set_map)))
            if(verbose) message("  Recoded ", nrow(.hist), " rows in set column: ", set_witch, " (MODEL->WITCH)")
          }
          # Store which direction we transformed for unit conversion
          transform_direction <- "MODEL_TO_WITCH"
        } else {
          if(verbose) message("  Could not determine transformation direction - set columns not found in expected format")
          transform_direction <- "NONE"
        }
      }

      #unit conversion if needed
      .conv <- map_var_hist[varname_model==varname_original] %>% select(set_model, element_model, conv)
      if(all(.conv$set_model=="")) {
        # Simple conversion: apply same factor to all rows
        .hist$conv <- unique(.conv$conv)
        .hist <- .hist %>% mutate(value = value * conv) %>% select(-conv)
        if(verbose) message("  Applied unit conversion: ", unique(.conv$conv))
      } else {
        # Set-specific conversion: need to join on the set column
        # Determine the actual column name based on transformation direction
        if(exists("transform_direction") && transform_direction == "WITCH_TO_MODEL") {
          # For RICE: column is now set_model (e.g., "ghg")
          actual_col_name <- set_model
          # Use element_model as the key for joining
          join_col <- "element_model"
        } else {
          # Standard: column is set_witch (e.g., "e")
          actual_col_name <- set_witch
          # Use element_model as the key for joining
          join_col <- "element_model"
        }

        if(actual_col_name %in% names(.hist)) {
          # Rename element_model to match the actual column name in .hist for joining
          .conv_copy <- .conv
          setnames(.conv_copy, join_col, actual_col_name)
          .conv_select <- .conv_copy %>% select(-set_model)
          # Join on the set column
          .hist <- .hist %>% left_join(.conv_select, by = actual_col_name)
          .hist <- .hist %>% mutate(value = value * conv) %>% select(-conv)
          if(verbose) message("  Applied set-specific unit conversion on column: ", actual_col_name)
        } else {
          if(verbose) warning(paste0("Cannot apply unit conversion for ", varname_original, " - column '", actual_col_name, "' not found in historical data"))
        }
      }
    }
  }

  # ALWAYS filter historical data to only include elements present in variable
  # Do this AFTER map_var_hist transformation so column names match
  # Find set columns (exclude standard columns: n, t, year, value, file, pathdir, tlen)
  standard_cols <- c("n", "t", "year", "value", "file", "pathdir", "tlen")
  set_cols_in_hist <- setdiff(names(.hist), standard_cols)
  set_cols_in_var <- setdiff(names(variable), standard_cols)
  common_set_cols <- intersect(set_cols_in_hist, set_cols_in_var)

  # Filter .hist to only include elements that exist in variable for each set column
  for(set_col in common_set_cols) {
    if(set_col %in% names(.hist) && set_col %in% names(variable)) {
      elements_in_var <- unique(variable[[set_col]])
      .hist <- .hist %>% filter(c_across(all_of(set_col)) %in% elements_in_var)
      if(verbose) message(sprintf("  Filtered historical data for '%s' to elements in variable: %s",
                                  set_col, paste(elements_in_var, collapse=", ")))
    }
  }

  #rename "valid" to "historical" in file names
  # Always keep historical data sources separate (check_calibration is always TRUE)
  .hist$file <- gsub("valid", "historical", .hist$file)
  
  #special case where categories do not match exactly
  if("q_in_valid_weo" %in% item) #add fuel column
  {
    .hist$fuel <- "oil"
    .hist[jfed=="elgastr"]$fuel <- "gas"
    .hist[jfed=="elpc"]$fuel <- "coal"
    .hist[jfed=="elpb"]$fuel <- "wbio"
    .hist[jfed=="elnuclear"]$fuel <- "uranium"
  }
  
  #merge with variable
  # Always keep historical data sources separate (check_calibration is always TRUE)
  # Multiply by pathdir so it appears for each pathdir
  .hist_temp <- .hist
  for(pd in basename(results_dir))
  {
    .hist_temp$pathdir <- pd
    if(pd==basename(results_dir[1])){.hist=.hist_temp}else{.hist <- data.table::rbindlist(list(.hist, .hist_temp), fill=TRUE)}
  }
  
  if(iiasadb){
    #adjusting region names
    
    #creating same data format as iiasadb
    .hist <- .hist %>%
      mutate(VARIABLE=varname_original, UNIT=unique(variable$UNIT)[1], SCENARIO="historical", MODEL=file) %>%
      select(-file, -pathdir) %>%
      #keep only historical, no valid data points
      filter(!str_detect(MODEL, "valid"))
    
    # Ensure .hist has same columns as variable in same order
    common_cols <- intersect(names(variable), names(.hist))
    variable <- variable %>% select(all_of(common_cols))
    .hist <- .hist %>% select(all_of(common_cols))
  } else {
    # For WITCH/RICE: Keep all columns from variable, add .hist columns that exist
    # DO NOT remove columns from variable that aren't in .hist!
    # rbindlist with fill=TRUE will handle missing columns by filling with NA
    # NOTE: Set column renaming (e.g., e <-> ghg) is now handled by map_var_hist above

    # Keep all variable columns as-is
    # Only reorder .hist to have common columns first (helps with rbindlist)
    common_cols <- intersect(names(variable), names(.hist))
    hist_only_cols <- setdiff(names(.hist), names(variable))

    # Reorder .hist: common columns first (in variable's order), then hist-only columns
    .hist <- .hist %>% select(all_of(c(common_cols, hist_only_cols)))
    # variable stays as-is with all its columns
  }

  # Debug: Check if .hist has data
  if(verbose) message(sprintf("  Historical data rows before merge: %d, columns: %s", nrow(.hist), paste(names(.hist), collapse=", ")))

  # CASE-INSENSITIVE REGION MATCHING: Match .hist region names to variable region names
  # Get region column name (for IIASADB it's still "n" at this point, renamed to REGION later)
  region_col <- "n"
  if(region_col %in% names(variable) && region_col %in% names(.hist)) {
    # Get unique regions from both (case-sensitive)
    var_regions <- unique(variable[[region_col]])
    hist_regions <- unique(.hist[[region_col]])

    # Create case-insensitive mapping: lowercase -> actual variable case
    var_regions_lower <- tolower(var_regions)
    names(var_regions_lower) <- var_regions

    # For each historical region, find matching variable region (case-insensitive)
    region_map <- setNames(character(0), character(0))  # empty named vector
    for(hr in hist_regions) {
      hr_lower <- tolower(hr)
      # Find matching variable region
      matching_var <- names(var_regions_lower)[var_regions_lower == hr_lower]
      if(length(matching_var) > 0) {
        region_map[hr] <- matching_var[1]  # Use first match
      } else {
        region_map[hr] <- hr  # No match, keep original
      }
    }

    # Apply the mapping to .hist
    if(length(region_map) > 0) {
      .hist <- .hist %>% mutate(!!region_col := dplyr::recode(.data[[region_col]], !!!region_map))
      if(verbose) {
        changed <- sum(names(region_map) != region_map)
        if(changed > 0) {
          message(sprintf("  Adjusted %d region names in historical data to match variable case", changed))
        }
      }
    }
  }

  # Use data.table rbindlist with fill=TRUE for safety
  merged_variable <- data.table::rbindlist(list(as.data.table(variable), as.data.table(.hist)), fill=TRUE)

  # For RICE E variable: if variable has "ghg" column but historical data doesn't,
  # assume historical data is CO2 equivalent (matching get_witch.R line 25 logic)
  var_to_check <- if(exists("varname_original")) varname_original else varname
  if(verbose) message(sprintf("  Checking if variable '%s' needs ghg fill", var_to_check))
  if(var_to_check %in% c("E", "EIND", "MIU", "ABATEDEMI", "ABATECOST")) {
    if("ghg" %in% names(merged_variable)) {
      # Count how many NA ghg values in historical data before filling
      na_count <- sum(is.na(merged_variable$ghg) & str_detect(merged_variable$file, "historical"))
      if(verbose) message(sprintf("  Found %d historical rows with NA ghg values", na_count))
      # Fill NA values in ghg column with "co2" for historical data
      merged_variable <- merged_variable %>%
        mutate(ghg = ifelse(is.na(ghg) & str_detect(file, "historical"), "co2", ghg))
      if(verbose) message("  Filled NA ghg values with 'co2' for historical data")
    }
  }
  
  if(iiasadb) {
    # For IIASADB, keep in WITCH format (n, year) but restore pipe in VARIABLE
    merged_variable <- merged_variable %>%
      mutate(VARIABLE=gsub("_","|",VARIABLE))
    # Ensure year is numeric
    merged_variable$year <- as.numeric(merged_variable$year)
  } else {
    # For WITCH/RICE, t is in time periods
    merged_variable$t <- as.numeric(merged_variable$t)
  }
  #remove additional columns and set elements if no set_model given but set-witch is not empty
  if(exists("map_var_hist") && exists("varname_original")) {
    if(varname_original %in% map_var_hist$varname_model) {
      if(map_var_hist[varname_model==varname_original]$set_witch[1]!="" & map_var_hist[varname_model==varname_original]$set_model[1]=="") {
        set_col <- map_var_hist[varname_model==varname_original]$set_witch[1]
        target_val <- map_var_hist[varname_model==varname_original]$element_witch[1]
        if(set_col %in% names(merged_variable)) {
          merged_variable <- merged_variable %>%
            filter(.data[[set_col]] == target_val) %>%
            select(-all_of(set_col))
        }
      }
    }
  }

  # Diagnostic: Check if historical data has matching regions with model data
  if(verbose) {
    # Get region column name (n for WITCH/RICE, REGION for IIASADB)
    region_col <- if(iiasadb) "REGION" else "n"

    if(region_col %in% names(merged_variable)) {
      hist_regions <- unique(merged_variable[str_detect(merged_variable$file, "historical"), ][[region_col]])
      model_regions <- unique(merged_variable[!str_detect(merged_variable$file, "historical"), ][[region_col]])

      common_regions <- intersect(hist_regions, model_regions)
      hist_only <- setdiff(hist_regions, model_regions)
      model_only <- setdiff(model_regions, hist_regions)

      if(length(common_regions) == 0) {
        warning(sprintf("No matching regions between historical data and model data!\n  Historical regions: %s\n  Model regions: %s",
                       paste(hist_regions, collapse=", "), paste(model_regions, collapse=", ")))
      } else if(length(hist_only) > 0 || length(model_only) > 0) {
        message(sprintf("Region mismatch detected:\n  Common regions: %s\n  Historical only: %s\n  Model only: %s",
                       paste(common_regions, collapse=", "),
                       if(length(hist_only) > 0) paste(hist_only, collapse=", ") else "none",
                       if(length(model_only) > 0) paste(model_only, collapse=", ") else "none"))
      }
    }
  }

  return(merged_variable)
}
