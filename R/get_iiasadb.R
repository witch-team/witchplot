#First install these packages if not yet available (for standalone mode)
#require(tidyverse)
#require(reticulate)
#require(yaml)

#Function to download data from IIASA database
download_iiasadb <- function(database="iamc15", varlist="Emissions|CO2", varname=NULL, modlist="*", scenlist="*", region="World", show_variables = F, add_metadata = T){
  require(reticulate)
  pyam <- import("pyam", convert = FALSE)

  # --- IIASA Authentication Check -------------------------------------------
  
  # Check if IIASA login configuration exists
  has_iiasa_login <- dir.exists("~/.config/ixmp4") || dir.exists("~/.local/share/ixmp4")
  
  if (!has_iiasa_login) {
    warning(
      "No IIASA login detected. You can still access *public* databases.\n\n",
      "If you need to access *private* IIASA databases, please run the following command\n",
      "in your system console (not in R):\n\n",
      "    ixmp4 login <your_username>\n\n",
      "You will be prompted for your password. After that, restart R and rerun this script.\n"
    )
  }
  
  #show variables in case
  if(show_variables) print(py_to_r(pyam$iiasa$Connection(database)$variables()))
  assign("iiasadb_variables_available", as.data.frame(py_to_r(pyam$iiasa$Connection(database)$variables())), envir = .GlobalEnv)
  print("Available Platforms:"); print(py_to_r(pyam$iiasa$platforms()))
  iiasadb_data <- pyam$read_iiasa(database, model=modlist, scenario=scenlist, variable=varlist, region=region, meta=1)
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

  # Filter by variable name (support both exact match and regex)
  variable_data <- all_data %>%
    filter(str_detect(VARIABLE, fixed(variable_name, ignore_case = TRUE)) | VARIABLE == variable_name)

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
