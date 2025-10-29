
add_historical_values <- function(variable, varname=deparse(substitute(variable)), scenplot=scenlist, check_calibration=T, overlap_years="model", verbose=TRUE, iiasadb = F){
  #have to decide what to do with years with both model and historical data: overlap_years = #historical"  or "model"
  if(!length(list.files(path=file.path(model_dir, paste0("data_", reg_id)), full.names = TRUE, pattern="^data_historical(.*).gdx$", recursive = FALSE))>0) return(as.data.table(variable))
  if(exists("map_var_hist")) if(!(varname %in% map_var_hist$varname_model)) return(as.data.table(variable))
 #from here process the historical data files
  variable_loaded_original <- variable
  if(iiasadb){ 
    #for IIASAdb rename relevant set columns    
    variable <- variable %>% dplyr::rename(n=REGION) %>% mutate(YEAR=yeartot(YEAR)) %>% dplyr::rename(t=YEAR) %>% mutate(VARIABLE=gsub("\\|","_",VARIABLE))
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
  if(varname=="Q_IN"){valid_suffix <- "_valid_notcompatible"}
  if(varname=="sigma"){valid_suffix <- "_valid_notcompatible"}
  if(varname=="quantiles"){valid_suffix <- "_valid_swiid"} #for quantiles
  if(varname=="K_EN"){valid_suffix <- c("_valid_platts_tot", "_valid_irena", "_valid_iaea", "_valid_gcpt")} #for quantiles, set it to 
  
  #treat special varnames
  if(str_detect(varname, "MAGICC")) varname <- gsub("MAGICC", "", varname)
  if(str_detect(varname, "HECTOR")) varname <- gsub("HECTOR", "", varname)
  
  #check which GDX file to use (all files that start with data_historical*.gdx)
  if(!dir.exists(file.path(model_dir, paste0("data_", reg_id[1])))) return(as.data.table(variable))
  gdxhistlist <- list.files(path=file.path(model_dir, paste0("data_", reg_id)), full.names = TRUE, pattern="^data_historical(.*).gdx$", recursive = FALSE)
  
  for(.gdxname in gdxhistlist){
    #print(.gdxname)
    .gdx <- gdx(.gdxname)
    if(length(grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE))!=0){break} #to find the hist file with the valid data (only one!)
  }
  #now checking if for the precise variable historical data is there 
  if(length(grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE))==0) return(as.data.table(variable_loaded_original)) 
  
  ####### #here continue only if we're sure data will be merged ########
  if(verbose) print(paste0("Historical values added for '", varname, "'."))
  item <- grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE) #use grep with ^ to have them start by varname
  if(!check_calibration) item <- item[1] #if not check calibration, just take the first (unique) data source)
  for(.item in item){
    for(.reg_id_file in list.files(path=file.path(model_dir, paste0("data_", reg_id)), full.names = TRUE, pattern=basename(.gdxname), recursive = FALSE)){
      .hist_single_one_reg_id <- as.data.table(gdx(.reg_id_file)[.item]);
      if(.reg_id_file==list.files(path=file.path(model_dir, paste0("data_", reg_id)), full.names = TRUE, pattern=basename(.gdxname), recursive = FALSE)[1]){.hist_single <- .hist_single_one_reg_id}else{.hist_single <- rbind(.hist_single, .hist_single_one_reg_id)}
    }
     .hist_single$file <- gsub(paste0(tolower(varname), "_"), "", .item); 
    if(.item==item[1]){.hist <- .hist_single}else{.hist <- rbind(.hist,.hist_single)} 
    } 
  
  #get set dependency based on /build/ folder
  use_build <- F; 
  if(use_build){
    .gdxiso3 <- gdx(file.path(model_dir, "input", "build", basename(.gdxname))); 
    #print(str(.hist)); print(str(variable)); print(str(.gdxiso3[item[1]]))
    colnames(.hist) <- c(colnames(.gdxiso3[item[1]]), "file")	
    #in built global data have set "global", but in input folder it gets converted to iso3, so:
    colnames(.hist) <- gsub("global", "iso3", colnames(.hist)) 
    #add "World" if no country level data but global
    if(!("iso3" %in% colnames(.hist))){.hist$n = "World"}else{colnames(.hist) <- gsub("iso3", "n", colnames(.hist))}
    setnames(.hist, "year", "t")
    #print(.hist)
  }else{
    if(!("n" %in% colnames(.hist))) .hist$n = "World"
    #try to get dependency from variable itself
    setdep <- setdiff(names(variable), c("n", "file", "pathdir", "t", "value"))
    if(iiasadb) setdep <- setdiff(names(variable), c("n", "VARIABLE", "UNIT", "SCENARIO", "MODEL", "t", "value"))
    setdep <- c(setdep, "t")
    setnames(.hist, paste0("V", seq(1:length(setdep))), setdep)
    #print(.hist)
  }
  
  #adjust time unit to model
  .hist$t <- yeartot(.hist$t)
  t_historical<-unique(.hist$t)
  #adjust scenario names
  if(exists("witch_regions")) .hist$n  <- dplyr::recode(.hist$n, !!!setNames(witch_regions, display_regions))
  #change set name and element if map_var_hist has set_model defined
  if(exists("map_var_hist")){
    if(!all(map_var_hist[varname_model==varname_original]$set_model=="")){
      .hist <- .hist %>% filter(c_across(map_var_hist[varname_model==varname_original]$set_model[1]) %in% map_var_hist[varname_model==varname_original]$element_witch) #keep only the data that are map_var_hist given for
      set_map <- map_var_hist[varname_model==varname_original]$element_model; names(set_map) = map_var_hist[varname_model==varname_original]$element_witch
      .hist <- .hist %>% mutate(across(all_of(map_var_hist$set_model[map_var_hist$varname_model == varname_original]), ~ dplyr::recode(., !!!set_map))) #map and change element names from witch to model [best way to replace mapvalues]
    }
    #unit conversion if needed
    if(varname_original %in% map_var_hist$varname_model){
      .conv <- map_var_hist[varname_model==varname_original] %>% select(set_model, element_model, conv)
      if(all(.conv$set_model=="")) .hist$conv <- unique(.conv$conv) else {
        setnames(.conv, "element_model", unique(.conv$set_model))
        .hist <- .hist %>% left_join(.conv %>% select(-set_model))
      }
    .hist <- .hist %>% mutate(value =  value * conv) %>% select(-conv)
    }
  }
  
  #if check_calibration, add validation as data points!
  if(check_calibration){
    .gdx_validation <- gdx(file.path(model_dir, paste0("data_", reg_id[1]), "data_validation.gdx")) #only first reg_id
    for(.item in intersect(item, .gdx_validation$parameters$name)){.hist_validation_single <- as.data.table(.gdx_validation[.item]); .hist_validation_single$file <- gsub(paste0(tolower(varname), "_"), "", .item); if(.item==item[1]){.hist_validation <- .hist_validation_single}else{.hist_validation <- rbind(.hist_validation,.hist_validation_single)} }
    if(exists(".hist_validation") & !exists("map_var_hist")){
    if(!("n" %in% colnames(.hist_validation))){.hist_validation$n = "World"}
    colnames(.hist_validation) <- colnames(.hist)
    .hist$file <- gsub("valid", "historical", .hist$file) #for the historical set, use "historical"
    .hist <- rbind(.hist,.hist_validation)
    }else{.hist$file <- gsub("valid", "historical", .hist$file)}
  }
  else{
    #if not check_calibration and historical files are added to the scenarios, compute the mean in case multiple historical sources for one sub-item (e.g., elhydro) and drop the file column
    .hist$file <- NULL
    .hist <- .hist %>% group_by_at(setdiff(names(.hist), "value")) %>% summarize(value=mean(value), .groups = "drop") %>% as.data.table()
  }

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
  if(check_calibration){
    #just multiply by the pathdir so it appears for each pathdir
    .hist_temp <- .hist
    for(pd in basename(fullpathdir))
    {
      .hist_temp$pathdir <- pd
      if(pd==basename(fullpathdir[1])){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
    }
  }else{
  #first multiply by scenplot, add missing columns here add historical data to results
  .hist_temp <- .hist
  
    for(scen in scenplot)
    {
      .hist_temp$file <- scen
      if(scen==scenplot[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
    }
    .hist_temp <- .hist
    for(pd in basename(fullpathdir))
    {
      .hist_temp$pathdir <- pd
      if(pd==basename(fullpathdir)[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
    }
    if(overlap_years=="model"){
      #display model data for overlapping years, delete historical data
      .hist <- subset(.hist, !(t %in% seq(1,10)))
    }else{
      #or display historical data years, delete model data for 2005 and 2010
      #variable <- subset(variable, !(t %in% unique(.hist$t)))
      #variable <- subset(variable, !(t %in% unique(.hist$t)))
      variable <- subset(variable, !(t %in% t_historical))
    }
  }

  if(iiasadb){
    #adjusting region names
    
    #creating same data format as iiasadb
    .hist <- .hist %>% mutate(VARIABLE=varname_original, UNIT=unique(variable$UNIT)[1], SCENARIO="historical", MODEL=file)
    .hist <- .hist %>% select(-file, -pathdir)
    #keep only historical, no valid data points
    .hist <- .hist %>% filter(!str_detect(MODEL, "valid"))
  }
  
  merged_variable <- rbind(variable, .hist)
  merged_variable$t <- as.numeric(merged_variable$t)
  if(iiasadb) merged_variable <- merged_variable %>% dplyr::rename(REGION=n) %>% mutate(t=ttoyear(t)) %>% dplyr::rename(YEAR=t) %>% mutate(VARIABLE=gsub("_","|",VARIABLE))
  if(iiasadb) merged_variable <- merged_variable %>% mutate(REGION=toupper(REGION)) #for now use upper case for all regions
  #remove additional columns and set elements if no set_model given but set-witch is not empty
  if(exists("map_var_hist")) if(map_var_hist[varname_model==varname_original]$set_witch[1]!="" & map_var_hist[varname_model==varname_original]$set_model[1]=="") merged_variable <- merged_variable %>% filter(get(map_var_hist[varname_model==varname_original]$set_witch)==map_var_hist[varname_model==varname_original]$element_witch) %>% select(-one_of(map_var_hist[varname_model==varname_original]$set_witch)) 
  return(merged_variable)
}
