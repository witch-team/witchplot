#Auxiliary Functions

ttoyear <- function(t, tlen = NULL){
  if(is.null(tlen)) {
    if(exists("tlen", envir = parent.frame())) {
      tlen <- get("tlen", envir = parent.frame())
    } else {
      tlen <- tstep
    }
  }
  year = ((as.numeric(t)-1) * tlen + year0)
  return(year)
}

yeartot <- function(year, tlen = NULL){
  if(is.null(tlen)) {
    if(exists("tlen", envir = parent.frame())) {
      tlen <- get("tlen", envir = parent.frame())
    } else {
      tlen <- tstep
    }
  }
  t = ((as.numeric(as.character(year)) - year0) / tlen) + 1
  return(t)
}


convert_pdftopng <- F #converts all created pdfs to png for better quality (needs pdftopng.exe in your PATH. Download from http://www.xpdfreader.com/download.html)
saveplot <- function(plotname, width=7, height=5, text_size=16, suffix="", transparent=FALSE, add_title=TRUE, forpaper=F, plotdata=NA){
  if(!deploy_online) if(!dir.exists(graphdir)){dir.create(graphdir)} #create directory for graphs
  if(figure_format!="png"){transparent = FALSE}
  if(figure_format=="pdf"){plot_device=cairo_pdf}else{plot_device=figure_format}
  if(figure_format=="eps"){plot_device=cairo_ps}
  #device=cairo_pdf makes PDFs work with greek symbols etc.
  legend_position <- "bottom"
  legend_direction <- "horizontal"
  if(transparent){transparent_background <- theme(legend.background = element_blank(), panel.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))}else{transparent_background = NULL}
  print(ggplot2::last_plot()) 
  if(!deploy_online){
  ggsave(filename=file.path(graphdir,paste0(as.character(gsub("[ |_|-]", "_", plotname)),suffix,".",figure_format)), plot = ggplot2::last_plot() + if(add_title){labs(title=plotname)}else{labs(title="")} + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()), width=width, height=height, bg = "white", device = plot_device)
  if(figure_format=="pdf" & convert_pdftopng) shell(str_glue('pdftopng.exe {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", figure_format))} - > {file.path(graphdir, paste0(as.character(gsub(" ", "_", plotname)),".", "png"))}'))
  if(write_plotdata_csv){
    if(!is.data.frame(plotdata)) plotdata <- ggplot_build(ggplot2::last_plot())$plot$data
    if("t" %in% colnames(plotdata)){plotdata$t <- ttoyear(plotdata$t)}
    if(length(plotdata)>0) write.xlsx(plotdata, file = file.path(graphdir,paste0(as.character(gsub("[ |_|-]", "_", plotname, suffix)),".xlsx")))}
  if(forpaper){
  if(!dir.exists(file.path(graphdir, "forpaper"))){dir.create(file.path(graphdir, "forpaper"))}
  file.copy(file.path(graphdir,paste0(as.character(gsub("[ |_|-]", "_", plotname)),suffix,".",figure_format)), file.path(graphdir, "forpaper", paste0(as.character(gsub("[ |_|-]", "_", plotname)),suffix,".",figure_format)), overwrite = T)
  }
  }
}


#function to compare all scenario to one
add_change_from_reference <- function(data, refscen="ssp2_bau"){
  data <- data %>% left_join(data %>% filter(file==refscen) %>% dplyr::rename(value_ref=value) %>% select(-file_group_columns))
  data <- data %>% mutate(value_difference = value - value_ref, value_percent_change=value/value_ref-1)
  return(data)
}


filetosep <- function(df, type = "separate", names = "file_new", sep = "_"){
  #type: separate or just last; name = name of the new column(s)
  # for historical or cvalidation, instead turn to NA
  df_hist <- df %>% filter(str_detect(file, "valid|historical"))
  if(nrow(df_hist)>0){
    for(n in names) df_hist[n] <- NA
    df <- df %>% filter(!str_detect(file, "valid|historical"))
  }
  if(type == "separate") {
    numsep = max(str_count(unique(df$file), pattern = sep))
    if(names[1]=="file_new") name <- paste0("f",seq(numsep))
    df <- df %>% mutate(file_new=file) %>% separate(file_new, names, sep = sep)
  }
  if (type == "last") {df$fx <- word(df$file,-1,sep = paste0("\\",sep)); setnames(df, "fx", names)}
  if (type == "first") {df$fx <- word(df$file,1,sep = paste0("\\",sep)); setnames(df, "fx", names)}
  if(nrow(df_hist)>0) df <- rbind(df, df_hist)
  return(df)
}


readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}


#this function produced a plottable map df, accepts one of the regional default mappings of witchtools or a mapped df from iso3 to your mapping
make_map_df <- function(region_mapping="witch17") {
  maps <- ggplot2::map_data("world")
  maps$ISO <- countrycode::countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
  maps <- dplyr::as_tibble(maps)
  if (is.character(region_mapping) ) {  
    if (! region_mapping %in% names(witchtools::region_mappings)) stop("Pass a regional aggregation present in witchtools or directly a dataframe with the mapping iso3 to region_mapping")
    reg <- left_join(maps %>% rename(iso3=ISO),witchtools::region_mappings[[region_mapping]] %>% rename(n = !!sym(region_mapping)))
  }
  else if ( length(dplyr::intersect(class(region_mapping),c("data.frame","data.table","tibble"))) > 0) 
  {
    reg <- left_join(maps %>% rename(iso3=ISO),region_mapping %>% rename(n = !!sym(arules::setdiff(names(region_mapping),c("iso3")))) )
  }
  else stop("Please choose a valid input (string character or dataframe)")
  return(reg)}

#make global transformations: options sum,mean, median, weighted mean (w_mean)
make_global_tr <- function(data,cols=c("t","n","file"),weights="w",nagg="sum") { 
  group_cols <- arules::setdiff(cols,"n")
  nms <- arules::setdiff(names(data),cols)
  if(nagg=="sum"){data <- data %>% group_by_at(group_cols) %>% summarise_at(., nms, sum) %>% mutate(n="World")}  
  if(nagg=="mean"){data <- data %>% group_by_at(group_cols) %>% summarise_at(., nms, mean) %>% mutate(n="World")}
  if(nagg=="median"){data <- data %>% group_by_at(group_cols) %>% summarise_at(., nms, median) %>% mutate(n="World")}
  if(nagg=="w_mean"){
    nms <- arules::setdiff(names(data),c(cols,weights))
    data <- data %>% group_by_at(group_cols) %>% summarise_at(., nms, ~weighted.mean(.,w) ) %>% mutate(n="World")}
  return(data) }

# make cumulative of variables
make_cumulative <- function(data,cols=c("t","n","file"),yearstart=2020,yearend=2100,dr=0) { 
  group_cols <- arules::setdiff(cols,"t")
  nms <- arules::setdiff(names(data),cols)
  
  data <- data %>% 
    filter(ttoyear(t)>=yearstart & ttoyear(t)<=yearend) %>%
    group_by_at(group_cols) %>% 
    complete(t=full_seq(t,0.2)) %>% 
    mutate_at(., nms, zoo::na.approx ) %>%
    summarise_at(nms, ~sum(./((1+dr)^(ttoyear(t)-yearstart)) ) ) %>%
    mutate(t=paste0(yearstart,"to",yearend))
  return(data) }



unit_conversion <- function(variable_name, unit="", convert=1){
  #if unit is not "", keep its unit and convert using convert factor
  if(unit!=""){
    unit_plot <- unit; unit_conversion <- convert
  }else{
  #automatic unit and conversion factor
  variable_description <- ""
  if(variable_name %in% all_var_descriptions$name) variable_description <- all_var_descriptions$description[match(variable_name, all_var_descriptions$name)]
  unit_witch <- gsub(".*\\[(.*).*", "\\1", sub(" *\\].*", "", variable_description))
  if(is.na(unit_witch) | unit_witch==""){unit_witch="na"}
  unit_conversion_table <-"witch_unit plot_unit conversion_factor
  TWh       EJ                 0.0036
  T$        'billion USD'      1e3
  T$/TWh    $/GJ               277777.777778
  GtCe      GtCO2              3.67
  TW        GW                 1e3
  T$/GTon   $/tCO2             272.727272727
  T$/GTonC  $/tCO2             272.727272727
  T$/GtCeq  $/tCO2             272.727272727
  GTonC     GtCO2              3.67
  GtonC     GtCO2              3.67
  GtCe      GtCO2              3.67
  na        na                 1   
  'deg C above preindustrial levels'    'degree C'   1
  "
  unit_conversion_table <- read.table(textConnection(unit_conversion_table), sep="", head=T, dec=".")
  unit_plot = unit_witch;unit_conversion=1 #by default, keep original
  if(!is.na(match(unit_witch, unit_conversion_table$witch_unit))){
    unit_plot <- as.character(unit_conversion_table$plot_unit[match(unit_witch, unit_conversion_table$witch_unit)])
    unit_conversion <- unit_conversion_table$conversion_factor[match(unit_witch, unit_conversion_table$witch_unit)]}
  }
  
  #Finally, for specific variables apply custom unit and conversion
  unit_conversion_user_specific <-"varname plot_unit conversion_factor
  tpes       EJ                 0.0036
  tpes_kali  EJ                 0.0036
  ei_kali    MJ/$               1     
  "
  unit_conversion_user_specific <- read.table(textConnection(unit_conversion_user_specific), sep="", head=T, dec=".")
  if(variable_name %in% unit_conversion_user_specific$varname){
    unit_plot <- unit_conversion_user_specific$plot_unit[unit_conversion_user_specific$varname==variable_name]
    unit_conversion <- unit_conversion_user_specific$conversion_factor[unit_conversion_user_specific$varname==variable_name]
  }
  #dollar deflator conversion if other base year than 2005
  usd_deflator = 1 #by default, all values in 2005 USD
  #usd_deflator = 108.686/91.987  #2014 USD
  #usd_deflator = 1.10774   #2010 USD
  if(str_detect(unit_plot, "$") | str_detect(unit_plot, "USD")){unit_conversion <- unit_conversion * usd_deflator}
  return(list(unit=unit_plot, convert=unit_conversion))
}



# default meta param (taken from scaling.R)
default_meta_param <- function(){
  "parameter,value
  I,sum
  K,sum
  Q,sum
  BAU_Q,sum
  COST_EN,mean
  COST_FUEL,mean
  I_EN,sum
  K_EN,sum
  MCOST_INV,mean
  MCOST_FUEL,mean
  Q_EN,sum
  Q_IN,sum
  Q_FUEL,sum
  SHARE_EL,mean
  COST_EMI,mean
  CUM_EMI,sum
  Q_EMI,sum
  BAU_Q_EMI,sum
  I_RD,sum
  K_RD,sum
  K_RD_F,sum
  SPILL,mean
  ABAT,sum
  Q_WBIO,sum
  Q_REDD,sum
  MCOST_EMI,mean
  I_EN_WINDOFF,sum
  I_EN_WINDON,sum
  K_EN_WINDOFF,sum
  K_EN_WINDON,sum
  Q_EN_WINDOFF,sum
  Q_EN_WINDON,sum
  I_EN_PV,sum
  I_EN_CSP,sum
  K_EN_PV,sum
  K_EN_CSP,sum
  Q_EN_PV,sum
  Q_EN_CSP,sum
  Q_EL_FLEX,sum
  K_EN_GRID,sum
  I_EN_GRID,sum
  ADDOILCAP,sum
  COST_OIL,mean
  CUM_OIL,sum
  I_OIL,sum
  I_OUT,sum
  OILCAP,sum
  OILPROD,sum
  Q_EMI_OUT,sum
  Q_OUT,sum
  RF,max
  TEMP,max
  TRF,max
  W_EMI,max
  WCUM_EMI,max
  OMEGA,mean
  QEL_EDV,sum
  QEL_FR_EDV,sum
  emi_cap,sum
  ken_policy,sum
  ren_share,mean
  temp_valid_hadcrut4,mean
  ctax,mean
  carbonprice,mean
  CPRICE,mean
  FPRICE,mean
  " -> defmap
  dm <- fread(defmap)
  dm[,type:="nagg"]
  dm = rbind(dm,data.table(parameter=dm$parameter, type="nweight", value="gdp"))
  setcolorder(dm,c("parameter", "type", "value"))
  return(dm)
}










