
#Regional or global line plots of already loaded data
plot_witch <- function(data, varname="value", regions="World", scenplot=scenlist, ylab=varname, ylim0=FALSE, conv_factor=1, nagg="sum", rm.NA = T){
  line_size = 1.5;
  data <- subset(data, file %in% c(scenplot, "historical") & ttoyear(t) <= yearmax & ttoyear(t) >= yearmin)
  if(rm.NA) data <- subset(data, !is.na(get(varname)))
  if(regions[1]=="World"){
    if(nagg=="sum"){data <- data %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(-n), sum) %>% mutate(n="World")}else
    if(nagg=="mean"){data <- data %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(-n), mean) %>% mutate(n="World")}
    }else{data <- data %>% filter(n %in% regions)}
  data <- data %>% mutate(plot_value=!!parse_quo(varname, env = .GlobalEnv)*conv_factor)
  p <- ggplot() + xlab("") +ylab(ylab)
  if(ylim0) p <- p + ylim(0, NA)
  if(regions[1]=="World" | length(regions)==1){
    p <- p + geom_line(data = data %>% filter(file!="historical"), aes(ttoyear(t),plot_value,colour=file), stat="identity", linewidth=line_size) + geom_line(data = data %>% filter(file=="historical"), aes(ttoyear(t),plot_value,colour=file), stat="identity", linewidth=0.5)
  }else{
    p <- p + geom_line(data = data %>% filter(file!="historical"), aes(ttoyear(t),plot_value,colour=n, linetype=file), stat="identity", linewidth=line_size) + scale_colour_manual(values = region_palette) + geom_line(data = data %>% filter(file=="historical"), aes(ttoyear(t),plot_value,colour=n, linetype=file), stat="identity", linewidth=0.5)
  }
  if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  return(p)
}





#get all the WITCH variables
get_plot_witch <- function(variable_name, additional_set="na", additional_set_id="na", convert=1, unit="", aggregation="regional", plot=TRUE, bar="", bar_x="time", bar_y="value", bar_setvalues="", bar_colors="", regions=witch_regions, scenplot=scenlist){
  #aggregation=none: no graph is created no aggregation performed, just loads the element
  #some default values, maybe not even needed to customize
  #removepattern="results_"
  #DEBUG:
  #variable_name="Q_OUT"; additional_set="f"; additional_set_id="oil"; convert=1; unit=""; aggregation="regional"; cumulative=FALSE; plot=TRUE; bar=""; bar_x="time"; bar_y="value"; bar_setvalues=""; bar_colors=""; regions=witch_regions; scenplot=scenlist; variable_field="l"; current_pathdir = fullpathdir[1]; file <- filelist[1];
  line_size = 1.5;
  show_legend_title = F
  if(additional_set_id=="all"){plot=FALSE}
  line_colour = "file"; line_type="pathdir"; #defaults for colour and linetype aesthetics
  
  #CALL MAIN READING FUNCTION
  allfilesdata <- get_witch(variable_name)
  
  if(exists("allfilesdata")){
  
    #clean data, edit sets, convert, replace NAs
    allfilesdata <- subset(allfilesdata, file %in% scenplot)
    #unit conversion
    unit_conversion <- unit_conversion(variable_name, unit, convert)
  
    #get time frame needed
    if(("t" %in% colnames(allfilesdata)) & !(variable_name=="t")) allfilesdata <- subset(allfilesdata, allfilesdata$t <= yeartot(yearmax) & allfilesdata$t >= yeartot(yearmin))
    
    #get reporting items to same structure
    if(any(colnames(allfilesdata)=="nrep"))
    {
      setnames(allfilesdata, "nrep", "n")
      allfilesdata <- allfilesdata[allfilesdata$n %in% regions]
    }
    
    if (additional_set != "na" & additional_set_id != "all" & additional_set_id != "sum") 
    {
      allfilesdata <- subset(allfilesdata, tolower(get(additional_set))==as.character(additional_set_id))
      #finally remove column containing the only remaining set since values are in "values" 
      allfilesdata <- subset(allfilesdata, select=-get(additional_set))
    }
    if (additional_set != "na" & additional_set_id == "sum")   #sum over the set if "sum" is choosen
    {
      if(length(fullpathdir)>=1){allfilesdata <- aggregate(value~n+t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~n+t+file, data=allfilesdata, sum, na.rm=TRUE)}
      allfilesdata <- as.data.frame(allfilesdata)
      allfilesdata <- as.data.table(allfilesdata)
    }    
    allfilesdata[is.na(allfilesdata)] <- 0
    allfilesdata$value = allfilesdata$value * unit_conversion$convert 
    #assign("test",allfilesdata,envir = .GlobalEnv)
    
    
    allfilesdata$n <- as.factor(allfilesdata$n)
    #if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
    #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
    
    
    #Plot for each variable
    if (aggregation == "global_sum")
    {
      allfilesdata$n <- NULL
      if(length(fullpathdir)>=1){allfilesdata <- aggregate(value~t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~t+file, data=allfilesdata, sum)}
      #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
      allfilesdata <- as.data.table(allfilesdata)
      #try for RCP:
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", linewidth=line_size) + xlab("year") +ylab(unit_conversion$unit)
      if(show_legend_title){p <- p + labs(linetype=line_type, colour=line_colour)}else{p <- p + theme(legend.title=element_blank())} 
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(fullpathdir)==1){p <- p + guides(linetype="none")}
      if(plot){saveplot(variable_name)}
    } 
    if (aggregation == "global_mean")
    {
      allfilesdata$n <- NULL      
  allfilesdata <- allfilesdata %>% group_by_at(c("pathdir", file_group_columns, "t")) %>% summarize(value=mean(value), .groups = "drop")
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", linewidth=line_size) + xlab("year") +ylab(unit_conversion$unit) + labs(linetype=line_type, colour=line_colour)
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(fullpathdir)==1){p <- p + guides(linetype="none")}
      if(plot){saveplot(variable_name)}
    } 
    if (aggregation == "regional") 
    {
      p <- ggplot(subset(allfilesdata, n %in% regions),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", linewidth=line_size) + xlab("year") +ylab(unit_conversion$unit) + scale_colour_manual(values = region_palette)
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(plot){saveplot(variable_name)}
    }
    if (aggregation == "all") 
    {
      allfilesdata <- allfilesdata
    }
    if (bar != "")  #area or bar, share or values
    {
      allfilesdata <- subset(allfilesdata, t%%2==0) #now also take only 10 year steps
      
      if(bar=="set"){
        if(bar_setvalues[1] != ""){allfilesdata <- subset(allfilesdata, get(additional_set) %in% bar_setvalues)}
        if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
        if(length(fullpathdir)>=1){allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set, "pathdir")]}else{allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set)]}
        allfilesdata$n <- NULL
        if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
        if(bar_setvalues[1] != ""){allfilesdata[[additional_set]] <- reorder.factor(allfilesdata[[additional_set]], new.order=bar_setvalues)}   #to keep order from setlist in function call
        if(bar_y=="share"){if(length(fullpathdir)!=1){allfilesdata <- allfilesdata %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% mutate(value=value/(sum(value))*100)}else{allfilesdata <- allfilesdata %>% group_by_at(c("t", "file")) %>% mutate(value=value/(sum(value))*100)}}
        if(str_detect(bar_x, "time")){
          if(!is.na(destring(bar_x))){allfilesdata <- subset(allfilesdata, t==yeartot(destring(bar_x)))}
          p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, fill=get(additional_set))) + geom_bar(stat="identity") + xlab("year") + facet_grid( ~ file) + guides(fill=guide_legend(title=NULL)) 
          if(bar_colors[1]!=""){p <- p + scale_fill_manual(values=bar_colors)}  
        }else{
          p <- ggplot(data=subset(allfilesdata, t==yeartot(destring(bar_x))),aes(file,value, fill=get(additional_set))) + geom_bar(stat="identity") + xlab("scenario") + guides(fill=guide_legend(title=NULL)) 
          if(bar_colors[1]!=""){p <- p  + scale_fill_manual(values=bar_colors)}            
        }
      }
      if(bar=="region"){
        allfilesdata[["n"]] <- reorder.factor(allfilesdata[["n"]], new.order=regions)   #to keep order from setlist in function call
        if(bar_y=="share"){if(length(fullpathdir)!=1){allfilesdata %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% mutate(value=value/(sum(value))*100)}else{allfilesdata <- allfilesdata %>% group_by_at(c("t", "file")) %>% mutate(value=value/(sum(value))*100)}}
        if(str_detect(bar_x, "time")){
          if(!is.na(destring(bar_x))){allfilesdata <- subset(allfilesdata, t==yeartot(destring(bar_x)))}
          p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, fill=n)) + geom_bar(stat="identity") + xlab("year") + facet_grid( ~ file) + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)
        }else{
          p <- ggplot(data=subset(allfilesdata, t==yeartot(destring(bar_x))),aes(file,value, fill=n)) + geom_bar(stat="identity") + xlab("scenario") + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)          
        }
      }
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ file)}
   	  if(plot){saveplot(variable_name)}
    } 
    #save the variable under the WITCH name in the global environment
    if(additional_set_id!="na") variable_name <- paste0(variable_name, "_", additional_set_id)
    assign(variable_name,allfilesdata,envir = .GlobalEnv)
  }
  #END OF THE STANDARD GRAPHS AND DATA READ LOOP    
}




getvar_witchhist <- function(varname, unit_conversion=1, hist_varname=varname, additional_sets=NA, ylab=varname){
  #additional _sets: e.g., c("iq"="y", "e="co2")
  tempvar <- get_witch(varname)
  n_model <- unique(tempvar$n)
  if(!is.na(additional_sets)){
    for(s in 1:length(additional_sets)) tempvar[[names(additional_sets[s])]] <- additional_sets[s]
  }
  tempvar$value <- tempvar$value * unit_conversion;
  tempvar <- add_historical_values(tempvar, varname = hist_varname)
  tempvar <- tempvar %>% filter(n %in% n_model)
  print(ggplot(tempvar) + geom_line(aes(ttoyear(t), value, color=file, linetype=n)) + xlab("") + ylab(ylab))
  assign(varname, tempvar, envir = .GlobalEnv)
}


#Function to create a snapshot and setup the data for gdxompaR witch-online to be self-containted and deployed e.g. through shinyapps.io
create_witch_plot_online <- function(list_of_variables=c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "I", "I_RD", "MCOST_INV", "COST_EMI", "MCOST_EMI", "CPRICE", "MCOST_FUEL", "TEMP", "TRF", "OMEGA", "Q_IN", "ykali", "tpes", "carbonprice", "emi_cap", "l"), deploy = F) {
  #preload all variables (execult eht followig lines separately before deploying)
  aux_vars <- c("ghg", "csi", "allerr", "allinfoiter", "all_optimal", "all_feasible", "price_iter")
  allvariables <- lapply(c(aux_vars, list_of_variables), get_witch)
  names(allvariables) <- as.list(c(aux_vars, list_of_variables))
  #now also store as variables in the environment
  for(i in 1:length(allvariables)) assign(names(allvariables)[i], allvariables[[i]])
  if(file.exists("gdxcompaR//witch//allvariables.Rdata")) file.remove("gdxcompaR//witch//allvariables.Rdata")
  deploy_online <<- TRUE
  save.image(file="gdxcompaR//witch//allvariables.Rdata")
  #deploy app
  if(deploy){
    library(rsconnect)
    deployApp(appDir = "gdxcompaR/witch")
  }
}

