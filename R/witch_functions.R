## Further Options ##
#restrict_regions <- c("usa") # if exists, only these regions will be loaded everywhere
#Change some regions to nice names in case (regions not specified will use standard witch name)
#nice_region_names <- c("ccasia"="Caucasus and Central Asia", "china"="PRC", "india"="India", "indonesia"="Indonesia", "sasia"="South Asia", "seasia"="Southeast Asia")
deploy_online <- FALSE #if not deployed online save graphs
figure_format="png"
historical = TRUE  #add historical data where available
ggplot2::theme_set(ggplot2::theme_bw()) #set default theme
show_numbers_2100 = FALSE
legend_position="bottom"    # "none", "bottom", or "right"
write_plotdata_csv = F #if true, saves data of plot as csv file
varlist_combine_old_new_j <- c("Q_EN", "K_EN", "I_EN", "Q_IN")  #variables for which to combine old and new j technologies
if(!exists("year0")) {year0 = 2005; tstep = 5;}
if(!exists("yearmin")) yearmin = 1980
if(!exists("yearmax")) yearmax = 2100
## End of further Options ##

witch_folder <- normalizePath(witch_folder)
main_folder <- normalizePath(main_folder)

fullpathdir = file.path(main_folder, subdir)
#Specify directory for graphs and data to be saved: by default: /graphs/ in the folder
graphdir = if(length(fullpathdir)>1){file.path(main_folder, "graphs") }else{file.path(fullpathdir, "graphs")}

#check if directory valid
if(any(!dir.exists(fullpathdir))){stop("Please check the main directory and sub directory!")}
if(!dir.exists(witch_folder)){stop("Please check your witch directory!")}

# witchtools
if (!"witchtools" %in% rownames(installed.packages())) {
  if (!"remotes" %in% rownames(installed.packages()))
    install.packages("remotes", repos = "http://cloud.r-project.org")
  remotes::install_github("witch-team/witchtools")
  if (!requireNamespace("witchtools")) stop("Package witchtools not found")
}
library(witchtools)

#Install and load packages
require_package <- function(package){
  if(!is.element(package, .packages(all.available = TRUE))){
    try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
  }
  suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
}

pkgs <- c('data.table', 'stringr', 'docopt', 'countrycode', 'ggplot2', 
          'ggpubr', 'scales', 'RColorBrewer', 
          'dplyr', 'openxlsx',
          'gsubfn', 'tidyr', 'rlang', 'shiny', 'shinyWidgets','bslib',
          'shinythemes', 
          'rworldmap',
          'sf', 'rnaturalearth', 'plotly', 'purrr', 
          #'reldist', 
          'tidytidbits',
          'forcats', 'arrow', 'memoise')
res <- lapply(pkgs, require_package)
require_gdxtools()
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#load basic functions
source('R/auxiliary_functions.R')
source('R/witch_load_and_plot.R')
source('R/add_historical_values.R')
source('R/get_iiasadb.R')
source('R/get_witch.R')


#from here only if GDX files are loaded
if(!exists("iamc_filename") & !exists("iamc_databasename")){
filelist <- gsub(".gdx","",list.files(path=fullpathdir[1], full.names = FALSE, pattern="*.gdx", recursive = FALSE))
if(!exists("restrict_files")) restrict_files <- "results_"
if(restrict_files[1]!=""){
  for(i in 1:length(restrict_files)){
    .filelist_res = filelist[apply(outer(filelist, restrict_files[i], str_detect), 1, all)]
    if(i==1) .filelist_res_all <- .filelist_res else .filelist_res_all <- c(.filelist_res_all, .filelist_res)
  }
  filelist <- unique(.filelist_res_all)
}
if(exists("exclude_files")) if(exclude_files[1]!="") filelist = filelist[!str_detect(filelist, paste(exclude_files, collapse = '|'))]
if(length(filelist)==0){stop("No GDX files found.")}
if(exists("scenlist")){
  #check if missing scenarios in scenlist
  if(length(names(scenlist[!(names(scenlist) %in% filelist)]))>0){print("Missing Scenarios:"); print(cat(names(scenlist[!(names(scenlist) %in% filelist)]), sep=", "))}
  filelist <- intersect(names(scenlist), filelist)
  scenlist <- scenlist[filelist]
  }
if(!exists("removepattern")) removepattern <- "results_"
if(!exists("scenlist")){scenlist <- gsub(paste(removepattern, collapse="|"), "", filelist); names(scenlist) <- filelist}
#print("GDX Files:")
#print(filelist)
#print(paste("Scenarios used:", length(scenlist)))
print(data.frame(scenlist=scenlist))

#file to separate check
if(exists("file_separate")) file_group_columns <- c("file", unname(file_separate[3:length(file_separate)])) else file_group_columns <- "file"

#check if multiple time steps
if(length(unique(suppressWarnings(batch_extract("tlen", files = file.path(fullpathdir,paste0(filelist,".gdx"))))$tlen$value)) > 1) flexible_timestep <- T else flexible_timestep <- F #allow for flexible time step

#in case some runs are stochastic, set flag and provide mapping
tset <- get_witch("t")
if("t" %in% names(tset)){
  if(any(str_detect((tset %>% select(t) %>% unique())$t, "_"))){
  stochastic_files <- tset %>% filter(str_detect(t, "_")) %>% mutate(numeric_t = as.numeric(sub(".*_(\\d+)$", "\\1", t))) %>% group_by(file) %>% summarise(num_branches = max(numeric_t, na.rm = TRUE))
  }else{stochastic_files <- NULL}
}else{stochastic_files <- NULL}


#get variable description of all variables from the 1st file
mygdx <- gdx(file.path(fullpathdir[1],paste0(filelist[1],".gdx")))
all_var_descriptions <- rbind(data.frame(name=mygdx$variables$name, description=mygdx$variables$text), data.frame(name=mygdx$parameters$name, description=mygdx$parameters$text))

#Palettes for WITCH regions and regional aggregation
if(!exists("reg_id")){
conf <- get_witch("conf")
if(!(exists("conf"))) stop("No conf set found. Please specify region_i = x manually!")
if(length(unique(subset(conf, V1=="regions")$V2))>1) print("Be careful: not all results files were run with the same regional aggregation!")
reg_id <- subset(conf, file==scenlist[1] & pathdir==basename(fullpathdir[1]) & V1=="regions")$V2
}
n <- suppressWarnings(batch_extract("n", files = file.path(fullpathdir,paste0(filelist,".gdx"))))
if(is.null(n$n)) {witch_regions <- "World"} else witch_regions <- unique(n$n$V1)

if(exists("nice_region_names")) witch_regions <- dplyr::recode(witch_regions, !!!nice_region_names)
display_regions <- witch_regions

if(!dir.exists(file.path(witch_folder, paste0("data_", reg_id)))) print("No data_* directory for historical data found.")

region_palette_specific <- setNames(rainbow(length(witch_regions)), witch_regions) #just in case have a fall back colour
region_palette_witch <- c(usa="darkblue",Usa="darkblue",oldeuro="blue", neweuro="cornflowerblue",kosau="darkgreen",Kosau="darkgreen",cajaz="chartreuse4",Cajaz="chartreuse4",te="gold2",Te="gold2",mena="darkgoldenrod4",Mena="darkgoldenrod4",ssa="goldenrod",Ssa="goldenrod",sasia="darkorange2","South Asia"="darkorange2",china="deeppink3",PRC="deeppink3",easia="orangered",ESEAP="orangered",laca="#fbb714",Laca="#fbb714",india="#fbf003",India="#fbf003",europe="blue",Europe="blue",indonesia="lightsalmon3",Indonesia="lightsalmon3",Rest_of_World="grey48",chinaw="darkorange",chinac="darkorange2",chinae="darkorange4",italy="green",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered",World="black", "Global Pool"="black")
#add ed57 region colors for RICE50+
region_palette_ed57 <- c("arg" =  "#000000","aus" =  "#48d1cc","aut" =  "#ae8000","bel" =  "#800000","bgr" =  "#003366","blt" =  "#bf4040","bra" =  "#ffd633","can" =  "#6600cc","chl" =  "#ffece6","chn" =  "#ff531a","cor" =  "#adebad","cro" =  "#808080","dnk" =  "#ff9933","egy" =  "#0044cc","esp" =  "#ffd6cc","fin" =  "#00cccc","fra" =  "#cc0000","gbr" =  "#ffffdd","golf57"  =  "#33d6ff","grc" =  "#00ffcc","hun" =  "#9999ff","idn" =  "#996633","irl" =  "#ff4dff","ita" =  "#ffff00","jpn" =  "#006600","meme"=  "#b32d00","mex" =  "#ccff33","mys" =  "#145252","nde" =  "#00d900","nld" =  "#c309bd","noan"=  "#ffff99","noap"=  "#ecf2f9","nor" =  "#ff3399","oeu" =  "#ffb3ff","osea"=  "#008fb3","pol" =  "#d6f5d6","prt" =  "#003300","rcam"=  "#4d1919","rcz" =  "#00ffff","rfa" =  "#deb887","ris" =  "#000080","rjan57"  =  "#bf00ff","rom" =  "#ff00ff","rsaf"=  "#ff8000","rsam"=  "#0000ff","rsas"=  "#ccd6dd","rsl" =  "#00ff00","rus" =  "#66757f","slo" =  "#ff3091","sui" =  "#61a62f","swe" =  "#cb1942","tha" =  "#efff14","tur" =  "#4b0082","ukr" =  "#c198ff","usa" =  "#ffcc00","vnm" =  "#3377ff","zaf" =  "#b3ccff")
#Add witch34 region colors
region_palette_witch34 <- c("bnl" =  "#800000","northeu" =  "#bf4040","balkan" =  "#808080","easteu" =  "#9999ff", "che"="#61a62f", "deu" =  "#deb887", "rou" =  "#ff00ff", "cze" =  "#00ffff", "japan"="green", korea="red")
region_palette <- replace(region_palette_specific, names(region_palette_witch), region_palette_witch)
region_palette <- replace(region_palette, names(region_palette_ed57), region_palette_ed57)
region_palette <- replace(region_palette, names(region_palette_witch34), region_palette_witch34)
#now keep only palette for regions actually used
region_palette <- region_palette[witch_regions]
if(exists("restrict_regions")) region_palette <- region_palette[restrict_regions]

print(paste(length(scenlist), "Scenarios and", length(witch_regions), "regions considered."))

witch_name_short <- function(witch_name){
  witch_name  <- gsub("indonesia", "IDN", witch_name)
  witch_name_shortened <- substr(toupper(witch_name), 1, 3)
  witch_name_shortened <- gsub("MEN", "MEA", witch_name_shortened)
  witch_name_shortened <- gsub("SOU", "ZAF", witch_name_shortened)
  witch_name_shortened <- gsub("CHI", "CHN", witch_name_shortened)
  witch_name_shortened <- gsub("TE", "TEC", witch_name_shortened)
  return(witch_name_shortened)
}
region_palette_specific_short <- region_palette; names(region_palette_specific_short) <- witch_name_short(names(region_palette_specific))

witch_region_names <-"n,longname
canada,Canada
jpnkor,Japan-Korea
oceania,Oceania
indonesia,Indonesia
southafrica,South Africa
brazil,Brazil
mexico,Mexico
china,China
india,India
te,Transition Economies
ssa,Sub-Saharan Africa
laca,Latin America-Caribbean
sasia,South Asia
seasia,South East Asia
mena,Middle East-North Africa
europe,Europe
usa,United States of America
easia,East Asia
kosau,South-Korea and Australia
cajaz,Canada Japan New-Zealand,
neweuro,Eastern Europe
oldeuro,Western Europe"
witch_region_names <- read.table(textConnection(witch_region_names), sep=",", head=T, dec=".")
region_palette_longnames <- region_palette
names(region_palette_longnames) <- dplyr::recode(names(region_palette), !!!setNames(paste0(as.character(witch_region_names$longname), " (",as.character(witch_region_names$n),")"), as.character(witch_region_names$n)))

#load specialized functions
source('R/map_functions.R')
source('R/export_variables.R')
source('R/diagnostics.R')
source('R/impact_plots.R')
source('R/energy_plots.R')
source('R/emission_plots.R')
source('R/climate_plots.R')
source('R/policy_cost.R')
source('R/inequality_plots.R')
source('R/RICE50x_plots.R')
}