#Functions for creating Maps



witchmap <- function(variable_report, file_report=scenlist[1], t_report=20, scale_min=0, scale_max=0, mapcolor="Reds", map_name="map", map_legend="Legend", add_region_names=FALSE, add_bars=FALSE, figure_format = "png"){
  #Palettes: Diverging: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
  #Palettes: Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
  #Palettes: Sequential: Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
  savemap <- function(plotname, figure_format = "png"){
    if(figure_format=="pdf"){plot_device=cairo_pdf}else{plot_device=figure_format}
    if(!deploy_online) if (!dir.exists(graphdir)){dir.create(graphdir)}
    print(ggplot2::last_plot()) 
    if(!deploy_online) ggsave(filename=file.path(graphdir,paste0(gsub(" ", "_", as.character(plotname)),"_map.", figure_format)), plot = ggplot2::last_plot(), width=14, height=7, device = plot_device)
  }
  # Get World data
  Nations <- data.table(map_data("world"))
  Nations <- Nations[region != "Antarctica"]
  Nations <- Nations[region != "Greenland"]
  Nations <- Nations[region == "USSR", region:="Russia" ]
  Nations <- Nations[region == "Zaire", region:="Congo" ]
  Nations <- Nations[region == "Czechoslovakia", region:="Germany" ]
  Nations <- Nations[region == "Yugoslavia", region:="Serbia" ]
  Nations <- Nations[region == "Sicily", region:="Italy" ]
  Nations <- Nations[region == "Sardinia", region:="Italy" ]
  # Find ISO3
  countries = unique(Nations$region)
  Nations[,ISO3 := rwmGetISO3(region),by=c("region")]
  #unique(Nations[is.na(ISO3),region]) # List of non-match
  
  #now get WITCH regions
  conf <- get_witch("conf", scenplot = file_report)
  reg_id_map <- subset(conf, file==scenlist[1] & pathdir==basename(fullpathdir[1]) & V1=="regions")$V2

  # Check if model_dir exists and regions.inc file is available
  if(is.null(model_dir)){
    warning("Cannot create map: model_dir is not available. Region mapping file (regions.inc) not found.")
    return(invisible(NULL))
  }

  mod.countries.filename = file.path(model_dir, paste0("data_", reg_id_map, "/regions.inc"))

  if(!file.exists(mod.countries.filename)){
    warning(sprintf("Cannot create map: Region mapping file not found: '%s'\nPlease ensure model_dir contains the required data_<reg_id>/regions.inc file.", mod.countries.filename))
    return(invisible(NULL))
  }

  # Read mod_countries
  mod.countries = tryCatch(
    readLines(mod.countries.filename),
    error = function(e) {
      warning(sprintf("Cannot read region mapping file '%s': %s", mod.countries.filename, e$message))
      return(NULL)
    }
  )

  if(is.null(mod.countries)){
    return(invisible(NULL))
  }
  mod.countries = mod.countries[mod.countries!=""]                                  # Remove empty lines
  mod.countries = mod.countries[!str_detect(mod.countries,"^\\*")]                  # Remove * comments
  mod.countries = str_trim(str_split_fixed(mod.countries,"#",2)[,1])                # Remove # comments
  set.begin = grep("map_n_iso3(n,iso3)*",tolower(mod.countries))[1]                 
  set.end = set.begin + grep(";",mod.countries[set.begin:length(mod.countries)])[1]  
  mod.countries = mod.countries[(set.begin+1):(set.end-2)]                          # Keep mapping data
  mod.countries = str_split(mod.countries,"\\.")
  mod.countries <- data.table(matrix(unlist(mod.countries), ncol=2, byrow=T))
  setnames(mod.countries,c("n","ISO3"))
  # create mod.countries to map regions
  #add for displaying center
  mod.countries$center <- (mod.countries$ISO3%in%c("USA","BRA","CAN","AUS","NER","SAU","FRA","POL","RUS","AFG","IND","CHN","IDN"))
  data_for_map_n <- subset(variable_report, t==t_report&file==file_report)
  data_for_map_n$t <- NULL
  data_for_map_n$file <- NULL
  data_for_map_n$pathdir <- NULL
  witch_data_on_iso3 <- merge(mod.countries,data_for_map_n, by="n")
  Nations = merge(Nations,witch_data_on_iso3,by=c("ISO3"))
  #get center location:
  region_centers <- aggregate(cbind(long, lat) ~ n, data=subset(Nations), FUN=function(x)mean(x+360)-360);
  if(scale_min==0){scale_min = min(data_for_map_n$value); scale_max = max(data_for_map_n$value)}
  World.map <- ggplot(Nations, aes(x = long, y = lat))
  Country.layer <- c(geom_polygon(data = Nations, aes(x = long, y = lat, group = group, fill=value)))
  Borders.layer <- c(geom_path(data = Nations, aes(x = long, y = lat, group = group), color="darkgray", size=0.1))  
  p <- World.map + Country.layer + Borders.layer +
  theme_minimal() + labs(x = "", y =  "") +
  theme(axis.line=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.x=element_blank(),axis.title.y=element_blank(),panel.grid.major=element_blank(),plot.background=element_blank(),panel.grid.minor=element_blank()) +
  theme(legend.position="right") + 
  scale_fill_distiller(name=map_legend, palette = mapcolor, direction = 1, breaks = pretty_breaks(n = 8), limits=c(scale_min, scale_max)) + ggtitle("")
    data_bars <- merge(region_centers, data_for_map_n, by="n")
    #data_bars$mapbarvalue <- data_bars["value"]
    assign("data_bars",data_bars,envir = .GlobalEnv)
    print(data_bars)
    if(add_bars!=FALSE){
    p <- p + geom_point(data=data_bars, aes(long, lat, size = value), color="deeppink3") + scale_size_continuous()#limits=c(1,8), range = c(1, 16), guide = guide_legend(title = ""))# + scale_size_area(min_size=1, max_size = 5) #+ scale_size_continuous(from=c(1), to=c(5)) #
    }
  if(add_region_names){p <- p + geom_text(data=region_centers, aes(long, lat, label = n), size=4)}
  #limit to Europe:   coord_cartesian(xlim = c(-10,33), ylim = c(36,73)) +
  print(p)
  savemap(map_name, figure_format = figure_format)
  
  
}









#function to creat a map based on ISO3 country data.
#data_for_map should contain only an ISO3 columns and a columns labeled datacolname
countrymap <- function(data_for_map, datacolname="value", scale_min=0, scale_max=0, mapcolor="Reds", map_name="map", map_legend="Legend", graphdir = "./"){
  # Get World data
  Nations <- data.table(map_data("world"))
  Nations <- Nations[region != "Antarctica"]
  Nations <- Nations[region != "Greenland"]
  Nations <- Nations[region == "USSR", region:="Russia" ]
  Nations <- Nations[region == "Zaire", region:="Congo" ]
  Nations <- Nations[region == "Czechoslovakia", region:="Germany" ]
  Nations <- Nations[region == "Yugoslavia", region:="Serbia" ]
  Nations <- Nations[region == "Sicily", region:="Italy" ]
  Nations <- Nations[region == "Sardinia", region:="Italy" ]
  # Find ISO3
  countries = unique(Nations$region)
  Nations[,ISO3 := rwmGetISO3(region),by=c("region")]
  unique(Nations[is.na(ISO3),region]) # List of non-match
  Nations = merge(Nations,data_for_map,by=c("ISO3"))
  World.map <- ggplot(Nations, aes(x = long, y = lat))
  Country.layer <- c(geom_polygon(data = Nations, aes(x = long, y = lat, group = group, fill=data_for_map)))
  
  Borders.layer <- c(geom_path(data = Nations, aes(x = long, y = lat, group = group), color="darkgray", size=0.1))                                      
  Map <- World.map + Country.layer + Borders.layer +
    coord_cartesian(xlim = c(-20,40), ylim = c(35,73)) +
    theme_minimal() + labs(x = "", y =  "") +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          panel.grid.major=element_blank(),plot.background=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position="bottom") +
    guides(color=FALSE) +
    scale_fill_manual(values = c("#8DD3C7", "#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#377EB8","#BC80BD","#CCEBC5","#FFED6F","#D95F02"), name="WITCH Regions", guide=guide_legend(nrow=1,keywidth = 2, title.position="top",direction="horizontal",label.position = "bottom")) 
  #limit to Europe:   coord_cartesian(xlim = c(-10,33), ylim = c(36,73)) +
  World.map + Country.layer + Borders.layer +
    #  theme_minimal() 
    theme_nothing() 
  + labs(x = "", y =  "") +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),
          panel.grid.major=element_blank(),plot.background=element_blank(),
          panel.grid.minor=element_blank()) +
    theme(legend.position="right") + scale_fill_continuous(name=map_legend, low = "green", high = "red" , na.value = "grey") + ggtitle("")
  savemap(map_name)
}












#functions to calculate distances (adapted from https://gist.github.com/sckott/931445)

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by 
# radian latitude/longitude using the Haversine formula
# Ouputs distance between sites 1 and 2 as meters
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = (R * c)*1000
  return(d) # Distance in meters
}

# Fxn to calculate matrix of distances between each two sites
# INPUT: a data frame in which longs are in first column and lats in second column
# OUTPUT: a distance matrix (class dist) between all pairwise sites
# Output distances are in meters
CalcDists <- function(longlats) {
  name <- longlats[1]
  n <- nrow(longlats)
  z <- matrix(0, n, n, dimnames = name)
  for (i in 1:n) {
    for (j in 1:n) z[i, j] <- gcd.hf(long1 = deg2rad(longlats[i, 2]), 
                                     lat1 = deg2rad(longlats[i, 3]), long2 = deg2rad(longlats[j, 2]), 
                                     lat2 = deg2rad(longlats[j, 3]))
  }
  z <- as.dist(z)
  return(z/1000) #in kilometers
}







#New maps for RICE+
map_simple <- function(data, yearmap=2100, title="", scenplot=scenlist, legend_title = "Variable") {
  sf::sf_use_s2(FALSE) #to avoid errors
  world <- ne_countries(scale = "medium", returnclass = "sf")
  #add geometry
  world <- suppressWarnings(cbind(world, sf::st_coordinates(sf::st_centroid(world$geometry))))
  #get model iso3 mapping
  mod.countries = readLines(file.path(model_dir, paste0("data_", reg_id, "/regions.inc")))
  mod.countries = mod.countries[mod.countries != ""]                     # Remove empty lines
  mod.countries = mod.countries[!str_detect(mod.countries, "^\\*")]      # Remove * comments
  mod.countries = str_trim(str_split_fixed(mod.countries, "#", 2)[, 1])  # Remove # comments
  mod.countries = mod.countries[(grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1] + 1):(grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1] + grep(";", mod.countries[grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1]:length(mod.countries)])[1] -2)]                            # Keep mapping data
  mod.countries = str_split(mod.countries, "\\.")
  mod.countries <- data.table(matrix(unlist(mod.countries), ncol = 2, byrow = T))
  setnames(mod.countries, c("n", "iso_a3"))
  #Add data to iso3 list
  data <- data %>% filter(t == yeartot(yearmap) & file %in% scenplot)
  data <- suppressMessages(data %>% full_join(mod.countries, by = "n", relationship = "many-to-many"))
  #Add data to world polygon
  data_map <- suppressMessages(data %>% select(t, file, pathdir, value, iso_a3) %>% full_join(world, by = "iso_a3", relationship = "many-to-many") %>% filter(!is.na(value) & !is.na(iso_a3) & !is.na(file)) %>% as.data.frame())
  p_map <- ggplot(data = data_map) + geom_sf(aes(fill = value, geometry = geometry)) +  scale_fill_viridis_c(option = "plasma", direction = -1) + ggtitle(title) + labs(fill = legend_title) + theme_bw() + theme(strip.background = element_rect(fill = "white"))
  if(length(scenplot)>1) p_map <- p_map + facet_wrap(file ~ ., ncol = 1)
  #remove Antarctica
  p_map <- p_map + coord_sf(ylim = c(-50, 90))
  saveplot("Map", width = 12, height = 10)
  return(p_map)
}



#Map of region definitions of a model
plot_map_region_definition <- function(regional_focus="World") {
  #regional_focus = "Europe" or "World"
  world <- ne_countries(scale = "medium", returnclass = "sf")
  #add geometry
  world <- suppressWarnings(cbind(world, sf::st_coordinates(sf::st_centroid(world$geometry))))
  #get model iso3 mapping
  mod.countries = readLines(file.path(model_dir, paste0("data_", reg_id, "/regions.inc")))
  mod.countries = mod.countries[mod.countries != ""]                     # Remove empty lines
  mod.countries = mod.countries[!str_detect(mod.countries, "^\\*")]      # Remove * comments
  mod.countries = str_trim(str_split_fixed(mod.countries, "#", 2)[, 1])  # Remove # comments
  mod.countries = mod.countries[(grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1] + 1):(grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1] + grep(";", mod.countries[grep("map_n_iso3(n,iso3)*", tolower(mod.countries))[1]:length(mod.countries)])[1] -2)]                            # Keep mapping data
  mod.countries = str_split(mod.countries, "\\.")
  mod.countries <- data.table(matrix(unlist(mod.countries), ncol = 2, byrow = T))
  setnames(mod.countries, c("n", "iso_a3"))
  p_map <- ggplot(data = mod.countries %>% full_join(world) %>% filter(!is.na(n) & !is.na(iso_a3))) + geom_sf(aes(fill = n, geometry = geometry)) +  scale_fill_manual(values = region_palette) + xlab("") + ylab("")  + ggtitle(str_glue("Regional aggregation: {reg_id}")) + theme_bw() + theme(strip.background = element_rect(fill = "white"), legend.position="bottom") + guides(fill = guide_legend(nrow = 3)) 
  if(regional_focus=="Europe") p_map <- p_map + coord_sf(xlim = c(-10,33), ylim = c(36,73), expand = FALSE)
  saveplot(str_glue("region_definition_{reg_id}"), width = 12, height = 8, add_title = F)
}



#regional temperature map with spatially gridded temperature downscaling (for RICE50+ and WITCH)
gridded_temp_map <- function(yearplot=2100, scenplot = scenlist, pathadj = ""){
tatm_data <- get_witch("TATM")
if(length(tatm_data)==0) tatm_data <- get_witch("TEMP") %>% filter(m=="atm" & n=="usa") %>% select(-m, -n)
tatm <- subset(tatm_data, file %in% scenplot & ttoyear(t)==yearplot)
df <- read_parquet(file.path(pathadj, "data/coefficients_modmean.parquet"))
df <- tatm %>% cross_join(df)
df <- df %>% mutate(TATM = gmt * value)
# Get world map data for borders
world_map <- map_data("world")
ggplot() +
    geom_tile(data = df, aes(x = lon, y = lat, fill = TATM)) +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 colour = "black", fill = NA, linewidth = 0.25) +
    ggtitle(paste("Downscaled temperature change in", yearplot)) +
    scale_fill_steps(
      low = "white",
      # mid = "white",
      high = "red",
      # midpoint = 1,
      n.break = 7
    ) +
    theme_minimal() +
    facet_wrap(file ~ ., ncol = 1) +
    labs(x="", y="", fill="°C")
}


