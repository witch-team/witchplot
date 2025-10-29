#Plots for WITCH runs with climate damages or climate_datas

climate_plot <- function(scenplot=scenlist, regions = "World"){
  Q_EMI <- get_witch("Q_EMI")
  ghg <- get_witch("ghg") # to get GHGs for non-co2 sets
  ghgs <- unique(ghg$e)
  Q_EMI <- Q_EMI %>% filter(e %in% ghgs) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value), emico2=sum(value[e=="co2"])) %>% mutate(nonco2=emiall-emico2)
  #get also BAU values
  BAU_Q_EMI <- get_witch("BAU_Q_EMI")
  BAU_Q_EMI <- BAU_Q_EMI %>% filter(e %in% ghgs) %>% group_by(pathdir, file, n, t) %>% summarize(emi_bau = sum(value), co2_bau=sum(value[e=="co2"]))
  climate_data <- Q_EMI %>% rename(emi=emiall, co2=emico2)
  climate_data <- merge(climate_data, BAU_Q_EMI, by = c("pathdir", "file", "n", "t"), all = T)
  
  TRF <- get_witch("TRF")
  climate_data <- merge(climate_data, TRF, by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "trf")
  #add external climate modules in case
  MAGICCTRF <- get_witch("MAGICCTRF")
  if(length(MAGICCTRF)>0) {setnames(MAGICCTRF, "value", "trf_magicc6"); climate_data <- merge(climate_data, MAGICCTRF, by = c("pathdir", "file", "n", "t"), all = T)}
  HECTORTRF <- get_witch("HECTORTRF")
  if(length(HECTORTRF)>0) {setnames(HECTORTRF, "value", "trf_hector"); climate_data <- merge(climate_data, HECTORTRF, by = c("pathdir", "file", "n", "t"), all = T)}
  
  
  TEMP <- get_witch("TEMP")
  climate_data <- merge(climate_data, TEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "temp")
  #add external climate modules in case
  MAGICCTEMP <- get_witch("MAGICCTEMP") 
  if(length(MAGICCTEMP)>0) {setnames(MAGICCTEMP, "value", "temp_magicc6"); climate_data <- merge(climate_data, MAGICCTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all = T)}
  HECTORTEMP <- get_witch("HECTORTEMP")
  if(length(HECTORTEMP)>0) {setnames(HECTORTEMP, "value", "temp_hector"); climate_data <- merge(climate_data, HECTORTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all = T); }
  
  #PLOTS:
  #CO2 emissions
  co2_plot <- plot_witch(climate_data, varname = "co2", regions = regions, scenplot = scenplot, ylab = "CO2 Emissions [GtCO2]", conv_factor=44/12)
  #GHG Emissions
  emi_plot <- plot_witch(climate_data, varname = "emi", regions = regions, scenplot = scenplot, ylab = "GHG Emissions [GtCO2eq]", conv_factor=44/12)
  #Non-CO2 Emissions
  nonco2_plot <- plot_witch(climate_data, varname = "nonco2", regions = regions, scenplot = scenplot, ylab = "Non-CO2 Emissions [GtCO2eq]", conv_factor=44/12)
  
  
  #Radiative Forcing
  trf_plot <- plot_witch(climate_data, varname = "trf", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (WITCH)", conv_factor=1, nagg="mean")
  if(length(MAGICCTRF)>0) trf_plot_magicc <- plot_witch(climate_data, varname = "trf_magicc6", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (MAGICC6)", conv_factor=1, nagg="mean") else trf_plot_magicc = trf_plot
  if(length(HECTORTRF)>0) trf_plot_hector <- plot_witch(climate_data, varname = "trf_hector", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (HECTOR)", conv_factor=1, nagg="mean") else trf_plot_hector = trf_plot

  #Temperature
  temp_plot <- plot_witch(climate_data, varname = "temp", scenplot = scenplot, regions = "World", ylab = "Temp. increase [degC] (WITCH)", conv_factor=1, nagg="mean")
  if(length(MAGICCTEMP)>0) temp_plot_magicc <- plot_witch(climate_data, varname = "temp_magicc6", scenplot = scenplot, regions = "World", ylab = "Temp. increase [degC] (MAGICC6)", conv_factor=1, nagg="mean") else temp_plot_magicc = temp_plot
  if(length(HECTORTEMP)>0) temp_plot_hector <- plot_witch(climate_data, varname = "temp_hector", scenplot = scenplot, regions = "World", ylab = "Temp. increase [degC] (HECTOR)", conv_factor=1, nagg="mean") else temp_plot_hector = temp_plot
  
  
  ymax_emi = max(ggplot_build(emi_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(co2_plot)$layout$panel_scales_y[[1]]$range$range[2])
  ymax_trf = max(ggplot_build(trf_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(trf_plot_magicc)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(trf_plot_hector)$layout$panel_scales_y[[1]]$range$range[2])
  ymax_temp = max(ggplot_build(temp_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(temp_plot_magicc)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(temp_plot_hector)$layout$panel_scales_y[[1]]$range$range[2])
  
  print(ggarrange(co2_plot + ylim(0,ymax_emi), emi_plot + ylim(0,ymax_emi), nonco2_plot + ylim(0,ymax_emi), trf_plot + ylim(0,ymax_trf), trf_plot_magicc + ylim(0,ymax_trf), trf_plot_hector + ylim(0,ymax_trf), temp_plot + ylim(0,ymax_temp), temp_plot_magicc + ylim(0,ymax_temp), temp_plot_hector + ylim(0,ymax_temp), ncol = 3, nrow=3, common.legend = T, legend = "bottom"))
  
}

