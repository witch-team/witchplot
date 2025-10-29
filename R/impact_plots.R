#Plots for WITCH runs with climate damages or impacts

SCC_plot <- function(scenplot=scenlist, regions = "World", normalization_region = "World", verbose = FALSE){
  gdp_measure <- "y" #"cc" #for consumption or "y" for GDP
  emi_sum <- "ghg" #or "co2" for only CO2 or ghg for all gases
  #Impacts and Damages computation
  OMEGA <- get_witch("OMEGA")
  Q <- get_witch("Q")
  Q <- Q %>% filter(iq == gdp_measure) %>% select(-iq)
  l <- get_witch("l")
  Q_EMI <- get_witch("Q_EMI") # nolint
  ghg <- get_witch("ghg") # to get GHGs for non-co2 sets
  if(emi_sum=="ghg") ghg_used <- unique(ghg$e) else if(emi_sum=="co2") ghg_used = c("co2")
  Q_EMI <- Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value))
  #get also BAU values
  BAU_Q <- get_witch("BAU_Q")
  BAU_Q <- BAU_Q %>% filter(iq == gdp_measure) %>% select(-iq)
  BAU_Q_EMI <- get_witch("BAU_Q_EMI")
  BAU_Q_EMI <- BAU_Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value))
  impact <- Q %>% rename(gdp=value)
  impact <- merge(impact, BAU_Q, by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "gdp_bau")
  impact <- merge(impact, Q_EMI, by = c("pathdir", "file", "n", "t")); setnames(impact, "emiall", "emi")
  impact <- merge(impact, BAU_Q_EMI, by = c("pathdir", "file", "n", "t")); setnames(impact, "emiall", "emi_bau")
  impact <- merge(impact, l, by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "pop")
  
  TEMP <- get_witch("TEMP")
  impact <- merge(impact, TEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "temp")
  #add external climate modules in case
  MAGICCTEMP <- get_witch("MAGICCTEMP")
  if(length(MAGICCTEMP)>0) {impact <- merge(impact, MAGICCTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all.x = T); setnames(impact, "value", "temp_magicc6")}
  
  
  scenplot_nopulse <- setdiff(scenplot, str_subset(scenlist, "emission_pulse"))
  #PLOTS:
  #COs Emissions
  emi_plot <- plot_witch(impact, varname = "emi", regions = regions, scenplot = scenplot_nopulse, ylab = "GHG Emissions [GtCO2eq]", conv_factor=44/12)
  #Plot of relative GDP loss
  gdp_loss_plot <- plot_witch(impact, varname = "-(gdp/gdp_bau-1)", regions = regions, scenplot = scenplot_nopulse, ylab = "% GDP loss", conv_factor=100, rm.NA = F)

  #Temperature
  #temp_plot <- plot_witch(impact, varname = "temp", scenplot = scenplot_nopulse, regions = "World", ylab = "Temperature increase [deg C]", conv_factor=1, nagg="mean")
  temp_plot <- ggplot() + geom_line(data = impact %>% filter(file %in% scenplot_nopulse & ttoyear(t) <= yearmax & ttoyear(t) >= yearmin) %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(str_subset(names(impact), "temp")), funs(mean)), aes(ttoyear(t),temp,colour=file), stat="identity", linewidth=1.5, linetype = "solid") + xlab("") + ylab("Temperature [deg C]")
  if(length(MAGICCTEMP)>0){temp_plot <- temp_plot + geom_line(data = impact %>% filter(file %in% scenplot_nopulse & ttoyear(t) <= yearmax & ttoyear(t) >= yearmin & !is.na(temp_magicc6)) %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(str_subset(names(impact), "temp_magicc6")), funs(mean)), aes(ttoyear(t),temp_magicc6,colour=file), stat="identity", linewidth=1.5, linetype = "dashed") + ylab("Temp., MAGICC dashed")}
  
  
  
  
  ########### COMPUTE THE SOCIAL COST OF CARBON ####################
  t0 = 1 #from which year to compute the SCC from
  tmax = 30

  #SCC from marginals
  m_eqq_emi_tree <- get_witch("m_eqq_emi_tree")
  m_eqq_y <- get_witch("m_eqq_y")
  scc <- m_eqq_emi_tree %>% filter(e=="co2") %>% rename(m_emi=value)
  scc <- merge(scc, m_eqq_y, by = c("pathdir", "file", "n", "t")); setnames(scc, "value", "m_eqq_y")
  #SCC from T$/GtC to $/tCO2 
  scc <- scc %>% filter(t>= t0 & t <= tmax) %>% mutate(SCC_tn = 1e3 * (-m_emi / m_eqq_y) / (44/12)) %>% group_by(pathdir, file, n) %>% mutate(SCC_t0 = 1e3 * (-m_emi / m_eqq_y[t==t0]) / (44/12))
  if(normalization_region=="World"){
    #scc <- scc %>% group_by(pathdir, file, t) %>% mutate(m_eqq_normalization = mean(m_eqq_y))
    #scc_value_marginals <- scc  %>% group_by(pathdir, file, t) %>% mutate(SCC_contrib_norm=1e3 * (-m_emi / m_eqq_normalization) / (44/12)) %>% summarize(SCC=sum(SCC_contrib_norm)) %>% mutate(n="World") %>% as.data.frame()
    scc_value_marginals <- scc  %>% group_by(pathdir, file, t) %>% mutate(SCC_contrib_norm=1e3 * (-m_emi / m_eqq_y) / (44/12)) %>% summarize(SCC=sum(SCC_contrib_norm)) %>% mutate(n="World") %>% as.data.frame()
    
  }else{
    scc <- scc %>% group_by(pathdir, file, t) %>% mutate(m_eqq_normalization = m_eqq_y[n==normalization_region])
    scc_value_marginals <- scc  %>% group_by(pathdir, file, t) %>% mutate(SCC_contrib_norm=1e3 * (-m_emi / m_eqq_normalization) / (44/12)) %>% summarize(SCC=sum(SCC_contrib_norm)) %>% mutate(n="World") %>% as.data.frame()
  }
  scc_value_marginals <- subset(scc_value_marginals, file %in% scenplot_nopulse)
  scc_value_marginals_t0 <- subset(scc_value_marginals, file %in% scenplot_nopulse & t==t0)
  if(verbose) print(scc_value_marginals_t0)
  SCC_bar_chart <- ggplot(scc_value_marginals_t0) + geom_bar(aes(file, SCC, fill=file), position = "dodge", stat="identity") + ylab("SCC [$/tCO2eq]") + xlab("") + geom_text(aes(file, SCC*0.9, label=paste0(round(SCC,1), "$"))) + guides(fill=FALSE)
  
  SCC_over_time_plot <- plot_witch(scc_value_marginals, varname = "SCC", regions = "World", scenplot = scenplot_nopulse, ylab = "SCC [$/tCO2eq]", conv_factor=1, rm.NA = F, ylim0 = T)
  
  
  #compute SCC based on emission pulse method
  SCC_bar_chart_pulse <- NULL
  if(any(str_detect(scenplot, "_emission_pulse"))){
    scc_regional <- get_witch("scc_regional")
    scc_regional$file = gsub("_emission_pulse" , "", scc_regional$file)
    #scc_regional$pathdir <- "Emission_pulse"
    if(normalization_region=="World"){
      scc_regional <- scc_regional %>% group_by(pathdir, file, t) %>% summarize(value=mean(value)) %>% mutate(n="World")#for now simple mean
    }else{
      scc_regional <- scc_regional %>% filter(n==normalization_region)
    }  
    
    scc_regional_t0 <- scc_regional %>% filter(t==t0)
    #add NAs for runs without pulse run to keep same structure and colors
    scc_regional_t0 <- rbind(as.data.frame(scc_regional_t0), scc_value_marginals_t0 %>% filter(!(file %in% unique(scc_regional$file))) %>% rename(value=SCC) %>% mutate(value=NA))
    
    if(verbose) print(scc_regional_t0)
    SCC_bar_chart_pulse <- ggplot(scc_regional_t0) + geom_bar(aes(file, value, fill=file), position = "dodge", stat="identity") + ylab("SCC [$/tCO2eq] (PULSE)") + xlab("")  + geom_text(aes(file, value*0.9, label=paste0(round(value,1), "$"))) + guides(fill=FALSE) + ylim(NA, max(c(scc_value_marginals$SCC, scc_regional$value)))
    SCC_bar_chart <- SCC_bar_chart + ylim(NA, max(c(scc_value_marginals_t0$SCC, scc_regional$value)))
  }
  
  print(suppressWarnings(ggarrange(emi_plot, temp_plot, gdp_loss_plot, NULL, SCC_bar_chart, SCC_bar_chart_pulse, ncol = 2, nrow=3, common.legend = T, legend = "bottom")))
  
}

