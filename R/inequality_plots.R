# Inequality plots

plot_inequality <- function(variable = variable, plot_type = "quantiles", q_shares = NULL, value_share="value", quantile_set = "dist", per_capita_var = 1000, scenplot = scenlist, regions = witch_regions[1], years = seq(yearmin, yearmax), years_lorenz = NULL, q_plot = NULL, q_fit = NULL, verbose = T){
  # variable can either be a variable name or dataframe.
  # value_share="value|share" if share, already shares (sum to 1, not 100!!), if value compute shares
  # plot_type = "quantiles|lorenz_curve|gini|distribution"
  # q_shares: dataframe with column quantile_set and share (in increasing order!)
  # q_plot, q_fit: chose which quantiles use to plot and which to fit the distribution (optional), by default all are used and plotted
  res <- lapply(c('reldist', 'ineq', 'gglorenz', 'GB2', 'GB2group', 'VGAM'), require_package)
  if(is.character(variable))ineq_data <- get_witch(variable) else{ineq_data <- variable; variable <- deparse(substitute(variable))}
  ineq_data <- ineq_data %>% filter(file %in% scenplot & ttoyear(t) %in% years & n %in% regions)
  if(quantile_set %in% names(ineq_data)){
  setnames(ineq_data, quantile_set, "dist")
  ineq_data <- subset(ineq_data, !is.na("dist")) #removes scenarios without distirbutive information
  if(is.null(q_shares)) q_shares <- data.frame(dist=unique(ineq_data[["dist"]]), share=rep(1/length(unique(ineq_data[["dist"]])),length(unique(ineq_data[["dist"]]))))
  ineq_data$dist <- factor(ineq_data$dist, levels = rev(q_shares$dist))
  if(is.null(years_lorenz)) years_lorenz <- range(unique(ttoyear(ineq_data$t)))
  if(value_share == "value") ineq_data <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", "dist"))) %>% mutate(value=value/sum(value))
  if(!is.null(q_plot)) ineq_data <- ineq_data %>% filter(dist %in% q_plot)
  ineq_data_indices <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", "dist"))) %>% dplyr::summarize(gini=reldist::gini(value))
  
  if(plot_type=="quantiles"){
    #facetted plot of quantiles over files and regions
    ggplot(ineq_data)  + geom_bar(aes(ttoyear(t), value, fill=dist), stat = "identity", position = "stack") + facet_grid(file ~ n) + xlab("")+ ylab(variable) #+ scale_fill_manual(values = quantile_colors, name = "Quantiles")
  }
  else if(plot_type=="gini"){
    #Gini plot based on Quantiles
    p <- ggplot(ineq_data_indices) + geom_line(aes(ttoyear(t), gini, color=file)) + facet_grid(. ~ n) + xlab("")+ ylab("Gini") + ylim(0,1) 
  }
  else if(plot_type=="lorenz_curve"){
    #Lorenz curve
    p <- ggplot(ineq_data %>% filter(ttoyear(t) %in% years_lorenz) %>% mutate(year=ttoyear(t))) + stat_lorenz(aes(value, color=file)) + geom_abline(color = "grey") + xlab("") + ylab("") + facet_grid(n  ~ year)
  }
  else if(plot_type=="distribution"){
    if(is.numeric(per_capita_var[1])) ineq_data$per_capita <- per_capita_var else ineq_data <- ineq_data %>% full_join(get_witch(per_capita_var) %>% filter(t %in% unique(ineq_data$t)) %>% rename(per_capita=value)) %>% filter(!is.na(dist))
    #Create complete dataframe
    ineq_data$dist <- as.character(ineq_data$dist); q_shares$dist <- as.character(q_shares$dist)
    ineq_data_plot <- ineq_data %>% full_join(ineq_data_indices) %>% full_join(q_shares) %>% mutate(value_cst_dist=value/share * per_capita) %>% filter(ttoyear(t) %in% years_lorenz)
    
    ### FUNCTIONS ###
    #Plot Lorenz curve
    Lc_manual <- function(x, n, return_param = "df"){
      .Lc <- Lc(x = x, n = n)
      .Lc <- data.frame(p=.Lc$p, Lc=.Lc$L)
      if(return_param=="df") return(.Lc) else return(.Lc[return_param])
    }
    fit_parametric_dist <- function(y, pc.inc, gini.e){
      #Singh-Maddala Distribution (rescale is just for fit, not per capita value!)
      rescale_fit <- 1
      fitdist <- suppressMessages(suppressWarnings(fitgroup.sm(y = y, pc.inc = pc.inc, gini.e = gini.e)))
      fitdistparam <- unlist((fitdist$ewmd.estimation))[1,]
      return(as.data.frame(t(fitdistparam)))
    }
    #Source: http://www.vcharite.univ-mrs.fr/PP/lubrano/cours/Lecture-4.pdf
    #Gini
    GINIsingh <- function(a,q){return(1-((gamma(q)*gamma(2*q-1/a))/(gamma(q-1/a)*gamma(2*q))))}
    ### END FUNCTIONS ###
    
    #compute fitted distribution parameters in dataframe
    ineq_data_plot_agg <- ineq_data_plot 
    if(!is.null(q_fit)) ineq_data_plot_agg <- ineq_data_plot_agg %>% filter(dist %in% q_fit)
    ineq_data_plot_agg <- ineq_data_plot_agg %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% dplyr::summarize(gini=mean(gini), per_capita=mean(per_capita), params = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini)) %>% unpack(cols = params) %>% as.data.frame()
    ineq_data_plot_everything <- (ineq_data_plot %>% full_join(ineq_data_plot_agg)) %>% mutate(year=ttoyear(t)) %>% select(-gini,-per_capita) %>% as.data.frame()
    
    if(verbose){
      print("Estimated Maddala-Sing Distribution Parameters")
      print(ineq_data_plot_agg %>% mutate(P99=qsinmad(0.99, shape1.a = a, shape3.q = q , scale = rescale_fit*b)))
    }
    
    #for now only one figure of first year and n in dataset (multiple not yet working as requires different stat-functions per facets)
    ineq_data_plot_everything <- ineq_data_plot_everything %>% filter(n %in% unique(ineq_data_plot$n)[1] & year %in% unique(ineq_data_plot_everything$year)[1] & file %in% unique(ineq_data_plot$file)[1])
    
    #Plot one distribution CDF
    p_cdf <- ggplot(ineq_data_plot_everything, aes(value_cst_dist)) + stat_ecdf(geom = "step") + stat_function(fun = psinmad, args = list(scale = mean(ineq_data_plot_everything$b), shape1.a = mean(ineq_data_plot_everything$a), shape3.q = mean(ineq_data_plot_everything$q)), colour = "red") + xlab("") + ylab("") + xlim(0,NA) #+ facet_grid(n  ~ year)
    
    #Plot one Lorenz curve
    p_lorenz <- ggplot(data.frame(x=seq(0,1,0.001))) + geom_line(data = Lc_manual(ineq_data_plot_everything$value, ineq_data_plot_everything$share), aes(p,Lc)) + xlab("") + ylab("") + geom_abline(color = "grey") + stat_function(fun = LCsingh, args = list(a=mean(ineq_data_plot_everything$a), q=mean(ineq_data_plot_everything$q)), color = "red") + geom_text(aes(0.4, 0.8), label = paste("Gini (q):",round(reldist::gini(ineq_data_plot_everything$value, ineq_data_plot_everything$share),3), "(dist):", round(GINIsingh(mean(ineq_data_plot_everything$a), mean(ineq_data_plot_everything$q)),3))) #+ facet_grid(n  ~ year)
    p <- ggarrange(p_cdf, p_lorenz, ncol = 1)
    #saveplot("Distribution and Lorenz curve with fitted distribution")
  }  
  #common for all plots
  p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank())
  saveplot(paste("Distribution of", variable, "as", plot_type))
  return(ineq_data_indices) #return inequality indices
  }else{print("No distributional information in this variable.")}
}





#Winners and Losers over time plot
plot_winners_losers_time <- function(scen0, scen1, showvar = "people", yeardist=2100) {
  if(showvar=="people") {conv = 100; ylabel <- "Population share [%]"}
  if(showvar=="money") {conv = 1e-3; ylabel <- "Total gain or loss [T$]"}
  
  CPC_DIST <- get_witch("CPC_DIST")
  pop <- get_witch("pop") 
  
  #population weights, no weighting on relative gains: who is better of?
  inequality_plot_data <- left_join( 
    CPC_DIST %>% 
      group_by_at(c("n", "dist", "pathdir", file_group_columns)) %>% complete(t=seq(min(t), max(t), 0.2)) %>% mutate(value=approxfun(t, value)(t)) %>% #expands and interpolates to yearly data
      mutate(dist = as.numeric(str_remove(dist,"D"))) %>%
      group_by(n,file, dist) %>% mutate(cpc2020=value[t==2]) %>% ungroup() %>%
      group_by(t,n,dist) %>%
      mutate(cpc=value) %>% 
      mutate(value = if_else(showvar=="people", sign(value[file==scen1] - value[file==scen0])), value[file==scen1] - value[file==scen0]) %>%
      filter(file == scen1), 
    pop  %>% 
      filter(t %in% unique(CPC_DIST$t) & file == scen1) %>%
      group_by_at(c("n", "pathdir", file_group_columns)) %>% complete(t=seq(min(t), max(t), 0.2)) %>% mutate(value=approxfun(t, value)(t)) %>% #expands and interpolates to yearly datafilter(file==scen1) %>%
      group_by(t) %>%
      mutate(pop = if_else(showvar=="people", value/(sum(value))), value) %>%
      mutate(pop_quantile=pop/10) %>%
      select(-value) ) %>% 
    mutate(w = value*pop_quantile) %>%
    as.data.frame()
  
#two half plots
  # ggplot() + 
  #   geom_area(data = inequality_plot_data %>% filter(t > 3 & w < 0), aes(x=ttoyear(t),y=w*conv, fill=n, alpha=as.factor(dist))) +
  #   scale_fill_manual(values = region_palette) + 
  #   scale_color_manual(values = region_palette) + xlab("") + ylab(ylabel) + guides(alpha = FALSE) + geom_area(data = inequality_plot_data %>% filter(t > 3 & w > 0), aes(x=ttoyear(t),y=reorder(w*conv, sort(cpc2020)), fill=n, alpha=as.factor(dist)))
  
  print("Unambiguos winners or losers in year t")
  print(inequality_plot_data %>% group_by(n, t) %>% dplyr::summarize(allsame=ifelse(sign(max(w))==sign(min(w)), sign(max(w)), NA)) %>% filter(t==18 & !is.na(allsame)) %>% select(n, allsame))
  
  p <- ggplot() + 
    geom_area(data = inequality_plot_data %>% filter(t > 3), aes(x=ttoyear(t),y=w*conv, fill=n, alpha=as.factor(dist))) +
    scale_fill_manual(values = region_palette) + 
    scale_color_manual(values = region_palette) + xlab("") + ylab(ylabel) + guides(alpha = "none")
  if(showvar=="people") p <- p + geom_text(data=inequality_plot_data %>% group_by(t) %>% dplyr::summarize(share_idifferent=sum(w[value==0])/sum(w)) %>% filter(ttoyear(t) %in% seq(2020,yearmax, 20)) %>% mutate(percent_indifferent=paste0(round(share_idifferent),"%")), aes(ttoyear(t), 0, label=percent_indifferent), color="grey20")
  saveplot("Inequality Plot - Winners and Losers")
  

  
  #now also plot thw two distributions in yeardist
  #since weihgte not fully working with grouped data, replicate based on million inhabitants
  dist_data <- CPC_DIST %>% filter(ttoyear(t)==yeardist) %>% left_join(pop %>% mutate(popdist=value/10) %>% select(-value)) %>% mutate(mil=as.integer(popdist))
  dist_data <- dist_data[rep(seq_len(nrow(dist_data)), dist_data$mil),]
  bw = 0.1  # (0.01 shows deciles all separately, 0.1 relatively smooth)
  ggplot(dist_data) + geom_density(aes(x=value*1e3, y=..count../sum(..count..), fill=n, color=n, group=n), position="stack", alpha = 0.8, bw=bw) + scale_fill_manual(values = region_palette) + scale_color_manual(values = region_palette) + scale_x_log10(breaks = c(100,1000,10000,100000, 1000000), labels=function(n){format(n, scientific = FALSE)}) + xlab("") + ylab(str_glue("Consumption distribution in {yeardist}")) + facet_wrap(file ~ ., scales = "free_y", nrow = 2)
  saveplot("Inequality Distribution Comparison")
  
  assign("inequality_plot_data", inequality_plot_data, envir = .GlobalEnv)
}


















#additional functions for computing inequality measures


#computes (model and historical) Gini and deciles at the global level based on Y_DIST and Y (=assumes GDP at PPP in the model! (historical is always using PPP))
compute_global_inequality <- function(Y_DIST="Y_DIST", Y="Y", l="l", scenplot=scenlist){
  res <- lapply(c('reldist', 'dineq', 'Hmisc'), require_package)
  inequality_dataset_model <- get_witch(Y_DIST) %>% full_join(get_witch(Y, scenplot = scenplot) %>% dplyr::rename(Y=value)) %>% full_join(get_witch(l, scenplot = scenplot) %>% dplyr::rename(pop=value)) %>% mutate(year=ttoyear(t), gdppcppp=Y*1e6/pop, value=value/Y)
  
  #now get historical data on ed57 aggregation
  data_hist_quantiles <- gdx(file.path(model_dir, paste0("data_", reg_id), "data_historical_values_inequality_converted.gdx"))
  data_hist_quantiles <- data_hist_quantiles["quantiles"] %>% mutate(year=as.numeric(year))
  data_historical <- gdx(file.path(model_dir, paste0("data_", reg_id), "data_historical_values.gdx"))
  gdp_historical <- data_historical["ykali_valid_wdi"] %>% dplyr::rename(Y=value) %>% full_join(data_historical["l_valid_wdi"] %>% dplyr::rename(pop=value)) %>% full_join(data_historical["mer2ppp_valid_wdi"] %>% dplyr::rename(mer2ppp=value))
  #Extrapolate fix PPP2MER rates
  gdp_historical <- gdp_historical %>% group_by(n) %>% tidyr::fill(mer2ppp, mer2ppp, .direction = "updown")
  gdp_historical <- gdp_historical %>% mutate(gdppcppp=Y*1e6*mer2ppp/pop, V1=as.numeric(V1)) %>% dplyr::rename(year=V1) %>% as.data.frame() %>% select(-mer2ppp)
  inequality_dataset_historical_modelregions <- data_hist_quantiles %>% full_join(gdp_historical) %>% mutate(file="historical")
  #combine model and historical at model regions
  inequality_dataset_merged <- rbind(inequality_dataset_historical_modelregions, inequality_dataset_model %>% select_at(names(inequality_dataset_historical_modelregions)))
  #global_inequality(inequality_dataset_merged %>% dplyr::rename(iso3=n), "value", "pop", "gdppcppp", na.rm = T)
  inequality_dataset_merged <- inequality_dataset_merged %>% group_by(n, year, file) %>% mutate(quantile_pcvalue = value *10 * gdppcppp)
  global_gini_past_future <- inequality_dataset_merged %>% group_by(year, file) %>% filter(!is.na(value) & !is.na(gdppcppp)) %>% dplyr::summarize(gini_total=reldist::gini(quantile_pcvalue, weights = pop), gini_between_unweighted=reldist::gini(gdppcppp), gini_between_popweighted=reldist::gini(gdppcppp, weights = pop), gdppcppp=weighted.mean(gdppcppp, w = pop), variance=Hmisc::wtd.var(quantile_pcvalue, weight = pop),
theil_total=dineq::mld_decomp(quantile_pcvalue, n, weights = pop)$mld_decomp$mld_total, theil_between=dineq::mld_decomp(quantile_pcvalue, n, weights = pop)$mld_decomp$mld_between, theil_within=dineq::mld_decomp(quantile_pcvalue, n, weights = pop)$mld_decomp$mld_within,
pop=sum(pop)) %>% mutate(CV=sqrt(variance)/gdppcppp) %>% as.data.frame()
  #Global Gini plot
  print(ggplot(global_gini_past_future, aes(year, gini_total, color=toupper(file))) + geom_line() + geom_point()+ theme(legend.position = "bottom") + xlab("") + ylab("Global Gini Index") + theme(legend.position = "right"))  + xlim(1990, 2100) #+ scale_x_continuous(breaks=seq(1990,2100, 10), minor_breaks = NULL) + ylim(0.2,.8)
  #add theil MLD based decomposition (=GE(0)) which can be convertet to Atkinson(1)
  
  #first get full distribution at global level
  global_distribution <- inequality_dataset_merged %>% group_by(file, year) %>% filter(!is.na(value)) %>% mutate(gdppcppp_pc = gdppcppp*value/0.1, pop_pc=pop/10) %>% arrange(gdppcppp_pc) %>% mutate(gdp_cum=cumsum(gdppcppp_pc*pop_pc), pop_cum=cumsum(pop_pc), gdp_between_cum=cumsum(gdppcppp*pop_pc), pctl=pop_cum/max(pop_cum), decile=(floor((pctl-(1e-9))*10)+1), dist_orig=dist, dist=paste0("D", decile), pop=pop*0.1)
  #old approach based on wtd quantiles
  #global_distribution <- inequality_dataset_merged %>% group_by(file, year) %>% filter(!is.na(value)) %>% mutate(gdppcppp_pc = gdppcppp*value/0.1, pop_pc=pop/10) %>% arrange(gdppcppp_pc) %>% mutate(gdp_cum=cumsum(gdppcppp_pc*pop_pc), pop_cum=cumsum(pop_pc), gdp_between_cum=cumsum(gdppcppp*pop_pc)); global_distribution %>% summarize(decile_global=reldist::wtd.quantile(gdp_cum, q=seq(0.1,1.0,0.1), na.rm = FALSE, weight=pop_pc), decile_between_popweighted=reldist::wtd.quantile(gdp_between_cum, q=seq(0.1,1.0,0.1), na.rm = FALSE, weight=pop_pc)); global_quantiles$dist <- rep(paste0("D", seq(1,10)), nrow(global_quantiles)/10)
  assign("global_distribution", global_distribution, envir = .GlobalEnv)
  global_quantiles <- global_distribution %>% group_by_at(c("file", "year", "dist")) %>% dplyr::summarize(decile_global=sum(gdppcppp_pc*pop_pc), decile_between_popweighted=sum(gdppcppp*pop_pc))
  #%>% summarize(decile_global=max(gdp_cum), decile_between_popweighted=max(gdp_between_cum))
  #old approach using wtd quantiles (previous seems more precise)
  #global_quantiles <- inequality_dataset_merged %>% group_by(file, year) %>% filter(!is.na(value)) %>% mutate(gdppcppp_pc = gdppcppp*value/0.1, pop_pc=pop/10) %>% arrange(gdppcppp_pc) %>% mutate(gdp_cum=cumsum(gdppcppp_pc*pop_pc), pop_cum=cumsum(pop_pc), gdp_between_cum=cumsum(gdppcppp*pop_pc)) 
  #now also compute deciles at the global level
  global_quantiles <- global_quantiles %>% full_join(global_quantiles %>% group_by(file, year) %>% dplyr::summarize(gini_global_globdec=reldist::gini(decile_global), gini_between_popweighted_globdec=reldist::gini(decile_between_popweighted), sum_global=sum(decile_global), sum_between=sum(decile_between_popweighted)), by = c("file", "year")) %>% mutate(decile_global=decile_global/sum_global, decile_between_popweighted=decile_between_popweighted/sum_between, t= yeartot(year)) %>% select(file, t, dist, decile_global, decile_between_popweighted, gini_global_globdec, gini_between_popweighted_globdec)
  #plot inequality based on global deciles instead
  print(ggplot(global_quantiles %>% filter(dist=="D1"), aes(year, gini_global_globdec, color=toupper(file))) + geom_line() + geom_point() + theme(legend.position = "bottom") + xlab("") + ylab("Global Gini Index") + theme(legend.position = "right") + xlim(1990, 2100))
  
  #combine data based on model resolution and global deciles
  global_inequality_data <- global_gini_past_future %>% mutate(t=yeartot(year)) %>% select(file, t, gini_total, gini_between_unweighted, gini_between_popweighted, gdppcppp, CV, theil_total, theil_between, theil_within) %>% full_join(global_quantiles %>% select(-year))
  return(global_inequality_data)
}


