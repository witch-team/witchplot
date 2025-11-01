# Compute Policy Costs and Carbon Prices


Policy_Cost <- function(discount_rate=5, tmin=4, tmax=20, bauscen="ssp2_bau", regions="World", show_numbers=TRUE, scenplot=scenlist, measure="GDP"){
  if(!(bauscen %in% scenlist)){stop("For policy costs define an existing BAU scenario")}
  Q <- get_witch("Q")
  #Q$value <- Q$value * usd_deflator    #Apply deflator
  Q <- subset(Q, (t %in% seq(1,100)))
  if(measure=="GDP"){GDP <- subset(Q, tolower(iq)=="y")}
  if(measure=="Consumption"){GDP <- subset(Q, iq=="cc")}
  GDP$iq <- NULL
  bau <- GDP %>% filter(file==bauscen) %>% rename(bau=value) %>% select(t,n,bau)
  GDP <- merge(GDP, bau, by=c("n", "t"), all = TRUE)
  GDP$"GDP Loss" <- -(GDP$"value" -GDP$"bau")
  GDP$GDP_Loss_discounted = (GDP$"GDP Loss")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP$GDP_MER_discounted = (GDP$"value")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP <- subset(GDP, file %in% scenplot)
  GDP_WORLD <- GDP %>% select(-n) %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% summarize_all(.funs = sum, na.rm=T) %>% mutate(n="World") %>% as.data.frame() 
  GDP <- rbind(GDP, GDP_WORLD)
  #PC over time plot (NOT discounted!)
  PC_annual_relative <- subset(GDP, t<=tmax&t>=tmin); PC_annual_relative$rel_cost <- PC_annual_relative$"GDP Loss"/PC_annual_relative$"bau";
  p <- ggplot(subset(PC_annual_relative, n %in% regions & file!=bauscen)) + geom_line(aes(ttoyear(t), rel_cost*100, color=file), show.legend = TRUE) +ylab(paste("% of", measure)) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(results_dir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "World" & (length(regions)>1)){p <- p + facet_grid(. ~ n)}
  #now compute also discounted NPV value
  Policy_Cost <- GDP %>% filter(t<=tmax & t>=tmin) %>% group_by_at(c("n", file_group_columns, "pathdir")) %>% summarize(GDP_Loss_discounted=sum(GDP_Loss_discounted), GDP_MER_discounted=sum(GDP_MER_discounted))
  Policy_Cost$PC = 100*Policy_Cost$GDP_Loss_discounted / Policy_Cost$GDP_MER_discounted
  #set negative Policy costs to zero
  #Policy_Cost$PC <- pmax(Policy_Cost$PC, 0)
  assign("POLCOST", Policy_Cost, envir = .GlobalEnv) 
  p <- ggplot(subset(Policy_Cost, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, PC, fill=file), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(results_dir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "World"){p <- p + facet_grid(. ~ n)}
  if(show_numbers){p <- p + geom_text(data=subset(Policy_Cost, n %in% regions & file!=bauscen), aes(x=file, y=PC+0.1, label=paste0(round(PC, 1),"%")), size=3)}
  p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  saveplot(paste0("Policy Cost (", measure, ")"))
}




Policy_Cost_Decomposition <- function(discount_rate=5, tmin=4, tmax=20, bauscen="ssp2_bau", regions="World", show_numbers=TRUE, scenplot=scenlist, measure="GDP", add_nonco2_emitrade=F){
  Q <- ("Q")
  if(measure=="GDP"){Q <- subset(Q, iq=="y")}
  if(measure=="Consumption"){Q <- subset(Q, iq=="cc")}
  Q$iq <- NULL
  Q <- subset(Q, t %in% seq(1,30))
  #add emission reduction and PES costs to get full gross GDP
  COST_FUEL <- get_witch("COST_FUEL") # oil, gas, coal, but also all biomass, rhc (small) and uranium
  COST_EMI <- get_witch("COST_EMI") # other GHG abatement, CCS costs (big!), peat abatement (big!), nip (if trade!)
  COST_Y <- get_witch("COST_Y") # trbiosub and rhc (big)
  CARBON_TRADE <- COST_EMI %>% filter(e=="co2") %>% select(-e) %>% dplyr::rename(CARBON_TRADE=value) #"nip" is only for internal of a coalition market!!!
  
  #for now replace by postprocessed trade
  reprocess_trade_from_emicap <- F
  if(reprocess_trade_from_emicap){
  emi_cap <- get_witch("emi_cap")
  carbonprice <- get_witch("carbonprice")
  CARBON_TRADE <- add_change_from_reference(carbonprice %>% full_join(emi_cap %>% dplyr::rename(emi_cap=value)) %>% mutate(value=emi_cap*value) %>% select(-emi_cap), refscen = "Accelerated net zero")
  CARBON_TRADE <- CARBON_TRADE %>% filter(str_detect(file, "[t|T]rade")) %>% select(-value_percent_change, -value_ref, -value) %>% dplyr::rename(CARBON_TRADE=value_difference) %>% mutate(CARBON_TRADE=-CARBON_TRADE)
  }
  
  #postprocess non-CO2 trade
  if(add_nonco2_emitrade){
    Q_EMI <- get_witch("Q_EMI")
    nonco2emitrade <- Q_EMI %>% filter(e %in% c("ch4", "n2o", "f-gases")) %>% filter(t %in% seq(1,30)) %>% group_by(file,pathdir,n,t) %>% summarize(nonco2=sum(value)) %>% left_join(get_witch("carbonprice") %>% rename(cprice=value))
    nonco2emitrade <- left_join(nonco2emitrade, Q_EMI %>% filter(e %in% c("ch4", "n2o", "f-gases", "co2")) %>% filter(t %in% seq(1,30)) %>% group_by(file,pathdir,n,t) %>% summarize(ghg=sum(value)))
    BAU_Q_EMI <- get_witch("BAU_Q_EMI")
    nonco2emitrade <- nonco2emitrade %>% left_join(BAU_Q_EMI %>% filter(e %in% c("ch4", "n2o", "f-gases")) %>% filter(t %in% seq(1,30)) %>% group_by(file,pathdir,n,t) %>% summarize(nonco2_bau=sum(value)))
    nonco2emitrade <- nonco2emitrade %>% left_join(BAU_Q_EMI %>% filter(e %in% c("ch4", "n2o", "f-gases", "co2")) %>% filter(t %in% seq(1,30)) %>% group_by(file,pathdir,n,t) %>% summarize(ghg_bau=sum(value)))
    #add do2 emicap
    nonco2emitrade <- nonco2emitrade %>% left_join(get_witch("emi_cap") %>% filter(value!=500) %>% rename(co2cap=value))
    #add population
    l <- get_witch("l")
    nonco2emitrade <- nonco2emitrade %>% left_join(l %>% filter(t %in% seq(1,30)) %>% rename(pop=value))
    #compute emipac only for non-CO2
    if(T){
      nonco2emitrade <- nonco2emitrade %>% group_by(n,file,pathdir) %>% mutate(nonco2_bau0=nonco2_bau[t==5]) %>% ungroup() %>% group_by(pathdir,file,t) %>% mutate(emicap_nonco2=ifelse(t>=5 & t<=20, sum(nonco2)* ( exp(-0.3*(t-5)) * (nonco2_bau0/sum(nonco2_bau0)) + (1-exp(-0.3*(t-5))) * (pop/sum(pop)) ),0)) %>% mutate(emicap_nonco2=ifelse(t>20, sum(nonco2)* ((pop/sum(pop))),emicap_nonco2)) #%>% mutate(totemicap=sum(emicap_nonco2), totemiactual=sum(nonco2))    
      nonco2emitrade <- nonco2emitrade %>% ungroup() %>% mutate(net_revenues_nonco2=-(nonco2-emicap_nonco2)*cprice) %>% mutate(net_revenues_nonco2=ifelse(is.na(net_revenues_nonco2), 0, net_revenues_nonco2)) 
      
      #in case only use trade scenario in name (now jut NOT in "w/o Trade")
      nonco2emitrade <- nonco2emitrade %>% filter(!str_detect(file, "[t|T]rade")) #REMOVE BEFORE COMMITTING!!
      
      CARBON_TRADE <- CARBON_TRADE %>% left_join(nonco2emitrade %>%  select(t,n,file,net_revenues_nonco2)) %>% mutate(CARBON_TRADE=CARBON_TRADE+net_revenues_nonco2) %>% select(-net_revenues_nonco2)
    }

    #compute emipac only for  all ghg
    if(F){
    nonco2emitrade <- nonco2emitrade %>% group_by(n,file,pathdir) %>% mutate(ghg_bau0=ghg_bau[t==5]) %>% ungroup() %>% group_by(pathdir,file,t) %>% mutate(emicap_ghg=ifelse(t>=5 & t<=20, sum(ghg)* ( exp(-0.3*(t-5)) * (ghg_bau0/sum(ghg_bau0)) + (1-exp(-0.3*(t-5))) * (pop/sum(pop)) ),0)) %>% mutate(emicap_ghg=ifelse(t>20, sum(ghg)* ((pop/sum(pop))),emicap_ghg)) #%>% mutate(totemicap=sum(emicap_nonco2), totemiactual=sum(nonco2))
    nonco2emitrade <- nonco2emitrade %>% ungroup() %>% mutate(net_revenues_ghg=-(ghg-emicap_ghg)*cprice) %>% mutate(net_revenues_ghg=ifelse(is.na(net_revenues_ghg), 0, net_revenues_ghg)) 
    #in case only use trade scenario in name
    #nonco2emitrade <- nonco2emitrade %>% filter(str_detect(file, "[t|T]rade"))
    CARBON_TRADE <- CARBON_TRADE %>% left_join(nonco2emitrade %>%  select(t,n,file,net_revenues_ghg)) %>% mutate(CARBON_TRADE=net_revenues_ghg) %>% select(-net_revenues_ghg)
    }
  }
  
  
  COST_EMI <- COST_EMI %>% filter(e!="co2") #all other emission costs
  COST_EMI$e <- NULL;
  COST_EMI <- COST_EMI[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_EMI, "value", "COST_EMI")
  COST_FUEL_OTHER <- COST_FUEL %>% filter(!(fuel %in% c("oil", "gas", "oil")))
  COST_FUEL <- COST_FUEL %>% filter((fuel %in% c("oil", "gas", "oil")))
  COST_FUEL$fuel <- NULL;
  COST_FUEL <- COST_FUEL[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_FUEL, "value", "COST_FUEL")
  COST_FUEL_OTHER$fuel <- NULL;
  COST_FUEL_OTHER <- COST_FUEL_OTHER[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_FUEL_OTHER, "value", "COST_FUEL_OTHER")
  COST_Y$ccy <- NULL;
  COST_Y <- COST_Y[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_Y, "value", "COST_Y")
  Q <- merge(Q, COST_EMI, by = c("t", "n", "file", "pathdir"))
  Q <- merge(Q, COST_FUEL, by = c("t", "n", "file", "pathdir"))
  Q <- merge(Q, COST_FUEL_OTHER, by = c("t", "n", "file", "pathdir"))
  Q <- merge(Q, COST_Y, by = c("t", "n", "file", "pathdir"), all = T)
  Q <- merge(Q, CARBON_TRADE, by = c("t", "n", "file", "pathdir"), all = T) %>% mutate(CARBON_TRADE=ifelse(is.na(CARBON_TRADE), 0, CARBON_TRADE))
  Q$ces_sum = Q$value + Q$COST_EMI + Q$COST_FUEL + Q$COST_FUEL_OTHER + Q$COST_Y + Q$CARBON_TRADE
  #COSTS from now on as changes (with negative signs)
  Q$COST_EMI <- -Q$COST_EMI; Q$COST_FUEL <- -Q$COST_FUEL; Q$COST_FUEL_OTHER <- -Q$COST_FUEL_OTHER; Q$COST_Y <- -Q$COST_Y; Q$CARBON_TRADE <- -Q$CARBON_TRADE;
  POLCOSTDECOMP <- Q %>% pivot_longer(cols = !c("t", file_group_columns, "pathdir", "n"), names_to = "variable") %>%
    full_join(Q %>% filter(file==bauscen) %>% pivot_longer(cols = !c("t", file_group_columns, "pathdir", "n"), names_to = "variable") %>% select(-file) %>% dplyr::rename(value_bau=value)) %>% 
    full_join(Q %>% filter(file==bauscen) %>% pivot_longer(cols = !c("t", file_group_columns, "pathdir", "n"), names_to = "variable") %>% select(-file) %>% filter(variable=="value") %>% dplyr::rename(gdp_bau=value) %>% select(-variable)) %>%
    filter(file!=bauscen) %>% mutate(variable=gsub("value", "GDP", variable))
  POLCOSTDECOMP <- POLCOSTDECOMP %>% filter(file %in% scenplot)
  #Line plot
  p <- ggplot(POLCOSTDECOMP %>% filter(n %in% regions & t>= tmin & t <= tmax)) + geom_line(aes(ttoyear(t), 100*(value-value_bau)/gdp_bau, color=file), show.legend = TRUE) +ylab(paste("Change in % of", measure)) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1)) + facet_grid(variable ~ n)
  if(length(results_dir) > 1){p <- p + facet_grid(variable ~ pathdir)}
  #now aggregate to NPV discounted values (PC)
  DAM_DECOMP_NPV <- POLCOSTDECOMP %>% mutate(diff=(value-value_bau), diff_disc=diff*(1+discount_rate/100)^(-(ttoyear(t)-ttoyear(tmin))), gdp_disc=gdp_bau*(1+discount_rate/100)^(-(ttoyear(t)-ttoyear(tmin)))) %>% filter(t >= tmin & t <= tmax) %>% group_by_at(c(file_group_columns, "pathdir", "n", "variable")) %>% summarize(NPV=sum(diff_disc)/sum(gdp_disc))
  #keep only relevant data and good naming
  
  #for now assign COST_EMI, COST_Y, and COST_FUEL_OTHER to Others Cost like CES sum
  DAM_DECOMP_NPV <- DAM_DECOMP_NPV %>% mutate(variable=gsub("ces_sum|COST_EMI|COST_FUEL_OTHER|COST_Y", "Mitigation Costs", variable)) %>% group_by_at(c("n", "file", "pathdir", "variable")) %>% summarize(NPV=sum(NPV)) %>% mutate(variable=gsub("COST_FUEL", "Fossil Fuel Net Costs", variable))# %>% filter(variable!="CARBON_TRADE") 
  
  
  #DAM_DECOMP_NPV <- DAM_DECOMP_NPV %>% mutate(variable=ifelse(file=="Accelerated NZ w/ Trade", gsub("COST_EMI", "Carbon Trade", variable), variable))
  assign("DAM_DECOMP_NPV", DAM_DECOMP_NPV, envir = .GlobalEnv) 
  #Bar chart
  p_bar <- ggplot(subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen & variable!="GDP")) + geom_bar(position=position_stack(), stat="identity",aes(file, NPV, fill=variable), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))  + facet_grid(. ~ n) + theme(axis.text.x=element_text(angle=90,hjust=1)) + scale_y_continuous(labels = scales::percent) + geom_point(data = subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen & variable=="GDP"), aes(file, NPV), color="black", shape=16)
  if(length(results_dir) > 1){p_bar <- p_bar + facet_grid(. ~ pathdir)}
  if(show_numbers){p_bar <- p_bar + geom_text(data = subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen & variable=="GDP"), aes(file, NPV*1.1, label=paste0(round(NPV*100, 1),"%")), size=3)}
  saveplot(paste0(measure, " loss decomposition"))
}











#Plots of Carbon Prices (TO BE FIXED)
Carbon_Price <- function(scenplot=scenlist){
  carbonprice <- get_witch("carbonprice")
  carbonprice <- subset(carbonprice, file %in% scenplot)
  #carbonprice$value <- carbonprice$value * usd_deflator    #Apply deflator
  p <- ggplot(subset(carbonprice, t==20 & n=="usa")) + geom_bar(position=position_dodge(), stat="identity",aes(file, value*1e3/(44/12), fill=file), show.legend = TRUE) +ylab("$/tCO2") + theme(legend.position="bottom",legend.direction="horizontal")+ guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(results_dir) > 1){p <- p + facet_grid(. ~ pathdir)}
  saveplot("Global Carbon Price 2100")
}


Social_Cost_of_Carbon <- function(regions=witch_regions, scenplot=scenlist){
  m_eqq_emi_tree <- get_witch("m_eqq_emi_tree")
  m_eqq_y <- get_witch("m_eqq_y")
  m_eqq_emi_tree <- subset(m_eqq_emi_tree, e=="co2")
  m_eqq_emi_tree$e <- NULL
  SCC <- m_eqq_emi_tree
  SCC$SCC <- (-1) * (m_eqq_emi_tree$value / m_eqq_y$value) * 1000 / (44/12)
  SCC$value <- NULL; #SCC$pathdir <- NULL
  
  p <- ggplot(subset(SCC, n %in% regions & ttoyear(t) <= yearmax & ttoyear(t)>=2015 & file %in% scenplot),aes(ttoyear(t),SCC,colour=file)) + geom_line(stat="identity", linewidth=1.2) + xlab("year") +ylab("$/tCO2")
  if(length(regions)>1){p <- p + facet_grid(. ~ n, scales="free")}
  if(length(results_dir)!=1){p <- p + facet_grid(pathdir ~ .)}
  saveplot("Social Cost of Carbon")
}
