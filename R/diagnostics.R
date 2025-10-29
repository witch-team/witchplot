#little diagnostics dash board based on ggplot

diagnostics_plots <- function(scenplot=scenlist){
  allerr <- get_witch("allerr", scenplot = scenplot)
  allinfoiter <- get_witch("allinfoiter", scenplot = scenplot)
  all_optimal <- get_witch("all_optimal", scenplot = scenplot); all_optimal$optimal <- 1;
  all_feasible <- get_witch("all_feasible", scenplot = scenplot); all_feasible$feasible <- 1;
  price_iter <- get_witch("price_iter", scenplot = scenplot)
  
  iterations <- allinfoiter %>% filter(iterrep=="itertime") %>% group_by(pathdir, file, run, siter) %>% mutate(time=value) %>% select(-value, -iterrep)
  iterations <- merge(iterations, all_optimal, by = c("pathdir", file_group_columns, "run", "siter"), all = T)
  iterations <- merge(iterations, all_feasible, by = c("pathdir", file_group_columns, "run", "siter"), all = T)
  
  #aggregate over runs
  iterations <- iterations %>% group_by_at(c("pathdir", file_group_columns)) %>% mutate(siter=as.numeric(gsub("i","", siter))) %>% arrange(run,siter) %>% mutate(siter=paste0("i",1:n())) %>% select(-run)
  
  scenarios <- iterations %>% group_by_at(c("pathdir", file_group_columns)) %>% summarize(total_time=sum(time), n_iter=length(siter), Optimal=sum(optimal, na.rm = T), n_iter_feas=sum(feasible, na.rm = T)) %>% mutate(Infeasible=n_iter-n_iter_feas, Nonoptimal=n_iter-Optimal-Infeasible) %>% as.data.table()
  scenarios_long <- suppressWarnings(melt(scenarios, id.vars = c("pathdir", file_group_columns)))
  
  #time
  time_dhms <- function(t){
    paste0(ifelse(t>86400, paste0(t %/% (60*60*24),"d"), ""),
    ifelse(t>3600,paste0(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0"),":"),""),
    paste0(formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"),":",formatC(t %% 60, width = 2, format = "d", flag = "0"))
  )}
  plot_time <- ggplot(scenarios) + geom_bar(aes(file, total_time/60), stat = "identity") + ylab("Total duration (minutes)") + xlab("") + facet_grid(. ~ pathdir) + geom_text(aes(x=file, y=total_time/60+10, label=time_dhms(total_time)))

  iterations <- iterations %>% mutate(ONI=ifelse(!is.na(optimal), "Optimal", ifelse(is.na(feasible), "Infeasible", "Nonoptimal")), one=1) %>% mutate(siter=as.numeric(gsub("i","", siter))) %>% arrange(pathdir, file, siter)

  plot_iterations <- ggplot(iterations) + geom_tile(aes(file, one, fill=ONI, group=one), stat = "identity", position = "stack") + ylab("Iterations") + scale_fill_manual(values = c("Optimal"="darkgreen", "Nonoptimal"="yellow", "Infeasible"="red")) + xlab("") + theme(legend.position="none") + facet_grid(. ~ pathdir) + geom_text(data=iterations %>% group_by(file, pathdir) %>% summarize(numiter=max(siter), .groups="drop"), aes(x=file, numiter+5, label=numiter))


  #aggregate ALLERR over runs
  allerr <- allerr %>% group_by_at(c("pathdir", file_group_columns)) %>% mutate(siter=as.numeric(gsub("i","", siter))) %>% arrange(run,siter) %>% mutate(siter=1:n()) %>% select(-run)
  plot_convergence <- ggplot(allerr) + geom_line(aes(siter, pmax(value,0.001), color=V3)) + facet_grid(. ~ pathdir + file) + ylab("Convergence") + xlab("Iteration") +  scale_y_log10(breaks = c(0.005, 0.1, 0.5, 1, 100), labels = c(0.005, 0.1, 0.5, 1, 100)) + geom_hline(yintercept = c(0.005), color="grey") + theme(legend.position = c(0.1,0.5), legend.title = element_blank(), legend.background = element_blank(), legend.key = element_blank())

  #aggregate price_iter over runs
  price_iter <- price_iter %>% group_by_at(c("pathdir", file_group_columns, "V3", "t")) %>% mutate(siter=as.numeric(gsub("i","", siter))) %>% arrange(run,siter) %>% mutate(siter=1:n()) %>% select(-run)
  price_iter <- price_iter %>% group_by_at(c("siter", "pathdir", file_group_columns, "V3")) %>% mutate(value = value / mean(value[1])) %>% as.data.frame() #convert in starting all from 1
  plot_prices <- ggplot(price_iter) + geom_line(data=subset(price_iter, V3=="nip"), aes(ttoyear(t), value, alpha=siter, group=siter), color="red") + facet_grid(. ~ pathdir + file) + ylab("Prices") + xlab("") +  scale_y_log10() + geom_line(data=subset(price_iter, V3=="oil"), aes(ttoyear(t), value, alpha=siter, group=siter), color="black") + facet_grid(. ~ pathdir + file) + ylab("Prices") + xlab("") + theme(legend.position="none")

  suppressWarnings(ggarrange(plot_time, plot_iterations + theme(strip.background = element_blank(), strip.text = element_blank()) , plot_convergence + theme(strip.background = element_blank()) , plot_prices + theme(strip.background = element_blank()) , nrow = 4))
  
  }

