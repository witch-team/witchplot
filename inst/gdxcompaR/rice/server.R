shinyServer(function(input, output, session) {
verbose <- FALSE
growth_rate <- FALSE
list_of_variables <- get_gdx_variable_list_simple(fullpathdir, filelist)
output$select_scenarios <- renderUI({create_scenario_selector(scenlist)})
output$select_variable <- renderUI({create_variable_selector(list_of_variables, default_var="E", use_picker=TRUE)})
output$select_regions <- renderUI({create_region_selector(witch_regions, include_aggregates=c("World"), default_region="World")})
variable_selected_reactive <- reactive({input$variable_selected})
output$varname <- renderText({paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse=","))})
observeEvent(input$button_saveplotdata, {
variable <- input$variable_selected
print("Current plot saved in subdirectory 'graphs'")
saveplot(variable, width=14, height=7)
})
output$gdxcompaRplot <- renderPlot({
assign("historical", input$add_historical, envir=.GlobalEnv)
ylim_zero <- input$ylim_zero
field_show <- input$field
growth_rate <- input$growth_rate
stacked_plot <- input$stacked_plot
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
afd <- get_witch(variable, , field=field_show)
if(verbose) print(str_glue("Variable {variable} loaded."))
set_info <- extract_additional_sets(afd, file_group_columns)
output$choose_additional_set <- renderUI({
variable <- variable_selected_reactive()
if(is.null(variable)) variable <- list_of_variables[1]
sel <- input$additional_set_id_selected
size_elements <- min(length(set_info$set_elements), 5)
selectInput("additional_set_id_selected", "Additional set element", set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
})
yearlim <- input$yearlim
additional_set_selected <- input$additional_set_id_selected
regions <- input$regions_selected
scenarios <- input$scenarios_selected
if(is.null(regions)) regions <- display_regions
if(is.null(additional_set_selected)) additional_set_selected <- set_info$set_elements[1]
if((set_info$additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_info$set_elements)) additional_set_selected <- set_info$set_elements[1]
plot_data <- prepare_plot_data(variable, field_show, yearlim, scenarios, set_info$additional_set_id, additional_set_selected, NULL, NULL, regions, growth_rate, time_filter=TRUE, compute_aggregates=TRUE, verbose=verbose)
afd <- plot_data$data
unit_conv <- plot_data$unit_conv
if(growth_rate){
unit_conv$unit <- " % p.a."
unit_conv$convert <- 1
}
# If stacked plot is requested, use stacked area plot
if(stacked_plot && length(regions) > 1){
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, fill=n)) + geom_area(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_fill_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(fill=guide_legend(title=NULL, nrow=2))
if(!is.null(scenarios) && length(scenarios)>1) p <- p + facet_wrap(. ~ file)
} else if(regions[1]=="World" | length(regions)==1){
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + xlim(yearlim[1], yearlim[2])
if(ylim_zero) p <- p + ylim(0, NA)
p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=file), stat="identity", linewidth=1.0, linetype="solid")
p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=file), size=4.0, shape=18)
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
}else{
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_colour_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=n, group=interaction(n, file)), linetype="solid", stat="identity", linewidth=1.0)
p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=n, shape=file), size=4.0)
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow=2), linetype=guide_legend(title=NULL))
}
if(length(fullpathdir)!=1 && !stacked_plot) p <- p + facet_grid(. ~ pathdir)
if(nrow(afd)>0) print(p + labs(title=variable))
})
output$gdxcompaRstackedplot <- renderPlot({
assign("historical", input$add_historical, envir=.GlobalEnv)
ylim_zero <- input$ylim_zero
field_show <- input$field
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
afd <- get_witch(variable, , field=field_show)
if(verbose) print(str_glue("Variable {variable} loaded."))
set_info <- extract_additional_sets(afd, file_group_columns)
output$choose_additional_set <- renderUI({
variable <- variable_selected_reactive()
if(is.null(variable)) variable <- list_of_variables[1]
sel <- input$additional_set_id_selected
size_elements <- min(length(set_info$set_elements), 5)
selectInput("additional_set_id_selected", "Additional set element", set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
})
yearlim <- input$yearlim
additional_set_selected <- input$additional_set_id_selected
regions <- input$regions_selected
scenarios <- input$scenarios_selected
if(is.null(regions)) regions <- display_regions
if(is.null(additional_set_selected)) additional_set_selected <- set_info$set_elements[1]
if((set_info$additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_info$set_elements)) additional_set_selected <- set_info$set_elements[1]
afd <- subset_by_additional_sets(afd, set_info$additional_set_id, additional_set_selected, NULL, NULL)
afd <- subset(afd, ttoyear(t)>=yearlim[1] & ttoyear(t)<=yearlim[2])
afd <- afd %>% filter(!is.na(value))
afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"), paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
afd_hist <- subset(afd, file %in% c(str_subset(unique(afd$file), "historical")[1]))
afd <- subset(afd, file %in% c(scenarios))
for(scen in scenarios){
afd_hist$file <- scen
if(scen==scenarios[1]) afd_hist_temp <- afd_hist else afd_hist_temp <- rbind(afd_hist_temp, afd_hist)
}
afd <- rbind(afd, afd_hist)
unit_conv <- unit_conversion(variable)
afd$value <- afd$value * unit_conv$convert
afd$year <- ttoyear(afd$t)
p_stacked <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, fill=n)) + geom_area(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_fill_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p_stacked <- p_stacked + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(fill=guide_legend(title=NULL, nrow=2))
if(!is.null(scenarios)) p_stacked <- p_stacked + facet_wrap(. ~ file)
if(nrow(afd)>0) print(p_stacked + labs(title=variable))
})
output$gdxompaRplotly <- renderPlot({
assign("historical", input$add_historical, envir=.GlobalEnv)
ylim_zero <- input$ylim_zero
field_show <- input$field
growth_rate <- input$growth_rate
stacked_plot <- input$stacked_plot
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
afd <- get_witch(variable, , field=field_show)
if(verbose) print(str_glue("Variable {variable} loaded."))
set_info <- extract_additional_sets(afd, file_group_columns)
yearlim <- input$yearlim
additional_set_selected <- input$additional_set_id_selected
regions <- input$regions_selected
scenarios <- input$scenarios_selected
if(is.null(regions)) regions <- display_regions
if(is.null(additional_set_selected)) additional_set_selected <- set_info$set_elements[1]
if((set_info$additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_info$set_elements)) additional_set_selected <- set_info$set_elements[1]
plot_data <- prepare_plot_data(variable, field_show, yearlim, scenarios, set_info$additional_set_id, additional_set_selected, NULL, NULL, regions, growth_rate, time_filter=TRUE, compute_aggregates=TRUE, verbose=verbose)
afd <- plot_data$data
unit_conv <- plot_data$unit_conv
if(growth_rate){
unit_conv$unit <- " % p.a."
unit_conv$convert <- 1
}
# Create plot using same logic as gdxcompaRplot
if(stacked_plot && length(regions) > 1){
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, fill=n)) + geom_area(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_fill_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(fill=guide_legend(title=NULL, nrow=2))
if(!is.null(scenarios) && length(scenarios)>1) p <- p + facet_wrap(. ~ file)
} else if(regions[1]=="World" | length(regions)==1){
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + xlim(yearlim[1], yearlim[2])
if(ylim_zero) p <- p + ylim(0, NA)
p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=file), stat="identity", linewidth=1.0, linetype="solid")
p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=file), size=4.0, shape=18)
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
}else{
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_colour_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=n, group=interaction(n, file)), linetype="solid", stat="identity", linewidth=1.0)
p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=n, shape=file), size=4.0)
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow=2), linetype=guide_legend(title=NULL))
}
if(length(fullpathdir)!=1 && !stacked_plot) p <- p + facet_grid(. ~ pathdir)
if(nrow(afd)>0) print(p + labs(title=variable))
})
output$gdxcompaRmap <- renderPlot({
variable <- input$variable_selected
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
data <- get_witch(variable)
map_simple(data, yearmap=yearlim[2], scenplot=scenarios, title=str_glue("{variable} in {yearlim[2]}"))
})
output$diagnostics <- renderPlot({
# Diagnostics should never load historical data
assign("historical", FALSE, envir=.GlobalEnv)
variable <- input$variable_selected
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
elapsed <- get_witch("elapsed")
if(!exists("elapsed")) elapsed <- data.frame(file=scenlist, value=0)
Y <- get_witch("Y") %>% mutate(year=ttoyear(t))
TATM <- get_witch("TATM") %>% mutate(year=ttoyear(t))
MIU <- get_witch("MIU") %>% mutate(year=ttoyear(t))
l <- get_witch("l") %>% mutate(year=ttoyear(t))
# Find common last year across all scenarios
last_year_by_scen <- Y %>% filter(file %in% scenarios) %>% group_by(file) %>% summarize(max_year=max(year, na.rm=TRUE))
common_last_year <- min(last_year_by_scen$max_year, yearlim[2])
# Filter all data to common year range
Y <- Y %>% filter(year >= yearlim[1] & year <= common_last_year)
TATM <- TATM %>% filter(year >= yearlim[1] & year <= common_last_year)
MIU <- MIU %>% filter(year >= yearlim[1] & year <= common_last_year)
l <- l %>% filter(year >= yearlim[1] & year <= common_last_year)
gini <- Y %>% left_join(l %>% rename(pop=value) %>% select(-year), by=c("t", "n", "file", "pathdir")) %>% group_by(t, year, file, pathdir) %>% summarize(value=reldist::gini(value/pop, weights=pop), .groups="drop")
diagplot <- list()
for(p in subdir){
diagplot[[p]] <- ggarrange(
ggplot(elapsed %>% filter(file %in% scenarios & pathdir==p)) + geom_bar(aes(file, value, fill=file), stat="identity") + ylab("Run time (minutes)") + theme(axis.text.x=element_text(angle=90, hjust=1)) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ggtitle(p) + scale_y_continuous(limits=c(0, max(elapsed$value)+1e-10), labels=function(l) strftime(as.POSIXct(l, origin="1970-01-01"), "%M:%S")),
ggarrange(
ggplot(MIU %>% group_by(year, file, pathdir) %>% summarise(value=mean(value), .groups="drop") %>% filter(file %in% scenarios & pathdir==p)) + geom_line(aes(year, value, color=file), linewidth=1) + ylab("MIU") + xlab("") + xlim(yearlim[1], common_last_year),
ggplot(Y %>% filter(file %in% scenarios & pathdir==p) %>% group_by(year, file, pathdir) %>% summarise(value=sum(value), .groups="drop")) + geom_line(aes(year, value, color=file), linewidth=1) + ylab("GDP [T$]") + xlab("") + xlim(yearlim[1], common_last_year),
ncol=2, common.legend=T, legend="none"
),
ggarrange(
ggplot(TATM %>% filter(file %in% scenarios & pathdir==p & !is.na(value))) + geom_line(aes(year, value, color=file), linewidth=1) + ylab("TATM") + xlab("") + xlim(yearlim[1], common_last_year),
ggplot(gini %>% filter(file %in% scenarios & pathdir==p)) + geom_line(aes(year, value, color=file), linewidth=1) + ylab("Gini index of GDPpc") + xlab("") + ylim(0, 1) + xlim(yearlim[1], common_last_year),
ncol=2, common.legend=T, legend="none"
),
nrow=3, common.legend=T, legend="bottom"
)
}
diagplot_all <- ggarrange(plotlist=diagplot, ncol=length(diagplot), common.legend=T)
print(diagplot_all)
})
output$inequalityplot <- renderPlot({
variable_ineq <- input$variable_selected
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
inequality_plot_type_selected <- input$inequality_plot_type_selected
inequality_value_share <- input$inequality_value_share
plot_inequality(variable=variable_ineq, plot_type=inequality_plot_type_selected, value_share=inequality_value_share, quantile_set="dist", regions=regions[1], years=seq(yearlim[1], yearlim[2]), years_lorenz=range(yearlim[1], yearlim[2]), scenplot=scenarios)
})
output$tatmplot <- renderPlot({
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
gridded_temp_map(yearplot=yearlim[2], scenplot=scenarios, pathadj="../../")
})
output$iterationplot <- renderPlot({
# Iterations should never load historical data
assign("historical", FALSE, envir=.GlobalEnv)
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
regions <- input$regions_selected
viter <- get_witch("viter")
viter <- viter %>% group_by(n, file, pathdir, v, iter) %>% arrange(t) %>% mutate(seen_nonzero=cumsum(value!=0)>0) %>% complete(t) %>% mutate(value=ifelse(is.na(value) & !seen_nonzero, 0, value)) %>% select(-seen_nonzero) %>% ungroup()
viter <- viter %>% group_by(n, file, pathdir, v, iter) %>% summarise(value=mean(value[ttoyear(t)>=yearlim[1] & ttoyear(t)<=yearlim[2]]), .groups="drop")
viter <- viter %>% filter(file %in% scenarios)
if(regions[1]!="World") viter <- viter %>% filter(n %in% regions)
p_iter <- ggplot(viter) + geom_line(aes(iter, value, color=n, group=n)) + facet_grid(v ~ file, scales="free_y") + theme(legend.position="none")
print(p_iter)
})
})
