shinyServer(function(input, output, session) {
verbose <- FALSE
growth_rate <- FALSE
list_of_variables <- get_gdx_variable_list_simple(results_dir, filelist)
list_of_variables <- str_subset(list_of_variables, pattern="_t$")
output$select_scenarios <- renderUI({selectInput("scenarios_selected", "Select scenarios", unname(scenlist), size=length(scenlist), selectize=FALSE, multiple=TRUE, selected=unname(scenlist))})
output$select_variable <- renderUI({
  default_var <- if("GDPr_t" %in% list_of_variables) "GDPr_t" else list_of_variables[1]
  selectInput("variable_selected", "Select variable", list_of_variables, size=1, selectize=FALSE, multiple=FALSE, selected=default_var)
})
output$select_regions <- renderUI({regions_for_selector <- c(witch_regions, "World"); selectInput("regions_selected", "Select regions", regions_for_selector, size=min(17, length(regions_for_selector)), selectize=FALSE, multiple=TRUE, selected=witch_regions)})
variable_selected_reactive <- reactive({input$variable_selected})
variable_input <- reactive({return(input$variable_selected)})
output$varname <- renderText({paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse=","))})
output$varname2 <- renderText({paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse=","))})
observeEvent(input$button_saveplotdata, {
variable <- input$variable_selected
print("Current plot saved in subdirectory 'graphs'")
saveplot(variable, width=14, height=7)
})
output$gdxcompaRplot <- renderPlot({
show_historical <- input$add_historical  # Checkbox controls plot visibility
ylim_zero <- input$ylim_zero
field_show <- input$field
growth_rate <- input$growth_rate
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
afd <- get_witch(variable, , field=field_show)
if(verbose) print(str_glue("Variable {variable} loaded."))
set_info <- extract_additional_sets(afd, file_group_columns)
output$choose_additional_set <- renderUI({
variable <- variable_input()
if(is.null(variable)) variable <- list_of_variables[1]
sel <- input$additional_set_id_selected
if(is.null(sel)){
if("co2_ffi" %in% set_info$set_elements) sel <- "co2_ffi" else sel <- set_info$set_elements[1]
}
size_elements <- min(length(set_info$set_elements), 5)
selectInput(inputId="additional_set_id_selected", label="Index 1:", choices=set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
})
output$choose_additional_set2 <- renderUI({
variable <- variable_input()
if(is.null(variable)) variable <- list_of_variables[1]
sel2 <- input$additional_set_id_selected2
size_elements2 <- min(length(set_info$set_elements2), 5)
selectInput(inputId="additional_set_id_selected2", label="Index 2:", choices=set_info$set_elements2, size=size_elements2, selectize=FALSE, multiple=TRUE, selected=sel2)
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
afd$n <- ifelse(afd$n=="World", "World", toupper(afd$n))
if(regions[1]=="World" | length(regions)==1){
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + xlim(yearlim[1], yearlim[2])
if(ylim_zero) p <- p + ylim(0, NA)
if(show_historical) {
  p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=file), stat="identity", linewidth=1.0, linetype="solid")
}
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
}else{
p <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_colour_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
if(show_historical) {
  p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=n, group=interaction(n, file)), linetype="solid", stat="identity", linewidth=1.0)
}
p <- p + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow=2), linetype=guide_legend(title=NULL))
}
if(length(results_dir)!=1) p <- p + facet_grid(. ~ pathdir)
if(nrow(afd)>0) print(p + labs(title=variable))
})
output$gdxcompaRstackedplot <- renderPlot({
show_historical <- input$add_historical  # Checkbox controls plot visibility
ylim_zero <- input$ylim_zero
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
afd <- get_witch(variable, )
if(verbose) print(str_glue("Variable {variable} loaded."))
set_info <- extract_additional_sets(afd, file_group_columns)
output$choose_additional_set <- renderUI({
variable <- variable_selected_reactive()
if(is.null(variable)) variable <- list_of_variables[1]
sel <- input$additional_set_id_selected
size_elements <- min(length(set_info$set_elements), 5)
selectInput("additional_set_id_selected", "Index 1:", set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
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
if(show_historical) {
  afd_hist <- subset(afd, file %in% c(str_subset(unique(afd$file), "historical")[1]))
  afd <- subset(afd, file %in% c(scenarios))
  for(scen in scenarios){
    afd_hist$file <- scen
    if(scen==scenarios[1]) afd_hist_temp <- afd_hist else afd_hist_temp <- rbind(afd_hist_temp, afd_hist)
  }
  afd <- rbind(afd, afd_hist)
} else {
  afd <- subset(afd, file %in% c(scenarios))
}
unit_conv <- unit_conversion(variable)
afd$value <- afd$value * unit_conv$convert
afd$year <- ttoyear(afd$t)
p_stacked <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(ttoyear(t), value, fill=n)) + geom_area(stat="identity", size=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_fill_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
p_stacked <- p_stacked + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(fill=guide_legend(title=NULL, nrow=2))
if(!is.null(scenarios)) p_stacked <- p_stacked + facet_wrap(. ~ file)
print(p_stacked + labs(title=variable))
})
output$gdxompaRplotly <- renderPlotly({
show_historical <- input$add_historical  # Checkbox controls plot visibility
ylim_zero <- input$ylim_zero
growth_rate <- input$growth_rate
field_show <- input$field
plotly_dynamic <- input$plotly_dynamic
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables[1]
plot_data <- prepare_plot_data(variable, field_show, input$yearlim, input$scenarios_selected, "na", "na", NULL, NULL, input$regions_selected, growth_rate, time_filter=TRUE, compute_aggregates=TRUE, verbose=verbose)
afd <- plot_data$data
unit_conv <- plot_data$unit_conv
yearlim <- input$yearlim
regions <- input$regions_selected
if(regions[1]=="World" | regions[1]=="EU" | length(regions)==1){
p_dyn <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(year, value, colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + xlim(yearlim[1], yearlim[2])
if(show_historical && nrow(afd %>% filter(n %in% regions & str_detect(file, "historical")))>0) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour=file), stat="identity", linewidth=1.0, linetype="solid")
if(nrow(afd %>% filter(n %in% regions & str_detect(file, "valid")))>0) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=file), size=4.0, shape=18)
if(ylim_zero) p <- p + geom_hline(yintercept=0, alpha=0.5)
if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour=file), size=4.0, shape=18)
p_dyn <- p_dyn + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
}else{
p_dyn <- ggplot(subset(afd, n %in% regions & !str_detect(file, "historical") & !str_detect(file, "valid")), aes(year, value, colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + scale_colour_manual(values=region_palette) + xlim(yearlim[1], yearlim[2])
if(show_historical && "historical" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")), aes(ttoyear(t), value, colour=n), linetype="solid", stat="identity", size=1.0)
if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, shape=file), size=4.0)
p_dyn <- p_dyn + theme(text=element_text(size=16), legend.position="bottom", legend.direction="horizontal", legend.box="vertical", legend.key=element_rect(colour=NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow=2), linetype=guide_legend(title=NULL))
}
if(length(results_dir)!=1) p_dyn <- p_dyn + facet_grid(. ~ pathdir)
p_dyn <- p_dyn + theme(legend.position="none")
print(p_dyn)
if(length(ggplot_build(p_dyn)$data[[1]])>0) ggplotly()
})
output$diagnostics <- renderPlot({
variable <- input$variable_selected
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
get_witch("elapsed")
if(!exists("elapsed")) elapsed <- data.frame(file=scenlist, value=0)
get_witch("Y")
get_witch("TATM")
get_witch("MIU")
get_witch("l")
gini <- Y %>% left_join(l %>% rename(pop=value), by=c("t", "n", "file", "pathdir")) %>% group_by(t, file, pathdir) %>% summarize(value=reldist::gini(value/pop, weights=pop))
diagplot <- ggarrange(
ggplot(elapsed %>% filter(file %in% scenarios)) + geom_bar(aes(file, value, fill=file), stat="identity") + ylab("Run time (minutes)") + theme(axis.text.x=element_text(angle=90, hjust=1)) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_y_time(labels=function(l) strftime(l, "%M:%S")),
ggarrange(
ggplot(MIU %>% group_by(t, file, pathdir) %>% summarise(value=mean(value)) %>% filter(file %in% scenarios)) + geom_line(aes(ttoyear(t), value, color=file), size=1) + ylab("MIU") + xlab(""),
ggplot(Y %>% filter(file %in% scenarios) %>% group_by(t, file, pathdir) %>% summarise(value=sum(value))) + geom_line(aes(ttoyear(t), value, color=file), size=1) + ylab("GDP [T$]") + xlab(""),
ncol=2, common.legend=TRUE, legend="none"
),
ggarrange(
ggplot(TATM %>% filter(file %in% scenarios & !is.na(value))) + geom_line(aes(ttoyear(t), value, color=file), size=1) + ylab("TATM") + xlab(""),
ggplot(gini %>% filter(file %in% scenarios)) + geom_line(aes(ttoyear(t), value, color=file), size=1) + ylab("Gini index") + xlab("") + ylim(0, 1),
ncol=2, common.legend=TRUE, legend="none"
),
nrow=3, common.legend=TRUE, legend="bottom"
)
print(diagplot)
})
})
