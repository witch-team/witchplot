shinyServer(function(input, output, session) {
verbose <- FALSE
if(deploy_online){
suppressPackageStartupMessages(require(tidyverse))
require(plotly)
require(shinyWidgets)
add_historical_values <- function(x, varname, iiasadb, verbose){return(x)}
get_witch <- function(variable, field){return(allvariables[[variable]])}
}
list_of_variables <- get_gdx_variable_list(fullpathdir, filelist, filter_time_dependent=FALSE)
output$select_scenarios <- renderUI({create_scenario_selector(scenlist)})
output$select_variable <- renderUI({create_variable_selector(list_of_variables, default_var="Q_EMI", use_picker=TRUE)})
output$select_regions <- renderUI({create_region_selector(witch_regions, include_aggregates=c("World", "EU"), default_region="World")})
variable_input <- reactive({return(input$variable_selected)})
output$varname <- renderText({paste0(variable_input(), "|", str_trunc(paste(input$additional_set_id_selected, collapse=","), 20), ifelse(is.null(input$additional_set_id_selected2) | input$additional_set_id_selected2=="na", "", paste0("|", str_trunc(paste(input$additional_set_id_selected2, collapse=","), 20))), "|", str_trunc(paste(input$regions_selected, collapse=","), 10))})
observeEvent(input$button_saveplotdata, {
variable <- input$variable_selected
print("Current plot saved in subdirectory 'graphs'")
saveplot(variable, width=14, height=7)
})
output$gdxompaRplot <- renderPlot({
assign("historical", input$add_historical, envir=.GlobalEnv)
ylim_zero <- input$ylim_zero
field_show <- input$field
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
selectInput(inputId="additional_set_id_selected", label="Indices 1:", choices=set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
})
output$choose_additional_set2 <- renderUI({
variable <- variable_input()
if(is.null(variable)) variable <- list_of_variables[1]
sel2 <- input$additional_set_id_selected2
size_elements2 <- min(length(set_info$set_elements2), 5)
selectInput(inputId="additional_set_id_selected2", label="Indices 2:", choices=set_info$set_elements2, size=size_elements2, selectize=FALSE, multiple=TRUE, selected=sel2)
})
yearlim <- input$yearlim
additional_set_selected <- input$additional_set_id_selected
additional_set_selected2 <- input$additional_set_id_selected2
regions <- input$regions_selected
scenarios <- input$scenarios_selected
if(is.null(regions)) regions <- display_regions
if(is.null(additional_set_selected)) additional_set_selected <- set_info$set_elements[1]
if((set_info$additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_info$set_elements)) additional_set_selected <- set_info$set_elements[1]
if(is.null(additional_set_selected2)) additional_set_selected2 <- set_info$set_elements2[1]
if((set_info$additional_set_id2!="na" & additional_set_selected2[1]=="na") | !(additional_set_selected2[1] %in% set_info$set_elements2)) additional_set_selected2 <- set_info$set_elements2[1]
plot_data <- prepare_plot_data(variable, field_show, yearlim, scenarios, set_info$additional_set_id, additional_set_selected, set_info$additional_set_id2, additional_set_selected2, regions, growth_rate_flag=FALSE, time_filter=input$time_filter, compute_aggregates=TRUE, verbose=verbose)
p <- create_gdx_plot(plot_data$data, variable, plot_data$unit_conv, regions, yearlim, ylim_zero, region_palette, fullpathdir)
if(!is.null(p)) print(p)
})

output$Diagnostics <- renderPlot({
yearlim <- input$yearlim
scenarios <- input$scenarios_selected
diagnostics_plots(scenplot=scenarios)
})
output$energymixplot <- renderPlot({
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
mix_plot_type_selected <- input$mix_plot_type_selected
mix_y_value_selected <- input$mix_y_value_selected
Primary_Energy_Mix(PES_y=mix_y_value_selected, regions=regions[1], years=seq(yearlim[1], yearlim[2], 1), plot_type=mix_plot_type_selected, scenplot=scenarios)
})
output$electricitymixplot <- renderPlot({
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
mix_plot_type_selected <- input$mix_plot_type_selected
mix_y_value_selected <- input$mix_y_value_selected
Electricity_Mix(Electricity_y=mix_y_value_selected, regions=regions[1], years=seq(yearlim[1], yearlim[2], 1), plot_type=mix_plot_type_selected, scenplot=scenarios)
})
output$investmentplot <- renderPlot({
scenarios <- input$scenarios_selected
Investment_Plot(regions="World", scenplot=scenarios)
})
output$policycostplot <- renderPlot({
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
Policy_Cost(discount_rate=5, regions=regions, bauscen=scenarios[1], show_numbers=TRUE, tmax=yeartot(yearlim[2]))
})
output$intensityplot <- renderPlot({
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
Intensity_Plot(years=c(yearlim[2], yearlim[2]-50), regions=regions, year0=2010, scenplot=scenarios, animate_plot=FALSE)
})

})
