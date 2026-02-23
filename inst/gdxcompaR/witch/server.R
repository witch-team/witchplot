shinyServer(function(input, output, session) {
# Re-initialize on session start to pick up new files (supports F5)
.initialize_witchplot_session()

# Reactive trigger for file refresh
refresh_trigger <- reactiveVal(0)

# Observe refresh button
observeEvent(input$refresh_files, {
  withProgress(message = 'Refreshing GDX files...', value = 0, {
    .initialize_witchplot_session()
    refresh_trigger(refresh_trigger() + 1)
  })
})

dataBrowserServer("data_browser")  # DATA_BROWSER
verbose <- FALSE
if(deploy_online){
suppressPackageStartupMessages(require(tidyverse))
require(plotly)
require(shinyWidgets)
add_historical_values <- function(x, varname, iiasadb, verbose){return(x)}
get_witch <- function(variable, field){return(allvariables[[variable]])}
}

# Make list of variables reactive so it updates on refresh
list_of_variables_reactive <- reactive({
  refresh_trigger()
  get_gdx_variable_list(results_dir, filelist, filter_time_dependent=FALSE)
})

output$select_scenarios <- renderUI({
  refresh_trigger()
  create_scenario_selector(scenlist)
})

output$select_variable <- renderUI({
  list_of_variables <- list_of_variables_reactive()
  create_variable_selector(list_of_variables, default_var="Q_EMI", use_picker=TRUE, descriptions=if(exists("all_var_descriptions")) all_var_descriptions else NULL)
})

output$select_regions <- renderUI({
  refresh_trigger()
  create_region_selector(witch_regions, include_aggregates=c("World", "EU"), default_region="World")
})

variable_input <- reactive({return(input$variable_selected)})

# PERFORMANCE FIX: Move index selectors OUTSIDE renderPlot
# This prevents them from re-rendering every time the plot updates
# Only update when variable changes
set_info_reactive <- reactive({
  refresh_trigger()
  variable <- variable_input()
  list_of_variables <- list_of_variables_reactive()
  if(is.null(variable)) variable <- list_of_variables_reactive()[1]
  field_show <- input$field
  afd <- get_witch(variable, , field=field_show)
  extract_additional_sets(afd, file_group_columns)
})

output$choose_additional_set <- renderUI({
  set_info <- set_info_reactive()
  variable <- variable_input()
  sel <- input$additional_set_id_selected

  # Smart default: prefer "co2_ffi" for Q_EMI, otherwise first element
  if(is.null(sel) || !all(sel %in% set_info$set_elements)){
    if(variable == "Q_EMI" && "co2_ffi" %in% set_info$set_elements) {
      sel <- "co2_ffi"
    } else {
      sel <- set_info$set_elements[1]
    }
  }

  size_elements <- min(length(set_info$set_elements), 5)
  label1 <- if(set_info$additional_set_id != "na") set_info$additional_set_id else "Index 1"
  if(exists("all_var_descriptions") && label1 %in% all_var_descriptions$name) {
    d <- all_var_descriptions$description[all_var_descriptions$name == label1]
    if(length(d) > 0 && nchar(d[1]) > 0) label1 <- paste0(label1, " (", d[1], ")")
  }
  selectInput(inputId="additional_set_id_selected", label=paste0(label1, ":"), choices=set_info$set_elements, size=size_elements, selectize=FALSE, multiple=TRUE, selected=sel)
})

output$choose_additional_set2 <- renderUI({
  set_info <- set_info_reactive()
  sel2 <- input$additional_set_id_selected2
  size_elements2 <- min(length(set_info$set_elements2), 5)
  label2 <- if(set_info$additional_set_id2 != "na") set_info$additional_set_id2 else "Index 2"
  if(exists("all_var_descriptions") && label2 %in% all_var_descriptions$name) {
    d <- all_var_descriptions$description[all_var_descriptions$name == label2]
    if(length(d) > 0 && nchar(d[1]) > 0) label2 <- paste0(label2, " (", d[1], ")")
  }
  selectInput(inputId="additional_set_id_selected2", label=paste0(label2, ":"), choices=set_info$set_elements2, size=size_elements2, selectize=FALSE, multiple=TRUE, selected=sel2)
})

output$varname <- renderText({
  var <- variable_input()
  desc <- ""
  if(exists("all_var_descriptions") && var %in% all_var_descriptions$name) {
    d <- all_var_descriptions$description[all_var_descriptions$name == var]
    if(length(d) > 0 && nchar(d[1]) > 0) desc <- paste0(" \u2014 ", d[1])
  }
  var_text <- paste0(var, desc)
  set_info <- set_info_reactive()
  # Apply same fallback logic as renderPlot so the title always reflects what is shown
  eff_sel <- input$additional_set_id_selected
  if(is.null(eff_sel) || eff_sel[1] == "na" || !(eff_sel[1] %in% set_info$set_elements)) {
    eff_sel <- set_info$set_elements[1]
  }
  if(set_info$additional_set_id != "na") {
    var_text <- paste0(var_text, " [", str_trunc(paste(eff_sel, collapse=", "), 20), "]")
  }
  eff_sel2 <- input$additional_set_id_selected2
  if(is.null(eff_sel2) || eff_sel2[1] == "na" || !(eff_sel2[1] %in% set_info$set_elements2)) {
    eff_sel2 <- set_info$set_elements2[1]
  }
  if(set_info$additional_set_id2 != "na") {
    var_text <- paste0(var_text, " [", str_trunc(paste(eff_sel2, collapse=", "), 20), "]")
  }
  if(!is.null(input$regions_selected) && length(input$regions_selected)==1) {
    var_text <- paste0(var_text, " \u2014 ", input$regions_selected[1])
  }
  var_text
})

observeEvent(input$button_saveplotdata, {
variable <- input$variable_selected
print("Current plot saved in subdirectory 'graphs'")
saveplot(variable, width=14, height=7)
})

output$gdxompaRplot <- renderPlot({
show_historical <- input$add_historical
ylim_zero <- input$ylim_zero
field_show <- input$field
variable <- input$variable_selected
if(is.null(variable)) variable <- list_of_variables_reactive()[1]
set_info <- set_info_reactive()
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
plot_data <- prepare_plot_data(variable, field_show, yearlim, scenarios, set_info$additional_set_id, additional_set_selected, set_info$additional_set_id2, additional_set_selected2, regions, growth_rate_flag=FALSE, time_filter=TRUE, compute_aggregates=TRUE, verbose=verbose)
p <- create_gdx_plot(plot_data$data, variable, plot_data$unit_conv, regions, yearlim, ylim_zero, region_palette, results_dir, show_historical)
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
# FIX: Handle EU region properly - convert to actual EU region list
plot_region <- regions[1]
if(plot_region == "EU") {
  eu <- tryCatch(get_witch("eu"), error = function(e) NULL)
  eu_regions <- if(is.null(eu) || nrow(eu)==0) c("europe") else unique(eu$n)
  plot_region <- eu_regions[1]  # Use first EU region for mix plot
}
Primary_Energy_Mix(PES_y=mix_y_value_selected, regions=plot_region, years=seq(yearlim[1], yearlim[2], 1), plot_type=mix_plot_type_selected, scenplot=scenarios)
})

output$electricitymixplot <- renderPlot({
yearlim <- input$yearlim
regions <- input$regions_selected
scenarios <- input$scenarios_selected
mix_plot_type_selected <- input$mix_plot_type_selected
mix_y_value_selected <- input$mix_y_value_selected
# FIX: Handle EU region properly - convert to actual EU region list
plot_region <- regions[1]
if(plot_region == "EU") {
  eu <- tryCatch(get_witch("eu"), error = function(e) NULL)
  eu_regions <- if(is.null(eu) || nrow(eu)==0) c("europe") else unique(eu$n)
  plot_region <- eu_regions[1]  # Use first EU region for mix plot
}
Electricity_Mix(Electricity_y=mix_y_value_selected, regions=plot_region, years=seq(yearlim[1], yearlim[2], 1), plot_type=mix_plot_type_selected, scenplot=scenarios)
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
