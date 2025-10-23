#Create gdxcompaR based on iiasa form csv or xlsx files or direct database connection

#require packages if online deployed
if(deploy_online){
  suppressPackageStartupMessages(require(tidyverse))
  require(plotly)
  require(shinyWidgets)
  add_historical_values <- function(x, varname, check_calibration, iiasadb, verbose){
    x <- rbind(x, iiasadb_historical %>% filter(VARIABLE==varname))
    return(x)
    }
} 


# Define server 
shinyServer(function(input, output, session) {
    #some global flags
    verbose = FALSE
    
    #get list of variables
    regions <- unique(iiasadb_snapshot$REGION)
    models <- unique(iiasadb_snapshot$MODEL)
    variables <- unique(iiasadb_snapshot$VARIABLE)
    variables <- sort(variables)
    variable_atstart <- ifelse("Population" %in% variables, "Population", variables[1])
    scenarios <- unique(iiasadb_snapshot$SCENARIO)

    #Scenario selector
    output$select_scenarios <- renderUI({
      selectInput("scenarios_selected", "Select scenarios", scenarios, size=length(scenarios), selectize = F, multiple = T, selected = scenarios)
    })  
    
    #Variable selector
    output$select_variable <- renderUI({
      pickerInput(
        inputId = "variable_selected",
        label = "Variable:", 
        choices = variables,
        selected = variable_atstart,
        options = list(
          `live-search` = TRUE)
      )
    })
    
    # Reactively update variable selector
    variable_input <- reactive({
      return(input$variable_selected)
    })

    #MODEL selector
    output$select_models <- renderUI({
      selectInput("models_selected", "Select models", models, size=length(models), selectize = F, multiple = T, selected = models)
    })  
    
    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- regions
    selectInput("regions_selected", "Select regions", regions_for_selector, size=1, selectize = F, multiple = F, selected = "World")
    })
    
    #Compare models or scenarios
    # output$compare_models_scenarios <- renderUI({
    #   compare_models_scenarios_selector <- "Scenarios"
    #   radioButtons("choice_models_scenarios", "Use color for", c("Scenarios", "Models"),selected = "Scenarios", inline=T) 
    # })

    observeEvent(input$button_saveplotdata, {
      variable <- input$variable_selected
      print("Current plot saved in subdirectory 'graphs'")
      saveplot(variable, width = 14, height = 7)
    })
    
    #Additional selector for specific Panels
    
    

    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaR <- renderPlot({
      ylim_zero <- input$ylim_zero
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      unitplot <- unique(allfilesdata$UNIT)[1]
      #add historical data
      allfilesdata <- add_historical_values(allfilesdata, varname = variable, check_calibration = T, iiasadb = T, verbose = F)

      #get input from sliders/buttons
      yearlim <- input$yearlim
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scenarios_selected <- input$scenarios_selected
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, !(MODEL %in% setdiff(models, models_selected)))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearlim[1] & YEAR<=yearlim[2])
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
     
      if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        if(length(models_selected)==1){
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
        }else{
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
        }
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value, linetype=MODEL), stat="identity", linewidth=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))
       }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unitplot) + xlim(yearlim[1],yearlim[2]) + facet_grid(. ~ REGION)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION, linetype=MODEL), stat="identity", linewidth=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      if(nrow(allfilesdata)>0) print(p + labs(title=variable))
      })
    
    
    
    
    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRly <- renderPlotly({
      ylim_zero <- input$ylim_zero
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      unitplot <- unique(allfilesdata$UNIT)[1]
      #add historical data
      allfilesdata <- add_historical_values(allfilesdata, varname = variable, check_calibration = T, iiasadb = T, verbose = F)
      
      #get input from sliders/buttons
      yearlim <- input$yearlim
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scenarios_selected <- input$scenarios_selected
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, !(MODEL %in% setdiff(models, models_selected)))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearlim[1] & YEAR<=yearlim[2])
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
      
      if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        if(length(models_selected)==1){
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
        }else{
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
        }
        if("historical" %in% unique(allfilesdata %>% filter(REGION %in% regions))$SCENARIO) p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value, linetype=MODEL), stat="identity", linewidth=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unitplot) + xlim(yearlim[1],yearlim[2]) + facet_grid(. ~ REGION)
        if("historical" %in% unique(allfilesdata %>% filter(REGION %in% regions))$SCENARIO) p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION, linetype=MODEL), stat="identity", linewidth=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      p_dyn <- p + theme(legend.position = "none") + labs(title=variable)
      #print(p_dyn)
      if(length(ggplot_build(p_dyn)$data[[1]]) > 0) ggplotly(p_dyn)
    })
 
    #plotoutput shiny frames for these three plots
    output$iiasadb_coverage_scenarios <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected
      #Scenarios
      suppressWarnings(ggplot(iiasadb_snapshot %>% filter(SCENARIO %in% scenarios_selected & MODEL %in% models_selected) %>% group_by(MODEL, SCENARIO) %>% filter(!str_detect(REGION, "\\|")) %>% summarize(REGION=unique(REGION)) %>% ungroup() %>% group_by(MODEL, SCENARIO) %>% summarize(REGIONS=length(REGION)), aes(SCENARIO, MODEL, fill=REGIONS)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=REGIONS))  + theme(text = element_text(size=16)) + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50)))
    })
      
    output$iiasadb_coverage_regions <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected
      #Regions
      suppressWarnings(ggplot(iiasadb_snapshot %>% filter(SCENARIO %in% scenarios_selected & MODEL %in% models_selected)  %>% group_by(MODEL, SCENARIO) %>% filter(!str_detect(REGION, "\\|")) %>% summarize(REGION=unique(REGION)) %>% ungroup() %>% group_by(MODEL, REGION) %>% summarize(SCENARIOS=length(SCENARIO)), aes(REGION, MODEL, fill=SCENARIOS)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=SCENARIOS))  + theme(text = element_text(size=16)) + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50)))
    })
    
    output$iiasadb_coverage_variables <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected
      #Variables
      #suppressWarnings(ggplot(iiasadb_snapshot %>% group_by(MODEL, SCENARIO) %>% filter(!str_detect(REGION, "\\|")) %>% summarize(VARIABLE=unique(VARIABLE)) %>% ungroup() %>% group_by(MODEL, VARIABLE) %>% summarize(SCENARIOS=length(SCENARIO)), aes(VARIABLE, MODEL, fill=SCENARIOS)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=SCENARIOS))  + theme(text = element_text(size=16)) + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50)))
      ggplot(iiasadb_snapshot %>% group_by(MODEL, SCENARIO) %>% filter(!str_detect(REGION, "\\|")) %>% reframe(VARIABLE=unique(VARIABLE)) %>% group_by(MODEL, VARIABLE) %>% summarize(SCENARIOS=length(SCENARIO)), aes(VARIABLE, MODEL, fill=SCENARIOS)) + geom_tile() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_text(aes(label=SCENARIOS), size=3)  + theme(text = element_text(size=10)) + scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") + scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) + coord_flip()
    })
      

    
    
    
})
