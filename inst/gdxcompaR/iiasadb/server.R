#Create gdxcompaR based on iiasa form csv or xlsx files or direct database connection

#require packages if online deployed
if(deploy_online){
  suppressPackageStartupMessages(require(tidyverse))
  require(plotly)
  require(shinyWidgets)
  add_historical_values <- function(x, varname, iiasadb, verbose){
    # Always keep historical data sources separate (check_calibration is always TRUE)
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

    #Scenario selector (max 10 rows with scrollbar)
    output$select_scenarios <- renderUI({
      selectInput("scenarios_selected", "Select scenarios", scenarios, size=min(10, length(scenarios)), selectize = F, multiple = T, selected = scenarios)
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

    #MODEL selector (max 10 rows with scrollbar)
    output$select_models <- renderUI({
      selectInput("models_selected", "Select models", models, size=min(10, length(models)), selectize = F, multiple = T, selected = models)
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
      show_legend <- input$show_legend
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data using new get_iiasadb() function (similar to get_witch())
      allfilesdata <- get_iiasadb(variable, add_historical = if(exists("add_historical")) add_historical else FALSE)
      unitplot <- unique(allfilesdata$UNIT)[1]

      #get input from sliders/buttons
      yearlim <- input$yearlim
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scenarios_selected <- input$scenarios_selected

      #select scenarios - use data.frame indexing to preserve all columns
      allfilesdata <- allfilesdata[allfilesdata$SCENARIO %in% c(scenarios_selected, "historical"), ]
      allfilesdata <- allfilesdata[!(allfilesdata$MODEL %in% setdiff(models, models_selected)), ]

      #time frame
      allfilesdata <- allfilesdata[allfilesdata$year>=yearlim[1] & allfilesdata$year<=yearlim[2], ]
      #clean data
      allfilesdata <- allfilesdata[!is.na(allfilesdata$value), ]
     
      if(is.null(regions)) regions <- "World"
      # Convert regions to lowercase for matching (since n column is lowercase)
      regions_lower <- tolower(regions)

      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        # Filter data first
        plot_data <- allfilesdata[allfilesdata$n %in% regions_lower & allfilesdata$SCENARIO!="historical", ]
        hist_data <- allfilesdata[allfilesdata$n %in% regions_lower & allfilesdata$SCENARIO=="historical", ]

        if(length(models_selected)==1){
          # Single model: use color for scenarios
          p <- ggplot(plot_data, aes(x=year, y=value, colour=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
          # Use a programmatic color palette that can handle many scenarios
          n_scenarios <- length(unique(plot_data$SCENARIO))
          if(n_scenarios > 12) {
            # Use viridis for many scenarios (colorblind-friendly and handles many values)
            p <- p + scale_colour_viridis_d(option = "turbo")
          }
          # Historical data in different colors (one per data source/MODEL)
          if(nrow(hist_data) > 0) {
            p <- p + geom_line(data=hist_data, aes(x=year, y=value, colour=MODEL), stat="identity", linewidth=1.0)
          }
        }else{
          # Multiple models: use color for MODEL, linetype for SCENARIO
          p <- ggplot(plot_data, aes(x=year, y=value, colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearlim[1],yearlim[2])
          # Use a programmatic color palette that can handle many models
          n_models <- length(unique(plot_data$MODEL))
          if(n_models > 12) {
            p <- p + scale_colour_viridis_d(option = "turbo")
          }
          # Historical data in different colors (one per MODEL)
          if(nrow(hist_data) > 0) {
            p <- p + geom_line(data=hist_data, aes(x=year, y=value, colour=MODEL), stat="identity", linewidth=1.0)
          }
        }
        if(ylim_zero) p <- p + ylim(0, NA)
        # Add faceting by pathdir if multiple directories
        if(exists("results_dir") && length(results_dir) > 1 && "PATHDIR" %in% names(allfilesdata)) {
          p <- p + facet_grid(. ~ PATHDIR)
        }
        #legends:
        if(show_legend) {
          p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))
        } else {
          p <- p + theme(text = element_text(size=16), legend.position="none")
        }
       }else{
        # Multiple regions: filter data first
        plot_data <- allfilesdata[allfilesdata$n %in% regions_lower & allfilesdata$SCENARIO!="historical", ]
        hist_data <- allfilesdata[allfilesdata$n %in% regions_lower & allfilesdata$SCENARIO=="historical", ]

        p <- ggplot(plot_data, aes(x=year, y=value, colour=interaction(n, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unitplot) + xlim(yearlim[1],yearlim[2]) + facet_grid(. ~ n)
        # Use a programmatic color palette for region-model combinations
        n_colors <- length(unique(interaction(plot_data$n, plot_data$MODEL)))
        if(n_colors > 12) {
          p <- p + scale_colour_viridis_d(option = "turbo")
        }
        if(nrow(hist_data) > 0) {
          p <- p + geom_line(data=hist_data, aes(x=year, y=value, colour=n, linetype=MODEL), stat="identity", linewidth=1.0)
        }
        if(ylim_zero) p <- p + ylim(0, NA)
        # Add additional faceting by pathdir if multiple directories
        if(exists("results_dir") && length(results_dir) > 1 && "PATHDIR" %in% names(allfilesdata)) {
          p <- p + facet_grid(PATHDIR ~ n)
        }
        #legends:
        if(show_legend) {
          p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
        } else {
          p <- p + theme(text = element_text(size=16), legend.position="none")
        }
      }
      if(nrow(allfilesdata)>0) print(p + labs(title=variable))
      })

    # Coverage plots
    output$iiasadb_coverage_scenarios <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected

      coverage_data <- iiasadb_snapshot %>%
        filter(SCENARIO %in% scenarios_selected & MODEL %in% models_selected) %>%
        group_by(MODEL, SCENARIO) %>%
        filter(!str_detect(REGION, "\\|")) %>%
        summarize(REGION=unique(REGION), .groups="drop") %>%
        group_by(SCENARIO, MODEL) %>%
        summarize(REGIONS=length(REGION), .groups = 'drop')

      ggplot(coverage_data, aes(MODEL, SCENARIO, fill=REGIONS)) +
        geom_tile() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, margin = margin(t = 10)),
          axis.text.y = element_text(size = 16, hjust = 1, margin = margin(r = 15)),
          text = element_text(size = 12),
          axis.title = element_text(size = 16, margin = margin(15, 15, 15, 15)),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(30, 30, 30, 30)
        ) +
        geom_text(aes(label=REGIONS), size=4, color="black") +
        scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        scale_y_discrete(labels = function(x) str_wrap(x, width = 35)) +
        labs(x = "Models", y = "Scenarios", fill = "Regions")
    })

    output$iiasadb_coverage_regions <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected

      coverage_data <- iiasadb_snapshot %>%
        filter(SCENARIO %in% scenarios_selected & MODEL %in% models_selected) %>%
        group_by(MODEL, SCENARIO) %>%
        filter(!str_detect(REGION, "\\|")) %>%
        summarize(REGION=unique(REGION), .groups="drop") %>%
        group_by(REGION, MODEL) %>%
        summarize(SCENARIOS=length(SCENARIO), .groups = 'drop')

      ggplot(coverage_data, aes(MODEL, REGION, fill=SCENARIOS)) +
        geom_tile() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, margin = margin(t = 10)),
          axis.text.y = element_text(size = 16, hjust = 1, margin = margin(r = 15)),
          text = element_text(size = 12),
          axis.title = element_text(size = 16, margin = margin(15, 15, 15, 15)),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(30, 30, 30, 30)
        ) +
        geom_text(aes(label=SCENARIOS), size=4, color="black") +
        scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        scale_y_discrete(labels = function(x) str_wrap(x, width = 35)) +
        labs(x = "Models", y = "Regions", fill = "Scenarios")
    })

    # Variables table with DT
    output$variables_table <- DT::renderDataTable({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected

      coverage_data <- iiasadb_snapshot %>%
        filter(SCENARIO %in% scenarios_selected) %>%
        filter(MODEL %in% models_selected) %>%
        filter(!str_detect(REGION, "\\|")) %>%
        group_by(VARIABLE, MODEL) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = MODEL, values_from = Count, values_fill = 0) %>%
        mutate(num_models = rowSums(select(., -all_of("VARIABLE")) > 0),
               single_model = case_when(
                 num_models == 1 ~ {
                   model_cols <- select(., -all_of(c("VARIABLE", "num_models")))
                   names(model_cols)[max.col(model_cols)]
                 },
                 TRUE ~ ""
               )) %>%
        arrange(desc(num_models), single_model, VARIABLE) %>%
        select(-all_of(c("num_models", "single_model")))

      DT::datatable(
        coverage_data,
        options = list(
          pageLength = 100,
          scrollX = TRUE,
          scrollY = "600px",
          dom = 'frtip',
          processing = TRUE,
          deferRender = TRUE,
          columnDefs = list(
            list(width = '200px', targets = 0),
            list(width = '80px', targets = 1:(ncol(coverage_data)-1)),
            list(className = 'dt-center', targets = 1:(ncol(coverage_data)-1))
          )
        ),
        rownames = FALSE,
        class = 'compact stripe'
      ) %>%
        DT::formatStyle(
          columns = names(coverage_data)[-1],
          backgroundColor = DT::styleInterval(
            cuts = c(0.5, 10, 50),
            values = c("white", "#ffffcc", "#a1dab4", "#2c7fb8")
          )
        )
    })

    # Years coverage plot
    output$iiasadb_coverage_years <- renderPlot({
      models_selected <- input$models_selected
      scenarios_selected <- input$scenarios_selected

      coverage_data <- iiasadb_snapshot %>%
        filter(SCENARIO %in% scenarios_selected & MODEL %in% models_selected) %>%
        filter(!str_detect(REGION, "\\|")) %>%
        group_by(YEAR, MODEL) %>%
        summarize(SCENARIOS_REGIONS = length(unique(paste(SCENARIO, REGION))), .groups = 'drop')

      ggplot(coverage_data, aes(MODEL, YEAR, fill=SCENARIOS_REGIONS)) +
        geom_tile() +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1, margin = margin(t = 10)),
          axis.text.y = element_text(size = 16, hjust = 1, margin = margin(r = 15)),
          text = element_text(size = 12),
          axis.title = element_text(size = 16, margin = margin(15, 15, 15, 15)),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(30, 30, 30, 30)
        ) +
        geom_text(aes(label=SCENARIOS_REGIONS), size=3, color="black") +
        scale_fill_gradient2(low = "white", mid = "yellow", high = "darkgreen") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
        scale_y_continuous(breaks = function(x) seq(ceiling(min(x)/5)*5, floor(max(x)/5)*5, by = 5)) +
        labs(x = "Models", y = "Years", fill = "Scenarios×Regions")
    })
      

    
    
    
})
