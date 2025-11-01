# Define UI
#shinyUI(fluidPage(theme = shinytheme("superhero"), pageWithSidebar(
shinyUI(fluidPage(
  
  pageWithSidebar(
  
  # Application title
  headerPanel("FIDELIO gdxcompaR"),
  
  # Sidebar with a slider of years and set elements
  sidebarPanel(
    uiOutput("select_variable"),
    uiOutput("choose_additional_set"),
    uiOutput("choose_additional_set2"),
    uiOutput("select_scenarios"),
    uiOutput("select_regions"),
    sliderInput("yearlim", 
                "Time", 
                min = 1970,
                max = 2150,
                value = c(1990,2100),
                step = 5),
    div(style="display:inline-block",
        checkboxInput("add_historical",
                      "Show historical",
                      value = if(exists("add_historical")) add_historical else TRUE)),
    div(style="display:inline-block",
        checkboxInput("ylim_zero",
                      "ymin=0",
                      value = FALSE)),
    div(style="display:inline-block",checkboxInput("growth_rate", "Show growth rates", value = F)),
    div(style="display:inline-block",radioButtons("field", "", choiceNames = c("l","up","lo"), choiceValues = c("l","up","lo"), inline = TRUE)),
    div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
),
    

  # Show the plot
  mainPanel(
    tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("gdxcompaR", id = "gdxcompaR", h2(textOutput("varname")),plotOutput("gdxcompaRplot", width = "100%", height = "80vh")),
                #tabPanel("Diagnostics", id = "diagnostics", plotOutput("diagnostics", width = "100%", height = "80vh")),
                tabPanel("gdxcompaR stacked", id = "gdxcompaR_stacked", h2(textOutput("varname2")),plotOutput("gdxcompaRstackedplot", width = "100%", height = "80vh")),

    )
  )
)))
