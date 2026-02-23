# Define UI
header_ui <- headerPanel("RICE50+ gdxcompaR")

sidebar_ui <- sidebarPanel(
  # Mode toggle                                                    # DATA_BROWSER
  shinyWidgets::radioGroupButtons(                                 # DATA_BROWSER
    "app_mode", NULL,                                              # DATA_BROWSER
    choices = c("Scenarios" = "scenarios", "Data" = "data"),       # DATA_BROWSER
    selected = "scenarios", justified = TRUE, size = "sm"          # DATA_BROWSER
  ),                                                               # DATA_BROWSER
  hr(),
  # Scenarios sidebar
  conditionalPanel("input.app_mode == 'scenarios'",
    uiOutput("select_scenarios"),
    uiOutput("select_variable"),
    uiOutput("choose_additional_set"),
    uiOutput("choose_additional_set2"),
    uiOutput("select_regions"),
    sliderInput("yearlim",
                "Time",
                min = 1970,
                max = 2300,
                value = c(1990,2100),
                step = 5),
    div(style="display:inline-block",checkboxInput("add_historical", "Show historical", value = if(exists("add_historical")) add_historical else TRUE)),
    div(style="display:inline-block",
        checkboxInput("ylim_zero",
                      "ymin=0",
                      value = FALSE)),
    div(style="display:inline-block",checkboxInput("growth_rate", "Show growth rates", value = F)),
    div(style="display:inline-block",checkboxInput("stacked_plot", "Stacked plot", value = F)),
    tags$div(style="display:inline-block",
      tags$label("Show:", style="display:inline-block; margin-right: 5px;"),
      div(style="display:inline-block", radioButtons("field", "", choiceNames = c("l","up","lo"), choiceValues = c("l","up","lo"), inline = TRUE))
    ),
    div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
  ),
  # Data browser sidebar                                           # DATA_BROWSER
  conditionalPanel("input.app_mode == 'data'",                    # DATA_BROWSER
    dataBrowserSidebarUI("data_browser")                          # DATA_BROWSER
  )                                                                # DATA_BROWSER
)

tabs_ui <- tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("gdxcompaR", id = "gdxcompaR", h2(textOutput("varname")),plotOutput("gdxcompaRplot", width = "100%", height = "80vh")),
                tabPanel("Diagnostics", id = "diagnostics", plotOutput("diagnostics", width = "100%", height = "80vh")),
                tabPanel("Iterations", id = "iterationplot", plotOutput("iterationplot", width = "100%", height = "80vh")),
                tabPanel("gdxcompaR MAP", id = "gdxcompaR_map", plotOutput("gdxcompaRmap", width = "100%", height = "80vh")),
                tabPanel("Temperature Map", id = "tatm_plot", plotOutput("tatmplot", width = "100%", height = "80vh"))
)

ui <- fluidPage(

  pageWithSidebar(

    header_ui,

    sidebar_ui,

    mainPanel(
      conditionalPanel("input.app_mode == 'scenarios'", tabs_ui),          # DATA_BROWSER
      conditionalPanel("input.app_mode == 'data'",                         # DATA_BROWSER
                       dataBrowserMainUI("data_browser"))                   # DATA_BROWSER
    )

))

shinyUI(ui)
