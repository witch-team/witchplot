# Global environment setup for FIDELIO Shiny app
# This file loads required packages and makes functions available

# Suppress package startup messages and warnings about namespace conflicts
suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(plotly)
  library(data.table)
  library(dplyr)  # Load dplyr after data.table to prioritize dplyr functions
  library(stringr)
  library(shinyWidgets)
  library(gdxtools)
  library(witchtools)
  library(tidyr)
  # Optional packages
  if(requireNamespace("arrow", quietly=TRUE)) library(arrow)
  if(requireNamespace("ggpubr", quietly=TRUE)) library(ggpubr)
  if(requireNamespace("rnaturalearth", quietly=TRUE)) library(rnaturalearth)
  if(requireNamespace("sf", quietly=TRUE)) library(sf)
})
