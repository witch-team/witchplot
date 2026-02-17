create_scenario_selector <- function(scenlist) {
  selectInput(inputId="scenarios_selected", label="Scenarios:", choices=unname(scenlist), size=length(scenlist), selectize=FALSE, multiple=TRUE, selected=unname(scenlist))
}
create_variable_selector <- function(list_of_variables, default_var="Q_EMI", use_picker=TRUE, descriptions=NULL) {
  if(use_picker) {
    if(!is.null(descriptions) && nrow(descriptions) > 0) {
      desc_text <- descriptions$description[match(list_of_variables, descriptions$name)]
      desc_text[is.na(desc_text)] <- ""
      pickerInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, selected=default_var,
                  options=list(`live-search`=TRUE), choicesOpt=list(subtext=desc_text))
    } else {
      pickerInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, selected=default_var, options=list(`live-search`=TRUE))
    }
  } else {
    selectInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, size=1, selectize=FALSE, multiple=FALSE, selected=default_var)
  }
}
create_region_selector <- function(witch_regions, include_aggregates=c("World", "EU"), default_region="World") {
  region_names_map <- if(exists("rice_region_names")) rice_region_names else witch_region_longnames
  long_names <- region_names_map[witch_regions]
  display_labels <- ifelse(!is.na(long_names),
                           paste0(long_names, " (", witch_regions, ")"),
                           witch_regions)
  named_regions <- setNames(as.list(witch_regions), display_labels)
  if(length(include_aggregates) > 0) {
    regions_for_selector <- list(Aggregate=as.list(setNames(include_aggregates, include_aggregates)), `Native regions`=named_regions)
  } else {
    regions_for_selector <- named_regions
  }
  selectInput(inputId="regions_selected", label="Regions:", regions_for_selector, size=max(10, length(regions_for_selector)), selectize=FALSE, multiple=TRUE, selected=default_region)
}
get_gdx_variable_list <- function(results_dir, filelist, filter_time_dependent=FALSE) {
  list_of_variables <- NULL
  for(f in filelist) {
    .gdx <- gdx(file.path(results_dir[1], paste0(f, ".gdx")))
    for(item in c("variables", "parameters")) {
      info_item <- .gdx[[item]]
      info_item <- info_item[info_item$dim<=4,]
      info_item <- info_item[sapply(info_item$domnames, function(x) "t" %in% x),]
      list_of_variables <- c(list_of_variables, info_item$name)
    }
  }
  list_of_variables <- unique(list_of_variables)
  list_of_variables <- c(sort(str_subset(list_of_variables, "^[:upper:]")), sort(str_subset(list_of_variables, "^[:lower:]")))
  if(filter_time_dependent) {
    list_of_variables <- str_subset(list_of_variables, pattern="_t$")
  }
  list_of_variables
}
get_gdx_variable_list_simple <- function(results_dir, filelist) {
  list_of_variables <- NULL
  for(f in filelist) {
    .gdx <- gdx(file.path(results_dir[1], paste0(f, ".gdx")))
    list_of_variables <- c(list_of_variables, all_items(.gdx)$variables)
    list_of_variables <- c(list_of_variables, all_items(.gdx)$parameters)
  }
  list_of_variables <- unique(list_of_variables)
  c(sort(str_subset(list_of_variables, "^[:upper:]")), sort(str_subset(list_of_variables, "^[:lower:]")))
}
