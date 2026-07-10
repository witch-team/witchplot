create_scenario_selector <- function(scenlist) {
  scenarios <- unname(scenlist)
  groups <- if (exists("scenlist_groups", envir = .GlobalEnv)) get("scenlist_groups", envir = .GlobalEnv) else NULL
  if (!is.null(groups)) {
    grp_labels <- groups[names(scenlist)]
    grp_labels[is.na(grp_labels)] <- ""
    if (any(grp_labels != "")) {
      grp_labels[grp_labels == ""] <- "root"
      root_scens <- scenarios[grp_labels == "root"]
      other_scens <- scenarios[grp_labels != "root"]
      other_labels <- grp_labels[grp_labels != "root"]
      choices <- c(setNames(as.list(root_scens), root_scens), split(other_scens, other_labels))
      return(selectInput(inputId="scenarios_selected", label="Scenarios:", choices=choices,
                         size=min(length(scenlist), 10), selectize=FALSE, multiple=TRUE, selected=scenarios))
    }
  }
  selectInput(inputId="scenarios_selected", label="Scenarios:", choices=scenarios,
              size=min(length(scenlist), 10), selectize=FALSE, multiple=TRUE, selected=scenarios)
}
create_variable_selector <- function(list_of_variables, default_var="Q_EMI", use_picker=TRUE, descriptions=NULL) {
  if(use_picker) {
    space_tokens <- gsub("_", " ", list_of_variables)
    picker_opts <- list(`live-search`=TRUE, `live-search-style`='contains')
    if(!is.null(descriptions) && nrow(descriptions) > 0) {
      desc_text <- descriptions$description[match(list_of_variables, descriptions$name)]
      desc_text[is.na(desc_text)] <- ""
      pickerInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, selected=default_var,
                  options=picker_opts, choicesOpt=list(subtext=desc_text, tokens=space_tokens))
    } else {
      pickerInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, selected=default_var,
                  options=picker_opts, choicesOpt=list(tokens=space_tokens))
    }
  } else {
    selectInput(inputId="variable_selected", label="Variable:", choices=list_of_variables, size=1, selectize=FALSE, multiple=FALSE, selected=default_var)
  }
}
create_region_selector <- function(witch_regions, include_aggregates=c("World", "EU"), default_region="World") {
  region_names_map <- if(exists("rice_region_names")) rice_region_names else witch_region_longnames
  long_names <- region_names_map[witch_regions]
  display_labels <- ifelse(!is.na(long_names),
                           paste0(witch_regions, " - ", long_names),
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
