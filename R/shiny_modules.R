extract_additional_sets <- function(afd, file_group_columns) {
additional_sets <- setdiff(names(afd), c(file_group_columns, "pathdir", "t", "n", "value", "tlen"))
if(length(additional_sets)==0) {
list(additional_set_id="na", set_elements="na", additional_set_id2="na", set_elements2="na")
} else if(length(additional_sets)==1) {
# Get sorted elements (keep alphabetical order)
elements <- sort(unique(tolower(afd[[additional_sets[1]]])))
list(
additional_set_id=additional_sets[1],
set_elements=elements,
additional_set_id2="na",
set_elements2="na"
)
} else {
# Get sorted elements for first set (keep alphabetical order)
elements <- sort(unique(tolower(afd[[additional_sets[1]]])))
list(
additional_set_id=additional_sets[1],
set_elements=elements,
additional_set_id2=additional_sets[2],
set_elements2=sort(unique(tolower(afd[[additional_sets[2]]])))
)
}
}
compute_regional_aggregates <- function(afd, variable, regions_to_aggregate=c("World", "EU")) {
if(nrow(afd)==0) return(afd)
for(region_name in regions_to_aggregate) {
if(region_name=="World") {
afd_agg <- afd
afd_agg$n <- NULL
} else if(region_name=="EU") {
eu <- get_witch("eu")
eu_regions <- if(!exists("eu")) c("europe") else unique(eu$n)
afd_agg <- dplyr::filter(afd, n %in% eu_regions)
afd_agg$n <- NULL
} else {
next
}
if(nrow(afd_agg)==0) next
if(variable %in% default_meta_param()$parameter) {
agg_type <- default_meta_param()[parameter==variable & type=="nagg"]$value
if(agg_type=="sum") {
afd_agg <- dplyr::group_by_at(afd_agg, setdiff(names(afd_agg), "value")) %>% dplyr::summarize(value=sum(value), .groups='drop')
} else if(agg_type=="mean") {
afd_agg <- dplyr::group_by_at(afd_agg, setdiff(names(afd_agg), "value")) %>% dplyr::summarize(value=mean(value), .groups='drop')
} else {
afd_agg <- dplyr::group_by_at(afd_agg, setdiff(names(afd_agg), "value")) %>% dplyr::summarize(value=sum(value), .groups='drop')
}
} else {
afd_agg <- dplyr::group_by_at(afd_agg, setdiff(names(afd_agg), "value")) %>% dplyr::summarize(value=sum(value), .groups='drop')
}
afd_agg <- dplyr::mutate(afd_agg, n=region_name) %>% as.data.frame()
afd <- rbind(afd, afd_agg[, names(afd)])
}
afd
}
subset_by_additional_sets <- function(afd, additional_set_id, additional_set_selected, additional_set_id2=NULL, additional_set_selected2=NULL) {
if(additional_set_id!="na") {
afd[[additional_set_id]] <- tolower(afd[[additional_set_id]])
afd <- subset(afd, get(additional_set_id) %in% additional_set_selected)
afd[[additional_set_id]] <- NULL
if(length(additional_set_selected)>1) {
afd <- dplyr::group_by_at(afd, setdiff(names(afd), "value")) %>% dplyr::summarize(value=sum(value), .groups='drop')
}
}
if(!is.null(additional_set_id2) && additional_set_id2!="na") {
afd[[additional_set_id2]] <- tolower(afd[[additional_set_id2]])
afd <- subset(afd, get(additional_set_id2) %in% additional_set_selected2)
afd[[additional_set_id2]] <- NULL
if(length(additional_set_selected2)>1) {
afd <- dplyr::group_by_at(afd, setdiff(names(afd), "value")) %>% dplyr::summarize(value=sum(value), .groups='drop')
}
}
afd
}
apply_growth_rate <- function(afd, growth_rate_flag) {
if(!growth_rate_flag) return(afd)
dplyr::group_by_at(afd, setdiff(names(afd), c("t", "value"))) %>%
dplyr::arrange(t) %>%
dplyr::mutate(year=ttoyear(t), growthrate=((value/dplyr::lag(value))^(1/(year-dplyr::lag(year)))-1)*100) %>%
dplyr::select(-year, -value) %>%
dplyr::rename(value=growthrate) %>%
dplyr::mutate(value=ifelse(is.na(value), 0, value)) %>%
dplyr::ungroup()
}
prepare_plot_data <- function(variable, field_show, yearlim, scenarios, additional_set_id, additional_set_selected, additional_set_id2=NULL, additional_set_selected2=NULL, regions, growth_rate_flag=FALSE, time_filter=TRUE, compute_aggregates=TRUE, verbose=FALSE) {
afd <- get_witch(variable, , field=field_show)
if(verbose) print(stringr::str_glue("Variable {variable} loaded."))
afd <- subset_by_additional_sets(afd, additional_set_id, additional_set_selected, additional_set_id2, additional_set_selected2)
if(time_filter) {
afd <- subset(afd, ttoyear(t)>=yearlim[1] & ttoyear(t)<=yearlim[2])
}
afd <- dplyr::filter(afd, !is.na(value))
if(compute_aggregates) {
afd <- compute_regional_aggregates(afd, variable)
}
afd <- apply_growth_rate(afd, growth_rate_flag)
afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"), paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | stringr::str_detect(file, "historical"))
unit_conv <- unit_conversion(variable)
if(growth_rate_flag) {
unit_conv$unit <- " % p.a."
unit_conv$convert <- 1
}
afd$value <- afd$value * unit_conv$convert
afd$year <- ttoyear(afd$t)
list(data=afd, unit_conv=unit_conv)
}
create_gdx_plot <- function(afd, variable, unit_conv, regions, yearlim, ylim_zero, region_palette, results_dir, show_historical=TRUE) {
if(nrow(afd)==0) return(NULL)
# Remove any rows with NA or infinite values in year or value
afd <- subset(afd, !is.na(year) & !is.infinite(year) & !is.na(value) & !is.infinite(value))
if(nrow(afd)==0) return(NULL)
# Filter data by year range BEFORE splitting
afd <- subset(afd, year >= yearlim[1] & year <= yearlim[2])
if(nrow(afd)==0) return(NULL)
# Separate model and historical data
model_data <- subset(afd, n %in% regions & !stringr::str_detect(file, "historical"))
hist_data <- subset(afd, n %in% regions & stringr::str_detect(file, "historical"))
# Return NULL if no model data
if(nrow(model_data)==0) return(NULL)
# Create plot with nice aesthetics and historical data overlay
if(length(regions)==1 || (length(regions)==1 && regions[1] %in% c("World","EU"))) {
p <- ggplot2::ggplot(model_data, ggplot2::aes(x=year, y=value, color=file)) +
ggplot2::geom_line(linewidth=1.5) +
ggplot2::labs(title=variable, x=NULL, y=unit_conv$unit) +
ggplot2::theme(text=ggplot2::element_text(size=16),
               legend.position="bottom",
               legend.direction="horizontal",
               legend.title=ggplot2::element_blank())
if(ylim_zero) p <- p + ggplot2::geom_hline(yintercept=0, alpha=0.5)
# Add historical data if available and show_historical is TRUE
if(show_historical && nrow(hist_data)>0) {
p <- p + ggplot2::geom_line(data=hist_data, ggplot2::aes(x=year, y=value, color=file), linewidth=1.0)
}
} else {
p <- ggplot2::ggplot(model_data, ggplot2::aes(x=year, y=value, color=n, linetype=file)) +
ggplot2::geom_line(linewidth=1.5) +
ggplot2::labs(title=variable, x=NULL, y=unit_conv$unit) +
ggplot2::scale_colour_manual(values=region_palette) +
ggplot2::theme(text=ggplot2::element_text(size=16),
               legend.position="bottom",
               legend.direction="horizontal",
               legend.title=ggplot2::element_blank())
if(ylim_zero) p <- p + ggplot2::geom_hline(yintercept=0, alpha=0.5)
# Add historical data if available and show_historical is TRUE
if(show_historical && nrow(hist_data)>0) {
p <- p + ggplot2::geom_line(data=hist_data, ggplot2::aes(x=year, y=value, color=n, group=interaction(n, file)), linewidth=1.0)
}
}
if(length(results_dir)!=1) p <- p + ggplot2::facet_grid(. ~ pathdir)
p
}
