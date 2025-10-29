## Region Color Palettes and Mappings
## This file contains region-specific color palettes and name mappings for WITCH models

#' Get witch region color palette
#'
#' Returns a named vector of colors for WITCH regions
#'
#' @param regions Character vector of region names to include in palette
#' @param reg_id Regional aggregation identifier (e.g., "witch17", "ed57")
#' @return Named vector of hex colors
#' @export
get_region_palette <- function(regions, reg_id = NULL) {
  # Base palette with specific colors for common regions
  region_palette_specific <- setNames(rainbow(length(regions)), regions)

  # WITCH standard regions
  region_palette_witch <- c(
    usa = "darkblue", Usa = "darkblue",
    oldeuro = "blue", neweuro = "cornflowerblue",
    kosau = "darkgreen", Kosau = "darkgreen",
    cajaz = "chartreuse4", Cajaz = "chartreuse4",
    te = "gold2", Te = "gold2",
    mena = "darkgoldenrod4", Mena = "darkgoldenrod4",
    ssa = "goldenrod", Ssa = "goldenrod",
    sasia = "darkorange2", "South Asia" = "darkorange2",
    china = "deeppink3", PRC = "deeppink3",
    easia = "orangered", ESEAP = "orangered",
    laca = "#fbb714", Laca = "#fbb714",
    india = "#fbf003", India = "#fbf003",
    europe = "blue", Europe = "blue",
    indonesia = "lightsalmon3", Indonesia = "lightsalmon3",
    Rest_of_World = "grey48",
    chinaw = "darkorange", chinac = "darkorange2", chinae = "darkorange4",
    italy = "green", mexico = "slateblue2", brazil = "tomato4",
    canada = "blueviolet", jpnkor = "darkseagreen", oceania = "forestgreen",
    southafrica = "indianred3", seasia = "orangered",
    World = "black", "Global Pool" = "black"
  )

  # RICE50+ ed57 regional aggregation
  region_palette_ed57 <- c(
    "arg" = "#000000", "aus" = "#48d1cc", "aut" = "#ae8000", "bel" = "#800000",
    "bgr" = "#003366", "blt" = "#bf4040", "bra" = "#ffd633", "can" = "#6600cc",
    "chl" = "#ffece6", "chn" = "#ff531a", "cor" = "#adebad", "cro" = "#808080",
    "dnk" = "#ff9933", "egy" = "#0044cc", "esp" = "#ffd6cc", "fin" = "#00cccc",
    "fra" = "#cc0000", "gbr" = "#ffffdd", "golf57" = "#33d6ff", "grc" = "#00ffcc",
    "hun" = "#9999ff", "idn" = "#996633", "irl" = "#ff4dff", "ita" = "#ffff00",
    "jpn" = "#006600", "meme" = "#b32d00", "mex" = "#ccff33", "mys" = "#145252",
    "nde" = "#00d900", "nld" = "#c309bd", "noan" = "#ffff99", "noap" = "#ecf2f9",
    "nor" = "#ff3399", "oeu" = "#ffb3ff", "osea" = "#008fb3", "pol" = "#d6f5d6",
    "prt" = "#003300", "rcam" = "#4d1919", "rcz" = "#00ffff", "rfa" = "#deb887",
    "ris" = "#000080", "rjan57" = "#bf00ff", "rom" = "#ff00ff", "rsaf" = "#ff8000",
    "rsam" = "#0000ff", "rsas" = "#ccd6dd", "rsl" = "#00ff00", "rus" = "#66757f",
    "slo" = "#ff3091", "sui" = "#61a62f", "swe" = "#cb1942", "tha" = "#efff14",
    "tur" = "#4b0082", "ukr" = "#c198ff", "usa" = "#ffcc00", "vnm" = "#3377ff",
    "zaf" = "#b3ccff"
  )

  # WITCH34 regional aggregation
  region_palette_witch34 <- c(
    "bnl" = "#800000", "northeu" = "#bf4040", "balkan" = "#808080",
    "easteu" = "#9999ff", "che" = "#61a62f", "deu" = "#deb887",
    "rou" = "#ff00ff", "cze" = "#00ffff", "japan" = "green", "korea" = "red"
  )

  # Combine palettes
  region_palette <- replace(region_palette_specific, names(region_palette_witch), region_palette_witch)
  region_palette <- replace(region_palette, names(region_palette_ed57), region_palette_ed57)
  region_palette <- replace(region_palette, names(region_palette_witch34), region_palette_witch34)

  # Keep only palette for regions actually used
  region_palette <- region_palette[regions]

  return(region_palette)
}

#' Get WITCH region short names
#'
#' Converts WITCH region names to 3-letter ISO-like codes
#'
#' @param witch_name Character vector of WITCH region names
#' @return Character vector of shortened names
#' @export
witch_name_short <- function(witch_name) {
  witch_name <- gsub("indonesia", "IDN", witch_name)
  witch_name_shortened <- substr(toupper(witch_name), 1, 3)
  witch_name_shortened <- gsub("MEN", "MEA", witch_name_shortened)
  witch_name_shortened <- gsub("SOU", "ZAF", witch_name_shortened)
  witch_name_shortened <- gsub("CHI", "CHN", witch_name_shortened)
  witch_name_shortened <- gsub("TE", "TEC", witch_name_shortened)
  return(witch_name_shortened)
}

#' WITCH region long names mapping
#'
#' Named vector mapping region codes to full names
#'
#' @export
witch_region_longnames <- c(
  "canada" = "Canada",
  "jpnkor" = "Japan-Korea",
  "oceania" = "Oceania",
  "indonesia" = "Indonesia",
  "southafrica" = "South Africa",
  "brazil" = "Brazil",
  "mexico" = "Mexico",
  "china" = "China",
  "india" = "India",
  "te" = "Transition Economies",
  "ssa" = "Sub-Saharan Africa",
  "laca" = "Latin America-Caribbean",
  "sasia" = "South Asia",
  "seasia" = "South East Asia",
  "mena" = "Middle East-North Africa",
  "europe" = "Europe",
  "usa" = "United States of America",
  "easia" = "East Asia",
  "kosau" = "South-Korea and Australia",
  "cajaz" = "Canada Japan New-Zealand",
  "neweuro" = "Eastern Europe",
  "oldeuro" = "Western Europe"
)
