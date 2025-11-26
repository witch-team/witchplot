#' Setup GDX Library for Reading GAMS Files
#'
#' Initializes the GAMS GDX library required for reading GDX files from WITCH/RICE models.
#' This function provides helpful diagnostics if GAMS is not installed or cannot be found.
#'
#' @param gams_path Optional path to GAMS installation directory. If NULL (default),
#'   gdxtools will try to auto-detect GAMS installation.
#'
#' @return TRUE if GDX library was loaded successfully, FALSE otherwise
#'
#' @details
#' The gdxtools package requires the GAMS GDX library to read GDX files.
#' If you see errors about "GDX library not loaded", you need to:
#'
#' 1. Install GAMS from https://www.gams.com/download/
#' 2. Run this function to initialize the library: \code{setup_gdx()}
#'
#' If GAMS is installed but not auto-detected, specify the path manually:
#' \code{setup_gdx("C:/GAMS/XX")} where XX is your GAMS version
#'
#' @examples
#' \dontrun{
#' # Auto-detect GAMS installation
#' setup_gdx()
#'
#' # Manually specify GAMS path
#' setup_gdx("C:/GAMS/47")
#' }
#'
#' @export
setup_gdx <- function(gams_path = NULL) {
  # Check if gdxtools is installed
  if (!requireNamespace("gdxtools", quietly = TRUE)) {
    message("✗ gdxtools package is not installed!")
    message("  Install it with: install.packages('gdxtools') or devtools::install_github('lolow/gdxtools')")
    return(FALSE)
  }

  # Try to initialize GDX library
  tryCatch({
    if (is.null(gams_path)) {
      message("Attempting to auto-detect GAMS installation...")
      result <- gdxtools::igdx()
    } else {
      message("Loading GDX library from: ", gams_path)
      result <- gdxtools::igdx(gams_path)
    }

    if (!is.null(result) && result) {
      message("✓ GDX library loaded successfully!")
      message("  You can now use run_witch() and run_rice()")
      return(TRUE)
    } else {
      message("✗ GDX library could not be loaded")
      message("  Please install GAMS from: https://www.gams.com/download/")
      message("  After installing GAMS, run: setup_gdx()")
      return(FALSE)
    }
  }, error = function(e) {
    message("✗ Error loading GDX library: ", conditionMessage(e))
    message("\nTroubleshooting steps:")
    message("  1. Install GAMS from: https://www.gams.com/download/")
    message("  2. After installation, try: setup_gdx()")
    message("  3. If auto-detection fails, specify path: setup_gdx('C:/GAMS/XX')")
    message("\nNote: IIASA database viewer (run_iiasadb) does not require GAMS")
    return(FALSE)
  })
}
