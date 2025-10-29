\name{run_witch}
\alias{run_witch}
\title{Run WITCH Model Visualization App}
\description{
Launch interactive Shiny application for WITCH model results visualization and comparison.
}
\usage{
run_witch(folder = NULL, main_folder = NULL, subdir = "", 
         restrict_files = "results_", exclude_files = "", 
         removepattern = "", yearmin = 1980, yearmax = 2100, ...)
}
\arguments{
\item{folder}{Path to WITCH model folder (required)}
\item{main_folder}{Main results directory (defaults to folder)}
\item{subdir}{Subdirectories to scan (default: "")}
\item{restrict_files}{File pattern filter (default: "results_")}
\item{exclude_files}{Files to exclude (default: "")}
\item{removepattern}{Pattern to remove from names (default: "")}
\item{yearmin}{Minimum year for visualization (default: 1980)}
\item{yearmax}{Maximum year for visualization (default: 2100)}
\item{...}{Additional options passed to options()}
}
\value{
Launches Shiny application (no return value)
}
\examples{
\dontrun{
library(witchplot)
run_witch(folder="../witch", yearmin=2000, yearmax=2100)
}
}
\author{Johannes Emmerling and WITCH Team}
\seealso{\code{\link{run_rice}}, \code{\link{run_fidelio}}, \code{\link{run_iiasadb}}}
