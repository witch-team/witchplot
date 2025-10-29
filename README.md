# witchplot

Interactive Visualization Toolkit for GAMS IAM Model Results (WITCH, RICE, DICE, FIDELIO, ...) with the aim to make model results easily accessible, able to validate, and comparable with an open source tool, see, e.g., https://doi.org/10.12688/openreseurope.18824.2.

## Installation

Install directly from GitHub using devtools:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install witchplot from GitHub
devtools::install_github("witch-team/witchplot")

# Load the package
library(witchplot)
```

Alternatively, install from a local source:

```r
install.packages("path/to/witchplot", repos=NULL, type="source")
library(witchplot)
```

## Usage

### WITCH Model
```r
library(witchplot)
run_witch()  # Uses defaults: model_dir="../", results_dir="./"

# Or specify custom paths:
run_witch(
  model_dir="../witch",
  results_dir=c("results", "results_v2"),
  yearmin=1980,
  yearmax=2100
)
```

### RICE50+ Model
```r
run_rice()  # Uses defaults

# Or specify custom paths:
run_rice(
  model_dir="../RICE50x",
  results_dir="results",
  reg_id="ed58",
  year0=2015,
  tstep=5
)
```

### FIDELIO Model
```r
run_fidelio()  # Uses defaults

# Or specify custom paths:
run_fidelio(
  model_dir="../fidelio",
  results_dir="results"
)
```

### IIASADB
```r
# Auto-load all CSV and XLSX files from results directory (default behavior)
run_iiasadb()  # Automatically finds and combines all .csv, .xlsx, .csv.zip files

# Or load a specific file
run_iiasadb(iamc_filename="data.csv")

# Or specify custom paths
run_iiasadb(
  model_dir="../witch",
  results_dir="EIEE-MIP"  # Will load all CSV/XLSX files in this directory
)

# Load specific file with custom paths
run_iiasadb(
  model_dir="../witch",
  results_dir="EIEE-MIP",
  iamc_filename="data.csv"  # Load only this file
)

# Connect to IIASA database directly
run_iiasadb(iamc_databasename="IIASA-database-name")
```

## Options (with defaults)

```r
options(
  deploy_online=FALSE,      # Save graphs if not deployed online
  figure_format="png",      # Output format
  add_historical=TRUE,      # Add historical data
  yearmin=1980,             # Minimum year for plots
  yearmax=2100,             # Maximum year for plots
  write_plotdata_csv=FALSE  # Save plot data as CSV
)
```

## Parameters

- **model_dir**: Path to model source code (default: "../")
- **results_dir**: Path(s) to results directories (default: "./", can be vector for multiple dirs)
- **restrict_files**: File pattern filter (default: "results_")
- **exclude_files**: Files to exclude
- **launch**: Whether to launch Shiny app (default: TRUE). Set to FALSE to load data without launching UI
- **All options**: Can be passed as function parameters

## Examples

### Basic Diagnostic Workflow

```r
library(witchplot)

# Run diagnostics to check model calibration and basic outputs
diagnostics_plots()

# Plot global emissions with baseline scenario
Plot_Global_Emissions(bauscen = "bau")



## Available Functions

### Main Application Launchers
- **run_witch()** - Launch WITCH model interactive visualization app
- **run_rice()** - Launch RICE50+ model interactive visualization app
- **run_fidelio()** - Launch FIDELIO model interactive visualization app
- **run_iiasadb()** - Launch IIASA database comparison app

### Data Loading & Processing
- **get_witch()** - Load WITCH model variables from GDX files
- **get_iiasadb()** - Retrieve data from IIASA scenario databases
- **add_historical_values()** - Add historical data to model projections
- **write_witch_data_csv()** - Export WITCH variables to CSV

### Plotting Functions

#### Energy Plots
- **Primary_Energy_Mix()** - Primary energy mix over time (area/bar plots)
- **Electricity_Mix()** - Electricity generation mix visualization
- **Energy_Trade()** - Fuel trade flows between regions
- **Investment_Plot()** - Energy investment trajectories
- **Power_capacity()** - Power generation capacity evolution

#### Emission Plots
- **Plot_Global_Emissions()** - Global emission trajectories
- **Global_Emissions_Stacked()** - Stacked emissions with carbon budget
- **Intensity_Plot()** - Emission intensity metrics
- **Sectoral_Emissions()** - Emissions by sector
- **Mitigation_Sources()** - Sources of emission reductions
- **Mitigation_Decomposition()** - Decomposition of mitigation effort

#### Climate Plots
- **climate_plot()** - Temperature and climate indicators
- **gridded_temp_map()** - Gridded temperature change maps

#### Policy & Cost Plots
- **Policy_Cost()** - Climate policy costs (GDP loss, consumption)
- **Policy_Cost_Decomposition()** - Decompose policy costs by component
- **Carbon_Price()** - Carbon price trajectories
- **Social_Cost_of_Carbon()** - Social cost of carbon by region
- **SCC_plot()** - Social cost of carbon visualization

#### Inequality Plots
- **plot_inequality()** - Income/consumption inequality (Lorenz, Gini, quantiles)
- **plot_winners_losers_time()** - Winners and losers from policy over time
- **compute_global_inequality()** - Global inequality metrics

#### Maps
- **witchmap()** - Regional maps of model variables
- **countrymap()** - Country-level map visualization
- **map_simple()** - Simple regional mapping
- **plot_map_region_definition()** - Show regional aggregation definitions

#### RICE50+ Specific
- **plot_macc_fit()** - Marginal abatement cost curve fitting

### Utility Functions
- **plot_witch()** - Generic plotting function for WITCH data
- **get_plot_witch()** - Load and plot WITCH variables
- **create_witch_plot_online()** - Generate online plot collections
- **diagnostics_plots()** - Model diagnostics and calibration checks

### Helper Functions
- **ttoyear()** - Convert time index to year
- **yeartot()** - Convert year to time index
- **saveplot()** - Save plots with consistent formatting
- **add_change_from_reference()** - Calculate change from reference scenario
- **make_global_tr()** - Aggregate to global totals
- **make_cumulative()** - Calculate cumulative values over time
- **unit_conversion()** - Convert units with metadata
- **default_meta_param()** - Get default regional aggregation metadata

### Shiny Modules (Internal)
- **extract_additional_sets()** - Extract additional dimensions from GDX data
- **compute_regional_aggregates()** - Compute World/EU aggregates
- **prepare_plot_data()** - Unified data preparation pipeline
- **create_gdx_plot()** - Generate ggplot2 visualizations
- **create_scenario_selector()** - UI component for scenario selection
- **create_variable_selector()** - UI component for variable selection
- **create_region_selector()** - UI component for region selection
- **get_gdx_variable_list()** - Extract available variables from GDX files

## Package Structure

```
witchplot/
├── R/           # Function code (20 files)
├── man/         # Documentation
├── tests/       # Unit tests
├── inst/        # Installed files
│   └── gdxcompaR/  # Shiny apps
├── data/        # Datasets
└── DESCRIPTION, NAMESPACE, LICENSE
```

## Authors

Copyright (c) 2025 Johannes Emmerling and WITCH Team

## License

MIT License
