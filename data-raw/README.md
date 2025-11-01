# Data-Raw Directory

This directory contains source data files and scripts for generating package data.

## Organization

```
data-raw/
├── data_historical_values.gdx       # Source file (NOT in git, NOT released)
├── generate_historical_data.R       # Script to process source file
└── README.md                        # This file
```

## Source File: `data_historical_values.gdx`

**Location**: `data-raw/data_historical_values.gdx`

**Status**:
- ❌ Not tracked in git (see `.gitignore`)
- ❌ Not released with package
- ✅ Contains full historical data (all regions, all variables)

**Purpose**: Master source file with complete historical data that gets processed into region-specific files.

## Processing Script: `generate_historical_data.R`

**Purpose**: Converts the source GDX file into region-specific files for package distribution.

**What it does**:
1. Reads `data-raw/data_historical_values.gdx`
2. Converts to multiple region mappings (witch17, witch20, ed58, etc.)
3. Saves processed files to `data/` directory:
   - `data/data_historical_values_witch17.gdx`
   - `data/data_historical_values_witch20.gdx`
   - `data/data_historical_values_ed58.gdx`
   - etc.
4. Extracts set dependencies and saves to:
   - `data/historical_data_set_dependencies.csv`
   - `data/historical_data_set_dependencies_summary.csv`

**Usage**:
```r
# From package root directory:
source("data-raw/generate_historical_data.R")
```

## Generated Files in `data/` Directory

These files ARE released with the package:

- `data_historical_values_{reg_id}.gdx` - Region-specific historical data
- `historical_data_set_dependencies.csv` - Full set dependency details
- `historical_data_set_dependencies_summary.csv` - Used by package functions

## Workflow

### Initial Setup
1. Place source file: `data-raw/data_historical_values.gdx`
2. Run: `source("data-raw/generate_historical_data.R")`
3. Commit generated files in `data/` to git
4. DO NOT commit source file (automatically ignored)

### Updating Historical Data
1. Update source file: `data-raw/data_historical_values.gdx`
2. Run: `source("data-raw/generate_historical_data.R")`
3. Review changes in `data/` directory
4. Commit updated files in `data/`

### Package Build
The `data/` directory files are automatically included in the package build.
The `data-raw/` source file is excluded.

## Why This Organization?

**Separation of concerns**:
- `data-raw/` = Private source data (large, complete, not released)
- `data/` = Processed data (optimized, regional, released with package)

**Benefits**:
- Smaller package size (only necessary regions)
- User privacy (full data not distributed)
- Reproducibility (script documents processing)
- Flexibility (easy to add new regions)

## Git Tracking

```
data-raw/
├── data_historical_values.gdx       # ❌ NOT in git (.gitignore)
├── generate_historical_data.R       # ✅ IN git
└── README.md                        # ✅ IN git

data/
├── data_historical_values_*.gdx     # ✅ IN git (processed files)
└── historical_data_set_dependencies*.csv # ✅ IN git
```

## Notes

- The source file must be manually provided/updated
- Region mappings processed are defined in `generate_historical_data.R`
- Set dependencies are automatically extracted during processing
- Processed files are ready for package distribution
