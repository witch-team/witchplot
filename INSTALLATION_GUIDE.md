# witchplot Installation Guide

This guide provides installation instructions for the `witchplot` package and its dependencies.

## Two Installation Methods

### Method 1: Install from Binary Packages (Recommended - No Rtools needed)

This is the **easiest method** and does not require Rtools or compilation.

**Steps:**

1. Download the shared folder containing:
   - `gdxtools_x.x.x.zip`
   - `witchtools_x.x.x.zip`
   - `witchplot_x.x.x.zip`
   - `install_witchplot.R`

2. Open R or RStudio

3. Set your working directory to the folder containing the files:
   ```r
   setwd("path/to/downloaded/folder")
   ```

4. Run the installation script:
   ```r
   source("install_witchplot.R")
   ```

5. Once installation completes, load the package:
   ```r
   library(witchplot)
   run_witch()
   ```

---

### Method 2: Install from GitHub (Requires Rtools on Windows)

This method installs directly from GitHub but **requires Rtools** on Windows because some dependencies need compilation.

**Prerequisites:**
- For Windows users: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) first

**Steps:**

1. Download `install_from_github.R`

2. Open the file and update the `github_repo` variable with the correct GitHub repository path

3. Run the installation script:
   ```r
   source("install_from_github.R")
   ```

4. Once installation completes, load the package:
   ```r
   library(witchplot)
   run_witch()
   ```

---

## Additional Setup for WITCH/RICE Models

The `witchplot` package can visualize two types of data:

1. **IIASA Database** (e.g., AR6, AR5) - **No additional setup needed**
   ```r
   run_iiasadb(iamc_databasename = "iamc15")
   ```

2. **WITCH/RICE GDX files** - **Requires GAMS installation**
   ```r
   run_witch()  # Requires GAMS
   run_rice()   # Requires GAMS
   ```

### Setting up GAMS for GDX files

If you want to work with WITCH/RICE model GDX files, you need GAMS:

1. **Download and install GAMS** from: https://www.gams.com/download/
   - A free demo license is available
   - Note the installation directory (e.g., `C:/GAMS/47`)

2. **Initialize the GDX library in R:**
   ```r
   library(witchplot)
   setup_gdx()  # Auto-detects GAMS installation

   # Or specify GAMS path manually if needed:
   setup_gdx("C:/GAMS/47")
   ```

3. **Verify it works:**
   ```r
   run_witch()  # Should now work!
   ```

**Note**: The package will automatically try to initialize GDX when loaded. If you see errors about "GDX library not loaded", follow the steps above.

---

## Troubleshooting

### GDX Library Errors

If you see:
```
Error: GDX library has not been loaded
```

**Solution:**
1. Install GAMS from https://www.gams.com/download/
2. Run `setup_gdx()` in R
3. See detailed troubleshooting guide at: `TEACHING/TROUBLESHOOTING_GDX.md`

**Alternative**: Use IIASA database viewer (no GAMS needed):
```r
run_iiasadb(iamc_databasename = "iamc15", add_historical = FALSE)
```

### "Package not found" errors
- **Method 1**: Make sure all .zip files are in the same folder as `install_witchplot.R`
- **Method 2**: Check your internet connection and that the GitHub repository path is correct

### "Rtools required" messages
- Use **Method 1** (binary packages) instead, or
- Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/

### Package loading errors
Try restarting R and loading the package again:
```r
# Restart R session, then:
library(witchplot)
```

### Permission denied errors
Close R/RStudio completely and restart, then try installation again.

---

## Package Information

- **witchplot**: Interactive visualization toolkit for GAMS IAM model results
- **Dependencies from GitHub**:
  - `gdxtools` (https://github.com/lolow/gdxtools) - Manipulate GDX files in R
  - `witchtools` (https://github.com/witch-team/witchtools) - Data management for IAMs
- **CRAN dependencies**: Automatically installed (data.table, ggplot2, shiny, etc.)
