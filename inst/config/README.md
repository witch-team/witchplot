# Configuration Files

This directory contains configuration files for mapping model variables to historical data sources.

## Variable-Historical Data Mapping Files

### map_var_hist_rice.csv
Maps RICE50+ model variables to historical data sources and unit conversions.

### map_var_hist_iiasadb.csv
Maps IAMC variables to WITCH variables and historical data sources.

## File Format

Each CSV file should have the following columns:

- `varname_model`: Variable name in the model/IAMC format
- `set_model`: Set name in model (if applicable)
- `element_model`: Element name in model (if applicable)
- `var_witch`: Corresponding WITCH variable name
- `set_witch`: Set name in WITCH (if applicable)
- `element_witch`: Element name in WITCH
- `conv`: Conversion factor (R expression, e.g., "44/12" for C to CO2)

## Customizing Mappings

To customize the variable mappings for your model:

1. **For RICE50+**: Edit `map_var_hist_rice.csv` or provide your own data.frame to `run_rice(map_var_hist=...)`

2. **For IIASADB**: Edit `map_var_hist_iiasadb.csv` or provide your own to `run_iiasadb(map_var_hist=...)`

### Example: Adding a new variable mapping

```csv
varname_model,set_model,element_model,var_witch,set_witch,element_witch,conv
Temperature,,,TATM,,,1
GDP,,,Q,iq,y,1e3
```

### Example: Using custom mapping in R

```r
# Create custom mapping
my_mapping <- data.frame(
  varname_model = c("GDP", "Population"),
  set_model = c("", ""),
  element_model = c("", ""),
  var_witch = c("Q", "l"),
  set_witch = c("iq", ""),
  element_witch = c("y", ""),
  conv = c("1e3", "1")
)

# Evaluate conversion expressions
library(dplyr)
my_mapping <- my_mapping %>%
  rowwise() %>%
  mutate(conv = eval(parse(text=conv)))

# Use in run function
run_rice(map_var_hist = my_mapping)
```

## Notes

- Empty cells in CSV should be left blank (no quotes or spaces)
- The `conv` column contains R expressions that are evaluated at runtime
- Common conversions:
  - Carbon to CO2: `44/12`
  - PJ to EJ: `0.001`
  - Million to units: `1e6`
