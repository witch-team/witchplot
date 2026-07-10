# witchplot — Developer Guide for Claude

## What this repo is

**witchplot** is an R package (~4,200 lines across 23 files) for interactive visualization of GAMS IAM (Integrated Assessment Model) results. It wraps GDX file reading into Shiny dashboards, supporting four models: WITCH, RICE50+, FIDELIO, IIASADB.

Entry points: `run_witch()`, `run_rice()`, `run_fidelio()`, `run_iiasadb()` — all defined in `R/witchplot.R`.

---

## Directory layout

```
R/                          # All package logic (23 files)
inst/gdxcompaR/
  witch/                    # WITCH Shiny app (global.R, ui.R, server.R)
  rice/                     # RICE50+ Shiny app
  fidelio/                  # FIDELIO Shiny app
  iiasadb/                  # IIASA database viewer
  config/                   # CSV variable mapping files
data/                       # Historical GDX files (bundled)
data-raw/                   # Scripts to regenerate historical data
man/                        # Roxygen docs
tests/                      # Minimal (framework only, no real tests)
```

---

## Key R files

| File | Role |
|------|------|
| `witchplot.R` | `run_*()` launchers, global state cleanup |
| `session_init.R` | `.initialize_witchplot_session()` — sets all globals |
| `gdx_loader_new.R` | Modern GDX discovery and session setup |
| `gdx_file_loader.R` | Legacy GDX loader (still used) |
| `get_witch.R` | **Core data loader** `get_witch()` — reads variables from GDX |
| `shiny_modules.R` | **Plot pipeline** — `prepare_plot_data()`, `create_gdx_plot()`, etc. |
| `shiny_ui_helpers.R` | UI generators — `create_variable_selector()`, `create_region_selector()` |
| `auxiliary_functions.R` | `unit_conversion()`, `ttoyear()`, `yeartot()`, `saveplot()` |
| `add_historical_values.R` | Merges historical data into model output |
| `region_palettes.R` | `witch_region_longnames`, `get_region_palette()` |
| `energy_plots.R` | `Primary_Energy_Mix()`, `Electricity_Mix()`, etc. |
| `emission_plots.R` | `Plot_Global_Emissions()`, `Sectoral_Emissions()`, etc. |
| `policy_cost.R` | `Policy_Cost()`, `Carbon_Price()` |
| `inequality_plots.R` | `plot_inequality()`, Gini/Lorenz analysis |
| `map_functions.R` | `witchmap()`, `map_simple()` |

---

## Data flow

```
results_dir/*.gdx
  → .discover_gdx_files() / .load_gdx_files()
      creates: scenlist, witch_regions, all_var_descriptions, year0, tstep
  → run_*() calls .initialize_witchplot_session(), launches Shiny

Shiny server.R:
  user picks variable/scenario/region/index
  → get_witch(variable)          # reads GDX, memoized
      → add_historical_values()  # appends historical rows (file="historical")
  → prepare_plot_data()
      → extract_additional_sets() # finds extra set dimensions (e, j, ghg, …)
      → subset_by_additional_sets()
      → compute_regional_aggregates()
      → unit_conversion()
  → create_gdx_plot()            # returns ggplot object
```

---

## Data structures

All variables are returned as **data.table** with these columns:
- `t` — time index (integer, 1-based); convert with `ttoyear(t)` / `yeartot(year)`
- `n` — region name (lowercase, e.g. `"usa"`, `"china"`, `"World"`)
- `file` — scenario name (or `"historical"` / `"historical_primap"` etc.)
- `value` — the numeric result
- `pathdir` — only present when multiple `results_dir` paths are used
- `tlen` — time-step length in years (loaded from GDX or defaulting to `tstep`)
- additional columns — set dimensions like `e`, `j`, `ghg`, `iq` (model-dependent)

---

## Sets / indexes

Sets are additional dimensions beyond time and region:

| Set | Meaning | Typical values |
|-----|---------|----------------|
| `e` | Emission type (WITCH) | `co2_ffi`, `co2_luc`, `ch4`, `n2o` |
| `ghg` | GHG type (RICE) | `co2`, `ch4`, `n2o` |
| `j` | Technology / fuel | `coal`, `gas`, `wind`, `solar`, `nuclear` |
| `f` | Fuel | used in energy variables |
| `iq` | Income quintile | `q1`–`q5`, `y` |

`extract_additional_sets()` discovers which sets a variable has at runtime. Sets are sorted alphabetically, so when nothing is selected the fallback is `set_elements[1]` (alphabetically first, e.g. `ch4` before `co2`).

---

## Global variables (set by session init)

These are set in `.GlobalEnv` and consumed everywhere:

```
results_dir       # character vector of paths to GDX results
filelist          # named list: path → filename
scenlist          # named list: scenario label → filename
witch_regions     # character vector of region codes
display_regions   # regions shown by default
region_palette    # named color vector
year0, tstep      # time parameters (WITCH: 2005/5, RICE: 2015/5)
yearmin, yearmax  # display range defaults
all_var_descriptions # data.frame(name, description) from GDX metadata
reg_id            # regional aggregation ID ("witch17", "ed58", etc.)
add_historical    # logical: load historical data?
deploy_online     # logical: running on server (disables some features)?
```

Cleaned up by `.cleanup_witchplot_globals()` before each new session.

---

## Shiny apps — WITCH vs RICE differences

| | WITCH (`witch/`) | RICE (`rice/`) |
|--|--|--|
| Default variable | `Q_EMI` | `E` |
| Region aggregates | World, EU | World only |
| Extra UI controls | — | Growth rate, stacked plot |
| Variable list fn | `get_gdx_variable_list()` (filtered) | `get_gdx_variable_list_simple()` |
| Index selector placement | Outside `renderPlot` (has `set_info_reactive`) | Inside `renderPlot` (also has `set_info_reactive` now) |
| Time range | 1970–2150 | 1970–2300 |
| reg_id | `witch17` | `ed58` |

Both apps share the same `shiny_modules.R` pipeline and `shiny_ui_helpers.R` components.

---

## Shiny server.R architecture (both apps)

```
variable_selected_reactive   # reactive wrapping input$variable_selected
set_info_reactive            # reactive: loads variable, extracts set info
output$choose_additional_set # renderUI: index selector (uses set_info_reactive)
output$varname               # renderText: plot title (uses set_info_reactive for accurate fallback)
output$gdxcompaRplot         # renderPlot: main time-series plot
output$gdxcompaRstackedplot  # renderPlot: stacked area plot (RICE)
output$gdxompaRplotly        # renderPlot: secondary plot (ggplotly-compatible)
output$gdxcompaRmap          # renderPlot: geographic map
output$diagnostics           # renderPlot: diagnostic overview
```

**Plot title format**: `VARNAME — Description [index_element] — Region`

---

## Historical data

- Bundled in `data/`: `witch17.gdx`, `ed58.gdx`, `r5.gdx`, `global.gdx`
- Mapped via `inst/config/map_var_hist_rice.csv` and `map_var_hist_iiasadb.csv`
- Historical rows have `file = "historical"` (or `"historical_{source}"`)
- For RICE variable `E` and similar: if no `ghg` column exists in source data, defaults to `"co2"` (see `get_witch.R`)
- `add_historical_values()` is called inside `get_witch()` when `add_historical=TRUE`

---

## Unit conversion

`unit_conversion(variable_name)` returns `list(unit, convert)`:

| GDX unit | Display unit | Factor |
|----------|-------------|--------|
| TWh | EJ | 0.0036 |
| T$ | billion USD | 1000 |
| GtCe | GtCO2 | 3.67 |
| T$/GTon | $/tCO2 | 272.73 |
| °C | °C | 1 |

Growth rate mode overrides unit to `" % p.a."`.

---

## Time handling

```r
ttoyear(t)        # t (1-based integer) → calendar year
yeartot(year)     # calendar year → t
# Uses globals: year0, tstep (or tlen column in data)
```

WITCH: `year0=2005`, `tstep=5` → t=1 is 2005, t=2 is 2010, …
RICE:  `year0=2015`, `tstep=5` → t=1 is 2015, t=2 is 2020, …

---

## Naming conventions

- **Function names**: `CamelCase` for plot functions (`Primary_Energy_Mix`), `snake_case` for utilities (`get_witch`, `unit_conversion`)
- **Internal helpers**: `.dot_prefix` (e.g. `.load_gdx_files`, `.initialize_witchplot_session`)
- **GDX files**: discovered by pattern `restrict_files="results_"` (default)
- **Scenario names**: derived by stripping `removepattern` from filename
- **Region codes**: lowercase (`usa`, `china`, `europe`); long names via `witch_region_longnames`

---

## Common patterns to follow

1. **Adding a new plot tab**: add `renderPlot` in server.R + `tabPanel` in ui.R; use `prepare_plot_data()` for data prep.
2. **Accessing set info in server.R**: always use `set_info_reactive()` — never compute `extract_additional_sets()` inside a reactive that also computes plot data.
3. **Effective index selection**: `input$additional_set_id_selected` can be NULL when nothing selected; always apply the same fallback as the plot (`set_info$set_elements[1]`).
4. **New variable special-casing**: add handling in `get_witch.R` near the `E`/`EIND` block if the variable needs a default index column added.
5. **Historical mapping**: add a row in `inst/config/map_var_hist_rice.csv` (or iiasadb variant).


## Important to note
We are currently focussing on RICE, not WITCH. 