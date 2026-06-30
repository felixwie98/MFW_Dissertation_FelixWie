# R Environment

This file documents the software environment used for the analysis. Package versions are left as placeholders: fill them in from your own machine so the record is exact.

## R version

- R version: **fill in** (run `R.version.string`; the analysis was developed on R ≥ 4.2).
- Platform / OS: **fill in** (from `sessionInfo()`).

## Packages used

The analysis relies on the following packages. Record the exact versions you used (see *How to capture your environment* below).

| Package | Role in the analysis |
|---------|----------------------|
| `readxl` | Reading the Bloomberg workbook (`Data/Data_hardcoded.xlsx`) |
| `dplyr` | Data manipulation; `dplyr::lag` for firm-sorted lags (revenue growth, sales CAGR) |
| `tidyr` | Reshaping the panel |
| `zoo` | Rolling-window calculations (earnings stability); its `lag` is masked by qualifying `dplyr::lag` |
| `fixest` | Within (fixed-effects) estimation (`feols`), firm-clustered and two-way clustered standard errors |
| `modelsummary` | Building the regression and descriptive tables (`modelsummary`, `datasummary`); writes the HTML tables in `output/tables/` |

`modelsummary` renders its HTML tables through `tinytable`, which is installed automatically as a dependency. Winsorisation uses a small custom function defined in the script, so no extra package is required. Variance inflation factors are computed from `lm()` replicas of the EV/Sales models (because `fixest` does not interface with `car::vif()`); `car` itself is not loaded.

If you add other packages later, list them here with their versions.

## How to capture your environment

Option A — quick record. After running the scripts, paste the output of:

```r
sessionInfo()
```

into a file named `sessionInfo.txt` in the repository root.

Option B — reproducible lockfile (recommended). Use `renv` to snapshot exact versions:

```r
install.packages("renv")
renv::init()      # run once in the project
renv::snapshot()  # writes renv.lock with all package versions
```

Commit the resulting `renv.lock` to the repository. Anyone can then restore the exact environment with `renv::restore()`.

## Installing the packages

```r
install.packages(c(
  "readxl", "dplyr", "tidyr",
  "zoo", "fixest", "modelsummary"
))
```
