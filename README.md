# Firm-Level Determinants of EV/Sales and EV/EBITDA Multiples in European Listed TMT Companies

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21063789.svg)](https://doi.org/10.5281/zenodo.21063789)


Replication materials for the Master in Finance dissertation:

> **Firm-Level Determinants of EV/Sales and EV/EBITDA Valuation Multiples in European Listed Technology, Media and Telecommunications Companies**
> Felix Wiese — ISEG Lisbon School of Economics & Management, Universidade de Lisboa, 2026.
> Supervisor: Pedro Rino Vieira.

This repository contains the R code, the data dictionary, and the derived regression outputs used in the empirical analysis. It does **not** contain the underlying firm-level data, which are proprietary to Bloomberg (see *Data availability* below).

---

## Overview

The study analyses which firm-level characteristics explain the EV/Sales and EV/EBITDA valuation multiples of European listed Technology, Media and Telecommunications (TMT) companies. It uses an unbalanced firm-year panel covering 2010 to 2025 and estimates within-panel regressions of the natural logarithms of EV/Sales and EV/EBITDA on profitability, revenue growth, risk, and intangible-investment proxies, together with subsector, country, and year fixed effects and firm-clustered standard errors.

A central feature is the identity that links the two multiples, EV/Sales = EBITDA margin × EV/EBITDA, which is used to decompose the EBITDA-margin association into a mechanical and a residual economic component.

## Citation

If you use this material, please cite the dissertation and the archived release:

> Wiese, F. (2026). *Firm-Level Determinants of EV/Sales and EV/EBITDA Valuation Multiples in European Listed Technology, Media and Telecommunications Companies* [Replication materials]. Zenodo. https://doi.org/10.5281/zenodo.21063789

---

## Data source and sample construction

The firm-level financial and market data are obtained from Bloomberg (Bloomberg L.P., accessed 2026) via the Bloomberg Excel Add-In.

The sample is **assembled using the Bloomberg Equity Screener and augmented with the constituents of the three STOXX Europe Total Market sector indices** for Technology, Media and Telecommunications. The Equity Screener defines the investable European TMT universe under transparent selection criteria, and the three STOXX Europe Total Market sector indices are added to ensure complete coverage of the large, index-eligible constituents in each subsector.

Firms are assigned to the TMT universe using the Global Industry Classification Standard: Technology is the Information Technology sector; Media is the Communication Services sector within the Media and Entertainment industry group; Telecommunications is the Communication Services sector within the Telecommunication Services industry group. Financial firms are excluded.

The estimation sample is obtained from a sequential funnel:

| Step | Filter | Firm-years | Firms |
|------|--------|-----------:|------:|
| 0 | Raw panel | 10,304 | 644 |
| 1 | TMT subsectors only | 8,400 | 525 |
| 2 | Revenue > 0 | 6,575 | 524 |
| 3 | Market capitalisation ≥ EUR 50m (in at least one year) | 4,884 | 359 |
| 4 | Positive EV/Sales (for the log transformation) | 4,300 | 356 |
| 5 | Positive EBITDA (for the log transformation) | 3,665 | 339 |

After dropping observations with missing explanatory variables, the baseline EV/Sales model is estimated on 3,851 firm-years and the baseline EV/EBITDA model on 3,322 firm-years. Specifications that add R&D intensity use smaller samples because R&D is missing for roughly 44 percent of firm-years.

All monetary figures are retrieved in euros. Market variables are taken as of fiscal year-end. Full variable definitions and the exact Bloomberg fields are in [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md).

---

## Data availability

The underlying firm-level panel is sourced from Bloomberg and **cannot be redistributed**, because the Bloomberg licence prohibits republishing raw data. This repository therefore publishes only:

- the **R code** that constructs the variables and runs the analysis,
- the **data dictionary** mapping each variable to its Bloomberg field ([`DATA_DICTIONARY.md`](DATA_DICTIONARY.md)),
- the **derived regression outputs** (the result tables in `output/tables/`).

The raw data can be reconstructed from a Bloomberg Terminal by retrieving the fields listed in the data dictionary for the sample defined above. The raw Bloomberg workbook is excluded from version control via [`.gitignore`](.gitignore).

Suggested availability statement for the thesis:

> The R code, a data dictionary, and the derived regression outputs are available at https://doi.org/10.5281/zenodo.21063789. The underlying firm-level data are proprietary and available from Bloomberg.

---

## Repository structure

```
.
├── README.md                 # this file
├── LICENSE                   # MIT licence (code)
├── DATA_DICTIONARY.md        # variable definitions and Bloomberg fields
├── R_ENVIRONMENT.md          # R version and package list
├── .gitignore                # excludes the raw Bloomberg workbook
├── R/
│   └── 01_panel_construction_and_baseline_regressions.R
│                             # self-contained analysis script (see below)
├── Data/
│   └── Data_hardcoded.xlsx   # PLACE the Bloomberg workbook here (git-ignored, not included)
└── output/
    └── tables/               # derived result tables written by the script (HTML)
```

The analysis is contained in a single script, `01_panel_construction_and_baseline_regressions.R`, which runs end to end. Its internal sections are:

| Sections | Step |
|----------|------|
| 0–7 | Configuration, sheet detection, loading the workbook (Yearly Financials, Market Data, Company Information), panel build, GICS-based TMT classification |
| 8–11 | Variable construction, sample funnel, winsorisation, R&D sub-samples |
| 12–14 | Descriptive statistics and baseline regressions (`table_main_regressions`) |
| 16–17 | Subsector heterogeneity; descriptive, correlation, and VIF tables |
| 18 | Effective-tax-rate and trailing three-year sales CAGR extensions |
| 20 | Identity-based decomposition of the EBITDA-margin effect |

It reads the workbook from `Data/Data_hardcoded.xlsx` and writes all result tables to `output/tables/` (the script creates that folder). Run it from the repository root so these relative paths resolve. If you split the analysis into additional scripts later, add them to the `R/` listing above.

---

## How to reproduce

1. **Obtain the raw data.** On a Bloomberg Terminal, build the sample as described under *Data source and sample construction* and retrieve the fields listed in [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md) for fiscal years 2010–2025. Paste the Bloomberg formulas as values to freeze the data, and arrange them as the workbook the script expects: yearly financial sheets, market-data (volatility) sheets, and a company-information sheet, with the column headers given in the script's configuration block.
2. **Place the workbook** at `Data/Data_hardcoded.xlsx` (this folder is git-ignored).
3. **Install the R packages** listed in [`R_ENVIRONMENT.md`](R_ENVIRONMENT.md).
4. **Run the script** `R/01_panel_construction_and_baseline_regressions.R` from the repository root. It builds the panel, runs every model, and writes the result tables to `output/tables/`.

The models are estimated with the within (fixed-effects) estimator across subsector, country, and year, with standard errors clustered at the firm level. Firm fixed effects are deliberately not included, since they would absorb the cross-sectional firm-level variation the analysis seeks to explain. Two-way clustering by firm and year is reported as a robustness check only, given the small number of year clusters.

---

## Software environment

The analysis was run in R. The exact version and package versions are documented in [`R_ENVIRONMENT.md`](R_ENVIRONMENT.md). To capture your own environment, run `sessionInfo()` after executing the scripts, or use `renv::snapshot()` to generate a lockfile.

---

## License

The code in this repository is released under the MIT License (see [`LICENSE`](LICENSE)). The MIT License covers the code only; it does not grant any rights to the underlying Bloomberg data.

---

## Author

Felix Wiese — Master in Finance, ISEG Lisbon School of Economics & Management, Universidade de Lisboa.

I thank my supervisor, Pedro Rino Vieira, for guidance and feedback throughout this dissertation.
