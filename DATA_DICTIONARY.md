# Data Dictionary

Variable definitions and Bloomberg data sources for the dissertation *Firm-Level Determinants of EV/Sales and EV/EBITDA Valuation Multiples in European Listed TMT Companies* (Felix Wiese, ISEG, 2026).

The Bloomberg fields below are those actually used in the data collection. All monetary fields are retrieved in euros (`Currency=EUR`); accounting fields use annual periodicity (`Per=Y`); market fields are taken at fiscal year-end (Dec 31, with `Days=A` and `Fill=P` to fall back to the previous available trading day).

The raw Bloomberg data are **not** distributed with this repository (Bloomberg licence). This dictionary allows the panel to be reconstructed from a Bloomberg Terminal.

In the frozen workbook (`Data/Data_hardcoded.xlsx`) these items appear as human-readable column headers in euro millions (for example, `Enterprise Value (in Mio. EUR)`, `Revenue / Sales (in Mio. EUR)`); the script maps those headers to the variable names below. The table lists the upstream Bloomberg field each header was retrieved from.

---

## 1. Raw Bloomberg fields

| Item | Bloomberg field | Notes |
|------|-----------------|-------|
| Market capitalisation | `CUR_MKT_CAP` | EUR; also used as an alternative size measure (ln) |
| Enterprise value | `ENTERPRISE_VALUE` | EUR; market cap + total debt − cash and short-term investments |
| Revenue | `SALES_REV_TURN` | EUR |
| EBITDA | `EBITDA` | EUR |
| EBIT | `EBIT` | EUR; used for the EBIT-margin robustness check |
| Total assets | `BS_TOT_ASSET` | EUR |
| Net debt | `NET_DEBT` | EUR; total debt − cash and equivalents |
| Total debt (robustness) | `SHORT_AND_LONG_TERM_DEBT` | EUR; `TOTAL_DEBT` also tested; `BS_TOT_DEBT` returned errors and was not used |
| Cash and near-cash | `BS_CASH_NEAR_CASH_ITEM` | EUR |
| Capital expenditures | `CF_CAP_EXPEND_PRPTY_ADD` | EUR; reported as a cash outflow, absolute value used |
| Net income | `NET_INCOME` | EUR; used for ROA |
| Book equity | `TOT_COMMON_EQY` | EUR; collected, used in auxiliary checks |
| Free cash flow | `CF_FREE_CASH_FLOW` | EUR; collected, used in auxiliary checks |
| R&D expense | `IS_RD_EXPEND` | EUR; missing for ~44% of firm-years |
| Stock volatility | `VOLATILITY_360D` | percent; 360-day price volatility |
| Effective tax rate | `EFF_TAX_RATE` | percent; values above 100% set to missing |

**Depreciation and amortisation** was not consistently available as a direct field and, where needed, was computed as `EBITDA − EBIT` whenever both were available.

**Forward-looking expected growth** (`BEST_SALES` with `BEST_FPERIOD_OVERRIDE = 1FY`) was attempted but **not used**: the relative period override resolves against the current date rather than the historical fiscal year-end, so it returned values for recent years only and would have introduced look-ahead bias for the early panel. This is documented as a data limitation; the trailing three-year sales CAGR is used as a backward-looking alternative instead.

---

## 2. Dependent variables

| Variable | Definition | Construction |
|----------|------------|--------------|
| `ln_EV_Sales` | Natural log of EV/Sales | `ln(ENTERPRISE_VALUE / SALES_REV_TURN)`, computed only when EV > 0 and revenue > 0 |
| `ln_EV_EBITDA` | Natural log of EV/EBITDA | `ln(ENTERPRISE_VALUE / EBITDA)`, computed only when EV > 0 and EBITDA > 0 |

Both multiples are right-skewed and bounded below at zero; the log transformation reduces skew, allows approximate proportional interpretation, and limits the influence of extreme values.

---

## 3. Baseline explanatory variables

| Variable | Definition | Construction |
|----------|------------|--------------|
| `rev_growth` | Year-on-year revenue growth | `Revenue_t / Revenue_{t-1} − 1` (firm-sorted lag) |
| `ebitda_margin` | EBITDA margin | `EBITDA / SALES_REV_TURN` |
| `ebitda_margin_pos` | Profitability slope | EBITDA margin for profitable firms; **set to 0 for loss firms** (Bhojraj & Lee, 2002) |
| `loss_firm` | Loss-firm indicator | `1` if EBITDA < 0, else `0`; **not winsorised** |
| `leverage` | Net leverage | `NET_DEBT / BS_TOT_ASSET` |
| `size` | Firm size | `ln(BS_TOT_ASSET)` |
| `cash_assets` | Cash ratio | `BS_CASH_NEAR_CASH_ITEM / BS_TOT_ASSET` |
| `capex_sales` | Capital intensity | `abs(CF_CAP_EXPEND_PRPTY_ADD) / SALES_REV_TURN` |
| `rd_sales` | R&D intensity | `IS_RD_EXPEND / SALES_REV_TURN` |
| `stock_vol` | Stock-return volatility | `VOLATILITY_360D / 100` (converted to decimal) |

The EBITDA margin enters the EV/Sales models only, through the `ebitda_margin_pos` + `loss_firm` split. It is excluded from the EV/EBITDA models to avoid the mechanical inversion that arises from EBITDA appearing in both the denominator of the dependent variable and the numerator of the regressor.

---

## 4. Extension and robustness variables

| Variable | Definition | Construction |
|----------|------------|--------------|
| `eff_tax_rate` | Effective tax rate | `EFF_TAX_RATE / 100`; values > 100% set to NA, then winsorised |
| `sales_cagr_3y` | Trailing three-year sales CAGR | `(Revenue_t / Revenue_{t-3})^(1/3) − 1` |
| `roa` | Return on assets | `NET_INCOME / BS_TOT_ASSET` |
| `asset_turnover` | Asset turnover | `SALES_REV_TURN / BS_TOT_ASSET` |
| `ebitda_growth` | EBITDA growth | `EBITDA_t / EBITDA_{t-1} − 1` |
| `earnings_stability` | Earnings stability | Rolling three-year standard deviation of `ebitda_margin` within firm (requires three consecutive non-missing observations) |
| `ebit_margin_pos` | EBIT-margin slope (robustness) | `EBIT / SALES_REV_TURN` for profitable firms |
| `leverage_totaldebt` | Total-debt leverage (robustness) | `SHORT_AND_LONG_TERM_DEBT / BS_TOT_ASSET` |
| `size_mktcap` | Size, market-cap basis (robustness) | `ln(CUR_MKT_CAP)` |

Two construction safeguards for `sales_cagr_3y`: lags are taken with `dplyr::lag` (explicitly qualified, because `zoo` also exports `lag` and is loaded later), and a calendar gate requires that the observation three rows back is exactly three calendar years earlier, so firm-year gaps do not produce a spurious CAGR.

`eff_tax_rate` and `sales_cagr_3y` are available for only part of each sample. Each augmented specification is therefore paired with a baseline re-estimated on the identical reduced sample (a "redux" baseline), so the contribution of the variable is separated from the change in sample composition. For the growth comparison, both growth measures are required to be present in the same row, so each pair is estimated on exactly the same observations.

---

## 5. Categorical (fixed-effect) variables

| Variable | Definition |
|----------|------------|
| `subsector` | GICS-based TMT subsector: Technology, Media, or Telecommunications |
| `country` | Country of the firm (17 European countries) |
| `year` | Fiscal year, 2010–2025 |

---

## 6. Sample and processing rules

- **Universe:** assembled with the Bloomberg Equity Screener and augmented with the constituents of the three STOXX Europe Total Market sector indices (Technology, Media and Telecommunications).
- **Security filters:** ordinary/common shares of active listed firms only; ETFs, funds, trusts, preferred shares, warrants, ADRs/GDRs, SPACs, and shell companies excluded; primary listings retained where possible; financial firms excluded.
- **Size filter:** market capitalisation ≥ EUR 50m in at least one year.
- **Positivity:** EV/Sales requires EV > 0 and revenue > 0; EV/EBITDA additionally requires EBITDA > 0.
- **Winsorisation:** all continuous variables winsorised at the 1st and 99th percentiles; the `loss_firm` dummy is not winsorised.
- **Missing values:** complete-case per regression; a firm-year enters a model only if all variables for that model are present.
- **Panel:** unbalanced firm-year panel; firm = Bloomberg ticker (e.g. `SAP GY Equity`, `DSY FP Equity`).

---

## 7. Estimation

- Estimator: within (fixed-effects) OLS across subsector, country, and year (`fixest::feols`).
- Standard errors: clustered at the firm level (Petersen, 2009).
- Robustness: two-way clustering by firm and year (Gow, Ormazabal & Taylor, 2010), reported as a check only given 16 year clusters.
- Firm fixed effects are not included.
- Reported fit measure: within-R² (the fixed effects absorb most of the overall variance).
- VIFs are computed from OLS replicas via `lm()`, because `fixest` does not interface with `car::vif()`.
