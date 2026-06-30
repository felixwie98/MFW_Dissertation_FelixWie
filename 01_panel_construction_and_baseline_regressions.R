# ==============================================================================
# 01_panel_construction_and_baseline_regressions.R
# Thesis: Firm-level Determinants of EV/Sales and EV/EBITDA Multiples
#         in European Listed TMT Companies
# Author: Felix Wiese | ISEG Lisbon | 2026
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(zoo)
  library(fixest)
  library(modelsummary)
})

# Version-robust winsorize — no external dependency
winsorize <- function(x, p_low = 0.01, p_high = 0.99) {
  q <- quantile(x, probs = c(p_low, p_high), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

# ==============================================================================
# 0. Configuration
# ==============================================================================

DATA_PATH <- "Data/Data_hardcoded.xlsx"
YEARS     <- 2010:2025

# Column names exactly as they appear in the Yearly Financials sheets.
FIN_COL <- c(
  ticker       = "Ticker",
  mkt_cap      = "Market Capitalization (in Mio. EUR)",
  ev           = "Enterprise Value (in Mio. EUR)",
  revenue      = "Revenue / Sales (in Mio. EUR)",
  ebitda       = "EBITDA (in Mio. EUR)",
  ebit         = "EBIT (in Mio. EUR)",
  total_assets = "Total Assets (in Mio. EUR)",
  total_debt   = "Total Debt (in Mio. EUR)",
  net_debt     = "Net Debt (in Mio. EUR)",
  cash         = "Cash and Cash Equivalents (in Mio. EUR)",
  capex        = "Capital Expenditures / Capex (in Mio. EUR)",
  net_income   = "Net Income (in Mio. EUR)",
  book_equity  = "Book Equity (in Mio. EUR)",
  fcf          = "Free Cash Flow (in Mio. EUR)",
  rd_exp       = "R&D Expense (in Mio. EUR)",
  da           = "Depreciation & Amortization (in Mio. EUR)"
)

# Column names exactly as they appear in the MD (Market Data) sheets.
MD_COL <- c(
  ticker     = "Ticker",
  volatility = "Stock Volatility (360D)"
)
# "Fiscal Year" is already in MD sheets — renamed below via rename().

# Column names in the Company Information sheet.
CI_COL <- c(
  ticker         = "Ticker",
  company        = "Company Name",
  country        = "Country of Domicile",
  sector         = "Sector",
  industry_group = "Industry Group",
  subsector_raw  = "Subsector / Industry"
)

# ==============================================================================
# 1. Detect sheets
# ==============================================================================

all_sheets <- excel_sheets(DATA_PATH)
cat("Sheets in workbook:\n  ", paste(all_sheets, collapse=" | "), "\n\n")

fin_sheets <- sort(all_sheets[all_sheets %in% as.character(YEARS)])
md_sheets  <- sort(all_sheets[grepl("^MD\\s*\\d{4}$", all_sheets, ignore.case = TRUE)])
ci_sheet   <- all_sheets[grepl("company information", all_sheets, ignore.case = TRUE)][1]

cat("Financial sheets  :", paste(fin_sheets, collapse=", "), "\n")
cat("Market data sheets:", paste(md_sheets,  collapse=", "), "\n")
cat("Company info sheet:", ci_sheet, "\n\n")

stopifnot("No financial sheets found"    = length(fin_sheets) > 0)
stopifnot("No market data sheets found"  = length(md_sheets)  > 0)
stopifnot("Company Information not found"= !is.na(ci_sheet))

# ==============================================================================
# 2. Helper: read sheet robustly
#    - reads everything as text to avoid bind_rows type conflicts
#    - strips trailing auto-named columns (readxl names them "...N")
#    - coerces all non-ticker/non-text columns to numeric
# ==============================================================================

read_sheet_robust <- function(sheet_name, text_cols = "Ticker") {
  df <- read_excel(
    DATA_PATH, sheet = sheet_name,
    col_types = "text",
    na = c("", "NA", "N/A", "#N/A", "#VALUE!", "—", "N.A.", "-")
  )
  # Drop trailing empty columns auto-named by readxl (e.g. "...18", "...19")
  df <- df %>% select(-matches("^\\.{3}\\d+$"))
  # Coerce all columns except designated text columns to numeric
  df %>% mutate(across(-all_of(text_cols[text_cols %in% names(.)]),
                        ~ suppressWarnings(as.numeric(.x))))
}

# ==============================================================================
# 3. Load Company Information (static — no year dimension)
# ==============================================================================

ci_raw <- read_excel(DATA_PATH, sheet = ci_sheet,
                     na = c("", "NA", "N/A", "#N/A", "—"))
# Drop trailing empty columns
ci_raw <- ci_raw %>% select(-matches("^\\.{3}\\d+$"))

cat("Company Information columns:\n  ", paste(names(ci_raw), collapse="\n   "), "\n\n")

ci <- ci_raw %>%
  rename(any_of(setNames(CI_COL, names(CI_COL)))) %>%
  select(any_of(names(CI_COL)))

cat("Company info loaded:", nrow(ci), "firms\n\n")

# ==============================================================================
# 4. Load and bind Yearly Financials
#    fiscal_year is taken from the sheet name (more reliable than the column)
# ==============================================================================

load_fin_sheet <- function(yr_str) {
  df <- read_sheet_robust(yr_str, text_cols = "Ticker")
  df$fiscal_year <- as.integer(yr_str)
  # Drop the in-sheet "Fiscal Year" column to avoid duplication
  df %>% select(-any_of("Fiscal Year"))
}

fin_raw <- bind_rows(lapply(fin_sheets, load_fin_sheet))

cat("Yearly Financials columns:\n  ", paste(names(fin_raw), collapse="\n   "), "\n\n")
cat("Financials loaded:", nrow(fin_raw), "rows |",
    n_distinct(fin_raw$Ticker), "unique tickers\n\n")

# Detect the effective tax-rate column case-insensitively (Bloomberg header
# "Tax Rate", delivered in percent). Add it to FIN_COL so it is renamed to
# eff_tax_rate_raw alongside the other financial fields. Fail loudly with the
# available column names if no candidate is found, rather than guessing.
tax_candidates <- names(fin_raw)[grepl("tax", names(fin_raw), ignore.case = TRUE)]
if (length(tax_candidates) == 0) {
  stop("Could not find an effective tax-rate column (no header contains 'tax'). ",
       "Available columns:\n  ", paste(names(fin_raw), collapse = "\n  "))
}
# Prefer an exact 'Tax Rate' header when several columns contain 'tax'.
tax_exact   <- tax_candidates[grepl("^\\s*tax\\s*rate\\s*$", tax_candidates,
                                    ignore.case = TRUE)]
tax_raw_col <- if (length(tax_exact) > 0) tax_exact[1] else tax_candidates[1]
cat("Detected effective tax-rate column:", shQuote(tax_raw_col), "\n\n")
FIN_COL <- c(FIN_COL, eff_tax_rate_raw = tax_raw_col)

fin <- fin_raw %>%
  rename(any_of(setNames(FIN_COL, names(FIN_COL)))) %>%
  rename(fiscal_year = fiscal_year)   # already named correctly

# ==============================================================================
# 5. Load and bind Market Data (MD) sheets
#    "Fiscal Year" column is used directly as fiscal_year
# ==============================================================================

load_md_sheet <- function(sh) {
  df <- read_sheet_robust(sh, text_cols = "Ticker")
  df %>% rename(fiscal_year = `Fiscal Year`)
}

md_raw <- bind_rows(lapply(md_sheets, load_md_sheet))

cat("Market Data columns:\n  ", paste(names(md_raw), collapse="\n   "), "\n\n")
cat("Market Data loaded:", nrow(md_raw), "rows\n\n")

md <- md_raw %>%
  rename(any_of(setNames(MD_COL, names(MD_COL))))

# ==============================================================================
# 6. Build panel: Yearly Financials + MD volatility + Company Information
# ==============================================================================

panel <- fin %>%
  left_join(
    md %>% select(any_of(c("ticker", "Ticker", "fiscal_year", "volatility"))),
    by = c("ticker" = if ("ticker" %in% names(md)) "ticker" else "Ticker",
           "fiscal_year")
  ) %>%
  left_join(ci, by = c("ticker" = if ("ticker" %in% names(ci)) "ticker" else "Ticker"))

# If ticker column retained its original capitalisation in fin, normalise
if (!"ticker" %in% names(panel) && "Ticker" %in% names(panel))
  panel <- rename(panel, ticker = Ticker)

cat("Panel after merge:", nrow(panel), "firm-years |",
    n_distinct(panel$ticker), "firms\n\n")

# Confirm key variables are present
key_vars <- c("ticker", "fiscal_year", "ev", "mkt_cap", "revenue", "ebitda",
              "net_debt", "total_assets", "cash", "capex", "volatility",
              "sector", "industry_group", "country")
missing_key <- setdiff(key_vars, names(panel))
if (length(missing_key) > 0)
  warning("Key variables missing after merge — check column names: ",
          paste(missing_key, collapse=", "))

# ==============================================================================
# 7. TMT classification and subsector assignment
# ==============================================================================
# IT sector: all industry groups qualify → subsector = Technology
# Communication Services: only Media & Entertainment and Telecom Services
# All other Communication Services (e.g. Interactive Media) → drop

panel <- panel %>%
  mutate(
    is_tmt = case_when(
      sector == "Information Technology"                                     ~ TRUE,
      sector == "Communication Services" &
        industry_group %in% c("Media & Entertainment",
                              "Telecommunication Services")                  ~ TRUE,
      TRUE                                                                   ~ FALSE
    ),
    subsector = case_when(
      sector == "Information Technology"              ~ "Technology",
      industry_group == "Media & Entertainment"       ~ "Media",
      industry_group == "Telecommunication Services"  ~ "Telecom",
      TRUE                                            ~ NA_character_
    )
  )

cat("=== TMT classification ===\n")
panel %>%
  count(sector, industry_group, is_tmt) %>%
  arrange(desc(is_tmt), sector, industry_group) %>%
  print(n = 40)
cat("\n")

# ==============================================================================
# 8. Compute all regression variables
# ==============================================================================

panel <- panel %>%
  arrange(ticker, fiscal_year) %>%
  group_by(ticker) %>%
  mutate(
    # Revenue growth: (Rev_t / Rev_{t-1}) - 1; requires prior-year observation
    # dplyr::lag() explicit: zoo is loaded after dplyr (see CLAUDE.md).
    rev_growth = revenue / dplyr::lag(revenue) - 1,
    # Trailing 3-year sales CAGR: (Rev_t / Rev_{t-3})^(1/3) - 1. Computed in the
    # SAME grouped pass as rev_growth and BEFORE the sample funnel. Requires a
    # non-missing, strictly positive revenue three years prior; the base is
    # guarded against non-positive values so revenue <= 0 (or a missing prior
    # observation) yields NA. A defensive calendar gate keeps it correct should
    # a future point-in-time panel introduce year gaps.
    rev_l3        = dplyr::lag(revenue, 3),
    yr_l3         = dplyr::lag(fiscal_year, 3),
    sales_cagr_3y = ifelse(
      !is.na(revenue) & !is.na(rev_l3) & revenue > 0 & rev_l3 > 0 &
        (fiscal_year - yr_l3 == 3),
      (revenue / rev_l3)^(1 / 3) - 1,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    ebitda_margin     = ebitda / revenue,
    # ebitda_margin_pos: used in Models 1/1b to isolate profitability effect
    # within profitable firms; set to 0 for loss firms so the continuous
    # variation of negative margins does not mechanically drive the coefficient.
    # loss_firm dummy captures the level shift (Bhojraj & Lee 2002).
    ebitda_margin_pos = ifelse(!is.na(ebitda) & ebitda > 0, ebitda_margin, 0),
    loss_firm         = as.integer(!is.na(ebitda) & ebitda < 0),
    leverage      = net_debt / total_assets,
    size          = log(total_assets),
    cash_assets   = cash / total_assets,
    # Bloomberg Capex is typically reported as negative (cash outflow)
    capex_sales   = abs(capex) / revenue,
    rd_sales      = rd_exp / revenue,
    # Bloomberg VOLATILITY_360D is in percent (e.g. 32.5 = 32.5%); divide by 100
    stock_vol     = volatility / 100,
    # Effective tax rate = Bloomberg "Tax Rate" / 100. Raw values above 100%
    # (> 1 after scaling) are noise from tiny pretax income -> set to NA.
    eff_tax_rate  = ifelse(!is.na(eff_tax_rate_raw) & eff_tax_rate_raw / 100 > 1,
                           NA_real_, eff_tax_rate_raw / 100),
    # Extended EV/EBITDA determinants (Models 2c / 2cb)
    roa           = net_income / total_assets,
    asset_turnover = revenue   / total_assets,
    ev_sales      = ev / revenue,
    ev_ebitda     = ifelse(!is.na(ebitda) & ebitda > 0, ev / ebitda, NA_real_),
    log_ev_sales  = ifelse(!is.na(ev_sales)  & ev_sales  > 0, log(ev_sales),  NA_real_),
    log_ev_ebitda = ifelse(!is.na(ev_ebitda) & ev_ebitda > 0, log(ev_ebitda), NA_real_)
  )

# Rolling variables for Models 2d / 2db (Harbula 2009)
# Computed in a separate grouped pass because ebitda_margin must exist first.
# ebitda_growth: year-on-year EBITDA growth, only when both years have EBITDA > 0.
# earnings_stability: rolling 3-obs SD of EBITDA margin per firm;
#   uses observation index (not calendar time) so panel gaps are tolerated.
#   Returns NA unless all 3 observations in the window are non-missing.
panel <- panel %>%
  arrange(ticker, fiscal_year) %>%
  group_by(ticker) %>%
  mutate(
    ebitda_growth = ifelse(
      !is.na(ebitda) & !is.na(dplyr::lag(ebitda)) &
        dplyr::lag(ebitda) > 0 & ebitda > 0,
      ebitda / dplyr::lag(ebitda) - 1,
      NA_real_
    ),
    earnings_stability = zoo::rollapply(
      ebitda_margin, width = 3, align = "right", fill = NA,
      FUN = function(x) if (sum(!is.na(x)) >= 3) sd(x, na.rm = TRUE) else NA_real_
    )
  ) %>%
  ungroup()

# Verify stock_vol scale — median should be ~0.25–0.45 after /100
cat("Stock volatility scale check (raw Bloomberg percent vs. decimal after /100):\n")
cat("  Raw quantiles : ")
cat(paste(round(quantile(panel$volatility, c(0.05,0.25,0.5,0.75,0.95), na.rm=TRUE), 1),
          collapse=" | "), "\n")
cat("  /100 quantiles: ")
cat(paste(round(quantile(panel$stock_vol,  c(0.05,0.25,0.5,0.75,0.95), na.rm=TRUE), 4),
          collapse=" | "), "\n\n")

# Loss-firm share
cat("Loss-firm firm-years:", sum(panel$loss_firm, na.rm=TRUE),
    "(", round(mean(panel$loss_firm, na.rm=TRUE)*100, 1), "% of panel)\n\n")

# ==============================================================================
# 9. Sample funnel
# ==============================================================================

funnel <- data.frame(step=character(), firm_years=integer(), firms=integer(),
                     stringsAsFactors=FALSE)

log_step <- function(df, label) {
  funnel <<- rbind(funnel, data.frame(step=label,
                                      firm_years=nrow(df),
                                      firms=n_distinct(df$ticker),
                                      stringsAsFactors=FALSE))
  invisible(df)
}

panel %>% log_step("0. Full raw panel")

tmt <- panel %>% filter(is_tmt) %>% log_step("1. TMT only")
tmt <- tmt  %>% filter(!is.na(revenue) & revenue > 0) %>% log_step("2. Revenue > 0")

firms_50m <- tmt %>%
  group_by(ticker) %>%
  summarise(ever_50m = any(!is.na(mkt_cap) & mkt_cap >= 50, na.rm=TRUE),
            .groups="drop") %>%
  filter(ever_50m) %>% pull(ticker)
tmt <- tmt %>% filter(ticker %in% firms_50m) %>%
  log_step("3. MktCap >= EUR 50M (ever)")

df_m1 <- tmt %>% filter(!is.na(log_ev_sales))  %>% log_step("4. EV/Sales > 0")
df_m2 <- tmt %>% filter(!is.na(log_ev_ebitda)) %>% log_step("5. EBITDA > 0 & EV > 0")

cat("=== Sample Funnel ===\n")
print(funnel, row.names=FALSE)
cat("\n")

# ==============================================================================
# 10. Winsorize at 1st / 99th percentile
# ==============================================================================

CONT_VARS <- c("log_ev_sales", "log_ev_ebitda",
               "rev_growth", "ebitda_margin", "ebitda_margin_pos",
               "leverage", "size", "cash_assets", "capex_sales",
               "stock_vol", "rd_sales", "roa", "asset_turnover",
               "ebitda_growth", "earnings_stability",
               "eff_tax_rate", "sales_cagr_3y")
# loss_firm is binary — not winsorized

wins <- function(x) winsorize(x, p_low=0.01, p_high=0.99)

df_m1_raw <- df_m1   # pre-winsorize copy — used by sensitivity column (H)

df_m1 <- df_m1 %>% mutate(across(any_of(CONT_VARS), wins))
df_m2 <- df_m2 %>% mutate(across(any_of(CONT_VARS), wins))

cat("After winsorizing:\n")
cat("  Model 1 (EV/Sales): ", nrow(df_m1), "firm-years |",
    n_distinct(df_m1$ticker), "firms\n")
cat("  Model 2 (EV/EBITDA):", nrow(df_m2), "firm-years |",
    n_distinct(df_m2$ticker), "firms\n\n")

# ==============================================================================
# 11. R&D sub-samples (Models 1b / 2b)
# ==============================================================================

df_m1b <- df_m1 %>% filter(!is.na(rd_sales))
df_m2b <- df_m2 %>% filter(!is.na(rd_sales))

cat("R&D sub-samples:\n")
cat("  Model 1b (EV/Sales  + R&D):", nrow(df_m1b), "firm-years |",
    n_distinct(df_m1b$ticker), "firms\n")
cat("  Model 2b (EV/EBITDA + R&D):", nrow(df_m2b), "firm-years |",
    n_distinct(df_m2b$ticker), "firms\n\n")

# ==============================================================================
# 12. Descriptive statistics
# ==============================================================================

VARS_DESC <- c("log_ev_sales", "log_ev_ebitda",
               "rev_growth", "ebitda_margin", "ebitda_margin_pos", "loss_firm",
               "leverage", "size", "cash_assets", "capex_sales",
               "stock_vol", "rd_sales")

cat("=== Descriptive Statistics — Model 1 sample (post-winsor) ===\n")
df_m1 %>%
  select(any_of(VARS_DESC)) %>%
  summarise(across(everything(), list(
    n    = ~ sum(!is.na(.)),
    mean = ~ mean(., na.rm=TRUE),
    sd   = ~ sd(.,   na.rm=TRUE),
    p25  = ~ quantile(., 0.25, na.rm=TRUE),
    p50  = ~ median(., na.rm=TRUE),
    p75  = ~ quantile(., 0.75, na.rm=TRUE)
  ), .names="{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to  = c("variable", ".value"),
               names_sep = "__") %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(n=30)
cat("\n")

# ==============================================================================
# 13. Baseline regressions (fixest, firm-clustered SE)
# ==============================================================================

# Models 1/1b (EV/Sales): ebitda_margin_pos + loss_firm. EBITDA Margin is
# theoretically valid here via the identity EV/Sales = EBITDA Margin × EV/EBITDA.
# Models 2/2b (EV/EBITDA): EBITDA Margin omitted. Including it would create a
# mechanical inversion because EBITDA appears in both the dependent variable's
# denominator and the regressor's numerator (Damodaran; Kumar #17).
BASE_RHS_M1 <- "rev_growth + ebitda_margin_pos + loss_firm + leverage + size +
                cash_assets + capex_sales + stock_vol"
BASE_RHS_M2 <- "rev_growth + leverage + size +
                cash_assets + capex_sales + stock_vol"
EXT_RHS_M1  <- paste(BASE_RHS_M1, "+ rd_sales")
EXT_RHS_M2  <- paste(BASE_RHS_M2, "+ rd_sales")
FE          <- "subsector + country + fiscal_year"

cat("Estimating main models...\n")

m1  <- feols(as.formula(paste("log_ev_sales  ~", BASE_RHS_M1, "|", FE)),
             data=df_m1,  cluster=~ticker)

m2  <- feols(as.formula(paste("log_ev_ebitda ~", BASE_RHS_M2, "|", FE)),
             data=df_m2,  cluster=~ticker)

m1b <- feols(as.formula(paste("log_ev_sales  ~", EXT_RHS_M1,  "|", FE)),
             data=df_m1b, cluster=~ticker)

m2b <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2,  "|", FE)),
             data=df_m2b, cluster=~ticker)

# ---- Sanity check: the main models must be untouched by the extension edits --
# eff_tax_rate / sales_cagr_3y were added to the pipeline but enter no main
# model, so M1 must be numerically identical to the pre-edit baseline.
m1_marg_chk <- unname(coef(m1)["ebitda_margin_pos"])
m1_r2_chk   <- fixest::r2(m1, "r2")
cat(sprintf(
  "\nSanity check M1: EBITDA Margin = %.3f (expect ~3.007) | R2 = %.3f (expect ~0.455)\n",
  m1_marg_chk, m1_r2_chk))
stopifnot(
  "M1 EBITDA-margin coefficient drifted from ~3.007" = abs(m1_marg_chk - 3.007) < 0.05,
  "M1 R-squared drifted from ~0.455"                 = abs(m1_r2_chk   - 0.455) < 0.01
)
cat("Sanity check passed: main models unchanged.\n")

cat("\n=== Quick etable (main models) ===\n")
etable(m1, m2, m1b, m2b,
       se.below    = TRUE,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
       title       = "Determinants of EV Multiples — European Listed TMT Firms")

# ==============================================================================
# 13b. Robustness check: Models 1/1b on EBITDA > 0 subsample
#      Plain ebitda_margin, no loss_firm dummy — confirms main results hold
#      for profitable firms only.
# ==============================================================================

df_m1_pos  <- df_m1  %>% filter(!is.na(ebitda) & ebitda > 0)
df_m1b_pos <- df_m1b %>% filter(!is.na(ebitda) & ebitda > 0)

BASE_RHS_ROB <- "rev_growth + ebitda_margin + leverage + size +
                 cash_assets + capex_sales + stock_vol"
EXT_RHS_ROB  <- paste(BASE_RHS_ROB, "+ rd_sales")

cat("Estimating robustness models (EBITDA > 0 subsample)...\n")
cat("  Robustness M1  sample:", nrow(df_m1_pos),  "firm-years\n")
cat("  Robustness M1b sample:", nrow(df_m1b_pos), "firm-years\n\n")

m1r  <- feols(as.formula(paste("log_ev_sales ~", BASE_RHS_ROB, "|", FE)),
              data=df_m1_pos,  cluster=~ticker)

m1br <- feols(as.formula(paste("log_ev_sales ~", EXT_RHS_ROB,  "|", FE)),
              data=df_m1b_pos, cluster=~ticker)

cat("\n=== Quick etable (robustness EV/Sales) ===\n")
etable(m1r, m1br,
       se.below    = TRUE,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
       title       = "Robustness: EV/Sales on EBITDA > 0 Subsample")

# ==============================================================================
# 13c. Extended EV/EBITDA models: Models 2c and 2cb
#      Add ROA and Asset Turnover to the baseline EV/EBITDA specification.
#      Same samples as Models 2 and 2b respectively.
# ==============================================================================

EXT_RHS_M2C  <- paste(BASE_RHS_M2, "+ roa + asset_turnover")
EXT_RHS_M2CB <- paste(EXT_RHS_M2,  "+ roa + asset_turnover")

cat("Estimating extended EV/EBITDA models (2c / 2cb)...\n")

m2c  <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2C,  "|", FE)),
              data=df_m2,  cluster=~ticker)

m2cb <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2CB, "|", FE)),
              data=df_m2b, cluster=~ticker)

cat("\n=== Quick etable (extended EV/EBITDA: 2c/2cb) ===\n")
etable(m2, m2b, m2c, m2cb,
       se.below    = TRUE,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
       title       = "Extended EV/EBITDA Models (ROA + Asset Turnover)")

# ==============================================================================
# 13d. Harbula EV/EBITDA models: Models 2d and 2db
#      Adds EBITDA growth and earnings stability on top of 2c specification.
#      Same samples as Models 2 and 2b respectively.
# ==============================================================================

BASE_RHS_M2D  <- paste(BASE_RHS_M2,
                        "+ ebitda_growth + earnings_stability + roa + asset_turnover")
EXT_RHS_M2DB  <- paste(EXT_RHS_M2,
                        "+ ebitda_growth + earnings_stability + roa + asset_turnover")

cat("Estimating Harbula EV/EBITDA models (2d / 2db)...\n")
cat("  Non-missing ebitda_growth in df_m2 :",
    sum(!is.na(df_m2$ebitda_growth)), "firm-years\n")
cat("  Non-missing earnings_stability in df_m2:",
    sum(!is.na(df_m2$earnings_stability)), "firm-years\n\n")

m2d  <- feols(as.formula(paste("log_ev_ebitda ~", BASE_RHS_M2D,  "|", FE)),
              data=df_m2,  cluster=~ticker)

m2db <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2DB, "|", FE)),
              data=df_m2b, cluster=~ticker)

cat("\n=== Quick etable (Harbula EV/EBITDA: 2d/2db) ===\n")
etable(m2d, m2db,
       se.below    = TRUE,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
       title       = "Harbula EV/EBITDA Models")

# ==============================================================================
# 14. Output tables
# ==============================================================================

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Single shared GOF map for ALL regression tables (main, EV/EBITDA extensions,
# sensitivity, subsector, tax, cagr): Observations, R², Within-R², in that order.
# Within-R² comes from fixest via the modelsummary GOF name "r2.within".
# Adj. R² is dropped for cross-table uniformity.
gof_rows <- data.frame(
  raw   = c("nobs", "r.squared", "r2.within"),
  clean = c("Observations", "R²", "Within-R²"),
  fmt   = c(0, 3, 3)
)

save_table <- function(models, coef_map, notes, stem) {
  modelsummary(models, coef_map=coef_map, stars=c("*"=0.1,"**"=0.05,"***"=0.01),
               gof_map=gof_rows, notes=notes,
               output=paste0("output/tables/", stem, ".html"))
  cat("Saved output/tables/", stem, ".html\n", sep="")
}

# ==============================================================================
# Table 1 — Main Results (Chapter 5)
# Columns: (1) EV/Sales | (2) EV/EBITDA | (1b) EV/Sales+R&D | (2b) EV/EBITDA+R&D
# ==============================================================================

coef_T1 <- c(
  rev_growth        = "Revenue Growth",
  ebitda_margin_pos = "EBITDA Margin (profitable only)",
  loss_firm         = "Loss Firm (EBITDA < 0)",
  leverage          = "Leverage (Net Debt / Assets)",
  size              = "Firm Size (ln Total Assets)",
  cash_assets       = "Cash / Assets",
  capex_sales       = "Capex / Sales",
  stock_vol         = "Stock Volatility (360d, decimal)",
  rd_sales          = "R&D / Sales"
)

models_T1 <- list(
  "(1) EV/Sales"       = m1,
  "(2) EV/EBITDA"      = m2,
  "(1b) EV/Sales+R&D"  = m1b,
  "(2b) EV/EBITDA+R&D" = m2b
)

NOTES_T1 <- paste(
  "Main regression results.",
  "EBITDA Margin in EV/Sales models (1 and 1b) uses the 'profitable only'",
  "specification with a separate Loss Firm dummy (Bhojraj & Lee 2002).",
  "EBITDA Margin omitted in EV/EBITDA models to avoid mechanical inversion.",
  "All models include subsector, country, and year fixed effects.",
  "Continuous variables winsorized at 1st/99th percentile.",
  "Firm-clustered standard errors in parentheses.",
  "Models 1b/2b estimated on sub-sample with non-missing R&D.",
  "* p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE 1: Main Results (markdown) ===\n")
modelsummary(models_T1, coef_map=coef_T1, stars=c("*"=0.1,"**"=0.05,"***"=0.01),
             gof_map=gof_rows, notes=NOTES_T1, output="markdown")
save_table(models_T1, coef_T1, NOTES_T1, "table_main_regressions")

# ==============================================================================
# Table 2 — EV/EBITDA Extensions (Chapter 6)
# Columns: (2) baseline | (2b) +R&D | (2c) ROA+AT | (2cb) ROA+AT+R&D |
#          (2d) Harbula | (2db) Harbula+R&D
# ==============================================================================

coef_T2 <- c(
  rev_growth         = "Revenue Growth",
  leverage           = "Leverage (Net Debt / Assets)",
  size               = "Firm Size (ln Total Assets)",
  cash_assets        = "Cash / Assets",
  capex_sales        = "Capex / Sales",
  stock_vol          = "Stock Volatility (360d, decimal)",
  roa                = "ROA (Net Income / Assets)",
  asset_turnover     = "Asset Turnover (Revenue / Assets)",
  ebitda_growth      = "EBITDA Growth",
  earnings_stability = "Earnings Stability (3Y rolling SD of EBITDA Margin)",
  rd_sales           = "R&D / Sales"
)

models_T2 <- list(
  "(2) Baseline"                    = m2,
  "(2b) + R&D"                      = m2b,
  "(2c) + ROA & Turnover"           = m2c,
  "(2cb) + ROA, Turnover & R&D"     = m2cb,
  "(2d) + Growth & Stability"       = m2d,
  "(2db) + Growth, Stability & R&D" = m2db
)

NOTES_T2 <- paste(
  "Extensions of the EV/EBITDA baseline.",
  "The '+ ROA & Turnover' columns add ROA and Asset Turnover",
  "(Damodaran 2007; Kumar 2019).",
  "The '+ Growth & Stability' columns additionally add EBITDA growth and earnings",
  "stability following Harbula (2009).",
  "Earnings stability is the rolling 3-year standard deviation of EBITDA margin",
  "within firm (requires 3 non-missing consecutive observations per firm).",
  "EBITDA Margin excluded from all EV/EBITDA models to avoid mechanical inversion.",
  "All models include subsector, country, and year fixed effects.",
  "Continuous variables winsorized at 1st/99th percentile.",
  "Firm-clustered standard errors in parentheses.",
  "Models 2b/2cb/2db estimated on sub-sample with non-missing R&D.",
  "* p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE 2: EV/EBITDA Extensions (markdown) ===\n")
modelsummary(models_T2, coef_map=coef_T2, stars=c("*"=0.1,"**"=0.05,"***"=0.01),
             gof_map=gof_rows, notes=NOTES_T2, output="markdown")
save_table(models_T2, coef_T2, NOTES_T2, "table_ev_ebitda_extensions")

# ==============================================================================
# 14c. Sensitivity (incremental) — leverage: Total Debt instead of Net Debt
# ==============================================================================

df_m1b_td <- df_m1b %>%
  mutate(leverage_td = wins(total_debt / total_assets))

m1b_totaldebt <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage_td + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data    = df_m1b_td,
  cluster = ~ticker
)

# ---- m1b_mktcap: ln(Market Cap) instead of ln(Total Assets) ----
df_m1b_mc <- df_m1b %>%
  mutate(size_mktcap = wins(log(ifelse(!is.na(mkt_cap) & mkt_cap > 0,
                                       mkt_cap, NA_real_))))

m1b_mktcap <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size_mktcap + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b_mc, cluster = ~ticker
)

# ---- m1b_ebit: EBIT Margin instead of EBITDA Margin ----
df_m1b_eb <- df_m1b %>%
  mutate(
    ebit_margin_pos = wins(ifelse(!is.na(ebit) & ebit > 0, ebit / revenue, 0)),
    ebit_loss_firm  = as.integer(!is.na(ebit) & ebit < 0)
  )

m1b_ebit <- feols(
  log_ev_sales ~ rev_growth + ebit_margin_pos + ebit_loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b_eb, cluster = ~ticker
)

# ---- m1b_no2020: exclude COVID year ----
m1b_no2020 <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b %>% filter(fiscal_year != 2020), cluster = ~ticker
)

# ---- m1b_no2025: exclude partial year ----
m1b_no2025 <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b %>% filter(fiscal_year != 2025), cluster = ~ticker
)

# ---- m1b_twoway: two-way clustered SE (firm + year) ----
m1b_twoway <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b, cluster = ~ticker + fiscal_year
)

# ---- m1b_nowinsor: pre-winsorisation data ----
df_m1b_raw <- df_m1_raw %>% filter(!is.na(rd_sales))

m1b_nowinsor <- feols(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales |
    subsector + country + fiscal_year,
  data = df_m1b_raw, cluster = ~ticker
)

cat("All 6 additional sensitivity models estimated.\n\n")

# ---- Coefficient map for the 8-column sensitivity table ----
coef_sens <- c(
  rev_growth        = "Revenue Growth",
  ebitda_margin_pos = "EBITDA Margin (profitable only)",
  ebit_margin_pos   = "EBIT Margin (profitable only)",
  loss_firm         = "Loss Firm (EBITDA < 0)",
  ebit_loss_firm    = "Loss Firm (EBIT < 0)",
  leverage          = "Leverage (Net Debt / Assets)",
  leverage_td       = "Leverage (Total Debt / Assets)",
  size              = "Firm Size (ln Total Assets)",
  size_mktcap       = "Firm Size (ln Market Cap)",
  cash_assets       = "Cash / Assets",
  capex_sales       = "Capex / Sales",
  stock_vol         = "Stock Volatility (360d, decimal)",
  rd_sales          = "R&D / Sales"
)

models_sens <- list(
  "(A) Baseline"        = m1b,
  "(B) Total Debt"      = m1b_totaldebt,
  "(C) ln(Market Cap)"  = m1b_mktcap,
  "(D) EBIT Margin"     = m1b_ebit,
  "(E) Excl. 2020"      = m1b_no2020,
  "(F) Excl. 2025"      = m1b_no2025,
  "(G) Two-Way SE"      = m1b_twoway,
  "(H) No Winsor"       = m1b_nowinsor
)

NOTES_SENS <- paste(
  "Sensitivity analysis for EV/Sales baseline (Model 1b).",
  "Each column varies one element: (B) Total Debt leverage,",
  "(C) ln(Market Cap) as size, (D) EBIT Margin instead of EBITDA Margin,",
  "(E) excludes 2020, (F) excludes 2025,",
  "(G) two-way clustered SE (firm + year; 16 year-clusters — interpret with caution),",
  "(H) no winsorising.",
  "All other elements held constant across columns.",
  "Firm-clustered SE except column (G). * p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE 3: Sensitivity Analysis — 8 columns (markdown) ===\n")
modelsummary(models_sens, coef_map = coef_sens,
             stars = c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = gof_rows, notes = NOTES_SENS, output = "markdown")

modelsummary(models_sens, coef_map = coef_sens,
             stars = c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = gof_rows, notes = NOTES_SENS,
             output = "output/tables/table_sensitivity.html")
cat("HTML saved: output/tables/table_sensitivity.html\n")

# ==============================================================================
# 16. Subsector Heterogeneity — Table 4
# ==============================================================================

# ---- Subsamples ----
df_m1_tech  <- df_m1 %>% filter(subsector == "Technology")
df_m1_media <- df_m1 %>% filter(subsector == "Media")
df_m1_tel   <- df_m1 %>% filter(subsector == "Telecom")
df_m2_tech  <- df_m2 %>% filter(subsector == "Technology")
df_m2_media <- df_m2 %>% filter(subsector == "Media")
df_m2_tel   <- df_m2 %>% filter(subsector == "Telecom")

cat("\n=== Subsector sample sizes ===\n")
cat("EV/Sales  — Technology:", nrow(df_m1_tech),
    "| Media:", nrow(df_m1_media),
    "| Telecom:", nrow(df_m1_tel), "\n")
cat("EV/EBITDA — Technology:", nrow(df_m2_tech),
    "| Media:", nrow(df_m2_media),
    "| Telecom:", nrow(df_m2_tel), "\n\n")

# ---- Fixed-effect specs ----
# Country + year FE for all subsamples; subsector FE omitted (constant per sample).
# For Telecom: if n < 200, drop country FE and note it.
FE_FULL    <- "country + fiscal_year"
FE_YEARONLY <- "fiscal_year"

telecom_fe_evs <- if (nrow(df_m1_tel) < 200) {
  cat("NOTE: Telecom EV/Sales n =", nrow(df_m1_tel),
      "< 200 — country FE dropped for this subsample.\n")
  FE_YEARONLY
} else FE_FULL

telecom_fe_eve <- if (nrow(df_m2_tel) < 200) {
  cat("NOTE: Telecom EV/EBITDA n =", nrow(df_m2_tel),
      "< 200 — country FE dropped for this subsample.\n")
  FE_YEARONLY
} else FE_FULL

# ---- RHS formulas ----
RHS_EVS <- "rev_growth + ebitda_margin_pos + loss_firm + leverage + size + cash_assets + capex_sales + stock_vol"
RHS_EVE <- "rev_growth + leverage + size + cash_assets + capex_sales + stock_vol"

fml_sub <- function(dv, rhs, fe) as.formula(paste(dv, "~", rhs, "|", fe))

# ---- Estimate ----
cat("Estimating subsector models...\n")

m_tech_evs  <- feols(fml_sub("log_ev_sales",  RHS_EVS, FE_FULL),
                     data = df_m1_tech,  cluster = ~ticker)
m_media_evs <- feols(fml_sub("log_ev_sales",  RHS_EVS, FE_FULL),
                     data = df_m1_media, cluster = ~ticker)
m_telecom_evs <- feols(fml_sub("log_ev_sales",  RHS_EVS, telecom_fe_evs),
                       data = df_m1_tel,  cluster = ~ticker)

m_tech_eve  <- feols(fml_sub("log_ev_ebitda", RHS_EVE, FE_FULL),
                     data = df_m2_tech,  cluster = ~ticker)
m_media_eve <- feols(fml_sub("log_ev_ebitda", RHS_EVE, FE_FULL),
                     data = df_m2_media, cluster = ~ticker)
m_telecom_eve <- feols(fml_sub("log_ev_ebitda", RHS_EVE, telecom_fe_eve),
                       data = df_m2_tel,  cluster = ~ticker)

cat("All 6 subsector models estimated.\n\n")

# ---- Coefficient labels (consistent with Table 1 and Table 3) ----
coef_T4 <- c(
  rev_growth        = "Revenue Growth",
  ebitda_margin_pos = "EBITDA Margin (profitable only)",
  loss_firm         = "Loss Firm (EBITDA < 0)",
  leverage          = "Leverage (Net Debt / Assets)",
  size              = "Firm Size (ln Total Assets)",
  cash_assets       = "Cash / Assets",
  capex_sales       = "Capex / Sales",
  stock_vol         = "Stock Volatility (360d, decimal)"
)

models_T4 <- list(
  "EV/Sales Technology"  = m_tech_evs,
  "EV/Sales Media"       = m_media_evs,
  "EV/Sales Telecom"     = m_telecom_evs,
  "EV/EBITDA Technology" = m_tech_eve,
  "EV/EBITDA Media"      = m_media_eve,
  "EV/EBITDA Telecom"    = m_telecom_eve
)

NOTES_T4 <- paste(
  "Subsector-specific regressions.",
  "The EV/Sales Technology, Media and Telecom columns estimate the EV/Sales",
  "specification separately for the Technology, Media, and Telecommunications",
  "subsamples.",
  "The EV/EBITDA Technology, Media and Telecom columns do the same for EV/EBITDA.",
  "Subsector fixed effects are omitted by construction.",
  "Country and year fixed effects are included where the subsample size allows.",
  "Firm-clustered SE in parentheses.",
  "Stars * p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE 4: Subsector Heterogeneity (markdown) ===\n")
modelsummary(models_T4, coef_map = coef_T4,
             stars   = c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = gof_rows, notes = NOTES_T4,
             output  = "markdown")

modelsummary(models_T4, coef_map = coef_T4,
             stars   = c("*"=0.1, "**"=0.05, "***"=0.01),
             gof_map = gof_rows, notes = NOTES_T4,
             output  = "output/tables/table_subsector_heterogeneity.html")
cat("HTML saved: output/tables/table_subsector_heterogeneity.html\n")

# ==============================================================================
# 17. Descriptive Statistics, Correlations, and VIFs
# ==============================================================================

if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

# Custom quartile helpers recognised by datasummary
P25 <- function(x) quantile(x, 0.25, na.rm = TRUE)
P75 <- function(x) quantile(x, 0.75, na.rm = TRUE)

# ---- 17a. Descriptive Statistics by Subsector --------------------------------

# FIX 1: Winsorise ev_sales and ev_ebitda locally for the descriptive table only.
# (df_m1 is NOT modified — these are table-local vectors.)
df_m1_desc <- df_m1 %>%
  mutate(
    ev_sales_w  = winsorize(ev_sales),
    ev_ebitda_w = winsorize(ev_ebitda)   # NA for EBITDA<=0 rows; winsorize skips NA
  )

# Duplicate with Subsector = "All TMT" for pooled column
df_desc <- bind_rows(
  df_m1_desc %>% mutate(Subsector = subsector),
  df_m1_desc %>% mutate(Subsector = "All TMT")
) %>%
  mutate(Subsector = factor(Subsector,
                            levels = c("All TMT", "Technology", "Media", "Telecom")))

# FIX 2: Footer counts — Firms and Firm-Years per group
counts_sub <- df_m1 %>%
  group_by(Subsector = subsector) %>%
  summarise(Firms = n_distinct(ticker), `Firm-Years` = n(), .groups = "drop")
counts_all <- df_m1 %>%
  summarise(Subsector = "All TMT",
            Firms = n_distinct(ticker), `Firm-Years` = n())
counts_tbl <- bind_rows(counts_all, counts_sub) %>%
  mutate(Subsector = factor(Subsector,
                            levels = c("All TMT", "Technology", "Media", "Telecom")))

cat("=== Firm / observation counts (df_m1) ===\n")
print(counts_tbl)
cat("\n")

NOTES_DESC <- paste(
  "Descriptive statistics for the final EV/Sales estimation sample after",
  "winsorising all continuous variables (including EV/Sales and EV/EBITDA)",
  "at the 1st and 99th percentiles.",
  "EV/EBITDA statistics computed on the EBITDA > 0 subsample.",
  "Firm and firm-year counts shown at the bottom.",
  "Source: Bloomberg; own calculations."
)

# Helper to run the same datasummary call for any output format
run_desc <- function(out) {
  datasummary(
    (`EV/Sales (w)`     = ev_sales_w)   +
    (`EV/EBITDA (w)`    = ev_ebitda_w)  +
    (`Revenue Growth`   = rev_growth)   +
    (`EBITDA Margin`    = ebitda_margin) +
    (`Leverage`         = leverage)     +
    (`Size (ln Assets)` = size)         +
    (`Cash / Assets`    = cash_assets)  +
    (`Capex / Sales`    = capex_sales)  +
    (`R&D / Sales`      = rd_sales)     +
    (`Stock Volatility` = stock_vol)    ~
    Subsector * (Mean + Median + SD + P25 + P75),
    data   = df_desc,
    notes  = NOTES_DESC,
    output = out
  )
}

cat("=== TABLE: Descriptive Statistics by Subsector (markdown) ===\n")
run_desc("markdown")

# Footer: Firms and Firm-Years — appended as a plain text block since datasummary
# does not support mixed-type rows. Both appear in the saved HTML/TeX via a
# second datasummary_df call concatenated after the main table.
counts_print <- counts_tbl %>%
  pivot_longer(c(Firms, `Firm-Years`), names_to = "Variable", values_to = "N") %>%
  pivot_wider(names_from = Subsector, values_from = N) %>%
  select(Variable, `All TMT`, Technology, Media, Telecom)
cat("\n--- Sample Counts ---\n")
print(counts_print, row.names = FALSE)
cat("\n")

run_desc("output/tables/table_descriptive_stats.html")
cat("HTML saved: output/tables/table_descriptive_stats.html\n")

# Append counts as a second minimal table to the same HTML file
counts_html_row <- counts_print %>%
  mutate(across(-Variable, as.character)) %>%
  { paste0("<br><b>Sample size</b><br>",
           knitr::kable(., format="html", row.names=FALSE)) }
write(counts_html_row,
      file = "output/tables/table_descriptive_stats.html", append = TRUE)
cat("Sample counts appended to HTML.\n\n")

# ---- 17b. Correlation Matrix -------------------------------------------------

COR_VARS <- c("rev_growth", "ebitda_margin_pos", "loss_firm", "leverage",
              "size", "cash_assets", "capex_sales", "rd_sales", "stock_vol")
COR_LABELS <- c("Rev. Growth", "EBITDA Margin (pos.)", "Loss Firm",
                "Leverage", "Size", "Cash/Assets", "Capex/Sales",
                "R&D/Sales", "Stock Vol.")

cor_mat <- cor(df_m1[, COR_VARS], use = "pairwise.complete.obs", method = "pearson")
cor_mat <- round(cor_mat, 3)
cor_mat[upper.tri(cor_mat, diag = FALSE)] <- NA   # lower triangle only
colnames(cor_mat) <- rownames(cor_mat) <- COR_LABELS

cor_df <- as.data.frame(cor_mat)
cor_df <- cbind(Variable = rownames(cor_df), cor_df)
rownames(cor_df) <- NULL

NOTES_COR <- paste(
  "Pearson correlations among continuous regressors in the EV/Sales",
  "estimation sample. Upper triangle suppressed (symmetric).",
  "Correlations above |0.5| in absolute value warrant attention for",
  "multicollinearity. Source: Bloomberg; own calculations."
)

cat("=== TABLE: Correlation Matrix (markdown) ===\n")
datasummary_df(cor_df, notes = NOTES_COR, output = "markdown")

datasummary_df(cor_df, notes = NOTES_COR,
               output = "output/tables/table_correlations.html")
cat("HTML saved: output/tables/table_correlations.html\n\n")

# ---- 17c. Variance Inflation Factors ----------------------------------------

NOTES_VIF <- paste(
  "Variance Inflation Factors for the EV/Sales regressors.",
  "VIFs computed from OLS replicas of Models 1 and 1b using lm(),",
  "since fixest does not interface with car::vif().",
  "Fixed-effect dummies (subsector, country, year) omitted from the table.",
  "VIF > 5 indicates moderate multicollinearity;",
  "VIF > 10 indicates serious multicollinearity (O'Brien 2007).",
  "Source: own calculations."
)

# Variables needed for lm (drop rows with NA in any regressor or FE)
VIF_COLS_M1  <- c("log_ev_sales", "rev_growth", "ebitda_margin_pos", "loss_firm",
                   "leverage", "size", "cash_assets", "capex_sales", "stock_vol",
                   "subsector", "country", "fiscal_year")
VIF_COLS_M1b <- c(VIF_COLS_M1, "rd_sales")

lm_m1_vif <- lm(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol +
    factor(subsector) + factor(country) + factor(fiscal_year),
  data = na.omit(df_m1[, VIF_COLS_M1])
)

lm_m1b_vif <- lm(
  log_ev_sales ~ rev_growth + ebitda_margin_pos + loss_firm +
    leverage + size + cash_assets + capex_sales + stock_vol + rd_sales +
    factor(subsector) + factor(country) + factor(fiscal_year),
  data = na.omit(df_m1b[, VIF_COLS_M1b])
)

vif_raw_m1  <- car::vif(lm_m1_vif)
vif_raw_m1b <- car::vif(lm_m1b_vif)

# car::vif returns a matrix (GVIF format) when the model contains multi-df
# factor terms. For single-df regressors (continuous / binary), GVIF = VIF.
extract_vif <- function(vif_obj, vars) {
  v <- if (is.matrix(vif_obj)) vif_obj[vars, "GVIF"] else vif_obj[vars]
  round(v, 3)
}

SUBST_M1  <- c("rev_growth", "ebitda_margin_pos", "loss_firm", "leverage",
               "size", "cash_assets", "capex_sales", "stock_vol")
SUBST_M1b <- c(SUBST_M1, "rd_sales")

vif_table <- data.frame(
  Variable = c("Revenue Growth", "EBITDA Margin (profitable only)",
               "Loss Firm (EBITDA < 0)", "Leverage (Net Debt / Assets)",
               "Firm Size (ln Total Assets)", "Cash / Assets",
               "Capex / Sales", "Stock Volatility (360d)", "R&D / Sales"),
  `VIF (Model 1)`  = c(extract_vif(vif_raw_m1,  SUBST_M1),  NA_real_),
  `VIF (Model 1b)` =   extract_vif(vif_raw_m1b, SUBST_M1b),
  check.names = FALSE
)

cat("=== TABLE: Variance Inflation Factors (markdown) ===\n")
datasummary_df(vif_table, notes = NOTES_VIF, output = "markdown")

datasummary_df(vif_table, notes = NOTES_VIF,
               output = "output/tables/table_vifs.html")
cat("HTML saved: output/tables/table_vifs.html\n")

# ==============================================================================
# 18. Extension: Effective Tax Rate & Trailing Sales CAGR (BOTH multiples)
# ==============================================================================
#   (A) Effective tax rate as a genuine new determinant, added to BOTH the
#       EV/Sales and the EV/EBITDA baselines.
#   (B) Trailing 3-year sales CAGR as a backward-looking growth measure, tested
#       as an ALTERNATIVE to one-year Revenue Growth (never alongside it) for
#       BOTH multiples.
#   (C) Forward / expected growth could NOT be sourced from Bloomberg BEst
#       (no historical point-in-time consensus for the early panel) -> documented
#       as a data limitation only, no model here.
#
# eff_tax_rate and sales_cagr_3y are built in the main pipeline (Block 8) and
# winsorized with the other continuous variables (Block 10), so they are already
# present in df_m1, df_m1b, df_m2 and df_m2b. The detected tax-rate column name
# was printed in Block 4. No existing model or table is modified; this block only
# adds same-sample redux pairs and the two extension tables.
#
# RATIONALE FOR SAME-SAMPLE BASELINES
# Both new variables shrink the sample (tax-rate coverage is partial; the 3Y CAGR
# needs three prior observations). To separate the variable's marginal
# contribution from the change in sample, every augmented model is paired with a
# "redux" baseline estimated on the IDENTICAL rows. Variable contribution is read
# off the WITHIN-R squared, because the subsector + country + year fixed effects
# absorb most of the overall variance.

cat("\n\n############################################################\n")
cat("## EXTENSION: Effective Tax Rate & Trailing Sales CAGR    ##\n")
cat("############################################################\n\n")

# Within-R squared helper (console only)
wr2 <- function(m) tryCatch(round(fixest::r2(m, "wr2"), 4),
                            error = function(e) NA_real_)

# ---- 18a. Coverage of the new variables -------------------------------------
cat("=== Coverage of new variables ===\n")
cov_tbl <- data.frame(
  Sample = c("df_m1 (EV/Sales)", "df_m1b (EV/Sales + R&D)",
             "df_m2 (EV/EBITDA)", "df_m2b (EV/EBITDA + R&D)"),
  N      = c(nrow(df_m1), nrow(df_m1b), nrow(df_m2), nrow(df_m2b)),
  n_tax  = c(sum(!is.na(df_m1$eff_tax_rate)),  sum(!is.na(df_m1b$eff_tax_rate)),
             sum(!is.na(df_m2$eff_tax_rate)),  sum(!is.na(df_m2b$eff_tax_rate))),
  n_cagr = c(sum(!is.na(df_m1$sales_cagr_3y)), sum(!is.na(df_m1b$sales_cagr_3y)),
             sum(!is.na(df_m2$sales_cagr_3y)), sum(!is.na(df_m2b$sales_cagr_3y))),
  check.names = FALSE
)
print(cov_tbl, row.names = FALSE)

cat("\nCorrelation rev_growth vs sales_cagr_3y (df_m1):",
    round(cor(df_m1$rev_growth, df_m1$sales_cagr_3y,
              use = "pairwise.complete.obs"), 3),
    "| (df_m2):",
    round(cor(df_m2$rev_growth, df_m2$sales_cagr_3y,
              use = "pairwise.complete.obs"), 3), "\n")

# Unified coefficient labels (identical wording to the main table).
coef_ext <- c(
  rev_growth        = "Revenue Growth",
  sales_cagr_3y     = "Trailing 3Y Sales CAGR",
  ebitda_margin_pos = "EBITDA Margin (profitable only)",
  loss_firm         = "Loss Firm (EBITDA < 0)",
  leverage          = "Leverage (Net Debt / Assets)",
  size              = "Firm Size (ln Total Assets)",
  cash_assets       = "Cash / Assets",
  capex_sales       = "Capex / Sales",
  stock_vol         = "Stock Volatility (360d, decimal)",
  rd_sales          = "R&D / Sales",
  eff_tax_rate      = "Effective Tax Rate"
)

# ==============================================================================
# 18b. PART A - Effective Tax Rate added to BOTH multiples (same-sample design)
# Each augmented column is paired with a redux baseline on the IDENTICAL rows.
# EV/Sales keeps ebitda_margin_pos + loss_firm; EV/EBITDA excludes EBITDA margin.
# ==============================================================================

RHS_M1_TAX  <- paste(BASE_RHS_M1, "+ eff_tax_rate")
RHS_M1B_TAX <- paste(EXT_RHS_M1,  "+ eff_tax_rate")
RHS_M2_TAX  <- paste(BASE_RHS_M2, "+ eff_tax_rate")
RHS_M2B_TAX <- paste(EXT_RHS_M2,  "+ eff_tax_rate")

df_m1_tax  <- df_m1  %>% filter(!is.na(eff_tax_rate))
df_m1b_tax <- df_m1b %>% filter(!is.na(eff_tax_rate))
df_m2_tax  <- df_m2  %>% filter(!is.na(eff_tax_rate))
df_m2b_tax <- df_m2b %>% filter(!is.na(eff_tax_rate))

cat("\nEstimating tax-rate models (same-sample redux vs +Tax)...\n")
cat("  EV/Sales  M1  tax sample:", nrow(df_m1_tax),  "| M1b:", nrow(df_m1b_tax), "\n")
cat("  EV/EBITDA M2  tax sample:", nrow(df_m2_tax),  "| M2b:", nrow(df_m2b_tax), "\n")

m1_redux  <- feols(as.formula(paste("log_ev_sales  ~", BASE_RHS_M1, "|", FE)),
                   data = df_m1_tax,  cluster = ~ticker)
m1_tax    <- feols(as.formula(paste("log_ev_sales  ~", RHS_M1_TAX,  "|", FE)),
                   data = df_m1_tax,  cluster = ~ticker)
m1b_redux <- feols(as.formula(paste("log_ev_sales  ~", EXT_RHS_M1,  "|", FE)),
                   data = df_m1b_tax, cluster = ~ticker)
m1b_tax   <- feols(as.formula(paste("log_ev_sales  ~", RHS_M1B_TAX, "|", FE)),
                   data = df_m1b_tax, cluster = ~ticker)

m2_redux  <- feols(as.formula(paste("log_ev_ebitda ~", BASE_RHS_M2, "|", FE)),
                   data = df_m2_tax,  cluster = ~ticker)
m2_tax    <- feols(as.formula(paste("log_ev_ebitda ~", RHS_M2_TAX,  "|", FE)),
                   data = df_m2_tax,  cluster = ~ticker)
m2b_redux <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2,  "|", FE)),
                   data = df_m2b_tax, cluster = ~ticker)
m2b_tax   <- feols(as.formula(paste("log_ev_ebitda ~", RHS_M2B_TAX, "|", FE)),
                   data = df_m2b_tax, cluster = ~ticker)

# Same-sample guarantee for every redux pair.
stopifnot(
  "M1 tax pair N mismatch"  = nobs(m1_redux)  == nobs(m1_tax),
  "M1b tax pair N mismatch" = nobs(m1b_redux) == nobs(m1b_tax),
  "M2 tax pair N mismatch"  = nobs(m2_redux)  == nobs(m2_tax),
  "M2b tax pair N mismatch" = nobs(m2b_redux) == nobs(m2b_tax)
)

cat("\n--- Within-R2: tax models ---\n")
cat("  M1  redux:", wr2(m1_redux),  "| +Tax:", wr2(m1_tax),  "\n")
cat("  M1b redux:", wr2(m1b_redux), "| +Tax:", wr2(m1b_tax), "\n")
cat("  M2  redux:", wr2(m2_redux),  "| +Tax:", wr2(m2_tax),  "\n")
cat("  M2b redux:", wr2(m2b_redux), "| +Tax:", wr2(m2b_tax), "\n")

models_E1 <- list(
  "(1) EV/Sales"               = m1_redux,
  "(2) EV/Sales + Tax"         = m1_tax,
  "(3) EV/Sales + R&D"         = m1b_redux,
  "(4) EV/Sales + R&D + Tax"   = m1b_tax,
  "(5) EV/EBITDA"              = m2_redux,
  "(6) EV/EBITDA + Tax"        = m2_tax,
  "(7) EV/EBITDA + R&D"        = m2b_redux,
  "(8) EV/EBITDA + R&D + Tax"  = m2b_tax
)

NOTES_E1 <- paste(
  "Effective tax rate added to both the EV/Sales and EV/EBITDA baselines,",
  "EV/Sales columns first (Damodaran: EBITDA and Sales are pre-tax, and country",
  "fixed effects absorb only average country-level tax differences, not firm-level",
  "variation).",
  "Effective tax rate = Bloomberg 'Tax Rate' / 100; raw values above 100% set to",
  "missing. Coverage is partial, so each '+ Tax' column is paired with the",
  "same-sample baseline immediately to its left, estimated on identical rows",
  "(column 2 with 1, 4 with 3, 6 with 5, 8 with 7).",
  "EV/Sales models retain the profitable-only EBITDA Margin and Loss Firm dummy;",
  "EBITDA Margin is excluded from all EV/EBITDA models to avoid mechanical inversion.",
  "All models include subsector, country, and year fixed effects.",
  "Continuous variables winsorized at 1st/99th percentile.",
  "Firm-clustered standard errors in parentheses.",
  "Within-R2 reported (FE absorb most overall variance).",
  "* p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE E1: Effective Tax Rate, both multiples (markdown) ===\n")
modelsummary(models_E1, coef_map = coef_ext,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_map = gof_rows, notes = NOTES_E1, output = "markdown")
save_table(models_E1, coef_ext, NOTES_E1, "table_ext_tax_rate")

# ==============================================================================
# 18c. PART B - Trailing 3Y Sales CAGR as alternative growth (both multiples)
# rev_growth is REPLACED by sales_cagr_3y; the same-sample baseline keeps
# rev_growth, so the two growth measures are compared on identical rows.
# ==============================================================================

RHS_M1_CAGR  <- sub("rev_growth", "sales_cagr_3y", BASE_RHS_M1, fixed = TRUE)
RHS_M1B_CAGR <- sub("rev_growth", "sales_cagr_3y", EXT_RHS_M1,  fixed = TRUE)
RHS_M2_CAGR  <- sub("rev_growth", "sales_cagr_3y", BASE_RHS_M2, fixed = TRUE)
RHS_M2B_CAGR <- sub("rev_growth", "sales_cagr_3y", EXT_RHS_M2,  fixed = TRUE)

df_m1_cagr  <- df_m1  %>% filter(!is.na(sales_cagr_3y) & !is.na(rev_growth))
df_m1b_cagr <- df_m1b %>% filter(!is.na(sales_cagr_3y) & !is.na(rev_growth))
df_m2_cagr  <- df_m2  %>% filter(!is.na(sales_cagr_3y) & !is.na(rev_growth))
df_m2b_cagr <- df_m2b %>% filter(!is.na(sales_cagr_3y) & !is.na(rev_growth))

cat("\nEstimating CAGR models (same-sample rev_growth vs sales_cagr_3y)...\n")
cat("  EV/Sales  M1  CAGR sample:", nrow(df_m1_cagr),  "| M1b:", nrow(df_m1b_cagr), "\n")
cat("  EV/EBITDA M2  CAGR sample:", nrow(df_m2_cagr),  "| M2b:", nrow(df_m2b_cagr), "\n")

m1_cagr_base  <- feols(as.formula(paste("log_ev_sales  ~", BASE_RHS_M1,  "|", FE)),
                       data = df_m1_cagr,  cluster = ~ticker)
m1_cagr       <- feols(as.formula(paste("log_ev_sales  ~", RHS_M1_CAGR,  "|", FE)),
                       data = df_m1_cagr,  cluster = ~ticker)
m1b_cagr_base <- feols(as.formula(paste("log_ev_sales  ~", EXT_RHS_M1,   "|", FE)),
                       data = df_m1b_cagr, cluster = ~ticker)
m1b_cagr      <- feols(as.formula(paste("log_ev_sales  ~", RHS_M1B_CAGR, "|", FE)),
                       data = df_m1b_cagr, cluster = ~ticker)

m2_cagr_base  <- feols(as.formula(paste("log_ev_ebitda ~", BASE_RHS_M2,  "|", FE)),
                       data = df_m2_cagr,  cluster = ~ticker)
m2_cagr       <- feols(as.formula(paste("log_ev_ebitda ~", RHS_M2_CAGR,  "|", FE)),
                       data = df_m2_cagr,  cluster = ~ticker)
m2b_cagr_base <- feols(as.formula(paste("log_ev_ebitda ~", EXT_RHS_M2,   "|", FE)),
                       data = df_m2b_cagr, cluster = ~ticker)
m2b_cagr      <- feols(as.formula(paste("log_ev_ebitda ~", RHS_M2B_CAGR, "|", FE)),
                       data = df_m2b_cagr, cluster = ~ticker)

stopifnot(
  "M1 cagr pair N mismatch"  = nobs(m1_cagr_base)  == nobs(m1_cagr),
  "M1b cagr pair N mismatch" = nobs(m1b_cagr_base) == nobs(m1b_cagr),
  "M2 cagr pair N mismatch"  = nobs(m2_cagr_base)  == nobs(m2_cagr),
  "M2b cagr pair N mismatch" = nobs(m2b_cagr_base) == nobs(m2b_cagr)
)

cat("\n--- Within-R2: growth-measure comparison ---\n")
cat("  M1  rev_growth:", wr2(m1_cagr_base),  "| 3Y CAGR:", wr2(m1_cagr),  "\n")
cat("  M1b rev_growth:", wr2(m1b_cagr_base), "| 3Y CAGR:", wr2(m1b_cagr), "\n")
cat("  M2  rev_growth:", wr2(m2_cagr_base),  "| 3Y CAGR:", wr2(m2_cagr),  "\n")
cat("  M2b rev_growth:", wr2(m2b_cagr_base), "| 3Y CAGR:", wr2(m2b_cagr), "\n")

models_E2 <- list(
  "(1) EV/Sales"               = m1_cagr_base,
  "(2) EV/Sales (CAGR)"        = m1_cagr,
  "(3) EV/Sales + R&D"         = m1b_cagr_base,
  "(4) EV/Sales + R&D (CAGR)"  = m1b_cagr,
  "(5) EV/EBITDA"              = m2_cagr_base,
  "(6) EV/EBITDA (CAGR)"       = m2_cagr,
  "(7) EV/EBITDA + R&D"        = m2b_cagr_base,
  "(8) EV/EBITDA + R&D (CAGR)" = m2b_cagr
)

NOTES_E2 <- paste(
  "Trailing three-year sales CAGR = (Revenue_t / Revenue_{t-3})^(1/3) - 1,",
  "a smoothed backward-looking growth measure used as an ALTERNATIVE to one-year",
  "Revenue Growth (the two are highly collinear and never enter together),",
  "for both multiples with EV/Sales columns first.",
  "The base columns use one-year Revenue Growth; the columns marked (CAGR) replace",
  "it with the Trailing 3Y Sales CAGR on identical samples, so the two growth",
  "measures are compared on the same rows.",
  "Forward-looking expected growth could not be sourced (no historical",
  "point-in-time Bloomberg BEst consensus for the early panel) and is documented",
  "as a data limitation only.",
  "EV/Sales models retain the profitable-only EBITDA Margin and Loss Firm dummy;",
  "EBITDA Margin is excluded from all EV/EBITDA models to avoid mechanical inversion.",
  "All models include subsector, country, and year fixed effects.",
  "Continuous variables winsorized at 1st/99th percentile.",
  "Firm-clustered standard errors in parentheses.",
  "Within-R2 reported. * p<0.1, ** p<0.05, *** p<0.01."
)

cat("\n=== TABLE E2: Trailing 3Y Sales CAGR, both multiples (markdown) ===\n")
modelsummary(models_E2, coef_map = coef_ext,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_map = gof_rows, notes = NOTES_E2, output = "markdown")
save_table(models_E2, coef_ext, NOTES_E2, "table_ext_sales_cagr")

cat("\n############################################################\n")
cat("## EXTENSION COMPLETE                                      ##\n")
cat("############################################################\n\n")

# ==============================================================================
# 19. Summary of all regenerated output tables
# ==============================================================================

written_tables <- c(
  "table_main_regressions", "table_ev_ebitda_extensions", "table_sensitivity",
  "table_subsector_heterogeneity", "table_ext_tax_rate", "table_ext_sales_cagr",
  "table_descriptive_stats", "table_correlations", "table_vifs"
)

cat("=== Output tables in output/tables/ (this run) ===\n")
all_ok <- TRUE
for (stem in written_tables) {
  f  <- file.path("output", "tables", paste0(stem, ".html"))
  ok <- file.exists(f)
  all_ok <- all_ok && ok
  cat(sprintf("  [%s] %s\n", if (ok) "OK" else "MISSING", f))
}
cat(sprintf("\n%d/%d tables present as .html.\n",
            sum(file.exists(file.path("output", "tables",
                 paste0(written_tables, ".html")))),
            length(written_tables)))
stopifnot("Not all expected tables were written" = all_ok)
cat("All tables regenerated in a single run.\n")

# ==============================================================================
# 20. Mechanical / non-mechanical decomposition of the EBITDA-margin effect
# ==============================================================================
# Self-contained add-on. Touches no existing object, model or table; writes ONE
# new file (output/tables/table_margin_decomposition.html).
#
# IDENTITY (positive-EBITDA firms only):
#   ln(EV/Sales) = ln(EBITDA Margin) + ln(EV/EBITDA)
# OLS property: on an IDENTICAL sample with IDENTICAL regressors, the coefficient
# of the EBITDA margin in a regression on ln(EV/Sales) equals the sum of its
# coefficients in two part-regressions, on ln(EBITDA Margin) and on ln(EV/EBITDA).
#   Model A (dep = log_ev_sales)      -> total margin loading
#   Model B (dep = log_ebitda_margin) -> purely MECHANICAL part
#   Model C (dep = log_ev_ebitda)     -> NON-MECHANICAL part (economic premium)
# Uses the existing winsorized object df_m1, the existing FE string FE
# (subsector + country + fiscal_year), the existing key regressor
# ebitda_margin_pos, firm-clustered SE (cluster = ~ticker), the shared GOF map
# gof_rows, and the existing save_table() HTML writer.

cat("\n\n############################################################\n")
cat("## DECOMPOSITION: mechanical vs non-mechanical margin effect ##\n")
cat("############################################################\n\n")

# Positive-EBITDA subsample: both ln(EBITDA margin) and ln(EV/EBITDA) are defined,
# i.e. exactly the rows of df_m1 with non-missing (winsorized) log_ev_ebitda.
# log_ebitda_margin is built as the difference of the EXISTING winsorized
# variables so the identity holds numerically in the data (not winsorized again).
df_decomp <- df_m1 %>%
  filter(!is.na(log_ev_ebitda)) %>%
  mutate(log_ebitda_margin = log_ev_sales - log_ev_ebitda)

cat("Decomposition subsample (profitable, EBITDA > 0):", nrow(df_decomp),
    "firm-years |", n_distinct(df_decomp$ticker), "firms\n")

# Same controls as the EV/Sales spec (BASE_RHS_M1) but WITHOUT loss_firm, which
# is constant (= 0) on the positive-EBITDA subsample and hence dropped.
DECOMP_RHS <- gsub("\\s*\\+\\s*loss_firm", "", BASE_RHS_M1)
cat("Decomposition RHS:", gsub("\\s+", " ", DECOMP_RHS), "\n")
cat("Fixed effects    :", FE, "\n\n")

m_decomp_A <- feols(as.formula(paste("log_ev_sales      ~", DECOMP_RHS, "|", FE)),
                    data = df_decomp, cluster = ~ticker)
m_decomp_B <- feols(as.formula(paste("log_ebitda_margin ~", DECOMP_RHS, "|", FE)),
                    data = df_decomp, cluster = ~ticker)
m_decomp_C <- feols(as.formula(paste("log_ev_ebitda     ~", DECOMP_RHS, "|", FE)),
                    data = df_decomp, cluster = ~ticker)

# Margin coefficient and clustered SE for each model.
cA <- unname(coef(m_decomp_A)["ebitda_margin_pos"])
cB <- unname(coef(m_decomp_B)["ebitda_margin_pos"])
cC <- unname(coef(m_decomp_C)["ebitda_margin_pos"])
seA <- unname(sqrt(diag(vcov(m_decomp_A)))["ebitda_margin_pos"])
seB <- unname(sqrt(diag(vcov(m_decomp_B)))["ebitda_margin_pos"])
seC <- unname(sqrt(diag(vcov(m_decomp_C)))["ebitda_margin_pos"])

# Validate the identity: coefficient A == coefficient B + coefficient C.
stopifnot("Decomposition identity A = B + C violated" =
            abs(cA - (cB + cC)) < 1e-6)

mech_pct    <- 100 * cB / cA   # mechanical share
nonmech_pct <- 100 * cC / cA   # non-mechanical (economic) share

# ---- Output table (modelsummary via save_table; HTML only) -------------------
models_decomp <- list(
  "(A) ln(EV/Sales)"                  = m_decomp_A,
  "(B) ln(EBITDA Margin), mechanical" = m_decomp_B,
  "(C) ln(EV/EBITDA), non-mechanical" = m_decomp_C
)

coef_decomp <- c(ebitda_margin_pos = "EBITDA Margin (profitable only)")

NOTES_DECOMP <- paste0(
  "Decomposition of the EBITDA-margin loading using the identity ",
  "ln(EV/Sales) = ln(EBITDA Margin) + ln(EV/EBITDA), so the margin coefficient ",
  "in column (A) equals the sum of columns (B) and (C): the mechanical part is ",
  sprintf("%.1f%%", mech_pct), " and the non-mechanical (economic) part is ",
  sprintf("%.1f%%", nonmech_pct),
  " of the total. All three models share the identical positive-EBITDA ",
  "subsample of profitable firms, the same controls (the EV/Sales specification ",
  "without the Loss Firm dummy, which is constant here) and subsector, country ",
  "and year fixed effects, with firm-clustered standard errors. The ",
  "non-mechanical part is conservatively biased downward by EBITDA measurement ",
  "error. * p<0.1, ** p<0.05, *** p<0.01."
)

save_table(models_decomp, coef_decomp, NOTES_DECOMP, "table_margin_decomposition")

# ---- Console summary ---------------------------------------------------------
star <- function(co, se) {
  p <- 2 * pnorm(-abs(co / se))
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}
cat("\n=== Margin-effect decomposition (ebitda_margin_pos) ===\n")
cat(sprintf("  (A) ln(EV/Sales)      : %8.4f (%.4f)%s\n", cA, seA, star(cA, seA)))
cat(sprintf("  (B) ln(EBITDA Margin) : %8.4f (%.4f)%s   [mechanical]\n",
            cB, seB, star(cB, seB)))
cat(sprintf("  (C) ln(EV/EBITDA)     : %8.4f (%.4f)%s   [non-mechanical]\n",
            cC, seC, star(cC, seC)))
cat(sprintf("  Identity check        : A = B + C  (|A-(B+C)| = %.2e)  PASSED\n",
            abs(cA - (cB + cC))))
cat(sprintf("  Mechanical share      : %.1f%%\n", mech_pct))
cat(sprintf("  Non-mechanical share  : %.1f%%\n", nonmech_pct))
cat("\nSaved output/tables/table_margin_decomposition.html\n")
