# data/generate_demo_data.R
# ---------------------------------------------------------------------------
# Generate a synthetic `lse` dataset that exercises the same regression /
# variable-selection pipeline as the original analysis.
#
# *** SYNTHETIC DATA — NOT REAL MARKET DATA ***
# The original 2023 coursework used a London Stock Exchange basket supplied
# with the assignment (`lse.RData`), which was never committed and is no
# longer available. To keep the analysis code runnable end-to-end, this script
# generates a synthetic daily-price data frame with the SAME column structure
# (VOD as the response, plus a basket of LSE tickers, plus Date/Weekday/Year/
# Month). Numbers produced by the analysis on this data are illustrative only
# and will not match the report's original values.
#
# Methodology: correlated geometric Brownian motion with a deliberately
# induced factor structure so the basket exhibits multicollinearity and a
# strong VOD signal — i.e. the kind of behaviour the variable-selection and
# Box-Cox stages are designed to address.
#
# Usage:
#   Rscript data/generate_demo_data.R
# ---------------------------------------------------------------------------

suppressMessages({ library(dplyr); library(lubridate) })

OUT_FILE <- "data/lse.RData"

# Basket from the original analysis. VOD is the response; the rest are
# candidate predictors. Several (e.g. EXPN, SVT, SMT) are collinear by design.
predictors <- c(
  "STJ", "SPX", "AHT", "EXPN", "SSE", "SVT", "SMT",
  "LLOY", "SDR", "ABF", "BATS", "ENT", "RR", "SMIN",
  "ANTO", "BA", "PSN", "PRU", "CCH", "CPG", "WTB",
  "MGGT", "TSCO", "AUTO", "ABDN", "RTO", "RMV"
)
all_stocks <- c("VOD", predictors)
n_stocks   <- length(all_stocks)

# Generate ~5 years of daily data.
dates <- seq.Date(from = as.Date("2016-01-04"), to = as.Date("2020-12-31"), by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]   # trading days only
n_days <- length(dates)

set.seed(2023)

# --- factor structure ------------------------------------------------------
# Two latent market factors drive co-movement; each stock has its own loading
# plus an idiosyncratic component. VOD loads on both factors so it has
# meaningful correlation with the basket (the signal leaps/stepwise will find).
factor_loadings <- matrix(runif(n_stocks * 2, min = 0.2, max = 1.0), ncol = 2)
rownames(factor_loadings) <- all_stocks
# Give VOD a stronger baseline so it has a clear trend.
factor_loadings["VOD", ] <- c(0.9, 0.7)

factor1 <- cumsum(rnorm(n_days, 0, 0.005))   # slow bull market
factor2 <- cumsum(rnorm(n_days, 0, 0.004))   # secondary factor

# --- simulate prices via GBM -----------------------------------------------
# Prices start near plausible LSE levels (units = GBP).
start_prices <- setNames(runif(n_stocks, min = 100, max = 1500), all_stocks)

prices <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                 dimnames = list(as.character(dates), all_stocks))
prices[1, ] <- start_prices

for (t in seq_len(n_days - 1) + 1) {
  for (s in all_stocks) {
    drift <- 0.0002 + 0.0004 * factor_loadings[s, 1] * factor1[t] / max(abs(factor1))
    vol   <- 0.012
    f_contr <- factor_loadings[s, 1] * (factor1[t] - factor1[t - 1]) +
               factor_loadings[s, 2] * (factor2[t] - factor2[t - 1])
    shock <- rnorm(1, drift + f_contr * 0.5, vol)
    prices[t, s] <- prices[t - 1, s] * exp(shock)
  }
}

# Build the data frame to match the schema the analysis expects.
lse <- as.data.frame(prices)
lse$Date    <- dates
lse$Year    <- year(dates)
lse$Month   <- month(dates)
lse$Weekday <- weekdays(dates)

# Reorder so Date / Weekday / Year / Month come first (matches the original
# script's `subset(lse, select = -c(Date, Weekday))` pattern).
lse <- lse[, c("Date", "Weekday", "Year", "Month", all_stocks)]

dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
save(lse, file = OUT_FILE, compress = "xz")

cat(sprintf("Generated synthetic lse: %d rows x %d cols\n", nrow(lse), ncol(lse)))
cat(sprintf("Date range: %s to %s\n", min(lse$Date), max(lse$Date)))
cat("Columns:", paste(head(colnames(lse), 8), collapse = ", "), "...\n")
cat(sprintf("\nSaved: %s\n", OUT_FILE))
cat("\nNOTE: This is synthetic data. See header of this script for details.\n")
