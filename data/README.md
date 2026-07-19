# Data

> **Synthetic data.** The original 2023 coursework used a London Stock Exchange
> basket (`lse.RData`) supplied with the assignment. That file was never
> committed and is no longer available. `generate_demo_data.R` reproduces the
> schema with synthetic prices so the analysis runs end-to-end.

## `generate_demo_data.R`

```bash
Rscript data/generate_demo_data.R
```

Generates `data/lse.RData` (gitignored — re-run the script to regenerate).

### Methodology

Correlated **geometric Brownian motion** with a two-factor structure:

* Two latent "market" factors drive co-movement across the basket.
* Each stock loads on the two factors with its own loading plus an
  idiosyncratic noise component.
* `VOD` is given a stronger factor loading so it has a clear, predictable
  relationship with the rest of the basket — the kind of signal that
  variable selection will reliably find.

The synthetic data is not a faithful reproduction of any real period; it is
designed to exhibit the **qualitative features** the analysis code addresses
(multicollinearity, a strong signal, the need for response transformation).
Numbers produced from it will not match the original 2023 report.

### Schema

The generated `lse` data frame matches the schema the analysis code expects:

| Column | Type | Description |
|--------|------|-------------|
| `Date` | Date | Trading day |
| `Weekday` | character | Day-of-week name |
| `Year` | integer | Year |
| `Month` | integer | Month (1–12) |
| `VOD` | numeric | Vodafone share price (response) |
| 27 other tickers | numeric | LSE-listed stock prices used as predictors |

Tickers: `STJ, SPX, AHT, EXPN, SSE, SVT, SMT, LLOY, SDR, ABF, BATS, ENT, RR,
SMIN, ANTO, BA, PSN, PRU, CCH, CPG, WTB, MGGT, TSCO, AUTO, ABDN, RTO, RMV`.
