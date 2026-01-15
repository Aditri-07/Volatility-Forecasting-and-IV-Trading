# Volatility Forecasting and Implied Volatility Trading

This repository contains a research project that explores a hybrid framework for volatility-based options trading, combining forward-looking implied volatility with backward-looking time-series volatility forecasts.

The central objective of the project is to study whether systematic deviations between market-implied volatility and statistically forecasted volatility can be used to construct interpretable and risk-managed trading signals.

---

## Project Overview

Options markets embed forward-looking information through implied volatility, while historical return dynamics provide backward-looking estimates of volatility persistence and clustering. This project bridges the two by integrating:

- **Black–Scholes–Merton implied volatility**, extracted from market option prices
- **GARCH-based volatility forecasts**, estimated using univariate time-series models with skewed Student-t innovations
- **Regression-based mappings** between forecasted and observed implied volatility
- **Simulation-based trading logic**, designed to reflect realistic option market behavior

The framework is evaluated on **Apple (AAPL)** and **Walmart (WMT)** to contrast a high-beta growth stock with a more defensive equity.

---

## Methodology Summary

1. **Implied Volatility Extraction**
   - Implied volatilities are recovered from observed option prices using the Black–Scholes model.
   - Numerical root-finding methods are used to solve for volatility.

2. **Volatility Forecasting**
   - GARCH(1,1) models with skewed Student-t distributions are estimated on rolling windows of daily returns.
   - One-step-ahead volatility forecasts are generated to mimic real-time information availability.

3. **Regression Analysis**
   - Multiple regression specifications are explored:
     - Linear, logarithmic, and polynomial regressions
     - Generalized Linear Models (GLMs)
     - Extensions incorporating macro variables such as VIX and CPI
   - Models are evaluated using statistical significance, goodness-of-fit, and Gauss–Markov diagnostics.

4. **Trading Signal Construction**
   - Forecasted implied volatility is treated as a model-implied benchmark.
   - Trading signals are generated when observed implied volatility deviates beyond a dynamic threshold.
   - A stop-loss rule is embedded to control downside risk.

---

## Key Findings

- The relationship between historical volatility forecasts and implied volatility is **asset- and option-type dependent**.
- Linear relationships perform reasonably for liquid growth-stock calls (AAPL), while nonlinear dynamics emerge for defensive-stock puts (WMT).
- Many intuitive regression specifications fail statistically, highlighting the fragility of volatility predictability.
- Trading strategies based on volatility deviations produce **modest but stable performance**, emphasizing risk containment rather than aggressive alpha extraction.

Overall, the results suggest that volatility-based trading strategies require careful modeling, robust diagnostics, and disciplined risk management to avoid overfitting and false signal discovery.

---

## Repository Contents

- `paper/`  
  Contains the full project report with methodology, empirical results, and backtesting analysis.

- `analysis/` *(to be organised and expanded)*  
  Research notebooks used for volatility modeling, regression analysis, and trading simulations.

- `data/` *(sourced from bloomberg, yahoo finance, fed data, and yet to be organised)*  
  Market and options data sourced from Bloomberg, including equity prices, option chains, and implied volatility measures for AAPL and WMT

---

## Notes on Reproducibility

This repository is intended as a **research reference and methodological exploration** rather than a production-ready trading system.  
Data sources include equity prices, option prices, and macroeconomic indicators obtained from Bloomberg and public sources.

Future extensions may include:
- Transaction cost modeling
- Out-of-sample testing
- Delta-hedged volatility strategies
- Multivariate or regime-switching volatility models

---

## Author

**Aditri**  
M.S. Quantitative Finance  
Rutgers Business School
