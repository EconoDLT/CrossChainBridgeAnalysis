# CrossChainBridgeAnalysis
**Interoperability, Bridge Protocols, Surveying, and Statistical Modelling to Examine the Impact of Bridge Protocols on DeFi/On-Chain Finance**

This repository contains the collected data, the R code used, additional explanations and clarifications for the research paper that analyzes the impact of cross-chain bridge protocols on on-chain financial services.

**Related Skills/Knowledge:** Interoperability, Distributed Systems, Qualitative Comparison, Econometrics, Statistical Modelling.

---

## [REDACTED - PAPER TITLE]

`{CITATION]`

[ABSTRACT - KEY WORDS]

---

## Repository Structure

```
RestakingDynamics/
│
├── README.md
│
├── bridge_analysis.py                    # Code to analyze the data
│
└── data/                                # This folder includes multiple raw data files
```

---

## 1 — Code

### `bridge_analysis.py`

The script is written in Python. Fundamental functions are:
- `lm()` for OLS regression.
- `VAR()` and `causality()` for Granger-causality test.
- `randomForest()`, `importance()` and `varImpPlot()` for random forest feature importance test.
- `anova()` for Chow‑type structural break test
- `residuals()` to see the regression residuals.
- `vif()` to see the variance inflation factor, to avoid multicollinearity.

---

## 2 — Data

The dataset includes the data of 3 cross-chain bridge protocols (Stargate, Across v2, Hop Protocol) across 6 blockchains (Ethereum, Avalanche, Binance Smart Chain, Polygon, Arbitrum and Optimism).
Data has a daily frequency and covers the period: 5 May 2022 to 21 February 2024. The period covers the days on which all three protocols have full data availability.

Separate raw data files are uploaded to the `data/` folder. The source of protocol data is The Graph. ETH and POL price data are collected from Etherscan and Polygonscan.

The following variables are included in the dataset:
- Core Financial Metrics: `Revenue` (total revenue of protocol), `TVL`, (total value locked in the smart contracts of bridge protocol), `Volume` (USD value of transferred crypto-assets through bridge protocol).
- Control Variables: `TxCount` (number of transactions made through bridge protocol), `GasPrice` (network volatility is controlled), `ETH` (ETH price as the benchmark asset used in DeFi).

---
