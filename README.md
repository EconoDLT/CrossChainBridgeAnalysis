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
- Core Financial Metrics: `Revenue` (Total revenue of Renzo Protocol), `TVL0`, (EigenLayer TVL), `TVL1` (Renzo TVL on Ethereum), `TVL2` (Renzo TVL on L2s),   `Share` (ezETH share in the liquid restaking market), `Premium` (ezETH premium variable), `ETH` (ETH price).
- Yield Data: `Yield` (ezETH yield rate), `APY` (stETH APY as the benchmark DeFi yield).
- Market Sentiment: `FGI` (Fear and Greed Index).
- Network Control Variables: `GasPrice`.
- Dummy Variable For Tokenization Events: `Events`.
---
