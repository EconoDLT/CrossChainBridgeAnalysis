# Connecting Distributed Ledgers: Surveying Novel Interoperability Solutions in On-chain Finance

This repository contains the collected data, the R code used, additional explanations and clarifications for the following research paper that analyzes the impact of cross-chain bridge protocols on on-chain financial services:

H. O. Sevim, “Connecting Distributed Ledgers: Surveying Novel Interoperability Solutions in On-chain Finance,” arXiv:2603.21797, 2026. https://arxiv.org/abs/2603.21797

***Abstract: This paper emphasizes the critical role of interoperability in enabling efficient and secure communication for the fragmented distributed ledger ecosystem, particularly within on-chain finance. The purpose of this study is to streamline and accelerate empirical research on the intersection of cross-chain interoperability solutions and their impact within on-chain finance. The analysis examines the relationship between financial use and interoperability while comparing the properties of novel cross-chain interoperability protocols (LayerZero, Wormhole, Connext, Chainlink Cross-Chain Interoperability Protocol, Circle Cross-chain Transfer Protocol, Hop Protocol, Across, Polkadot, and Cosmos), focusing on their design, mechanisms, consensus, and limitations. To encourage further empirical study, the paper proposes a set of network metrics and sample statistical models and provides a framework for evaluating the performance and financial implications of interoperability solutions.***

**Keywords:** Distributed Ledger Technologies, Blockchain, Cross-Chain Interoperability, Decentralized Finance

**Related Skills/Knowledge:** Interoperability, Distributed Systems, Qualitative Comparison, Econometrics, Statistical Modelling.

---

## Repository Structure

```
RestakingDynamics/
│
├── README.md
│
├── bridge_analysis.R                    # Code to analyze the data
│
└── data/                                # This folder includes multiple raw data files
```

---

## 1 — Code

### `bridge_analysis.R`

The script is written in R. Fundamental functions are:
- `feols()` for fixed effects regression.
- `lm()` for OLS regression with linear model.
- `vif()` to see the variance inflation factor, to avoid multicollinearity.
- `cor()` to calculate the correlation matrix among independent variables.
- `summary()` to calculate summary statistics.
- `mutate()` to create new variables. `log()` for logarihmic transformation, `asinh()` for inverse hyperbolic sine (a way to handle negative values like net volume in a log-like fashion).

**Configuration:** `data/` directory should be set to run the script.

---

## 2 — Data

The dataset includes the data of 3 cross-chain bridge protocols (Stargate, Across v2, Hop Protocol) across 6 blockchains (Ethereum, Avalanche, Binance Smart Chain, Polygon, Arbitrum and Optimism).
Data has a daily frequency and covers the period: 25 May 2022 to 21 February 2024. The period covers the days on which all three protocols have full data availability.

Separate raw data files are uploaded to the `data/` folder. The source of protocol data is The Graph. ETH and POL price data are collected from Etherscan and Polygonscan.

The following variables are included in the dataset:
- Core Financial Metrics: `Revenue` (total revenue of protocol), `TVL`, (total value locked in the smart contracts of bridge protocol), `Volume` (USD value of transferred crypto-assets through bridge protocol).
- Control Variables: `TxCount` (number of transactions made through bridge protocol), `GasPrice` (network volatility is controlled), `ETH` (ETH price as the benchmark asset used in DeFi).

---

Contact for more details and clarifications: hasretozan.sevim@unicatt.it
