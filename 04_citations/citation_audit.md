# Citation Audit — 390 Paper
## Generated 2026-03-16

### Answers to Preliminary Questions

**Is the 20-day gap from the quiet period?**
No. It's from the **IBES Summary monthly refresh cycle** (`statpers` = Thursday before 3rd Friday of each month). The calculation is calendar days: `earnings_date - consensus_date`. Reg FD quiet period (1-3 weeks) is a secondary factor. If you switched to IBES Detail (individual analyst estimates, which update in real-time), the gap would shrink substantially.

**Trading days or calendar days?**
Calendar days. The R code uses `as.numeric(earnings_date - consensus_date)`, which counts calendar days including weekends/holidays. In trading days, the 20-day median would be ~14 trading days.

---

## ERRORS FOUND IN PAPER

### Error 1: "Betfair" should be "Intrade" (line 93)
Paper says: "using data from the Betfair political prediction market"
Rothschild & Sethi (2016) used **Intrade**, not Betfair. Betfair is mentioned only for cross-market arbitrage comparisons. Fix this.

### Error 2: Livnat & Mendenhall misattribution (line 105)
Paper says: "extended the finding to revenue surprises"
Livnat & Mendenhall (2006) is about **analyst vs. time-series surprise computation**, not revenue surprises. The revenue surprise paper is **Jegadeesh & Livnat (2006)**, "Revenue Surprises and Stock Returns," FAJ 62(2), 22-34. Either fix the description or swap the citation.

### Error 3: Easley & O'Hara PIN attribution (line 111)
Paper attributes "probability of informed trading (PIN)" to Easley & O'Hara (1987). PIN was formally developed in **Easley, Kiefer, O'Hara & Paperman (1996)**, JF 51(4). The 1987 paper is the conceptual precursor (trade size carries information) but not the PIN paper. Minor — fix the description.

---

## CITATIONS TO DROP (4)

| Key | Why Drop |
|-----|----------|
| `roll1984simple` | Roll (1984) is about bid-ask spread estimation from serial covariance. Has nothing to do with VWAP. No citation needed for VWAP — it's standard. |
| `goyal2009cross` | Cited for trivial liquidity filters ($500 min volume, 20 min trades) that don't even bind in the sample. Goyal & Saretto is about option returns. Academic credentialism. |
| `barber2008all` | About attention-driven buying by retail investors. The wallet archetype being described is overconfidence/skill transfer, not attention. Wrong paper for the claim. |
| `coval2005asset` | Purely rhetorical ("a pattern Coval & Shumway would recognize"). Their paper is about loss aversion in CBOT floor traders, not miscalibrated self-assessment. Name-drop. |

---

## CITATIONS TO KEEP — with specific borrowed claims

### Prediction Market Theory & Accuracy

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 1 | `wolfers2004prediction` | "Simple markets can aggregate disperse information into efficient forecasts of uncertain future events." Markets outperform moderately sophisticated benchmarks. | [AEA/JEP](https://www.aeaweb.org/articles?id=10.1257/0895330041371321) |
| 2 | `arrow2008promise` | 22 economists argue prediction markets "aggregate dispersed beliefs into market prices." IEM erred 1.5pp vs Gallup's 2.1pp. Note: does NOT use "incentive compatibility" language — that's Hayek. This is a prestige/advocacy piece in Science. | [Science](https://www.science.org/doi/10.1126/science.1157679) |
| 3 | `forsythe1992anatomy` | Origin of the **marginal trader hypothesis**: "judgment bias refers to average behavior, while in markets it is marginal traders who influence price." Political stock market accuracy driven by small informed cohort, not median participant. | [AER/JSTOR](https://www.jstor.org/stable/2117471) |
| 4 | `page2012prediction` | Prediction markets are well-calibrated at short horizons but show favourite-longshot bias at longer horizons. Prior calibration work focused on political/macro events. Positions your paper as extending to corporate earnings. | [Economic Journal/Wiley](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1468-0297.2012.02561.x) |
| 5 | `gomezcram2025financial` | Concurrent work: Polymarket earnings markets are ~68% accurate 1 week before earnings, 77% day-before vs. 62% for analyst consensus (Sep-Nov 2025 window). Must cite — closest concurrent paper. | [SSRN 5933475](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5933475) |
| 6 | `rothschild2016trading` | Identified "rich ecology" of trading strategies on **Intrade** (NOT Betfair — fix line 93). 6,300 accounts; market accuracy driven by heterogeneous strategies, not uniform wisdom. Also documented single manipulator who lost ~$4M. | [SSRN 2322420](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2322420) |

### Analyst Forecasting & Institutional Bias

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 7 | `richardson2004walk` | Analysts issue optimistic initial forecasts, then systematically revise downward so firms can beat. The walk-down is strongest for net equity issuers and insider sellers. Central to your "beatable floor" argument. | [Wiley/CAR](https://onlinelibrary.wiley.com/doi/abs/10.1506/KHNW-PJYL-ADUB-0RP6) |
| 8 | `matsumoto2002management` | Managers avoid negative surprises via (1) upward earnings management through accruals and (2) downward guidance of analyst expectations. Complements Richardson — management side of the walk-down. | [AAA/Accounting Review](https://doi.org/10.2308/accr.2002.77.3.483) |
| 9 | `ke2006effect` | Analysts who walk down (optimistic early → pessimistic late) get better management access and survive longer. Analysts who stay too optimistic lose access. Completes the walk-down triad: why analysts participate. | [Wiley/JAR](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1475-679X.2006.00221.x) |
| 10 | `hong2000security` | Career-concerns model: inexperienced analysts herd toward consensus because bold deviations are penalized. Experienced analysts deviate more. Explains why consensus suppresses private information. | [Columbia PDF](http://www.columbia.edu/~hh2679/rje-analyst.pdf) |
| 11 | `heflin2003regulation` | Reg FD (2000) prohibited selective disclosure, creating a practical quiet period before earnings. Cited for the regulation's existence more than the paper's specific findings. | [ScienceDirect/JAE](https://doi.org/10.1016/S0165-4101(02)00098-9) |

### Crowdsourced Earnings Forecasts

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 12 | `jame2016value` | 51,012 Estimize forecasts are "incrementally useful in forecasting earnings." With 5+ contributors, Estimize dominates IBES consensus in explaining announcement returns. Most direct prior work on crowd vs. analyst accuracy. | [Wiley/JAR](https://onlinelibrary.wiley.com/doi/abs/10.1111/1475-679X.12121) |
| 13 | `schafhautle2024crowdsourced` | Estimize coverage reduces mispricing of analyst bias and lowers earnings announcement premia. Managers respond to crowdsourced coverage with more downward guidance. Main contribution is market efficiency, not raw accuracy — your description could be tighter. | [AAA/Accounting Review](https://doi.org/10.2308/TAR-2022-0151) |

### PEAD & Returns

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 14 | `ball1968empirical` | First documentation that prices drift in the direction of earnings surprise for weeks post-announcement. Foundational PEAD. | [JSTOR](https://doi.org/10.2307/2490232) |
| 15 | `bernard1989post` | Confirmed PEAD as robust anomaly: top-bottom SUE decile spread positive in 41/48 quarters. Drift is delayed price response, not risk premium. | [JSTOR](https://doi.org/10.2307/2491062) |
| 16 | `martineau2022pead` | Unconditional PEAD has disappeared in non-microcap stocks since ~2006. Motivates your conditional PEAD finding: residual drift exists only in prediction-market-identified subsets. | [CFR](https://doi.org/10.1561/104.00000122) |
| 17 | `livnat2006comparing` | PEAD is larger when surprise is computed from analyst forecasts vs. time-series models. **NOT about revenue surprises** — fix the description or swap to Jegadeesh & Livnat (2006). | [Wiley/JAR](https://doi.org/10.1111/j.1475-679X.2006.00196.x) |
| 18 | `shleifer1997limits` | Arbitrage requires capital and carries risk. Short-sale constraints (borrowing costs, margin calls, fiduciary restrictions) make arbitrage least effective when mispricing is greatest. Explains your short-side alpha persistence. | [Wiley/JF](https://doi.org/10.1111/j.1540-6261.1997.tb03807.x) |

### Microstructure

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 19 | `kyle1985continuous` | Equilibrium model: informed insider, noise traders, competitive market maker. Prices are linear function of aggregate order flow (lambda = price impact). Private information revealed through flow. Foundation for your flow imbalance measure. | [JSTOR/Econometrica](https://www.jstor.org/stable/1913210) |
| 20 | `easley1987price` | Trade size carries information about informed trading: informed traders prefer larger sizes, creating adverse selection. Conceptual precursor to PIN (formalized in Easley, Kiefer, O'Hara & Paperman 1996 — **fix attribution in paper**). | [ScienceDirect](https://doi.org/10.1016/0304-405X(87)90029-8) |
| 21 | `easley2012flow` | Introduces VPIN (Volume-Synchronized Probability of Informed Trading) and "toxic flow" concept. Flow that carries adverse selection risk for liquidity providers. Connects your flow imbalance to established HFT microstructure literature. | [Oxford/RFS](https://doi.org/10.1093/rfs/hhs053) |
| 22 | `lee1991inferring` | Lee-Ready algorithm: classify trades as buyer/seller-initiated using quote test (vs. bid-ask midpoint) + tick test (uptick/downtick). Your four-cell decomposition is described as an analogy. | [Wiley/JF](https://doi.org/10.1111/j.1540-6261.1991.tb02683.x) |
| 23 | `menkveld2013high` | HFT market maker on Chi-X: 75% participation rate, loss on inventory but profit on spread, near-zero net PnL. Analogy for your Archetype B wallet (algorithmic liquidity provider). | [ScienceDirect](https://doi.org/10.1016/j.finmar.2013.06.006) |

### Other (Foundational / Methods)

| # | Key | Specific Claim Borrowed | URL |
|---|-----|------------------------|-----|
| 24 | `hayek1945use` | "The economic problem of society is the utilization of knowledge which is not given to anyone in its totality." Price system communicates information for decentralized coordination. General intellectual foundation. | [JSTOR](https://www.jstor.org/stable/1809376) |
| 25 | `brier1950verification` | Defines the Brier score: BS = (1/N) * SUM[(f_i - o_i)^2]. Your primary accuracy metric. | [AMS](https://journals.ametsoc.org/view/journals/mwre/78/1/1520-0493_1950_078_0001_vofeit_2_0_co_2.xml) |
| 26 | `fama1993common` | Three-factor model: MKT + SMB + HML. Used for risk-adjusting your short-side strategy returns. Standard. | [ScienceDirect](https://doi.org/10.1016/0304-405X(93)90023-5) |

### Borderline Keep

| # | Key | Specific Claim Borrowed | URL | Note |
|---|-----|------------------------|-----|------|
| 27 | `tetlock2015superforecasting` | "Superforecasters" = small cohort with outsized accuracy. Connects to marginal trader hypothesis from a different domain. Does real argumentative work linking your wallet analysis to forecasting literature. Consider swapping for Mellers et al. (2014) in Psychological Science if reviewer objects to citing a popular book. | [Amazon](https://www.amazon.com/Superforecasting-Science-Prediction-Philip-Tetlock/dp/0804136718) |
| 28 | `daniel1998investor` | Overconfidence model: investors overreact to private signals, underreact to public. Used loosely for wallet archetype showing overconfidence in skill transfer. Defensible but doing rhetorical work more than empirical. | [Wiley/JF](https://doi.org/10.1111/0022-1082.00077) |
| 29 | `skinner1994earnings` | Firms disclose bad news early to reduce litigation risk. Cited for beat/miss return asymmetry, but Skinner is really about disclosure timing, not return asymmetry per se. Consider whether Ball & Brown (1968) or Bernard & Thomas (1989) already cover this. | [JSTOR/JAR](https://www.jstor.org/stable/2491338) |

---

## Final Count

- **Essential (keep as-is):** 23 citations
- **Borderline (keep but optional):** 3 citations
- **Drop:** 4 citations (Roll, Goyal & Saretto, Barber & Odean, Coval & Shumway)
- **Errors to fix:** 3 (Betfair→Intrade, Livnat & Mendenhall description, PIN attribution)

## Bib entries in references.bib NOT cited in main.tex

Check whether these are actually used — they appear in the .bib but may not have \citep/\citet calls:
- `fama1970efficient`
- `diercks2026prediction`
- `snowberg2013prediction`
- `hasbrouck1995one`
- `manski2006interpreting`
- `tetlock2008liquidity`
- `chen2014wisdom`
- `baker2016measuring`
- `da2011search`
- `cameron2011robust`
- `granger1969investigating`
- `leigh2006competing`
- `bliss2004option`
- `cotter2006expectations`
- `ng2026price`
- `reichenbach2025polymarket`

These can be cleaned from the .bib file if unused.

---

## Text Cut from Lit Review (2026-03-17 revision)

### Hong herding paragraph (was line 97):
"\citet{hong2000security} provide a career-concerns model of analyst herding: because deviating from the consensus is costly to analysts' reputations, individual forecasts cluster around the group mean, suppressing private information. When the consensus converges on an overly optimistic estimate, no individual analyst signals the miss, and the subsequent correction is larger. The prediction market has no such incentive structure: pseudonymous traders profit from accuracy, not from maintaining relationships with investor relations departments."

### "Interaction" paragraph (was line 99):
"The interaction between analyst bias and prediction market accuracy is central to this paper. The crowd draws on the same public information (guidance, channel checks, macro data) but processes it through an incentive structure that rewards accuracy rather than relationship maintenance. The crowd's accuracy is therefore a test of whether incentive-compatible pricing corrects the institutional biases embedded in the sell-side consensus."

### Microstructure of Informed Trading subsection (was lines 107-113):
"The seminal models of \citet{kyle1985continuous} and \citet{easley1987price} establish that order flow carries private information. In Kyle's framework, a single informed trader strategically conceals private information by splitting orders over time, and the market maker infers information from the aggregate order flow. \citet{easley1987price} introduce the probability of informed trading (PIN) as a measure of information asymmetry in order flow. The concept of \textit{toxic flow}---order flow that carries adverse selection risk for liquidity providers---has become central to modern market microstructure \citep{easley2012flow}.

These frameworks were developed for equity markets, but their logic applies with equal force to prediction markets. In a prediction market, a trader who buys a large number of ``yes'' tokens at \$0.25 (implying the market prices the event at 25\% probability) is either uninformed and overpaying, or informed and exploiting a mispricing. The four-cell flow decomposition developed in this paper---which classifies each trade by both direction (buy/sell) and token type (yes/no)---is a direct analog of the Lee--Ready trade classification algorithm \citep{lee1991inferring} adapted for the binary contract structure of prediction markets. The flow imbalance variable measures the net directional pressure from informed participants, and its predictive power for earnings outcomes ($z = 7.47$) is evidence that prediction market order flow, like equity order flow, carries private information beyond what is reflected in the price level.

\citet{gomezcram2025financial} study Polymarket earnings markets using API price snapshots and document crowd accuracy relative to analysts. The present paper extends their analysis in two dimensions. First, the Dome API provides trade-level data with directional classification (buy/sell $\times$ yes/no), which the Polymarket REST API's price snapshots do not. This allows the construction of flow imbalance, the separation of price information from flow information, and the identification of toxic flow. Second, because every settled trade is recorded on the Polygon blockchain with a persistent wallet address, I can construct wallet-level trading histories and directly test the marginal trader hypothesis, a test that is not possible with aggregated price data."
