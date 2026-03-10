# EDI Recruitment Bias Monitoring in Type 2 Diabetes Clinical Trials

**GitHub:** github.com/sm-hasanulbari | **Contact:** dr.hasanulbari@gmail.com  
**Stack:** Python 3.13 · Pandas · Scikit-learn · Matplotlib · R Shiny (in development)  
**Developed as:** Portfolio project for EDICT MSCA Doctoral Network application (2026)

---

## Overview

This repository implements a three-strand analytical framework for monitoring
equity, diversity and inclusion (EDI) in clinical trial recruitment, with a
focus on European Type 2 Diabetes trials.

| Strand | Data | Method | Key Finding |
|--------|------|--------|-------------|
| 1 — Trial Landscape | ClinicalTrials.gov API v2 (real, n=856) | 28-flag EDI scoring instrument | Mean EDI 37.5/100; 76% of trials below threshold |
| 2 — Drift Detection | Synthetic simulation (5 EU trials, 52 weeks) | CUSUM · EWMA · Rolling Z-test | Female enrollment drifts −5.0pp; CUSUM alarm at Week 35 |
| 3 — AI Fairness | Derived from synthetic data | Disparate Impact · EOD · Calibration | DIR minority 0.932 (borderline) |

---

## Methods

### Strand 1: Trial Registry Analysis
- **Data source:** ClinicalTrials.gov API v2, query: Type 2 Diabetes, Completed, Interventional
- **EDI scoring:** 28 binary flags across 6 domains (eligibility criteria, reporting, population targets, site accessibility, language provision, diversity planning). Composite score 0–100.
- **PRISMA selection:** 7,544 fetched → 856 included after completeness filtering
- **Limitation:** EDI scores are proxy indicators from structured registry fields, not validated against actual enrollment demographics. Ross et al. (2009) NEJM documented systematic incompleteness in ClinicalTrials.gov entries.

### Strand 2: Within-Trial Drift Detection
- **Data:** Synthetic participant-level data (n ≈ 4,000 enrolled) across 5 simulated European trials over 52 weeks
- **Drift model:** Linear ramp from EDI-ambitious targets to published historical means (Steinberg et al. 2021 Diabetes Care 44(7):1524-32) over 9 weeks post onset, then stable — modelling operational recruitment fatigue (Fouad et al. 2014; Linden et al. 2017)
- **Algorithms:** CUSUM (Page 1954), EWMA (Roberts 1959), Rolling two-proportion z-test
- **Calibration:** μ₀ from observed baseline (Weeks 1–25); K = 3pp, H = 20pp (Montgomery 2009)
- **Limitation:** Drift timing is a modelling assumption. No empirical study directly quantifies the within-trial temporal inflection point.

### Strand 3: AI Fairness Metrics
- **Process fairness:** Disparate Impact Ratio (Feldman et al. 2015 KDD), Statistical Parity Difference (Dwork et al. 2012), Intersectional fairness (Kearns et al. 2018)
- **Model fairness:** Logistic regression (5-fold CV), ROC by subgroup (Obermeyer et al. 2019), Equal Opportunity Difference (Hardt et al. 2016 NeurIPS), Calibration (Chouldechova 2017)
- **Limitation:** All fairness metrics derived from synthetic data. AUC reflects simulated patterns, not real clinical screening decisions.

---

## Repository Structure
```
diabetes-trial-edi-equity/
└── edi-recruitment-bias-monitoring/
    └── python/
        ├── recruitment_bias_monitor.ipynb   # Main notebook
        ├── diabetes_trials_edi_scored.csv   # 856 trials with EDI scores
        ├── diabetes_trials_edi_europe.csv   # 311 European trials
        ├── synthetic_participants.csv       # Simulated participant data
        └── edi_key_metrics_summary.csv      # All key metrics
```

---

## Key References
- Steinberg JR et al. (2021) Diabetes Care 44(7):1524-32 [PMID 34078623]
- Page ES (1954) Biometrika 41(1/2):100-115
- Hardt M, Price E, Srebro N (2016) NeurIPS [arXiv:1610.02413]
- Feldman M et al. (2015) KDD [doi:10.1145/2783258.2783311]
- Chouldechova A (2017) Big Data 5(2):153-163 [PMID 28632438]
- Obermeyer Z et al. (2019) Science 366:447-53 [PMID 31649194]
- Kearns M et al. (2018) ICML [arXiv:1711.05144]
- Ross JS et al. (2009) NEJM 361(3):268-73 [PMID 19605831]
