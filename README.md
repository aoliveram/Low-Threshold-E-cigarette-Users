# Low-threshold e-cigarette users (ADVANCE)

Code for **"Low Threshold E-cigarette Users: An Updated Two-step Flow Model"** (manuscript in
preparation).

**Investigators**: Yuchan Cao, Kristina Miljkovic, Sarah E. Piombo, Aníbal Olivera Morales,
Jessica L. Barrington-Trimis, Thomas W. Valente.

The study asks whether exposure to e-cigarette content on social media is associated with starting
to use e-cigarettes when few or none of a student's friends have used them ("low-threshold"
adoption), as an updated two-step flow model would predict. Social media would then act as a
channel of influence that school friendship networks do not capture.

## Data

**ADVANCE** longitudinal panel of Southern California high-school students, surveyed twice a year
from 9th to 12th grade (W1–W10, fall 2020 – spring 2025). The analyses use grade semesters 3–8
(fall of 10th grade to spring of 12th grade). School codes: the first digit is the cohort (1 = class of 2024, 2 = class
of 2025), and the last two digits are the school, so 112 and 212 are the same high school. Friendship
nominations (up to seven same-grade schoolmates, from the roster or typed by hand) give a
time-varying network.

Restricted access (data-use agreement with the ADVANCE investigators). The data are not in this
repository, and git ignores them. To run the code, place the two workbooks here:

```text
Cleaned_Data/ADVANCE_W1-W8.xlsx
Cleaned_Data/ADVANCE_W9-W10.xlsx
```

Typed friend names are hashed when the data are read (script 00); raw names are never stored or
printed.

## Outcomes

Discrete-time event-history logistic regressions at the student-semester level. A transition
requires no use at *t* − 1, use at *t*, and no earlier reported use (grade 9 included).

- **A4 (primary)**: first observed transition into past-30-day e-cigarette use (any of 1–30 days).
- **A3 (secondary)**: first observed transition into past-six-month e-cigarette use.

## Paper specification

- **Peer exposure (E)**, measured at *t* − 1: the share of the friends named at *t* − 1 who had ever
  used e-cigarettes by *t* − 1. Every named own-school friend counts, whether or not they took the
  survey.
- **Friends whose use is not observed** get their expected probability of use:
  - friends on the roster who never took the survey: *q* = (*U* + 2*p*) / (*N* + 2), where *N*
    students in the survey name the friend, *U* of them had used, and *p* is the school × semester
    prevalence;
  - unmatched typed names: *p*;
  - participants who skipped the wave: their own earlier reports.
- **Students who named nobody**: E = 0, plus an indicator.
- **Social media exposure (S3)**, measured at *t* − 1: the mean of six platform scores (Facebook,
  Instagram, Twitter/X, YouTube, TikTok, Twitch), from 1 (never sees vaping content) to 5 (several
  times a day). Platforms used less than weekly count as "never"; at least four valid scores are
  required.
- **Covariates**:
  - sex, Hispanic/Latine, Asian, sexual minority and parent education, each from the first valid
    answer;
  - depressive symptoms (RCADS MDD) and out- and in-degree, at *t*;
  - cohort; grade-semester and school fixed effects.
- **Models**: logistic regressions with standard errors clustered by student:
  - the 25% threshold as types of starter (Table 2) and as a moderator (Table 3, Panel A);
  - continuous peer exposure (Table 3, Panel B).
- **R** = social media OR at E = 0 / social media OR at E = 50% = 1 / (interaction OR per 25 pp)².
  R > 1 means social media matters more when fewer friends had used. Its 95% CI and *p* also come
  from a cluster bootstrap (B = 1,000) that redraws the school × semester prevalences.

## Pipeline

Run every script from `codes/`, in order. Each one writes `../outputs_261023/<script>_<date-time>/`
(CSV tables and a log).

```bash
cd codes
Rscript 261023_00_prepare_slim_v2.R              # once: writes ../Cleaned_Data/ADVANCE_slim_v2.rds
Rscript 261023_01_main_recommended_models.R
Rscript 261023_02_robustness.R
Rscript 261023_03_simulation_imputation_power.R
Rscript 261023_04_descriptives_methods.R
Rscript 261103_06_scenarios_25pct_cut.R
Rscript 261120_07_contemporaneous.R
Rscript 261120_08_no_imputation.R
```

| Script | What it does | Manuscript |
| --- | --- | --- |
| `261023_functions_network_exposure.R` | shared functions: frame, exposure, imputation, models, bootstrap | — |
| `00_prepare_slim_v2` | reads the workbooks once; hashes typed names | — |
| `01_main_recommended_models` | interaction model, R and its bootstrap, predicted probabilities | Results (R, bootstrap); Table S3 |
| `02_robustness` | other estimates of unseen friends' use, leave-one-school/semester-out, counts of using and non-using friends | Table S1, Table S2 |
| `03_simulation_imputation_power` | known-truth simulation (expected value vs. multiple imputation) and power | Supplementary Methods |
| `04_descriptives_methods` | nominations by type, sample flow and missing social media, Cronbach's α | Methods |
| `06_scenarios_25pct_cut` | the 25% cut as types of starter (A), as a moderator (B), and continuous exposure (C) | Tables 1, 2, 3 |
| `07_contemporaneous` | Tables 2–3 refitted with social media and/or the network measured at *t* | Tables S4, S5 |
| `08_no_imputation` | unseen friends counted as non-users, or observed friends only | Table S1 |

There is no script 05: it was an exploratory check and is kept locally.

A full run takes about 10 minutes on 8 cores with fork-based parallelism (`parallel::mclapply`) and
writes under 1 MB. Where forking is not available, the scripts run on a single core and take longer;
the results are the same. A reference run of every
script is in `results/manuscript/<script>/`.

The multiple-imputation row of Table S1 and the simulation in script 03 use parallel random draws. A
rerun can differ from the reference in the third decimal; all other numbers reproduce exactly.

### Requirements

R 4.5 and the CRAN packages data.table, readxl, digest, stringi, sandwich, lmtest, MASS, nnet and
ggplot2.

```r
install.packages(c("data.table", "readxl", "digest", "stringi", "sandwich", "lmtest",
                   "MASS", "nnet", "ggplot2"))
```

## Repository structure

```text
.
├── codes/                 # paper pipeline (see above)
├── results/
│   └── manuscript/        # reference run: aggregated tables, logs and the figure
└── README.md
```

Local only (gitignored):

- `Cleaned_Data/` (data) and `outputs_*/` (run outputs);
- `docs/` (internal reports and manuscript drafts);
- `playground/` (exploratory scripts);
- `report-support/` (scripts behind the internal reports and supplement drafts);
- the exploratory work that led to the specification: the sweep of operationalizations
  (`codes/paneo/`, `results/paneo/`), diagnostics of the earlier pipeline
  (`codes/review-261015/`), the friendship–social media substitution analysis
  (`codes/substitution/`, `results/substitution/`), and earlier pipelines (`archive/`). These remain
  in the git history.
