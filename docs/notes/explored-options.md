# Explored options: what has been tried, and what it showed

The record of every model and operationalization tried for this paper, from April to October 2026.
Check here before trying a new specification: if it is listed, the result and the script are already known.

- Codes (A1, U3, N4, …) are the ones used in the spec IDs of `results/paneo/` (legend in §3).
- Marks: **★★** central finding · **★** something worth saying · **○** tried, no relevant change ·
  **–** not tried yet.
- The detailed working documents (Spanish) are kept locally in `docs/paneo/` and are not in git.

## 1. Timeline

| When | What | Code |
|:--|:--|:--|
| Apr–May | First pipelines: one diffusion network per school, pooled; quasibinomial threshold regression; exposure (hazard) regression; adopter classes; network plots | `archive/pipelines-2604-2607/0419_…`, `260427_…`, `0512_…` |
| May 12 | Replication and extensions: threshold data rebuilt; degree diagnostics; degree-weighted grouped-binomial threshold model | `archive/threshold-extensions-2605-2606/01–06` |
| Jun 3 | Susceptibility and infectiousness models; 18-model social-media sweep (6 measures × 3 subgroups); subgroup-membership tables | `archive/pipelines-2604-2607/260603_…` |
| Jun 12–13 | Mixed-effects hazard with school fixed-effects sensitivity; grouped-binomial threshold; susceptibility retired | `archive/pipelines-2604-2607/260612_…revised13`, `260613_…revised16` |
| Jun 15 | Corrections: Firth threshold model (separation), social-media power, fully corrected tables | `archive/threshold-extensions-2605-2606/07–09` |
| Jul 4 | Firth threshold, social-media average (S1), lagged social-media models: the line that led to the submitted manuscript | `archive/pipelines-2604-2607/260704_…` |
| Oct 15 | Baseline: the submitted Table 2 models plus a continuous interaction | `codes/261015_…` |
| Oct 19–21 | Code review of the baseline (§5); friendship–social-media substitution | `codes/review-261015/`, `codes/substitution/` |
| Oct 22–23 | Sweep of operationalizations and models: layers 1–3, stage 2, verdict | `codes/paneo/` |
| Oct 23 | Recommended models (A3 and A4) with robustness, simulations and descriptives | `codes/261023_00–04` |

The June performance rewrites (`260617_…efficient*`) changed speed, not specifications, and are not archived.

## 2. Before the sweep (April–July)

| Model or check | Mark | What it showed |
|:--|:-:|:--|
| Threshold as a quasibinomial on the adopter's share of using friends | ★ | Over-weights low-degree adopters, whose threshold lives on a coarse grid (0, 1/2, 1/3, …); replaced by the grouped binomial |
| Threshold as a degree-weighted grouped binomial | ★★ | The grade-period block is completely separated: every adopter in the first period has k = 0 by construction (149 of 149), and school 105 too. Coefficients of 10⁷–10⁸ are artifacts |
| Fix: drop first-period adopters | ○ | Same substantive odds ratios to 3 decimals; school 105 still separated |
| Fix: Firth penalized fit (`brglm2`) | ★★ | Removes both separations; threshold rises steeply across grade periods. Adopted in July |
| Adoption hazard: mixed effects (student and school) vs. school fixed effects | ○ | Same signs and significance; subject-specific vs. population-average odds ratios |
| Susceptibility and infectiousness (quasibinomial) | ○ | Retired from scope in June |
| Subgroup membership ("no exposure", "no perceived friends", "neither", "low threshold" among users; three groups among non-users) | ★ | The two significant social-media cells point in opposite directions (no exposure 2.31; no perceived friends 0.40) |
| 18-model social-media sweep (6 measures × 3 subgroups) | ★ | Best p = 0.063 with no multiplicity control: compatible with the null |
| Social-media power | ★★ | Items exist only in waves 4–8, so only 456 of 841 adopters have a value at adoption |
| "No exposure" social-media cell under Firth and baseline-only confounders | ★ | Survives Firth (2.12, p = .021) but falls to ≈1.5 (p ≈ .10) with baseline-only confounders; adjusting for friends' use measured at adoption is post-treatment |
| Hispanic threshold "paradox" | ○ | Absorbed by school fixed effects |
| Parent education "don't know" (9) as missing | ○ | Done in June; homogeneous 1–6 scale |
| Perceived friends' use sensitivity and degree histograms (July draft) | ○ | Made for an unsubmitted draft; not kept |

## 3. Legend of the sweep (October)

**Spec IDs.** `L2_###_<base>_<dim>_<option>` (layer 2: one change from base R0 or H1),
`L3_###_<A>_<U>_<T>_<N_I>_<Z>_<SM>` (layer 3: full product), `S2_####_<stage>_…` (stage 2: `cuts`, `U`,
`qgrid`), `V_###_<base>_<dim>_<option>` (verdict runs from R0 or S*). Quantities: `R` (see below),
`OR_SM`, `OR_peer25` (per 25 points of exposure), `OR_SM_low@<cut>` / `OR_SM_high@<cut>`,
`OR_interaction`, `OR_Uc` / `OR_NUc` (using and non-using friends), `slope_naive` / `slope_corrected`.

**R** = odds ratio of social media with low peer exposure ÷ odds ratio with high exposure.
R > 1 means social media matters more when friends do not use (substitution); R = 1, additive channels;
R < 1, social media amplifies peers.

**Labels** in the `label` column: D defensible, Disc debatable, I incorrect (included on purpose, to see
whether a wrong specification produced the paper's story).

### 3.1 Named specifications

| Spec | Definition |
|:--|:--|
| H1–H5 | The submitted manuscript's Table 2 (H1), Table 3 (H2) and supplementary tables S1–S3 (H3–H5) |
| H6 | The October 15 continuous interaction (OR 1.028, p = .644) |
| R0 | Reference: A3, U1, T3, N4 + I6, Z3, S3 lagged, X2, F2, model M-B |
| S* | A4 with I3 + I5 (deterministic prevalence imputation); otherwise as R0 |
| S** | A4 with I6 (homophily imputation); otherwise as R0 |

### 3.2 Models

| Code | Model | Mark | What it showed |
|:--|:--|:-:|:--|
| M-0 | Additive: adoption ~ social media + peer exposure | ★ | Social media ↔ any adoption ≈ 1.4–1.8 |
| M-A | Event type (the paper): logit of "adopts with exposure ≤ cut", other adopters as 0 | ★★ | Robust (288/288 specs) but not specific: social media predicts any adoption equally (288/288) |
| M-B | Interaction: any adoption ~ social media × exposure; R between exposure 0 and 50% | ★★ | The direct test. S** R = 1.72, p = .020 |
| M-C | Stratified: social-media effect at low vs. high exposure | ★ | Same direction (1.38), not significant |
| M-D | Multinomial competing risks (no adoption / low / high) | ★ | R < 1 often; confounded |
| M-E | Threshold among adopters (naive and corrected) | ○ | Null |
| M-F | Counts: social media × users + social media × non-users | ★ | Non-using friends protective (S**: OR 0.87, p = .018), partly an isolates artifact (Z1) |
| M-G | Friendship substitution (does social-media use change when friends are gained or lost?) | ★★ | No substitution (§4) |

### 3.3 Options by decision

**D1. Outcome (A)**

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| A1 | First observed use, even if already using (the paper) | I | ★ | 13–14% of events are prevalent users or re-starts; association barely moves |
| A2 | Observed non-use → use between consecutive waves | D | ○ | ≈ A3 |
| A3 | A2 without use before the window (past 6 months) | D | ★★ | Co-primary: R = 1.41, p = .118 |
| A4 | As A3 with past-30-day use | D | ★★ | Co-primary: R = 1.72, p = .020 |
| A5 | Sustained adoption (uses at t and t + 1) | D | ★ | R ≈ 1.8–2.1, but only 65–169 events |

**D2. When a friend counts as a user (U)**

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| U1 | Ever used within the window (Valente standard) | D | ★★ | Recommended; substitution direction stable (MB median 1.11) |
| U2 | Ever used, including before the window | D | ○ | ≈ U1 |
| U3 | Used in the past 6 months at t − 1 | D | ★ | R back to ≈ 1: what social media "replaces" is the friends' history of use |
| U4 | Used in the past 30 days at t − 1 | D | ★ | R ≈ 0.88 |
| U5 | Ego's perception of friends' use (not network) | Disc | ★ | Weaker association (1.25); possible mediator |

**D3. Timing of exposure (T)**

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| T1 | Network and status at t, counting same-period co-adopters (paper's S1–S3) | I | ★ | Counts friends who start at the same time as "previous" |
| T2 | Network at t, status at t − 1 | D | ○ | ≈ T3 |
| T3 | Network and status at t − 1 (paper's Tables 2–3) | D | ○ | Kept |
| T4 | T3 + count of same-period co-adopters | D | – | |

**D4–D5. Who the friends are, and what is assumed about unseen friends (N, I)**

47% of named friendships point to students who are not observed that wave.

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| N1 | Only friends visible in the school network (the paper) | Disc | ★ | Ignores 47% of named friends |
| N2 | Only participants who answered that wave | Disc | ○ | ≈ N1 |
| N3 | All named roster friends | D | ○ | ≈ N4 |
| N4 | All named friends, roster and typed | D | ★★ | Recommended denominator |
| I0 | Unseen friends = non-users (implicit in the paper; lower bound) | I | ★ | S* R 1.85 (p = .051) |
| I1 | Unseen friends = users (upper bound) | – | ★ | Removes the effect |
| I2 | Constant share q of unseen friends use (grid 0–1) | D | ★★ | Significant or borderline up to q ≈ 0.12 (observed prevalence); R > 1 up to q ≈ 0.3 |
| I3 | q = prevalence by school × wave (also by school, by wave) | D | ★ | S* 1.72 / 1.78 / 1.58 (p .047 / .040 / .080) |
| I4 | Prevalence × 1.25, 1.5 or 2 | D | ○ | Slightly weaker (p .050–.073) |
| I5 | Participants missing a wave: carry their last or next status | D | ○ | Used with I3 (I35 = I3 + I5) |
| I6 | Homophily: share of users among those who name the unseen friend, shrunk to school prevalence (expected value) | D | ★★ | Recommended, with bootstrap. A logistic network model gives almost the same |
| I7 | Multiple imputation (I5 + I6, I3 for typed), 20 draws, Rubin's rules | D | ★★ | Biased toward the null here: in a known-truth simulation (true R 1.7) MI gives 1.54–1.60 and halves power; expected value gives 1.78–1.87 with 91–96% coverage |

**Typed (hand-written) friends (W):** W0 not grade friends (out of the denominator), W1 friends who do not use,
W2 friends who use, W3 like an average participant: W0, W1 and W3 do not change R (○); only W2 moves it (★).
Matching typed names to the roster would add little. W4 (partial match) – not tried.

**D6. Students with no visible friends (Z)**

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| Z1 | Exposure 0 for all of them (the paper) | I | ★★ | Produces part of the "non-using friends protect" effect |
| Z2 | Excluded | Disc | ★ | Protective effect no longer significant with the paper's network |
| Z3 | "Named nobody" = 0 plus own indicator; the rest imputed | D | ★ | Recommended. 78% of students who name nobody report close friends (`sni_1`) |
| Z4 | "Named nobody" = missing; the rest imputed | D | ○ | ≈ Z2 |
| Z5 | Egos missing at t − 1: use their network at t | D | – | |

**D7–D8. Form of exposure and the low-threshold cut (E)**

| Option | Mark | What it showed |
|:--|:-:|:--|
| E1 continuous share (per 25 points), in the interaction | ★★ | R 0.95 with the paper's specification, 1.41 in R0, 1.72 in S** |
| E2 number of using friends | ○ | Only inside M-F |
| E3 users and non-users separately (M-F) | ★ | Non-using friends protective (with the isolates caveat) |
| E4 three categories; E5 at least one using friend | – | |
| Fixed fractions: 0, 1/7, 2/7 (all named), 0, 1/5, 1/3 (visible), 1/4 (paper) always | ★ | Explicit rule for the cut; the paper's 25% equals P75 of adopters' thresholds |
| Count cuts (0, ≤ 1, ≤ 2 using friends) | ○ | Same picture |
| Percentiles among adopters / among students at risk | ★ / ○ | Same picture |
| Valente (1996) classes; relative to school prevalence | – | Classes collapse descriptively (mean 15%, SD 21%) |

**D9. Social-media measure (S), each lagged (`_lag`) or current (`_cur`)**

| Code | Option | Label | Mark | What it showed |
|:--|:--|:-:|:-:|:--|
| S1 | Mean of answered platforms, reversed (the paper) | Disc | ★★ | R ≈ 1.0–1.1 (1.04 in A3, 1.19 in A4) |
| S2 | Mean of a common core (items 1–6) | D | ○ | ≈ S1 |
| S3 | Common core, platforms used less than weekly counted as "never" (the item is asked only for weekly platforms) | D | ★★ | Recommended; clearest associations and R. Alpha 0.76; 87% of missing items are skip logic. The decision that moves R most |
| S4 | Sees vaping content weekly on any platform (yes/no) | D | ★ | Largest odds ratios (1.5–1.9) |
| S5 | Maximum frequency across platforms | D | ○ | ≈ S1 |
| S6 | Number of platforms with weekly content | D | ★ | R 1.33, significant near S* |
| S7 | Instagram only | D | ○ | ≈ S1 |
| S8 | Snapchat only (from wave 5) | D | ○ | No power (40–93 events) |
| `_cur` vs `_lag` | | | ★ | Current gives larger associations but R ≈ 1 and risks reverse causation |

**D10–D12. Covariates (X), fixed effects (F), clustering (CL), sample**

| Option | Mark | What it showed |
|:--|:-:|:--|
| X1 the paper's covariates (code 10 as sexual minority, baseline-wave demographics) | ★ | Coding errors; small effect |
| X2 corrected (code 10 missing, first non-missing answer) | ○ | Reference |
| X3 time-varying covariates at t − 1 | ★ | Lowers R in A3 (1.15, p = .49); A4 holds (1.61, p = .036); sensitivity |
| X4 with perceived friends' use | ★ | Attenuates R: possible mediator |
| X4 with/without in- and out-degree; with general social-media use | – | |
| F1 (Cohort 2 schools in school 101) vs. F2 (physical school) | ○ | Social media unchanged |
| CL by school-prefixed ID vs. `record_id` | ○ | Identical |
| Firth | – | |
| Cohort 1 only / Cohort 2 only (`C1`, `C2`) | ○ | Same direction; Cohort 2 underpowered |
| Without students who name 7 friends (`no_cap`) | ★★ | Social media no longer significant |
| With school 108 | – | |
| Leave one school / one semester out (`drop_school`, `drop_gp`) | ★★ | S**: significant in 9 of 10 and 5 of 6 |

### 3.4 Power and effect sizes

| Check | Mark | What it showed |
|:--|:-:|:--|
| Power of the interaction (A3, 272 events) | ★★ | Minimum detectable R at 80% power: 2.17 (M-B), 1.76 (M-C). R = 1.3 needs ≈ 2,400 events (M-B) or ≈ 1,300 (M-C) |
| Fair power (same true pattern for both tests) | ★ | Neither M-B nor M-C dominates |
| Predicted probabilities | ★★ | Social media adds ≈ 1.1 points regardless of peers; the relative effect is larger without using friends (×1.50 vs ×1.31). That is what R > 1 means |

## 4. Friendship–social-media substitution (M-G)

Friends named (roster and typed); "few" ≤ P25 (4), "several" ≥ P75 (7, the cap); social-media use = number of
11 core platforms used daily. No substitution: gaining friends does not lower social-media use, losing friends
lowers it slightly, and within students more friends go with slightly more use. Gaining friends raises exposure
to vaping content (+0.20 on the item's scale). Results in `results/substitution/` and `results/paneo/stage2/`.

## 5. Code review of the baseline (October 19–21)

Diagnostics in `codes/review-261015/`. Each item became a decision in §3.

| ID | Finding | Becomes |
|:--|:--|:--|
| A1 | With lag 0, same-period co-adopters count as previous adopters | T1 vs. T3 |
| A2 | Students already using at the first analysed wave count as adopters | A1 vs. A3 |
| A3 | Students with no matched friends get exposure 0 (they adopt at 4.8% vs. 2.7% for "friends don't use") | Z1–Z4 |
| A4 | Sexuality "prefer not to disclose" (code 10) coded as sexual minority | X1 vs. X2 |
| A5 | Demographics taken from the baseline wave only | X1 vs. X2 |
| A6 | Exposure counts friends who ever used (36% of "adopted" friends were not using at t − 1) | U1 vs. U3 |
| B1 | Other adopters coded as non-events | M-A vs. M-B, M-D |
| B2 | Different models use different person-period builders | single frame in `paneo/11-frame.R` |
| B3 | Ties built differently from the disadoption study | N1–N4 |
| B4 | Covariate timing mixed | X3 |
| B5 | Social-media composite changes across waves | S1–S3 |
| B6 | Cohort 2 schools take school 101's fixed effect | F1 vs. F2 |
| B7 | Clustering by school-prefixed IDs | CL |

## 6. Where we landed (October 23)

Co-primary outcomes A3 and A4 with U1, N4 + I6 (homophily expected value) and bootstrap, Z3, S3 lagged, X2,
F2, model M-B (`codes/261023_01`):

| | Events | R [95% CI] | p | Bootstrap p (B = 1000) |
|:--|--:|:--|--:|--:|
| A3 (past 6 months) | 272 | 1.41 [0.92, 2.18] | .118 | .134 |
| A4 (past 30 days) | 135 | 1.72 [1.09, 2.73] | .020 | .038 |

Sensitivity (A3 / A4): S1 instead of S3 (R 1.04 / 1.19), X3 (1.15 / 1.61), multiple imputation with m = 50
(1.26 / 1.47). All in `results/for-yuchan/02_robustness_*/robustness_summary.csv`.

## 7. Not tried yet

T4; Z5; students who changed school (3); W4 (partial match of typed names); E4 and E5; Valente classes and the
school-relative cut; controlling for general social-media use; with/without network degree; Firth in the sweep;
school 108.
