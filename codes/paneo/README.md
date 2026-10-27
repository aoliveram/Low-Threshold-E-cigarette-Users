# Sweep of operationalizations and models (paneo)

Run everything from `codes/`, in order: `Rscript paneo/<script>.R`. Uses 8 cores (`NCORES` in `paneo/00-config.R`).

- Inputs: `../Cleaned_Data/ADVANCE_W1-W8.xlsx` and `../Cleaned_Data/ADVANCE_W9-W10.xlsx` (not in git).
- Outputs: `../outputs_paneo/data/` (student-level intermediate files) and `../outputs_paneo/out/` (results). Both are git-ignored.
- The aggregated tables and figures that the sweep documents cite are copied in `results/paneo/`.

| Script | What it does | Time (M4 Pro) |
|---|---|---|
| `paneo/10-build.R` | Reads the raw ADVANCE workbooks once; hashes typed friend names (salt in `../outputs_paneo/data/hash_salt.txt`; never print names) | ~20 s |
| `paneo/check-10-vs-slim.R` | Confirms the build matches the slim RDS of the baseline (`../Cleaned_Data/ADVANCE_slim_v1.rds`; 0 mismatches) | ~5 s |
| `paneo/11-frame.R` | Vertex x semester frame, networks (paper's and named), alter statuses, SM measures, covariates | ~2 s |
| `paneo/12-engine.R` | Library: exposure, imputation (incl. multiple-imputation draws) and models M0, MA–MF | — |
| `paneo/13-layer1.R` | Layer 1: reproduces the submitted paper's Tables 2–3 and S1–S3 (20/20) and the 15-Oct interaction | ~10 s |
| `paneo/14-sni-vs-named.R` | "How many close friends?" (sni_1) vs. the name generator | ~3 s |
| `paneo/15-cutoffs.R` | Adopter shares under candidate low-threshold fractions (decision P7) | ~10 s |
| `paneo/16-layer2.R` | Layer 2: one decision at a time from R0 and H1 (109 specs) | ~8 min |
| `paneo/17-layer3.R` | Layer 3: bounded multiverse (288 specs) | ~45–50 min |
| `paneo/18-summary.R` | Flags (rule P6), heatmaps and specification curves | ~20 s |
| `paneo/20-stage2.R` | Stage 2: cut families (count, percentiles) on the multiverse, U2/U4/U5, q grid (678 jobs) | ~40 min |
| `paneo/21-power.R` | Simulated power for the interaction (MB) and stratified (MC) tests | ~2 min |
| `paneo/22-friendship.R` | Friendship substitution (M-G) with all/roster/visible friends | ~10 s |
| `paneo/23-summary2.R` | Stage 2 tables and figures (out/stage2) | ~15 s |
| `paneo/30-verdict-runs.R` | Robustness of S* and R0: one decision at a time, leave one school/semester out, imputation seeds (78 jobs) | ~4 min |
| `paneo/31-power-fair.R` | Power of MB and MC under the same true pattern (continuous or step) | ~3 min |
| `paneo/32-probs.R` | Predicted probabilities (g-computation) for the paper, R0 and S* | ~1 min |
| `paneo/33-verdict-figs.R` | Verdict figures (out/verdict) | ~5 s |
| `paneo/40-sstar-imputation.R` | S* under every imputation of the unseen friends' use, bootstrap, known-truth simulation (out/sstar) | — |
| `paneo/41-r0-imputation.R` | The same for R0 (A3 by default; `BASE_A=A4` for the 30-day outcome) (out/imp_A3) | — |
| `paneo/42-loso-table.R` | S* leave-one-school-out with the characteristics of each school removed (out/sstar) | — |

Documents (working documents, kept locally in `docs/paneo/`): `paneo-resumen-ejecutivo` (plan and decisions),
`paneo-resultados` (stage 1), `paneo-resultados-2` (stage 2), `paneo-resultados-3-veredicto` (verdict).

Four tables in `results/paneo/` were produced interactively and have no script: `sstar/loso_homophily.csv`,
`sstar/lowo_homophily.csv`, `sstar/recommended_I6.csv` and `layer2/layer2_classification.csv`. The homophily (I6)
results are reproduced by `codes/261023_01_main_recommended_models.R` and `codes/261023_02_robustness.R`.
