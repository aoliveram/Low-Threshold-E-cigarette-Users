# Low-threshold e-cigarette adopters

Analysis code for the paper on adolescents who start using e-cigarettes with few or no friends who use
("low-threshold adopters"), and on whether social media matters more for them. The data are from the ADVANCE
cohort of Southern California high-school students (waves 1–10). Lead author: Yuchan Cao.

## Data (not included)

The analyses use restricted, student-level ADVANCE survey data, available from the ADVANCE investigators under a
data use agreement. The data are not in this repository, and git ignores them. To run the code, place the two
workbooks here:

```
Cleaned_Data/ADVANCE_W1-W8.xlsx
Cleaned_Data/ADVANCE_W9-W10.xlsx
```

Every script runs from `codes/`, reads `../Cleaned_Data/` and writes to `../outputs_*/` (also git-ignored).

## Requirements

R 4.5 and these CRAN packages: data.table, readxl, openxlsx, digest, stringi, MASS, nnet, sandwich, lmtest, fixest,
lme4, car, brglm2, ggplot2, patchwork, gridExtra, ggalluvial and netdiffuseR.

The baseline script (step 2) needs netdiffuseR 1.25.x from CRAN. The 1.26.0 development version fails in
`hazard_rate()` on combined diffnets (see `docs/notes/`).

## Order of execution (from `codes/`)

1. **Recommended models for the revision**

   ```
   Rscript 261023_00_prepare_slim_v2.R              # once: writes ../Cleaned_Data/ADVANCE_slim_v2.rds
   Rscript 261023_01_main_recommended_models.R      # main results: outcomes A3 and A4
   Rscript 261023_02_robustness.R
   Rscript 261023_03_simulation_imputation_power.R
   Rscript 261023_04_descriptives_methods.R
   ```

   Each script writes `../outputs_261023/<script>_<date-time>/` with CSV tables and a log.
   A reference run is in `results/for-yuchan/`.

2. **Baseline: the pipeline behind the submitted version**

   ```
   Rscript 260617_netdiffuseR_prepare_slim-AO.R     # once: writes ../Cleaned_Data/ADVANCE_slim_v1.rds
   Rscript 261015_PROVISIONAL_ORIGINAL_DEGREE_THRESHOLD_CONTINUOUS.R
   ```

   The baseline writes its results and log under `../Cleaned_Data/261015_provisional_original_degree_results/`.

3. **Sweep of operationalizations and models**: see `codes/paneo/README.md`. Results are in `results/paneo/`.
4. **Diagnostics of the baseline**: see `codes/review-261015/README.txt`.
5. **Social-media substitution**: see `codes/substitution/README.txt`. Results are in `results/substitution/`.

## What has been tried

`docs/notes/explored-options.md` lists every model and operationalization explored since April 2026, what
each one showed, and the legend of the specification IDs in `results/paneo/`. Check it before trying a new
specification.

## Layout

```
codes/          scripts (run from here)
results/        aggregated tables and figures only (no student-level rows)
docs/notes/     technical notes and the catalog of explored options
archive/        earlier pipelines (April–July 2026), kept as a record; not maintained
Cleaned_Data/   data (local, git-ignored)
outputs_*/      run outputs (local, git-ignored)
```
