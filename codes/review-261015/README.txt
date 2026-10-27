# Diagnostics for the review of 261015_PROVISIONAL_ORIGINAL_DEGREE_THRESHOLD_CONTINUOUS.R (2026-10-19)
Findings A1-A6 of the revision plan. The 261015 script needs netdiffuseR 1.25.x (CRAN); the 1.26.0
development version fails in hazard_rate() on combined diffnets (see docs/notes/).

1. Copy the 261015 script and append before the final closing brace:
     save.image(file.path(out_path, "all_objects.RData"))
2. From codes/, run:
     AO_SELF_LOG_CHILD=1 AO_FINAL_RUN_DIR=<run_dir> Rscript <copy>.R
3. Run each diag script from codes/ with S=<parent of run_dir>, where the workspace is <S>/run_yuchan/all_objects.RData:
     S=<dir> Rscript review-261015/diag1.R   (demographics, samples, degree)
     diag2.R  (sexual identity code 10, SM items, pre-window use, same-period co-adopters, alters' current use)
     diag3.R  (SM effect by peer-exposure stratum)
     diag4.R  (ties to non-respondents, physical-school FE)
     diag5.R  (why students have no matched friend at t-1; reads W3-W9 from ../Cleaned_Data; corrected 2026-10-22)
