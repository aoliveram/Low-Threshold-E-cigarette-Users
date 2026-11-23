# =============================================================================
# 261120_08_no_imputation.R
# WITHOUT ESTIMATING UNKNOWN FRIENDS: the recommended specification of script 01 with the two ways of
# computing peer exposure that do not estimate the use of friends who are not in the survey.
#
#   Reference         unseen friends get their expected value (homophily, script 01)
#   Unseen = non-users all friends not in the survey count as friends who had not used (they stay in the
#                     denominator). This is what the earlier versions of the paper did implicitly.
#   Observed only     peer exposure is the share of users among the friends whose use we observe (study
#                     participants). Students with no observed friend get E = 0 and the "no friend" flag.
#
# Everything else is fixed: outcomes A4 and A3, lagged S3 and lagged network, covariates X2, school and
# grade-semester fixed effects, SE clustered by student. Inference is Wald (no imputation to propagate).
#
# Output (../outputs_261023/08_no_imputation_<time>/):
#   no_imputation.csv         R (continuous interaction) and the low-threshold social media AOR, per variant
#   08_no_imputation_log.txt  full log
#
# How to run (from codes/, after script 00):   Rscript 261120_08_no_imputation.R
# Run time: under 1 minute.
# =============================================================================
rm(list = ls())
suppressWarnings(source("261023_functions_network_exposure.R"))
run <- start_run("08_no_imputation")
F <- build_frame(readRDS(DATA_FILE))
OUT <- c(A4 = "Past-30-day use (primary)", A3 = "Past-six-month use (secondary)")
fmt <- function(est, lo, hi, p) sprintf("%.2f (%.2f-%.2f), p = %s", est, lo, hi, ifelse(p < .001, "<.001", sprintf("%.3f", p)))

ROWS <- list()
for (o in names(OUT)) {
  section(OUT[o])
  P0 <- prepare_spec(F, outcome = o)
  Pobs <- P0; keep <- !(P0$ed$type %in% c("nonparticipant", "typed"))
  Pobs$ed <- P0$ed[keep]; Pobs$key <- P0$key[keep]
  D <- list(`Reference: unseen friends estimated (homophily)` = assemble(P0),
            `Unseen friends counted as non-users`              = make_data(F, outcome = o, imputation = "const", q = 0),
            `Observed friends only`                            = assemble(Pobs))
  for (v in names(D)) {
    d <- D[[v]]; it <- fit_interaction(d); lt <- fit_lowthreshold(d)
    ROWS[[length(ROWS) + 1]] <- data.table(outcome = o, variant = v,
      n = nrow(d), events = sum(d$y), mean_E = round(mean(d$E), 3), pct_flag_no_friend = round(100 * mean(d$iso), 1),
      R = fmt(it$R, it$R_lo, it$R_hi, it$p), low_threshold_events = lt$events, low_threshold_AOR = fmt(lt$OR, lt$OR_lo, lt$OR_hi, lt$p))
  }
  print(rbindlist(ROWS)[outcome == o], row.names = FALSE)
}
ROWS <- rbindlist(ROWS)
fwrite(ROWS, file.path(run$dir, "no_imputation.csv"))
say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
