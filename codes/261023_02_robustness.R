# =============================================================================
# 261023_02_robustness.R
# ROBUSTNESS of the interaction R for the two co-primary outcomes (A3, A4). Each check changes ONE
# decision of the recommended specification (script 01) and keeps everything else fixed.
#
#   2. Leave one school out (10 buildings; Cohort 2 schools go with their Cohort 1 building)
#   3. Leave one semester out (grade semesters 3-8)
#   4. Other ways to fill in unseen friends: school rate only, network model, fixed shares, multiple imputation
#   5. Other measurement choices: social media S1 (paper), friends' past-6-month use (U3), covariates at t-1 (X3)
#   6. Other model forms: stratified at 1 in 7 friends; counts of friends who used / did not use
#
# Outputs (../outputs_261023/02_robustness_<time>/):
#   robustness_summary.csv   one row per check, R (95% CI) and p for A3 and A4
#   leave_one_school_out.csv, leave_one_semester_out.csv, counts_model.csv, 02_robustness_log.txt
#
# How to run (from codes/, after script 00):   Rscript 261023_02_robustness.R
# Run time: under 1 minute with 8 cores.
# =============================================================================
rm(list = ls())
source("261023_functions_network_exposure.R")
run <- start_run("02_robustness")
F <- build_frame(readRDS(DATA_FILE))
OUT <- c("A3", "A4")
r_row <- function(check, variant, o, x) data.table(check, variant, outcome = o, R = x$R, R_lo = x$R_lo, R_hi = x$R_hi, p = x$p,
                                                    events = if ("events" %in% names(x)) x$events else NA_integer_)

# ══════════════════════════════════════════════════════════════════════════════
# 1. Reference: the recommended specification
# ══════════════════════════════════════════════════════════════════════════════
section("1. Reference (script 01)")
REF <- rbindlist(lapply(OUT, function(o) r_row("Reference", "Recommended specification", o, fit_interaction(make_data(F, outcome = o)))))
print(REF, row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 2. Leave one school out
# ══════════════════════════════════════════════════════════════════════════════
section("2. Leave one school out")
# Exposures are computed on the full network; only the dropped school's students leave the model.
SCH <- sort(unique(physical_school(SCHOOLS)))
label <- function(s) ifelse(s %in% c(101, 112, 113, 114), sprintf("%d + %d", s, s + 100L), as.character(s))
LOSO <- rbindlist(lapply(OUT, function(o) rbindlist(lapply(SCH, function(s)
  r_row("Leave one school out", paste("without", label(s)), o, fit_interaction(make_data(F, outcome = o, drop_school = s)))))))
print(LOSO[, .(outcome, variant, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = round(p, 3), events)], row.names = FALSE)
print(LOSO[, .(schools = .N, R_min = round(min(R), 2), R_max = round(max(R), 2), p_below_05 = sum(p < .05)), by = outcome])
fwrite(LOSO, file.path(run$dir, "leave_one_school_out.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Leave one semester out
# ══════════════════════════════════════════════════════════════════════════════
section("3. Leave one semester out")
LOWO <- rbindlist(lapply(OUT, function(o) rbindlist(lapply(GPS, function(g)
  r_row("Leave one semester out", paste("without grade semester", g), o, fit_interaction(make_data(F, outcome = o, drop_gp = g)))))))
print(LOWO[, .(outcome, variant, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = round(p, 3), events)], row.names = FALSE)
print(LOWO[, .(semesters = .N, R_min = round(min(R), 2), R_max = round(max(R), 2), p_below_05 = sum(p < .05)), by = outcome])
fwrite(LOWO, file.path(run$dir, "leave_one_semester_out.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 4. Unseen friends: other ways to fill them in
# ══════════════════════════════════════════════════════════════════════════════
section("4. Unseen friends")
# About half of the named friendships are with students whose use we never see. The recommended rule gives
# each one its expected value. Multiple imputation (random 0/1 draws) is shown for completeness: it adds
# noise to the exposure and pulls R towards 1 (script 03 shows this with a known-truth simulation).
IMP <- list(`School x wave rate only (no homophily)` = list(imputation = "prevalence"),
            `Network model (logistic)`               = list(imputation = "model"),
            `All unseen friends: 6% used`            = list(imputation = "const", q = 0.06),
            `All unseen friends: 12% used`           = list(imputation = "const", q = 0.12),
            `All unseen friends: 20% used`           = list(imputation = "const", q = 0.20))
IMPR <- rbindlist(lapply(OUT, function(o) rbind(
  rbindlist(lapply(names(IMP), function(v) r_row("Unseen friends", v, o, fit_interaction(do.call(make_data, c(list(F, outcome = o), IMP[[v]])))))),
  r_row("Unseen friends", "Multiple imputation (m = 50), homophily", o, mi_R(prepare_spec(F, outcome = o), m = 50L)))))
print(IMPR[, .(outcome, variant, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = round(p, 3))], row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 5. Measurement choices
# ══════════════════════════════════════════════════════════════════════════════
section("5. Measurement choices")
MEAS <- list(`Social media: S1, the paper's average` = list(sm = "S1"),
             `Friends' status: used in the past 6 months (U3)` = list(status = "U3"),
             `Covariates measured at t-1 (X3)` = list(covs = "X3"))
MEASR <- rbindlist(lapply(OUT, function(o) rbindlist(lapply(names(MEAS), function(v)
  r_row("Measurement", v, o, fit_interaction(do.call(make_data, c(list(F, outcome = o), MEAS[[v]]))))))))
print(MEASR[, .(outcome, variant, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = round(p, 3), events)], row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 6. Model form
# ══════════════════════════════════════════════════════════════════════════════
section("6. Model form")
STR <- rbindlist(lapply(OUT, function(o) r_row("Model form", "Stratified: at most 1 in 7 friends used vs more", o, fit_stratified(make_data(F, outcome = o)))))
print(STR[, .(outcome, variant, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = round(p, 3))], row.names = FALSE)
CNT <- rbindlist(lapply(OUT, function(o) fit_counts(make_data(F, outcome = o))[, outcome := o]))
cat("\nCounts model (Reviewer 1): friends who used and friends who did not, each interacting with social media\n")
print(CNT[, .(outcome, term, OR = round(OR, 3), OR_lo = round(OR_lo, 3), OR_hi = round(OR_hi, 3), p = round(p, 3))], row.names = FALSE)
fwrite(CNT, file.path(run$dir, "counts_model.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 7. Summary
# ══════════════════════════════════════════════════════════════════════════════
section("7. Summary")
fmt <- function(R, lo, hi, p) sprintf("%.2f (%.2f, %.2f), p = %.3f", R, lo, hi, p)
agg <- function(X, name) X[, .(check = name, variant = sprintf("%d of %d with R > 1; %d of %d with p < .05; R from %.2f to %.2f",
                                 sum(R > 1), .N, sum(p < .05), .N, min(R), max(R))), by = outcome]
ALL <- rbind(REF, IMPR, MEASR, STR)
SUM <- dcast(ALL[, .(check, variant, outcome, value = fmt(R, R_lo, R_hi, p))], check + variant ~ outcome, value.var = "value")
SUM <- SUM[order(match(check, c("Reference", "Unseen friends", "Measurement", "Model form")), match(variant, unique(ALL$variant)))]
LS <- dcast(rbind(agg(LOSO, "Leave one school out"), agg(LOWO, "Leave one semester out")), check ~ outcome, value.var = "variant")
SUM <- rbind(SUM, LS[, .(check, variant = "", A3, A4)])
setnames(SUM, c("Check", "Variant", "A3: R (95% CI), p", "A4: R (95% CI), p"))
print(SUM, row.names = FALSE)
fwrite(SUM, file.path(run$dir, "robustness_summary.csv"))
say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
