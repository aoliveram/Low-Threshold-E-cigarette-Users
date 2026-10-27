# =============================================================================
# 261023_01_main_recommended_models.R
# MAIN RESULTS for the revision: the two co-primary outcomes with the recommended exposure.
#
#   A3  start of past-6-month e-cigarette use   (wX_past_6mo_use_3)
#   A4  start of past-30-day e-cigarette use    (wX_past30_sub_3_any; W7-W8: wX_past30_sub_3 > 0 days)
#
# Recommended specification (same for both outcomes):
#   - friends  = every friend named at t-1 in the own-school slots (roster wX_friendK_SCH + typed
#                wX_friendK_other_SCH), whether or not they answered the survey
#   - exposure = share of those friends who had EVER used e-cigarettes by t-1; unseen friends get their
#                expected value (homophily: what the students who name them do, shrunk to the school rate)
#   - isolates = students who named nobody get exposure 0 plus a flag
#   - social media = S3: vaping content seen on the 6 core platforms (wX_sm_post_k), at t-1;
#                platforms used less than weekly (wX_socialmed_k) count as "never"
#   - covariates as in the paper, measured with the improvements noted in the functions file
#   - discrete-time logit, SE clustered by student; cluster bootstrap (B = 1000) that also redraws the
#     school rates used for unseen friends
#
# Outputs (one folder per run in ../outputs_261023/):
#   table1_characteristics.csv   sample description
#   table2_main_models.csv       the "Now" table (paper format)
#   table3_predicted_probabilities.csv
#   main_models_full.csv         every estimate with more decimals
#   01_main_log.txt              full log
#
# How to run (from codes/, after script 00):   Rscript 261023_01_main_recommended_models.R
# Run time: about 1 minute with 8 cores.
# =============================================================================
rm(list = ls())
source("261023_functions_network_exposure.R")
run <- start_run("01_main")
B_BOOT <- 1000L

# ══════════════════════════════════════════════════════════════════════════════
# 1. Data
# ══════════════════════════════════════════════════════════════════════════════
section("1. Data")
F <- build_frame(readRDS(DATA_FILE))
say("Students in the frame: %d | student-semesters: %d", uniqueN(F$PP$record_id), nrow(F$PP))
OUTCOMES <- c(A3 = "Started past-6-month use (A3)", A4 = "Started past-30-day use (A4)")
P <- lapply(names(OUTCOMES), function(o) prepare_spec(F, outcome = o)); names(P) <- names(OUTCOMES)
D <- lapply(P, assemble)
for (o in names(D)) say("%s: %d student-semesters, %d students, %d starts", o, nrow(D[[o]]), uniqueN(D[[o]]$record_id), sum(D[[o]]$y))

# ══════════════════════════════════════════════════════════════════════════════
# 2. Table 1: sample characteristics
# ══════════════════════════════════════════════════════════════════════════════
section("2. Table 1: sample characteristics")
msd <- function(x, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f)"), mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
pct <- function(x) sprintf("%.1f", 100 * mean(x, na.rm = TRUE))
describe <- function(o) {
  d <- D[[o]]; s <- d[order(record_id, gp)][, .SD[1], by = record_id]          # one row per student for fixed traits
  ed <- P[[o]]$ed
  data.table(characteristic = c(
      "Students, n", "Student-semesters at risk, n", "Starts, n", "Starts per 100 student-semesters",
      "Female, %", "Hispanic, %", "Asian, %", "Sexual minority, %", "Parental education (1-6), mean (SD)",
      "Depressive symptoms (RCADS), mean (SD)", "Vaping content on social media (S3, 1-5), mean (SD)",
      "Friends named at t-1, mean (SD)", "Named no friends at t-1, %", "Named friends not seen in the survey, %",
      "Share of friends who had used (E), mean (SD)", "E = 0 (no friend had used), %"),
    value = c(
      format(uniqueN(d$record_id), big.mark = ","), format(nrow(d), big.mark = ","), format(sum(d$y), big.mark = ","),
      sprintf("%.2f", 100 * mean(d$y)),
      pct(s$female), pct(s$hispanic), pct(s$asian), pct(s$sexmin), msd(s$paredu),
      msd(d$mddv), msd(d$smv), msd(d$n), pct(d$iso),
      pct(ed$type %in% c("nonparticipant", "typed")), msd(d$E, 3), pct(d$E == 0)))
}
T1 <- merge(describe("A3"), describe("A4"), by = "characteristic", sort = FALSE, suffixes = c("_A3", "_A4"))
setnames(T1, c("Characteristic", OUTCOMES))
print(T1, row.names = FALSE)
cat("Fixed traits use one row per student; the other rows are averages over student-semesters at risk.\n")
fwrite(T1, file.path(run$dir, "table1_characteristics.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Models (Table 2, "Now")
# ══════════════════════════════════════════════════════════════════════════════
section("3. Models")
stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
fmt <- function(est, lo, hi, p) sprintf("%.2f (%.2f, %.2f)%s", est, lo, hi, stars(p))
FULL <- list(); NOW <- list()
for (o in names(OUTCOMES)) {
  d <- D[[o]]
  add <- fit_additive(d); int <- fit_interaction(d); cnt <- fit_counts(d); low <- fit_lowthreshold(d)
  say("\n--- %s: interaction model, clustered SE ---", OUTCOMES[o])
  f <- glm_fit(paste("y ~ smc * E25 +", cov_terms(d)), d)
  ct <- lmtest::coeftest(f, vcov. = sandwich::vcovCL(f, cluster = d$record_id)); print(round(ct[!grepl("factor", rownames(ct)), ], 4))
  say("Bootstrap (B = %d) running on %d cores...", B_BOOT, N_CORES)
  bt <- bootstrap_R(P[[o]], B = B_BOOT, seed = if (o == "A3") 2026L else 4026L)
  print(bt)
  nu <- cnt[term == "Friends who did not use (per friend)"]
  NOW[[o]] <- data.table(row = c(
      "Student-semesters (students)", "Starts",
      "Social media, OR per point (additive model)", "Friends who used, OR per +25 pp (additive model)",
      "Social media x friends who used, OR", "R = OR(social media | no friend used) / OR(social media | half used)",
      "R, bootstrap 95% CI", "R, bootstrap p",
      "Friends who did not use, OR per friend (counts model)", "Social media, OR for low-threshold starts (E <= 1/4)"),
    value = c(sprintf("%s (%s)", format(int$n, big.mark = ","), format(int$students, big.mark = ",")), as.character(int$events),
      fmt(add$OR[1], add$OR_lo[1], add$OR_hi[1], add$p[1]), fmt(add$OR[2], add$OR_lo[2], add$OR_hi[2], add$p[2]),
      fmt(int$OR_int, exp(int$b - 1.96 * int$se), exp(int$b + 1.96 * int$se), int$p), sprintf("%.2f (%.2f, %.2f), p = %.3f", int$R, int$R_lo, int$R_hi, int$p),
      sprintf("%.2f, %.2f", bt$R_boot_lo, bt$R_boot_hi), sprintf("%.3f", bt$p_boot),
      fmt(nu$OR, nu$OR_lo, nu$OR_hi, nu$p), fmt(low$OR, low$OR_lo, low$OR_hi, low$p)))
  FULL[[o]] <- rbind(add[, .(outcome = o, quantity = term, est = OR, lo = OR_lo, hi = OR_hi, p)],
    int[, .(outcome = o, quantity = c("Interaction OR", "R"), est = c(OR_int, R), lo = c(exp(b - 1.96 * se), R_lo), hi = c(exp(b + 1.96 * se), R_hi), p = p)],
    bt[, .(outcome = o, quantity = "R (bootstrap)", est = int$R, lo = R_boot_lo, hi = R_boot_hi, p = p_boot)],
    cnt[, .(outcome = o, quantity = term, est = OR, lo = OR_lo, hi = OR_hi, p)],
    low[, .(outcome = o, quantity = term, est = OR, lo = OR_lo, hi = OR_hi, p)])[, `:=`(n = int$n, events = int$events, students = int$students)]
}
T2 <- merge(NOW$A3, NOW$A4, by = "row", sort = FALSE); setnames(T2, c("", OUTCOMES))
section("Table 2 (Now)"); print(T2, row.names = FALSE)
cat("* p < .05, ** p < .01, *** p < .001. SE clustered by student. Models adjust for semester, cohort, sex, Hispanic,\n",
    "Asian, parental education, depressive symptoms, out- and in-degree, sexual minority, school, and a flag for students\n",
    "who named no friends. The interaction (R) is the direct test of the two-step-flow narrative: R > 1 means social\n",
    "media matters more for students whose friends do not vape.\n", sep = "")
fwrite(T2, file.path(run$dir, "table2_main_models.csv"))
fwrite(rbindlist(FULL), file.path(run$dir, "main_models_full.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 4. Table 3: predicted chance of starting in a semester
# ══════════════════════════════════════════════════════════════════════════════
section("4. Predicted probabilities (interaction model)")
# Typical social media = median (P50) of S3, high = P90. Peers: no friend who used vs 2 of 7 friends.
PR <- rbindlist(lapply(names(OUTCOMES), function(o) predicted_grid(D[[o]])[, outcome := o]))
PR[, peers := fifelse(E == 0, "No friend had used", "2 of 7 friends had used")]
T3 <- dcast(PR, outcome + peers ~ social_media, value.var = "prob_pct")
setcolorder(T3, c("outcome", "peers", "typical", "high"))
T3[, `:=`(ratio = high / typical, difference_pp = high - typical)]
T3 <- T3[order(outcome, -peers)]
T3[, `:=`(typical = round(typical, 2), high = round(high, 2), ratio = round(ratio, 2), difference_pp = round(difference_pp, 2))]
setnames(T3, c("Outcome", "Friends", "Typical social media (P50), %", "High social media (P90), %", "High / typical", "High - typical (pp)"))
print(T3, row.names = FALSE)
say("S3 levels used (1 = never sees vaping posts ... 5 = several times a day): %s", paste(unique(PR[, sprintf("%s %s = %.2f", outcome, social_media, sm_value)]), collapse = "; "))
fwrite(T3, file.path(run$dir, "table3_predicted_probabilities.csv"))

say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
