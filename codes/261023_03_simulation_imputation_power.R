# =============================================================================
# 261023_03_simulation_imputation_power.R
# SIMULATIONS that support two methods choices (supplement material):
#
#   Part A  Why expected values (and not multiple imputation) for unseen friends.
#           We create a world where we KNOW the truth: every unseen friend gets a true 0/1 status, and starts
#           are generated with a known interaction (true R = 1.7) on the TRUE share of friends who used.
#           Then we estimate R as in the paper, (i) with the expected-value exposure and (ii) with multiple
#           imputation, and check which one recovers R = 1.7.
#   Part B  Power: how often the interaction test detects R = 1.3, 1.5, 1.7, 2.0 with our sample.
#
# Outputs (../outputs_261023/03_simulation_<time>/):
#   simulation_imputation.csv, simulation_power.csv, 03_simulation_log.txt
#
# How to run (from codes/, after script 00):   Rscript 261023_03_simulation_imputation_power.R
# Run time: about 3 minutes with 8 cores.
# =============================================================================
rm(list = ls())
source("261023_functions_network_exposure.R")
run <- start_run("03_simulation")
F <- build_frame(readRDS(DATA_FILE))
OUT <- c("A3", "A4")
N_SIM <- 160L          # simulated data sets per outcome in Part A
M_MI  <- 20L           # imputations per data set in Part A
N_POW <- 200L          # simulated data sets per outcome and R in Part B
TRUE_R <- 1.7

# Base model without interaction: gives every student-semester a realistic baseline chance of starting.
# The simulated interaction is added on top: logit = baseline - (log(R) / 2) * social media (centered) * E / 0.25,
# so that R = OR(social media | E = 0) / OR(social media | E = 0.5), as in the main model.
base_fit <- function(D) { center(D); glm_fit(paste("y ~ smc + E25 +", cov_terms(D)), D) }
simulate_y <- function(f0, Dtrue, R) { center(Dtrue)
  eta <- predict(f0, Dtrue, type = "link") - (log(R) / 2) * Dtrue$smc * Dtrue$E25
  data.table(rid = Dtrue$rid, ysim = rbinom(nrow(Dtrue), 1, plogis(eta))) }
with_y <- function(D, ys) { D <- D[ys, on = "rid", nomatch = 0L]; D[, y := ysim]; D }

# ══════════════════════════════════════════════════════════════════════════════
# Part A. Expected value vs multiple imputation, known truth
# ══════════════════════════════════════════════════════════════════════════════
section(sprintf("Part A. Known-truth simulation (true R = %.1f, %d data sets per outcome, %d imputations each)", TRUE_R, N_SIM, M_MI))
SIMA <- rbindlist(lapply(OUT, function(o) {
  P <- prepare_spec(F, outcome = o); Dev <- assemble(P); f0 <- base_fit(Dev)
  rbindlist(pmap(seq_len(N_SIM), function(s) {
    set.seed(9000L + s)
    Dtrue <- assemble(P, mi_seed = 50000L + s)            # the true (unknown to the analyst) friends' statuses
    ys <- simulate_y(f0, Dtrue, TRUE_R)
    ev <- fit_interaction(with_y(Dev, ys))                # what we recommend
    mi <- rbindlist(lapply(seq_len(M_MI), function(i) fit_interaction(with_y(assemble(P, mi_seed = 70000L + 100L * s + i), ys))[, .(b, se)]))
    b <- mean(mi$b); se <- sqrt(mean(mi$se^2) + (1 + 1 / M_MI) * var(mi$b))
    data.table(outcome = o, sim = s, b_ev = ev$b, se_ev = ev$se, b_mi = b, se_mi = se)
  }))
}))
truth <- -log(TRUE_R) / 2
SA <- SIMA[, .(method = c("Expected value (recommended)", "Multiple imputation"),
               mean_R = exp(-2 * c(mean(b_ev), mean(b_mi))),
               coverage_95 = c(mean(abs(b_ev - truth) <= 1.96 * se_ev), mean(abs(b_mi - truth) <= 1.96 * se_mi)),
               power = c(mean(b_ev < 0 & abs(b_ev / se_ev) > 1.96), mean(b_mi < 0 & abs(b_mi / se_mi) > 1.96)), sims = .N), by = outcome]
SA[, true_R := TRUE_R]
print(SA[, .(outcome, method, true_R, mean_R = round(mean_R, 2), coverage_95 = round(coverage_95, 2), power = round(power, 2), sims)], row.names = FALSE)
cat("Reading: the expected value recovers R (mean close to the truth, coverage close to 95%). Multiple imputation\n",
    "adds random 0/1 noise to the exposure, which pulls R towards 1 and lowers power.\n", sep = "")
fwrite(SA, file.path(run$dir, "simulation_imputation.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# Part B. Power of the interaction test
# ══════════════════════════════════════════════════════════════════════════════
section(sprintf("Part B. Power (%d data sets per outcome and R)", N_POW))
jobs <- CJ(outcome = OUT, R = c(1.3, 1.5, 1.7, 2.0), sim = seq_len(N_POW))
PREP <- lapply(setNames(OUT, OUT), function(o) { D <- assemble(prepare_spec(F, outcome = o)); list(D = D, f0 = base_fit(D)) })
POW <- rbindlist(pmap(seq_len(nrow(jobs)), function(j) {
  set.seed(20000L + j); p <- PREP[[jobs$outcome[j]]]
  x <- fit_interaction(with_y(p$D, simulate_y(p$f0, p$D, jobs$R[j])))
  data.table(outcome = jobs$outcome[j], R = jobs$R[j], R_hat = x$R, p = x$p, se = x$se, events = x$events)
}))
PB <- POW[, .(power = mean(R_hat > 1 & p < .05), median_R_hat = median(R_hat), mean_events = mean(events), sims = .N), by = .(outcome, R)]
# events needed for 80% power at R = 1.3 (standard error shrinks with the square root of the number of starts)
need <- POW[R == 1.3, .(se = median(se), ev = mean(events)), by = outcome][, events_for_80pct_at_R1.3 := round(ev * (se / (log(1.3) / 2 / 2.8))^2)]
print(PB[, .(outcome, true_R = R, power = round(power, 2), median_R_hat = round(median_R_hat, 2), mean_events = round(mean_events), sims)], row.names = FALSE)
print(need[, .(outcome, starts_now = round(ev), events_for_80pct_at_R1.3)], row.names = FALSE)
fwrite(merge(PB, need[, .(outcome, events_for_80pct_at_R1.3)], by = "outcome"), file.path(run$dir, "simulation_power.csv"))

say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
