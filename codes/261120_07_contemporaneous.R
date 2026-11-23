# =============================================================================
# 261120_07_contemporaneous.R
# CONTEMPORANEOUS EXPOSURES: the manuscript's Tables 2 and 3 refit with social media and/or the friendship
# network measured in the SAME grade semester as the possible start, instead of one semester before.
#
# Timing (t = the semester in which a start is observed: no use at t-1, use at t):
#   lagged network         friends named at t-1, and whether they had ever used by t-1   (manuscript)
#   contemporaneous network friends named at t,   and whether they had ever used by t
#   lagged social media    S3 reported at t-1                                            (manuscript)
#   contemporaneous SM     S3 reported at t (the same survey that reports the start)
#
# Four variants: SM t-1 / network t-1 (reproduces the manuscript), SM t / network t-1, SM t-1 / network t,
# SM t / network t. Everything else is the recommended specification of script 01: S3 social media, every
# named own-school friend, unseen friends by homophily, flag for students who named nobody, covariates X2
# (sex, ethnicity, race, sexual minority, parental education, cohort; depressive symptoms and degrees at t),
# grade-semester and school fixed effects, SE clustered by student, cluster bootstrap (B = 1000) for R.
#
# One change needed only for the contemporaneous network: a friend who is not in the survey gets the share
# of users among the OTHER students who name that friend. The student is left out of his or her own
# friends' estimate, because at t the student's own status is the outcome. (With the lagged network the
# student is never a user at t-1, so script 01 does not need this.)
#
# Outputs (../outputs_261023/07_contemporaneous_<time>/):
#   contemporaneous_tables.csv   Table 2 (types of starter) and Table 3 (25% cut and continuous E) rows,
#                                plus R with its bootstrap CI and p, for each variant and outcome
#   07_contemporaneous_log.txt   full log
#
# How to run (from codes/, after script 00):   Rscript 261120_07_contemporaneous.R
# Run time: about 2 minutes with 8 cores (8 bootstraps).
# =============================================================================
rm(list = ls())
suppressWarnings(source("261023_functions_network_exposure.R"))
run <- start_run("07_contemporaneous")
B_BOOT <- 1000L
slim <- readRDS(DATA_FILE)
F <- build_frame(slim)
OUT <- c(A4 = "Past-30-day use (primary)", A3 = "Past-six-month use (secondary)")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Social media measured at t (same S3 rule as the lagged measure in the functions file)
# ══════════════════════════════════════════════════════════════════════════════
section("1. Social media at t")
sm_t <- slim$panel[, {
  smp <- as.matrix(.SD[, paste0("smp", 1:9), with = FALSE]); smp[!(smp %in% 1:5)] <- NA
  smu <- as.matrix(.SD[, paste0("smu", 1:15), with = FALSE]); smu[!(smu %in% 0:5)] <- NA
  core <- smp[, 1:6]; use_core <- smu[, c(1, 2, 3, 7, 8, 11)]
  core[is.na(core) & !is.na(use_core) & use_core < 2] <- 5          # used less than weekly = never sees posts
  .(record_id = record_id, wave = wave, S3_t = ifelse(rowSums(!is.na(core)) < 4, NA, 6 - rowMeans(core, na.rm = TRUE)))
}]
F$PP <- merge(F$PP, sm_t, by = c("record_id", "wave"), all.x = TRUE)
say("Social media items exist only in waves 4-8. Rows with S3 at t-1: %d | with S3 at t: %d",
    sum(!is.na(F$PP$S3_lag)), sum(!is.na(F$PP$S3_t)))

# ══════════════════════════════════════════════════════════════════════════════
# 2. One specification with a chosen timing (prepare_spec of the functions file, with two switches)
# ══════════════════════════════════════════════════════════════════════════════
prepare_spec_time <- function(F, outcome, sm_time = "lag", net_time = "lag") {
  R <- F$PP[get(paste0("risk_", outcome)) == TRUE]
  R[, `:=`(y = get(paste0("y_", outcome)), wp = gp_to_wave(school, gp - 1L))]
  R <- R[F$RESP[, .(record_id, wp = wave)], on = .(record_id, wp), nomatch = 0L]   # the student answered at t-1
  R[, wn := if (net_time == "lag") wp else wave]                                     # wave of the network
  R <- merge(R, F$DEG[, .(school, wave, record_id, outdeg = outdeg, indeg = indeg)], by = c("school", "wave", "record_id"), all.x = TRUE)
  for (v in c("outdeg", "indeg")) set(R, which(is.na(R[[v]])), v, 0L)
  R[, `:=`(mddv = mdd, smv = if (sm_time == "lag") S3_lag else S3_t)]
  need <- c("y", "gp", "cohort", "female", "hispanic", "asian", "paredu", "mddv", "smv", "sexmin", "outdeg", "indeg")
  R <- R[complete.cases(R[, ..need])]
  setorder(R, vid, gp); R[, rid := .I]

  # Friends named at wn, with whether they had ever used by wn
  ed <- F$EN[R[, .(rid, school, cohort, ego = record_id, wn)], on = .(school, wave = wn, ego), nomatch = 0L,
             allow.cartesian = TRUE, .(rid, school, cohort, wn = i.wn, ego, alter, akey, type)]
  ed <- F$B[, .(alter = record_id, wn = wave, ever_w3, ever_w5)][ed, on = .(alter, wn)]
  ed[, s := fifelse(cohort == "C2", ever_w5, ever_w3)]
  ed[type %in% c("nonparticipant", "typed"), s := NA_real_]
  ed[, fill := s]
  pv <- F$PREV[, .(school, wn = wave, n_p = n_U1, u_p = u_U1)]
  # Homophily for friends not in the survey: share of users among the students who name them at wn
  nom <- unique(F$EN[type == "nonparticipant", .(school, wn = wave, akey, ego)])
  nom <- F$B[, .(ego = record_id, wn = wave, ever_w3, ever_w5)][nom, on = .(ego, wn)]
  nom[, u := fifelse(is_c2(school), ever_w5, ever_w3)][is.na(u), u := 0]
  ed <- nom[, .(Nn = .N, Un = sum(u)), by = .(school, wn, akey)][ed, on = .(school, wn, akey)]
  if (net_time == "current") {                       # leave the student out of his or her own friends' estimate
    own <- F$B[, .(ego = record_id, wn = wave, e3 = ever_w3, e5 = ever_w5)]
    ed <- own[ed, on = .(ego, wn)]
    ed[, u_own := fifelse(is_c2(school), e5, e3)][is.na(u_own), u_own := 0]
    k <- ed$type == "nonparticipant" & !is.na(ed$Nn)
    ed[k, `:=`(Nn = Nn - 1L, Un = Un - u_own)]
    ed[k & Nn == 0L, `:=`(Nn = NA_integer_, Un = NA_real_)]   # nobody else names the friend: school rate
  }
  ed[, unk := is.na(s)]
  list(R = R, ed = ed, pv = pv, outcome = outcome, status = "U1", imputation = "homophily", q = NA_real_, covs = "X2",
       key = ed[, paste(school, wn, akey)])
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. The manuscript's rows for one specification (as script 06)
# ══════════════════════════════════════════════════════════════════════════════
pfmt <- function(p) ifelse(p < .001, "<.001", sprintf("%.3f", p))
orci <- function(b, se) sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se))
line <- function(tab, row, b, se, events = NA) data.table(table = tab, row, events, OR = orci(b, se),
                                                          p = pfmt(2 * pnorm(-abs(b / se))), b = b, se = se)
paper_rows <- function(P, seed) {
  D <- assemble(P); center(D)
  vis <- P$ed[type == "visible", .(kv = sum(s, na.rm = TRUE)), by = rid]
  D <- merge(D, vis, by = "rid", all.x = TRUE)[is.na(kv), kv := 0]
  D[, high := as.integer(E > 0.25 + 1e-9)]
  T <- list()
  # Table 2: types of starter vs everyone else
  for (r in list(list("Any transition", D$y == 1), list("No visible using friend", D$y == 1 & D$kv == 0 & D$iso == 0),
                 list("Low-threshold transition (E <= 25%)", D$y == 1 & D$high == 0), list("High-threshold transition (E > 25%)", D$y == 1 & D$high == 1))) {
    D[, yy := as.integer(r[[2]])]
    t <- clustered(glm_fit(paste("yy ~ smc +", cov_terms(D)), D), D, "smc")
    T[[length(T) + 1]] <- line("Table 2", r[[1]], t$b, t$se, sum(D$yy))
  }
  # Table 3, Panel A: the 25% cut as a moderator
  f <- glm_fit(paste("y ~ smc * high +", cov_terms(D)), D); V <- sandwich::vcovCL(f, cluster = D$record_id); b <- coef(f)
  ta <- clustered(glm_fit(paste("y ~ smc + high +", cov_terms(D)), D), D, c("smc", "high"))
  T[[length(T) + 1]] <- rbind(
    line("Table 3A", "Social media, model without interaction", ta$b[1], ta$se[1], sum(D$y)),
    line("Table 3A", "High vs. low peer exposure, model without interaction", ta$b[2], ta$se[2]),
    line("Table 3A", "Social media effect at low exposure (<= 25%)", b["smc"], sqrt(V["smc", "smc"])),
    line("Table 3A", "Social media effect at high exposure (> 25%)", b["smc"] + b["smc:high"],
         sqrt(V["smc", "smc"] + V["smc:high", "smc:high"] + 2 * V["smc", "smc:high"])),
    line("Table 3A", "Social media x high exposure", b["smc:high"], sqrt(V["smc:high", "smc:high"])))
  # Table 3, Panel B: continuous peer exposure
  f <- glm_fit(paste("y ~ smc * E25 +", cov_terms(D)), D); V <- sandwich::vcovCL(f, cluster = D$record_id); b <- coef(f)
  at <- function(e) { w <- c(1, e / .25); k <- c("smc", "smc:E25"); c(sum(b[k] * w), sqrt(drop(t(w) %*% V[k, k] %*% w))) }
  ta <- clustered(glm_fit(paste("y ~ smc + E25 +", cov_terms(D)), D), D, c("smc", "E25"))
  T[[length(T) + 1]] <- rbind(
    line("Table 3B", "Social media (additive)", ta$b[1], ta$se[1], sum(D$y)),
    line("Table 3B", "Peer exposure per +25 pp (additive)", ta$b[2], ta$se[2]),
    line("Table 3B", "Social media effect at 0% peer exposure", at(0)[1], at(0)[2]),
    line("Table 3B", "Social media effect at 50% peer exposure", at(.5)[1], at(.5)[2]),
    line("Table 3B", "Social media x peers (per +25 pp)", b["smc:E25"], sqrt(V["smc:E25", "smc:E25"])),
    line("Table 3B", "R = OR at 0% / OR at 50% (Wald)", -2 * b["smc:E25"], 2 * sqrt(V["smc:E25", "smc:E25"])))
  bt <- bootstrap_R(P, B = B_BOOT, seed = seed)
  T[[length(T) + 1]] <- data.table(table = "Table 3B", row = "R, bootstrap 95% CI and p", events = NA,
    OR = sprintf("%.2f (%.2f-%.2f)", exp(-2 * b["smc:E25"]), bt$R_boot_lo, bt$R_boot_hi), p = pfmt(bt$p_boot), b = NA, se = NA)
  T[[length(T) + 1]] <- data.table(table = "Sample", row = "Student-semesters (students)", events = NA,
    OR = sprintf("%s (%s)", format(nrow(D), big.mark = ","), format(uniqueN(D$record_id), big.mark = ",")), p = "", b = NA, se = NA)
  T[[length(T) + 1]] <- data.table(table = "Sample", row = "Named friends whose use was estimated, %", events = NA,
    OR = sprintf("%.1f", 100 * mean(P$ed$unk)), p = "", b = NA, se = NA)
  rbindlist(T)
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. Run the four timing variants
# ══════════════════════════════════════════════════════════════════════════════
VAR <- list(`SM t-1 / network t-1 (manuscript)` = c("lag", "lag"),
            `SM t / network t-1`                = c("current", "lag"),
            `SM t-1 / network t`                = c("lag", "current"),
            `SM t / network t`                  = c("current", "current"))
# Same bootstrap seeds as script 01 (A3 2026, A4 4026) in every variant, so the manuscript column
# reproduces script 01's bootstrap and the variants are compared with the same random draws.
SEED <- c(A3 = 2026L, A4 = 4026L)
ALL <- list()
for (o in names(OUT)) for (v in names(VAR)) {
  section(sprintf("%s | %s", OUT[o], v))
  P <- prepare_spec_time(F, o, sm_time = VAR[[v]][1], net_time = VAR[[v]][2])
  X <- paper_rows(P, seed = SEED[[o]])[, `:=`(outcome = o, variant = v)]
  print(X[, .(table, row, events, OR, p)], row.names = FALSE)
  ALL[[length(ALL) + 1]] <- X
}
ALL <- rbindlist(ALL)

# ══════════════════════════════════════════════════════════════════════════════
# 5. Summary: one column per variant
# ══════════════════════════════════════════════════════════════════════════════
section("5. Summary (AOR (95% CI), p; events in brackets for Table 2)")
ALL[, value := fifelse(is.na(events), sprintf("%s %s", OR, fifelse(p == "", "", paste0("p=", p))),
                       sprintf("[%d] %s p=%s", events, OR, p))]
for (o in names(OUT)) {
  cat("\n", OUT[o], "\n", sep = "")
  w <- dcast(ALL[outcome == o], table + row ~ variant, value.var = "value")
  w <- w[match(unique(ALL[outcome == o, paste(table, row)]), paste(table, row))]
  setcolorder(w, c("table", "row", names(VAR)))
  print(w, row.names = FALSE)
}
fwrite(ALL[, .(outcome, variant, table, row, events, OR, p)], file.path(run$dir, "contemporaneous_tables.csv"))
say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
