# 13-layer1.R — Layer 1: rebuild the submitted paper's tables with the engine (paper settings).
source("paneo/12-engine.R")
H <- function(...) default_spec(A = "A1", U = "U1", N = "N1", I = "I0", Z = "Z1", S = "S1", X = "X1", F = "F1", CL = "vid", paperlag = TRUE, ...)
specs <- list(
  H1_overall = H(T = "T3", Stime = "lag", fix = TRUE),  H1_insta = H(T = "T3", S = "S7", Stime = "lag", fix = TRUE),
  H2_overall = H(T = "T3", Stime = "cur", fix = FALSE), H2_insta = H(T = "T3", S = "S7", Stime = "cur", fix = FALSE),
  H3_overall = H(T = "T1", Stime = "lag", fix = FALSE), H3_insta = H(T = "T1", S = "S7", Stime = "lag", fix = FALSE),
  H4_overall = H(T = "T1", Stime = "cur", fix = FALSE), H4_insta = H(T = "T1", S = "S7", Stime = "cur", fix = FALSE),
  H5_snap_cur = H(T = "T1", S = "S8", Stime = "cur", fix = FALSE), H5_snap_lag = H(T = "T1", S = "S8", Stime = "lag", fix = FALSE))
target <- data.table(spec = rep(names(specs), each = 2), outcome = rep(c("no_exposure", "low_threshold"), length(specs)),
  N = c(6797, 6797, 5751, 5751, 8051, 8051, 6939, 6939, 6797, 6797, 5751, 5751, 8939, 8939, 7672, 7672, 2579, 2579, 1845, 1845),
  events = c(153, 192, 136, 173, 202, 240, 188, 224, 125, 162, 106, 141, 173, 231, 156, 213, 65, 93, 40, 55),
  OR = c(1.16, 1.14, 1.15, 1.14, 1.24, 1.27, 1.24, 1.26, 1.18, 1.11, 1.15, 1.10, 1.28, 1.24, 1.26, 1.23, 1.16, 1.17, 1.18, 1.23))
res <- rbindlist(parallel::mclapply(names(specs), function(nm) {
  r <- run_spec(specs[[nm]], models = "MA"); r[, spec := nm] }, mc.cores = NCORES))
res <- res[grepl("^OR_SM_low", quantity)]
res[, outcome := ifelse(grepl("c0.000", quantity), "no_exposure", "low_threshold")]
cmp <- merge(target, res[, .(spec, outcome, N_mine = n, events_mine = events, OR_mine = round(est, 4), p = signif(p, 3))], by = c("spec", "outcome"))
cmp[, match := N == N_mine & events == events_mine & round(OR, 2) == round(OR_mine, 2)]
print(cmp[order(match(spec, names(specs)))], row.names = FALSE)
fwrite(cmp, file.path(OUT, "layer1", "layer1_reproduction.csv"))
msg("Layer 1: %d of %d cells reproduced exactly (N, events, OR to 2 decimals)", sum(cmp$match), nrow(cmp))

# H6 — the 15-Oct stage 2 (after submission): additive and interaction models on the H1 sample
h6 <- run_spec(H(T = "T3", Stime = "lag", fix = TRUE), models = c("M0", "MB"))
print(h6[, .(model, quantity, n, events, est = round(est, 3), lo = round(est_lo, 3), hi = round(est_hi, 3), p = signif(p, 3))])
msg("H6 target: additive OR_SM 1.207, OR_peer25 1.224; interaction 1.028 (p = 0.644)")
fwrite(h6, file.path(OUT, "layer1", "layer1_H6_stage2.csv"))
