# 32-probs.R — the interaction on the probability scale (g-computation): average predicted chance of
# starting to vape in a semester, by social-media exposure (P10/P50/P90) and peer exposure (0, 1/7, 2/7).
source("paneo/12-engine.R")
SPECS <- list(`Paper (Table 2 data)` = default_spec(A = "A1", U = "U1", N = "N1", I = "I0", Z = "Z1", S = "S1", X = "X1", F = "F1", CL = "vid", paperlag = TRUE, T = "T3", fix = TRUE),
              `Proposed R0 (A3)` = default_spec(), `S* (A4, p = .047)` = default_spec(A = "A4", I = "I35"))
grid_E <- c(0, 1/7, 2/7)
pred_one <- function(D, sp) {
  cv <- covs(sp, D); m <- mean(D$sm); D[, smc := sm - m]; D[, E25 := E / 0.25]
  f <- glm(as.formula(paste("y ~ smc * E25 +", cv)), binomial, D)
  qs <- quantile(D$sm, c(.1, .5, .9), type = 1)
  rbindlist(lapply(seq_along(qs), function(i) rbindlist(lapply(grid_E, function(e) {
    nd <- copy(D); nd[, smc := qs[i] - m]; nd[, E25 := e / 0.25]
    data.table(sm_level = c("P10", "P50", "P90")[i], sm_value = qs[i], E = e, prob = mean(predict(f, nd, type = "response")))
  }))))
}
out <- rbindlist(parallel::mclapply(names(SPECS), function(nm) {
  sp <- SPECS[[nm]]; ds <- build_data(sp)
  r <- rbindlist(lapply(ds, pred_one, sp = sp))[, .(sm_value = mean(sm_value), prob = mean(prob)), by = .(sm_level, E)]
  r[, `:=`(spec = nm, base_rate = mean(sapply(ds, function(D) mean(D$y))))]
}, mc.cores = 3))
out[, prob_pct := round(100 * prob, 2)]
print(dcast(out, spec + sm_level ~ round(E, 3), value.var = "prob_pct"))
fwrite(out, file.path(OUT, "verdict", "predicted_probabilities.csv"))
