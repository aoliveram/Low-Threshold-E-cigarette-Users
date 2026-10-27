# 21-power.R — how large must "substitution" be for us to detect it?
# Simulation on the observed risk sets: outcomes drawn from the fitted additive model plus an
# interaction of known size R; power = share of simulations with R-hat > 1 and p < .05.
source("paneo/12-engine.R")
dir.create(file.path(OUT, "stage2"), showWarnings = FALSE)
BASES <- list(`A3 (reference)` = default_spec(I = "I35"), `A4 (30-day)` = default_spec(A = "A4", I = "I35"))
TRUE_R <- c(1, 1.25, 1.5, 2, 2.5, 3); NSIM <- 300L
prep <- lapply(BASES, function(sp) {
  D <- build_data(sp)[[1]]; cv <- covs(sp, D)
  D[, smc := sm - mean(sm)]; D[, E25 := E / 0.25]; D[, lowf := as.integer(E <= 1/7 + 1e-9)]
  f0 <- glm(as.formula(paste("y ~ smc + E25 +", cv)), binomial, D)
  list(D = D, cv = cv, eta0 = predict(f0, type = "link"))
})
jobs <- CJ(base = names(BASES), model = c("MB", "MC"), R = TRUE_R, chunk = 1:8)
sim_one <- function(j) {
  p <- prep[[jobs$base[j]]]; D <- copy(p$D); set.seed(1000 + j)
  b <- log(jobs$R[j])
  eta <- if (jobs$model[j] == "MB") p$eta0 - (b / 2) * D$smc * D$E25 else p$eta0 + (b / 2) * D$smc * (2 * D$lowf - 1)
  out <- replicate(ceiling(NSIM / 8), {
    D[, y := rbinom(.N, 1, plogis(eta))]
    if (jobs$model[j] == "MB") {
      f <- glm(as.formula(paste("y ~ smc * E25 +", p$cv)), binomial, D); V <- sandwich::vcovCL(f, cluster = D$cl)
      est <- -2 * coef(f)["smc:E25"]; se <- 2 * sqrt(V["smc:E25", "smc:E25"])
    } else {
      f <- glm(as.formula(paste("y ~ lowf + sm:factor(lowf) +", p$cv)), binomial, D); V <- sandwich::vcovCL(f, cluster = D$cl)
      t1 <- "sm:factor(lowf)1"; t0 <- "sm:factor(lowf)0"
      est <- coef(f)[t1] - coef(f)[t0]; se <- sqrt(V[t1, t1] + V[t0, t0] - 2 * V[t1, t0])
    }
    c(est = unname(est), p = unname(2 * pnorm(-abs(est / se))), se = unname(se))
  })
  data.table(base = jobs$base[j], model = jobs$model[j], R = jobs$R[j], est = out["est", ], p = out["p", ], se = out["se", ])
}
S <- rbindlist(parallel::mclapply(seq_len(nrow(jobs)), sim_one, mc.cores = NCORES, mc.preschedule = FALSE))
P <- S[, .(power = mean(p < .05 & est > 0), median_R_hat = exp(median(est)), se = median(se), sims = .N), by = .(base, model, R)]
fwrite(P, file.path(OUT, "stage2", "power_simulation.csv"))
print(P[order(base, model, R)], row.names = FALSE)
# events needed for 80% power at R = 1.3, scaling the simulated SE by sqrt(events)
ev <- sapply(prep, function(p) sum(p$D$y))
need <- P[R == 1, .(se = median(se)), by = .(base, model)][, events_now := ev[base]]
need[, events_for_R1.3 := round(events_now * (se / (log(1.3) / 2.8))^2)][, MDE_80 := round(exp(2.8 * se), 2)]
print(need); fwrite(need, file.path(OUT, "stage2", "power_mde.csv"))
