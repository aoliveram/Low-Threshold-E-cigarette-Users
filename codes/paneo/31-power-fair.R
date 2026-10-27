# 31-power-fair.R — fair power comparison: simulate under ONE true pattern and test with BOTH
# the interaction (MB) and the stratified model (MC, cut 1/7).
#   truth "continuous": SM effect shrinks smoothly with peer exposure; R_true defined as in MB (E 0 vs 50%)
#   truth "step":       SM effect differs between E <= 1/7 and E > 1/7; R_true defined as in MC
source("paneo/12-engine.R")
BASES <- list(`R0 (A3)` = default_spec(I = "I35"), `S* (A4)` = default_spec(A = "A4", I = "I35"))
prep <- lapply(BASES, function(sp) {
  D <- build_data(sp)[[1]]; cv <- covs(sp, D)
  D[, smc := sm - mean(sm)]; D[, E25 := E / 0.25]; D[, lowf := as.integer(E <= 1/7 + 1e-9)]
  f0 <- glm(as.formula(paste("y ~ smc + E25 +", cv)), binomial, D); list(D = D, cv = cv, eta0 = predict(f0, type = "link"))
})
jobs <- CJ(base = names(BASES), truth = c("continuous", "step"), R = c(1.3, 1.5, 2), chunk = 1:8)
one <- function(j) {
  p <- prep[[jobs$base[j]]]; D <- copy(p$D); set.seed(5000 + j); b <- log(jobs$R[j])
  eta <- if (jobs$truth[j] == "continuous") p$eta0 - (b / 2) * D$smc * D$E25 else p$eta0 + (b / 2) * D$smc * (2 * D$lowf - 1)
  out <- replicate(38, {
    D[, y := rbinom(.N, 1, plogis(eta))]
    f1 <- glm(as.formula(paste("y ~ smc * E25 +", p$cv)), binomial, D); V1 <- sandwich::vcovCL(f1, cluster = D$cl)
    eB <- -2 * coef(f1)["smc:E25"]; sB <- 2 * sqrt(V1["smc:E25", "smc:E25"])
    f2 <- glm(as.formula(paste("y ~ lowf + sm:factor(lowf) +", p$cv)), binomial, D); V2 <- sandwich::vcovCL(f2, cluster = D$cl)
    a <- "sm:factor(lowf)1"; h <- "sm:factor(lowf)0"; eC <- coef(f2)[a] - coef(f2)[h]; sC <- sqrt(V2[a, a] + V2[h, h] - 2 * V2[a, h])
    c(MB = unname(eB > 0 & 2 * pnorm(-abs(eB / sB)) < .05), MC = unname(eC > 0 & 2 * pnorm(-abs(eC / sC)) < .05))
  })
  data.table(base = jobs$base[j], truth = jobs$truth[j], R = jobs$R[j], MB = out["MB", ], MC = out["MC", ])
}
S <- rbindlist(parallel::mclapply(seq_len(nrow(jobs)), one, mc.cores = NCORES, mc.preschedule = FALSE))
P <- S[, .(sims = .N, power_MB = round(mean(MB), 2), power_MC = round(mean(MC), 2)), by = .(base, truth, R)]
print(P[order(base, truth, R)], row.names = FALSE); fwrite(P, file.path(OUT, "verdict", "power_fair.csv"))
