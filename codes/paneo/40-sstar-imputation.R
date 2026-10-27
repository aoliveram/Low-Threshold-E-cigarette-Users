# 40-sstar-imputation.R — S* (A4, U1, T3, N4, Z3, S3 lagged, X2, F2) under every imputation of the
# unseen friends' use, plus bootstrap inference and a known-truth simulation.
#   Expected-value (deterministic) imputations: the exposure is E[share of friends who used | data]
#   Multiple imputation (draws): each draw assigns 0/1 to every unseen friend; Rubin's rules
#   Proper versions: the imputation parameters themselves are drawn from their sampling distribution
source("paneo/12-engine.R")
suppressMessages(library(MASS))
OD <- file.path(OUT, "sstar"); dir.create(OD, showWarnings = FALSE)
SP <- default_spec(A = "A4", I = "I3")                       # base: everything as S*; I set per variant
R <- risk_rows(SP)
ED <- alter_table(R, SP)                                     # one row per (risk row, friend); st = known status
R <- merge(R, RESPW[, .(record_id, wn = wave, ego_resp_net = TRUE)], by = c("record_id", "wn"), all.x = TRUE)
R[is.na(ego_resp_net), ego_resp_net := FALSE]; R[, net_ok := wn >= 2L]

# ---- ingredients ------------------------------------------------------------------
# prevalence of U1 status among respondents: by school x wave (S*), by school, by wave; with counts for Beta draws
BW <- merge(ST[resp == TRUE, .(record_id, ws = wave, ever_w3, ever_w5)],
            readRDS(file.path(DATA, "panel_long.rds"))[, .(record_id, ws = wave, school = schoolid)], by = c("record_id", "ws"))
BW <- BW[school %in% SCHOOLS][, s := fifelse(is_c2(school), ever_w5, ever_w3)]
PSW <- BW[, .(n_sw = .N, u_sw = sum(s)), by = .(school, ws)]
PS  <- BW[, .(n_s = .N, u_s = sum(s)), by = .(school)]
PW  <- BW[, .(n_w = .N, u_w = sum(s)), by = .(cohort = fifelse(is_c2(school), "C2", "C1"), ws)]
ED <- merge(ED, PSW, by = c("school", "ws"), all.x = TRUE); ED <- merge(ED, PS, by = "school", all.x = TRUE)
ED <- merge(ED, PW, by = c("cohort", "ws"), all.x = TRUE)
# who names each friend (any type) and how many of them had used by the status wave
NOM <- unique(EN[, .(school, wn = wave, akey, ego)])
NOM <- NOM[unique(ED[, .(school, wn, ws)]), on = .(school, wn), nomatch = 0L, allow.cartesian = TRUE]
NOM <- ST[, .(ego = record_id, ws = wave, ever_w3, ever_w5)][NOM, on = .(ego, ws)]
NOM[, s := fifelse(is_c2(school), ever_w5, ever_w3)][is.na(s), s := 0]
NS <- NOM[, .(Nn = .N, Un = sum(s)), by = .(school, wn, ws, akey)]
ED <- merge(ED, NS, by = c("school", "wn", "ws", "akey"), all.x = TRUE)
ED[is.na(Nn), `:=`(Nn = 0L, Un = 0)]
ED[, unk := is.na(st)]
# network-informed model: trained on visible friends (status known) with the same features unseen friends have
TR <- unique(ED[type == "visible" & !is.na(st), .(school, wn, ws, akey, st, Nn, Un)])
TR[, `:=`(share = fifelse(Nn > 0, Un / Nn, 0), lnN = log1p(Nn))]
FM <- glm(st ~ share + lnN + factor(school) + factor(ws), binomial, TR)
ED[, `:=`(share = fifelse(Nn > 0, Un / Nn, 0), lnN = log1p(Nn))]
msg("training rows for the network model: %d visible friend-waves; AUC-free summary: share coef %.2f", nrow(TR), coef(FM)["share"])

# probability that each unknown friend used, by variant (theta = optional parameter draw)
prob <- function(v, theta = NULL) {
  p_sw <- if (is.null(theta$psw)) ED$u_sw / ED$n_sw else theta$psw
  switch(v$kind,
    const    = rep(v$q, nrow(ED)),
    sw       = pmin(1, v$f * p_sw),
    school   = ED$u_s / ED$n_s,
    wave     = ED$u_w / ED$n_w,
    homophily = { q <- (ED$Un + v$alpha * p_sw) / (ED$Nn + v$alpha); fifelse(ED$type == "typed", p_sw, q) },
    model    = { b <- if (is.null(theta$beta)) coef(FM) else theta$beta
                 X <- model.matrix(delete.response(terms(FM)), ED[, .(share, lnN, school = factor(school, levels = sort(unique(TR$school))),
                                                                       ws = factor(ws, levels = sort(unique(TR$ws))))], xlev = FM$xlevels)
                 as.vector(plogis(X %*% b)) })
}
draw_theta <- function(v) {                     # parameter uncertainty for "proper" versions
  d <- PSW[, .(school, ws, psw = rbeta(.N, 1 + u_sw, 1 + n_sw - u_sw))]       # one draw per school x wave
  th <- list(psw = d$psw[match(paste(ED$school, ED$ws), paste(d$school, d$ws))])
  if (v$kind == "model") th$beta <- MASS::mvrnorm(1, coef(FM), vcov(FM))
  th
}
# keyed so the same friend gets the same value for every ego in a draw
key <- ED[, paste(school, wn, ws, akey)]
assemble <- function(val) {                     # val: imputed value for unknown friends (prob or 0/1)
  x <- fifelse(ED$unk, val, ED$st)
  agg <- data.table(rid = ED$rid, x = x)[, .(n = .N, k = sum(x)), by = rid]
  D <- merge(R, agg, by = "rid", all.x = TRUE); D[is.na(n), `:=`(n = 0L, k = 0)]
  D[, `:=`(E = fifelse(n > 0, k / n, 0), nu = n - k, iso = as.integer(n == 0))]
  D <- D[ego_resp_net == TRUE]
  finish_data(D, SP)
}
fitMB <- function(D, boot_ids = NULL) {
  if (!is.null(boot_ids)) { D <- D[data.table(record_id = boot_ids, bid = seq_along(boot_ids)), on = "record_id", allow.cartesian = TRUE]; D[, cl := paste(record_id, bid)] }
  cv <- covs(SP, D); D[, smc := sm - mean(sm)]; D[, E25 := E / 0.25]
  f <- glm(as.formula(paste("y ~ smc * E25 +", cv)), binomial, D)
  V <- sandwich::vcovCL(f, cluster = D$cl)
  c(b = unname(coef(f)["smc:E25"]), se = unname(sqrt(V["smc:E25", "smc:E25"])), events = sum(D$y), n = nrow(D))
}
fitMC <- function(D) {
  cv <- covs(SP, D); D[, lowf := as.integer(E <= 1/7 + 1e-9)]
  f <- glm(as.formula(paste("y ~ lowf + sm:factor(lowf) +", cv)), binomial, D); V <- sandwich::vcovCL(f, cluster = D$cl)
  a <- "sm:factor(lowf)1"; h <- "sm:factor(lowf)0"
  c(b = unname(coef(f)[a] - coef(f)[h]), se = unname(sqrt(V[a, a] + V[h, h] - 2 * V[a, h])))
}
uniq_draw <- function(pr) { u <- data.table(kk = key, pr = pr)[, .(pr = pr[1]), by = kk]; u[, v := as.numeric(runif(.N) < pr)]; u$v[match(key, u$kk)] }

VARIANTS <- list(
  `I0 none use (lower bound)` = list(kind = "const", q = 0), `I1 all use (upper bound)` = list(kind = "const", q = 1),
  `I2 q = 0.06 (current-use prevalence)` = list(kind = "const", q = 0.06), `I2 q = 0.12 (ever-use prevalence)` = list(kind = "const", q = 0.12),
  `I2 q = 0.20` = list(kind = "const", q = 0.20), `I2 q = 0.30` = list(kind = "const", q = 0.30),
  `I3 school x wave prevalence (S*)` = list(kind = "sw", f = 1), `I3 school prevalence` = list(kind = "school"), `I3 wave prevalence` = list(kind = "wave"),
  `I4 x 1.25` = list(kind = "sw", f = 1.25), `I4 x 1.5` = list(kind = "sw", f = 1.5), `I4 x 2` = list(kind = "sw", f = 2),
  `I6 homophily, alpha = 1` = list(kind = "homophily", alpha = 1), `I6 homophily, alpha = 2` = list(kind = "homophily", alpha = 2),
  `I6 homophily, alpha = 5` = list(kind = "homophily", alpha = 5), `Network model (logistic)` = list(kind = "model"))
B <- 300L; M <- 50L
jobs <- rbind(data.table(variant = names(VARIANTS), how = "expected value"),
              data.table(variant = c("I3 school x wave prevalence (S*)", "I6 homophily, alpha = 2", "Network model (logistic)", "I4 x 1.5"), how = "expected value + bootstrap"),
              data.table(variant = c("I3 school x wave prevalence (S*)", "I6 homophily, alpha = 2", "Network model (logistic)"), how = "multiple imputation"),
              data.table(variant = c("I3 school x wave prevalence (S*)", "I6 homophily, alpha = 2", "Network model (logistic)"), how = "proper multiple imputation"))
run_job <- function(j) {
  v <- VARIANTS[[jobs$variant[j]]]; how <- jobs$how[j]; set.seed(100 + j)
  if (how == "expected value") {
    D <- assemble(prob(v)); a <- fitMB(D); c2 <- fitMC(D)
    return(data.table(variant = jobs$variant[j], how, b = a["b"], se = a["se"], p = 2 * pnorm(-abs(a["b"] / a["se"])), lo = a["b"] - 1.96 * a["se"], hi = a["b"] + 1.96 * a["se"],
                      bC = c2["b"], seC = c2["se"], events = a["events"], n = a["n"], meanE = mean(D$E)))
  }
  if (how == "expected value + bootstrap") {
    ids <- unique(assemble(prob(v))$record_id)
    bs <- replicate(B, { D <- assemble(prob(v, draw_theta(v))); fitMB(D, sample(ids, length(ids), TRUE))["b"] })
    b0 <- fitMB(assemble(prob(v)))["b"]
    return(data.table(variant = jobs$variant[j], how, b = b0, se = sd(bs), p = 2 * min(mean(bs <= 0), mean(bs >= 0)),
                      lo = quantile(bs, .025), hi = quantile(bs, .975), bC = NA_real_, seC = NA_real_, events = NA_real_, n = NA_real_, meanE = NA_real_))
  }
  proper <- how == "proper multiple imputation"
  est <- t(replicate(M, { pr <- prob(v, if (proper) draw_theta(v) else NULL); D <- assemble(uniq_draw(pr)); fitMB(D)[c("b", "se")] }))
  b <- mean(est[, "b"]); se <- sqrt(mean(est[, "se"]^2) + (1 + 1 / M) * var(est[, "b"]))
  data.table(variant = jobs$variant[j], how, b = b, se = se, p = 2 * pnorm(-abs(b / se)), lo = b - 1.96 * se, hi = b + 1.96 * se,
             bC = NA_real_, seC = NA_real_, events = NA_real_, n = NA_real_, meanE = NA_real_)
}
res <- rbindlist(parallel::mclapply(seq_len(nrow(jobs)), run_job, mc.cores = NCORES, mc.preschedule = FALSE))
res[, `:=`(R = exp(-2 * b), R_lo = exp(-2 * hi), R_hi = exp(-2 * lo), R_MC = exp(bC), p_MC = 2 * pnorm(-abs(bC / seC)))]
fwrite(res, file.path(OD, "sstar_imputation.csv"))
print(res[, .(variant, how, R = round(R, 2), R_lo = round(R_lo, 2), R_hi = round(R_hi, 2), p = signif(p, 2), R_MC = round(R_MC, 2), p_MC = signif(p_MC, 2), meanE = round(meanE, 3))], row.names = FALSE)

# ---- known-truth simulation: which approach recovers the true R? -------------------
# Truth: unseen friends' use drawn once from the school x wave prevalence; outcomes generated with a
# known interaction on the TRUE exposure; then R estimated from the expected-value exposure and from MI.
v <- VARIANTS[["I3 school x wave prevalence (S*)"]]; pr0 <- prob(v)
D_ev <- assemble(pr0)
f0 <- glm(as.formula(paste("y ~ smc + E25 +", covs(SP, D_ev))), binomial, D_ev[, `:=`(smc = sm - mean(sm), E25 = E / 0.25)])
sim <- function(s) {
  set.seed(9000 + s)
  Dt <- assemble(uniq_draw(pr0)); Dt[, `:=`(smc = sm - mean(sm), E25 = E / 0.25)]
  eta <- predict(f0, Dt, type = "link") - (log(1.7) / 2) * Dt$smc * Dt$E25          # true R = 1.7 on the true exposure
  y <- rbinom(nrow(Dt), 1, plogis(eta)); yk <- data.table(rid = Dt$rid, ysim = y)
  ev <- copy(D_ev)[yk, on = "rid", nomatch = 0L][, y := ysim]
  a <- fitMB(ev)
  mi <- t(replicate(20, { Dm <- assemble(uniq_draw(pr0))[yk, on = "rid", nomatch = 0L][, y := ysim]; fitMB(Dm)[c("b", "se")] }))
  bm <- mean(mi[, "b"]); sem <- sqrt(mean(mi[, "se"]^2) + 1.05 * var(mi[, "b"]))
  data.table(sim = s, b_ev = a["b"], se_ev = a["se"], b_mi = bm, se_mi = sem)
}
S <- rbindlist(parallel::mclapply(1:160, sim, mc.cores = NCORES))
truth <- -log(1.7) / 2
SS <- S[, .(true_R = 1.7, mean_R_expected = exp(-2 * mean(b_ev)), mean_R_MI = exp(-2 * mean(b_mi)),
            cover_expected = mean(abs(b_ev - truth) <= 1.96 * se_ev), cover_MI = mean(abs(b_mi - truth) <= 1.96 * se_mi),
            power_expected = mean(b_ev < 0 & abs(b_ev / se_ev) > 1.96), power_MI = mean(b_mi < 0 & abs(b_mi / se_mi) > 1.96), sims = .N)]
print(SS); fwrite(SS, file.path(OD, "sstar_truth_simulation.csv"))
