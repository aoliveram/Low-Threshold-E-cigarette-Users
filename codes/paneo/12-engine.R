# 12-engine.R — builds the analysis rows for one specification and fits the models.
# A specification is a named list; see default_spec() for every field and its codes.
source("paneo/00-config.R")
suppressMessages({ library(sandwich); library(nnet) })

FR <- readRDS(file.path(DATA, "frame.rds"))
V <- FR$V; PP <- FR$PP; E1 <- FR$E1; EN <- FR$EN; ST <- FR$ST; PREV <- FR$PREV
setkey(ST, record_id, wave)

default_spec <- function(...) {
  s <- list(A = "A3", U = "U1", T = "T3", N = "N4", I = "I7", q = NA_real_, f = NA_real_, Z = "Z3",
            S = "S3", Stime = "lag", X = "X2", F = "F2", CL = "record", sample = "all",
            fix = TRUE, cuts = c(0, 0.25), draws = 20L, typed = "as_spec", typed_m = NA_real_, seed = 1L,
            paperlag = FALSE)
  m <- list(...); s[names(m)] <- m
  if (!"cuts" %in% names(m)) s$cuts <- cut_grid(s$N)
  s
}
# D8 grid (fractions), chosen by the rule in §5 D8: consecutive cuts add >= 8 pp of adopters and the
# high group keeps >= 8% of adopters; the paper's 1/4 is always added as an anchor.
cut_grid <- function(N) if (N %in% c("N1", "N2")) c(0, 1/5, 1/4, 1/3) else c(0, 1/7, 1/4, 2/7)

# ---- degrees in the paper's network (E1) -----------------------------------------
DEG <- rbind(E1[, .(out_E1 = .N), by = .(school, wave, record_id = ego)][, in_E1 := NA_integer_],
             E1[, .(in_E1 = .N), by = .(school, wave, record_id = alter)][, out_E1 := NA_integer_], fill = TRUE)
DEG <- DEG[, .(out_E1 = sum(out_E1, na.rm = TRUE), in_E1 = sum(in_E1, na.rm = TRUE)), by = .(school, wave, record_id)]
NAMED <- EN[, .(n_named = .N), by = .(school, wave, record_id = ego)]
RESPW <- ST[resp == TRUE, .(record_id, wave)]

status_col <- function(U, fill = FALSE) switch(U, U1 = "ever_win", U2 = "ever_all",
                                               U3 = if (fill) "cur6_fill" else "cur6", U4 = if (fill) "cur30_fill" else "cur30")
ST[, ever_win := NA_real_]   # resolved per cohort at join time
prev_col <- function(U) switch(U, U1 = "p_ever_win", U2 = "p_ever_all", U3 = "p_cur6", U4 = "p_cur30")

# ---- risk rows and outcome -----------------------------------------------------------
risk_rows <- function(sp) {
  R <- copy(PP)
  if (sp$A == "A1") { R <- R[risk_A1 == TRUE]; R[, y := y_A1]; if (isTRUE(sp$fix)) R <- R[obs_A1 == TRUE] }
  else { R <- R[get(paste0("risk_", sp$A)) == TRUE]; R[, y := get(paste0("y_", sp$A))] }
  R[, tn := if (sp$T == "T3") gp - 1L else gp]
  R[, ts := if (sp$T == "T1") gp else gp - 1L]
  R[, `:=`(wn = gp_to_wave(school, tn), ws = gp_to_wave(school, ts))]
  R[, rid := .I]
  R
}

# ---- alter statuses for the network of each risk row -----------------------------------
alter_table <- function(R, sp) {
  key <- R[, .(rid, school, cohort, ego = record_id, wn, ws)]
  if (sp$N == "N1") {
    ed <- E1[key, on = .(school, wave = wn, ego), nomatch = 0L, allow.cartesian = TRUE,
             .(rid, school, cohort, wn = i.wn, ws, alter, type = "visible")]
    ed[, akey := alter]
  } else {
    keep <- switch(sp$N, N2 = "visible", N3 = c("visible", "part_noresp", "nonpart"),
                   N4 = c("visible", "part_noresp", "nonpart", "typed"))
    if (sp$typed == "W0") keep <- setdiff(keep, "typed")
    ed <- EN[type %in% keep][key, on = .(school, wave = wn, ego), nomatch = 0L, allow.cartesian = TRUE,
             .(rid, school, cohort, wn = i.wn, ws, alter, akey, type)]
  }
  # observed status of participant alters at the status wave
  sc <- status_col(sp$U); scf <- status_col(sp$U, fill = TRUE)
  stt <- ST[, .(alter = record_id, ws = wave, ever_w3, ever_w5, ever_all, cur6, cur30, cur6_fill, cur30_fill)]
  ed <- stt[ed, on = .(alter, ws)]
  if (sp$U == "U1") { ed[, st := fifelse(cohort == "C2", ever_w5, ever_w3)]; ed[, stf := st] }
  else { ed[, st := get(sc)]; ed[, stf := get(scf)] }
  ed[type %in% c("nonpart", "typed"), `:=`(st = NA_real_, stf = NA_real_)]
  # prevalence for the school and status wave
  pv <- PREV[, .(school, ws = wave, p_ever_w3, p_ever_w5, p_ever_all, p_cur6, p_cur30)]
  ed <- pv[ed, on = .(school, ws)]
  ed[, p := switch(sp$U, U1 = fifelse(cohort == "C2", p_ever_w5, p_ever_w3), U2 = p_ever_all, U3 = p_cur6, U4 = p_cur30)]
  ed[is.na(p), p := mean(p, na.rm = TRUE)]
  ed
}

# homophily-based probability for non-participants: share of users among everyone who names them
homophily_q <- function(ed, sp, alpha = 2) {
  nom <- EN[type == "nonpart", .(school, wn = wave, akey, ego)]
  nom <- unique(nom)
  ws_of <- unique(ed[type == "nonpart", .(school, wn, ws)])
  nom <- nom[ws_of, on = .(school, wn), nomatch = 0L, allow.cartesian = TRUE]
  stt <- ST[, .(ego = record_id, ws = wave, ever_w3, ever_w5, ever_all, cur6_fill, cur30_fill)]
  nom <- stt[nom, on = .(ego, ws)]
  nom[, s := switch(sp$U, U1 = fifelse(is_c2(school), ever_w5, ever_w3), U2 = ever_all, U3 = cur6_fill, U4 = cur30_fill)]
  nom[is.na(s), s := 0]
  nom[, .(Nn = .N, Un = sum(s)), by = .(school, wn, ws, akey)]
}

# probability that each unknown alter uses, by imputation rule
impute_prob <- function(ed, sp) {
  ed[, pr := st]                                                   # known statuses
  unk <- is.na(ed$st)
  rule <- sp$I
  if (rule == "I0") ed[unk, pr := 0]
  if (rule == "I1") ed[unk, pr := 1]
  if (rule == "I2") ed[unk, pr := sp$q]
  if (rule == "I3") ed[unk, pr := p]
  if (rule == "I4") ed[unk, pr := pmin(1, sp$f * p)]
  if (rule %in% c("I35", "I6", "I7")) {
    ed[unk & type %in% c("visible", "part_noresp"), pr := fifelse(is.na(stf), p, stf)]
    ed[unk & type %in% c("nonpart", "typed"), pr := p]
    if (rule %in% c("I6", "I7")) {
      hq <- homophily_q(ed, sp)
      ed[hq, on = .(school, wn, ws, akey), `:=`(Nn = i.Nn, Un = i.Un)]
      ed[unk & type == "nonpart" & !is.na(Nn), pr := (Un + 2 * p) / (Nn + 2)]
    }
  }
  # value-of-information scenarios for typed names (§4.3)
  if (sp$typed == "W1") ed[type == "typed", pr := 0]
  if (sp$typed == "W2") ed[type == "typed", pr := 1]
  if (sp$typed == "W3") ed[type == "typed", pr := p]
  ed
}

# ---- one analysis data set (deterministic) or a list of them (draws) ----------------------
build_data <- function(sp) {
  if (sp$U == "U5") {                     # perceived friends' use (0-5 friends), no network
    R <- risk_rows(sp)
    R[, k := if (sp$T == "T1") pfu else pfu_lag]
    R <- R[!is.na(k)]
    R[, `:=`(n = 5L, E = k / 5, nu = 5 - k, iso = 0L)]
    return(list(finish_data(R, sp)))
  }
  R <- risk_rows(sp)
  ed <- impute_prob(alter_table(R, sp), sp)
  # which networks exist: the paper's network only on the school's analysis waves
  if (sp$N == "N1") R[, net_ok := wn %in% 3:10 & mapply(function(s, w) w %in% school_waves(s), school, wn)]
  else R[, net_ok := wn >= 2L]
  R <- merge(R, RESPW[, .(record_id, wn = wave, ego_resp_net = TRUE)], by = c("record_id", "wn"), all.x = TRUE)
  R[is.na(ego_resp_net), ego_resp_net := FALSE]
  u <- unique(ed[is.na(st), .(school, wn, ws, akey, pr)])
  stochastic <- sp$I == "I7" && nrow(u) > 0          # nothing to draw if every status is known
  draws <- if (stochastic) seq_len(sp$draws) else 1L
  set.seed(sp$seed)
  if (stochastic) U <- matrix(runif(nrow(u) * length(draws)) < u$pr, nrow(u))
  lapply(draws, function(d) {
    e <- ed
    if (stochastic) { u[, val := as.numeric(U[, d])]; e <- copy(ed); e[u, on = .(school, wn, ws, akey), val := i.val]
      e[, pr2 := fifelse(is.na(st), val, st)] } else e[, pr2 := pr]
    agg <- e[, .(n = .N, k = sum(pr2)), by = rid]
    D <- merge(R, agg, by = "rid", all.x = TRUE)
    D[is.na(n), `:=`(n = 0L, k = 0)]
    D[net_ok == FALSE, `:=`(n = NA_integer_, k = NA_real_)]
    D[, `:=`(E = fifelse(n > 0, k / n, 0), nu = n - k, iso = as.integer(n == 0))]
    if (sp$Z == "Z2") D <- D[n > 0]
    if (sp$Z == "Z3") D <- D[ego_resp_net == TRUE | sp$N == "N1"]
    D[is.na(n), E := NA_real_]
    finish_data(D, sp)
  })
}

finish_data <- function(D, sp) {
  D <- merge(D, V[, .(vid, female_X1, hispanic_X1, asian_X1, sexmin_X1, paredu_X1, female_X2, hispanic_X2, asian_X2, sexmin_X2, paredu_X2)], by = "vid")
  D <- merge(D, DEG[, .(school, wave, record_id, out_t = out_E1, in_t = in_E1)], by = c("school", "wave", "record_id"), all.x = TRUE)
  D <- merge(D, DEG[, .(school, wn = wave, record_id, in_tn = in_E1)], by = c("school", "wn", "record_id"), all.x = TRUE)
  D <- merge(D, NAMED[, .(school, wn = wave, record_id, n_named = n_named)], by = c("school", "wn", "record_id"), all.x = TRUE)
  for (v in c("out_t", "in_t", "in_tn", "n_named")) set(D, which(is.na(D[[v]])), v, 0L)
  x <- if (sp$X == "X1") "_X1" else "_X2"
  D[, `:=`(female = get(paste0("female", x)), hispanic = get(paste0("hispanic", x)), asian = get(paste0("asian", x)),
           sexmin = get(paste0("sexmin", x)), paredu = get(paste0("paredu", x)))]
  if (sp$X %in% c("X1", "X2")) D[, `:=`(mddv = mdd, outdeg = out_t, indeg = in_t)]
  else D[, `:=`(mddv = mdd_lag, outdeg = n, indeg = in_tn)]
  D[, pfuv := if (sp$X == "X4") pfu_lag else 0]
  D[, sm := get(paste0(sp$S, if (sp$Stime == "lag") "_lag" else ""))]
  if (isTRUE(sp$paperlag)) D[gp == min(GPS), `:=`(sm = if (sp$Stime == "lag") NA_real_ else sm, mdd_lag = NA_real_, pfu_lag = NA_real_)]  # the paper: no lag in the first analysed GP
  D[, fe_school := if (sp$F == "F1") school_c1fe else school_phys]
  D[, cl := if (sp$CL == "vid") vid else record_id]
  if (sp$sample == "C1") D <- D[cohort == "C1"]
  if (sp$sample == "C2") D <- D[cohort == "C2"]
  if (sp$sample == "no_cap") D <- D[n_named < 7]
  if (!is.null(sp$drop_school)) D <- D[school_phys != sp$drop_school]      # leave-one-school-out
  if (!is.null(sp$drop_gp)) D <- D[gp != sp$drop_gp]                        # leave-one-semester-out
  need <- c("y", "gp", "cohort", "female", "hispanic", "asian", "paredu", "mddv", "outdeg", "indeg", "sm", "sexmin", "fe_school", "E")
  if (sp$X == "X4") need <- c(need, "pfuv")
  D[complete.cases(D[, ..need])]
}

# ---- models ----------------------------------------------------------------------
covs <- function(sp, D) {
  v <- c("factor(gp)", "cohort", "female", "hispanic", "asian", "paredu", "mddv", "outdeg", "indeg", "sexmin", "factor(fe_school)")
  if (sp$X == "X4") v <- c(v, "pfuv")
  if (sp$Z == "Z3" && any(D$iso == 1)) v <- c(v, "iso")
  if (uniqueN(D$cohort) < 2) v <- setdiff(v, "cohort")      # single-cohort samples
  if (uniqueN(D$fe_school) < 2) v <- setdiff(v, "factor(fe_school)")
  paste(v, collapse = " + ")
}
robust <- function(fit, cl) sandwich::vcovCL(fit, cluster = cl)
term_row <- function(model, quantity, b, se, n, ev) data.table(model, quantity, b, se, n, events = ev)

fit_all <- function(D, sp, models = c("M0", "MA", "MB", "MC", "MD", "ME", "MF")) {
  out <- list(); cv <- covs(sp, D); ev <- sum(D$y)
  smc <- D$sm - mean(D$sm); D[, smc := smc]; D[, E25 := E / 0.25]
  safe <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)
  if ("M0" %in% models) {
    f <- safe(glm(as.formula(paste("y ~ sm + E25 +", cv)), binomial, D))
    if (!is.null(f)) { V <- robust(f, D$cl)
      out[["M0sm"]] <- term_row("M0", "OR_SM", coef(f)["sm"], sqrt(V["sm", "sm"]), nrow(D), ev)
      out[["M0E"]]  <- term_row("M0", "OR_peer25", coef(f)["E25"], sqrt(V["E25", "E25"]), nrow(D), ev) }
  }
  if ("MB" %in% models) {
    f <- safe(glm(as.formula(paste("y ~ smc * E25 +", cv)), binomial, D))
    if (!is.null(f)) { V <- robust(f, D$cl); b <- coef(f)["smc:E25"]; s <- sqrt(V["smc:E25", "smc:E25"])
      out[["MBint"]] <- term_row("MB", "OR_interaction", b, s, nrow(D), ev)
      out[["MBR"]]   <- term_row("MB", "R", -2 * b, 2 * s, nrow(D), ev) }
  }
  cutsets <- if (!is.null(sp$cutsets)) sp$cutsets else list(frac = sp$cuts)
  for (fam in names(cutsets)) for (c in cutsets[[fam]]) {
    low <- switch(fam,
      frac      = D$E <= c + 1e-9,
      count     = D$k <= c + 1e-9,
      pct_adopt = D$E <= quantile(D$E[D$y == 1], c, type = 1) + 1e-9,
      pct_risk  = D$E <= quantile(D$E, c, type = 1) + 1e-9)
    cl <- if (fam == "frac") sprintf("c%.3f", c) else sprintf("%s%.2f", fam, c)
    if (all(low) || !any(low)) next
    if ("MA" %in% models) {
      for (ty in c("low", "high")) {
        D[, yt := as.integer(y == 1 & (if (ty == "low") low else !low))]
        if (sum(D$yt) < 10) next
        f <- safe(glm(as.formula(paste("yt ~ sm +", cv)), binomial, D))
        if (!is.null(f)) { V <- robust(f, D$cl)
          out[[paste0("MA", ty, cl)]] <- term_row("MA", paste0("OR_SM_", ty, "@", cl), coef(f)["sm"], sqrt(V["sm", "sm"]), nrow(D), sum(D$yt)) }
      }
      if (!is.null(out[[paste0("MAlow", cl)]]) && !is.null(out[[paste0("MAhigh", cl)]])) {
        a <- out[[paste0("MAlow", cl)]]; h <- out[[paste0("MAhigh", cl)]]
        out[[paste0("MAR", cl)]] <- term_row("MA", paste0("R@", cl), a$b - h$b, sqrt(a$se^2 + h$se^2), nrow(D), ev)
      }
    }
    if ("MC" %in% models && sum(D$y[low]) >= 10 && sum(D$y[!low]) >= 10) {
      D[, lowf := as.integer(low)]
      f <- safe(glm(as.formula(paste("y ~ lowf + sm:factor(lowf) +", cv)), binomial, D))
      if (!is.null(f)) { V <- robust(f, D$cl); t1 <- "sm:factor(lowf)1"; t0 <- "sm:factor(lowf)0"
        b <- coef(f)[t1] - coef(f)[t0]; s <- sqrt(V[t1, t1] + V[t0, t0] - 2 * V[t1, t0])
        out[[paste0("MC", cl)]] <- term_row("MC", paste0("R@", cl), b, s, nrow(D), ev) }
    }
    if ("MD" %in% models && sum(D$y[low]) >= 10 && sum(D$y[!low]) >= 10) {
      D[, y3 := factor(ifelse(y == 0, "none", ifelse(low, "low", "high")), levels = c("none", "low", "high"))]
      f <- safe(nnet::multinom(as.formula(paste("y3 ~ sm +", cv)), D, trace = FALSE, maxit = 300))
      if (!is.null(f)) { cf <- coef(f); Vm <- vcov(f); a <- "low:sm"; h <- "high:sm"
        if (all(c(a, h) %in% rownames(Vm))) {
          b <- cf["low", "sm"] - cf["high", "sm"]; s <- sqrt(Vm[a, a] + Vm[h, h] - 2 * Vm[a, h])
          out[[paste0("MD", cl)]] <- term_row("MD", paste0("R@", cl), b, s, nrow(D), ev) } }
    }
  }
  if ("ME" %in% models) {
    A <- D[y == 1 & n > 0]
    if (nrow(A) >= 30) {
      f <- safe(glm(as.formula(paste("E ~ sm +", sub(" \\+ iso", "", cv))), quasibinomial, A, weights = n))
      if (!is.null(f)) { V <- robust(f, A$cl); out[["MEnaive"]] <- term_row("ME", "slope_naive", coef(f)["sm"], sqrt(V["sm", "sm"]), nrow(A), nrow(A)) }
    }
    Bd <- D[n > 0]
    f <- safe(glm(as.formula(paste("E ~ sm * y +", sub(" \\+ iso", "", cv))), quasibinomial, Bd, weights = n))
    if (!is.null(f)) { V <- robust(f, Bd$cl); out[["MEcorr"]] <- term_row("ME", "slope_corrected", coef(f)["sm:y"], sqrt(V["sm:y", "sm:y"]), nrow(Bd), sum(Bd$y)) }
  }
  if ("MF" %in% models) {
    D[, `:=`(Uc = k, NUc = nu)]
    f <- safe(glm(as.formula(paste("y ~ smc * Uc + smc * NUc +", sub("outdeg \\+ ", "", cv))), binomial, D))
    if (!is.null(f)) { V <- robust(f, D$cl)
      for (tm in c("smc:Uc", "smc:NUc", "NUc", "Uc")) if (tm %in% names(coef(f)))
        out[[paste0("MF", tm)]] <- term_row("MF", paste0("OR_", tm), coef(f)[tm], sqrt(V[tm, tm]), nrow(D), ev) }
  }
  rbindlist(out)
}

# Rubin's rules over imputation draws (log scale)
pool <- function(lst) {
  X <- rbindlist(lapply(seq_along(lst), function(i) lst[[i]][, draw := i]))
  if (!nrow(X)) return(X)
  X[, .(b = mean(b), se = { m <- .N; sqrt(mean(se^2) + (1 + 1 / m) * var(b)) }, n = round(mean(n)), events = round(mean(events)), draws = .N),
    by = .(model, quantity)]
}

run_spec <- function(sp, models = c("M0", "MA", "MB", "MC", "MD", "ME", "MF")) {
  ds <- build_data(sp)
  res <- lapply(ds, fit_all, sp = sp, models = models)
  if (all(vapply(res, nrow, 1L) == 0)) return(data.table(error = "no model could be fitted"))
  P <- if (length(res) > 1) pool(res) else res[[1]][, draws := 1L]
  P[, `:=`(lo = b - 1.96 * se, hi = b + 1.96 * se, p = 2 * pnorm(-abs(b / se)))]
  P[, `:=`(est = exp(b), est_lo = exp(lo), est_hi = exp(hi))]
  P[, students := uniqueN(ds[[1]]$record_id)]
  P
}
