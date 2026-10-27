# =============================================================================
# 261023_functions_network_exposure.R
# Shared functions for scripts 01-04. Sourced by them, not run on its own.
#
#   start_run()          output folder + Stata-style log for one script run
#   build_frame()        student x semester frame: outcomes, social media, covariates, friendships
#   prepare_spec()       everything for one specification that does not change across imputations
#   assemble()           peer exposure for that specification (expected value, a bootstrap draw, or one MI draw)
#   fit_*()              the models: additive, interaction, low-threshold, stratified, counts
#   bootstrap_R(), mi_R(), predicted_grid()
# =============================================================================
suppressMessages({ library(data.table); library(sandwich) })

# ══════════════════════════════════════════════════════════════════════════════
# 0. Study design and run settings
# ══════════════════════════════════════════════════════════════════════════════
C1_SCHOOLS <- c(101:107, 112:114)          # school 108 excluded, as in the paper
C2_SCHOOLS <- c(201, 212:214)
SCHOOLS    <- c(C1_SCHOOLS, C2_SCHOOLS)
GPS        <- 3:8                          # grade semesters: fall 10th ... spring 12th
is_c2        <- function(s) s %in% C2_SCHOOLS
gp_to_wave   <- function(s, gp) ifelse(is_c2(s), gp + 2L, gp)
school_waves <- function(s) if (is_c2(s)) 5:10 else 3:8
# Cohort 2 schools are the same buildings as four Cohort 1 schools (201 = 101, 212 = 112, 213 = 113,
# 214 = 114). Improvement: each building keeps its own school fixed effect (the earlier scripts put all
# Cohort 2 students in school 101).
physical_school <- function(s) ifelse(s == 201, 101L, ifelse(s %in% 212:214, as.integer(s) - 100L, as.integer(s)))

# Cores: the scripts were run with 8 cores; with fewer they give the same numbers, only slower.
N_CORES <- max(1L, min(8L, parallel::detectCores(logical = FALSE), na.rm = TRUE))
pmap <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = N_CORES, mc.preschedule = FALSE)

DATA_FILE <- "../Cleaned_Data/ADVANCE_slim_v2.rds"     # made by script 00
OUT_ROOT  <- "../outputs_261023"

start_run <- function(name) {
  dir <- file.path(OUT_ROOT, paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  con <- file(file.path(dir, paste0(name, "_log.txt")), open = "wt")
  sink(con, split = TRUE); sink(con, type = "message"); options(width = 200)
  cat("Script:", name, "\nStart time:", as.character(Sys.time()), "\nCores used:", N_CORES,
      "\nData:", normalizePath(DATA_FILE, mustWork = FALSE), "\n\n")
  list(dir = dir, con = con, t0 = Sys.time())
}
end_run <- function(run) {
  cat("\nEnd time:", as.character(Sys.time()), sprintf("(%.1f minutes)\n", as.numeric(difftime(Sys.time(), run$t0, units = "mins"))))
  sink(type = "message"); sink(); close(run$con)
}
say <- function(...) cat(sprintf(...), "\n", sep = "")
section <- function(title) cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Frame: one row per student x semester, plus the friendship nominations
# ══════════════════════════════════════════════════════════════════════════════
build_frame <- function(slim) {
  L <- copy(slim$panel); N <- slim$noms; setkey(L, record_id, wave)

  # Students: one entry per school x student (the school's respondents in its analysis waves),
  # the same construction as the earlier scripts.
  V <- rbindlist(lapply(SCHOOLS, function(s) data.table(school = s,
        record_id = sort(unique(L[wave %in% school_waves(s) & schoolid == s, record_id])))))
  V[, `:=`(vid = paste0(school, "_", record_id), cohort = ifelse(is_c2(school), "C2", "C1"), school_phys = physical_school(school))]

  # Own use at every wave (W1-W10). "Ever used" is counted inside the analysis window
  # (W3+ for Cohort 1, W5+ for Cohort 2), the definition used for friends' status.
  grid <- CJ(record_id = unique(L$record_id), wave = 1:10)
  B <- L[grid, .(record_id, wave, use6, use30, resp = !is.na(schoolid), schoolid), on = .(record_id, wave)]
  B[is.na(resp), resp := FALSE]
  B[, `:=`(u6 = fifelse(is.na(use6), 0, as.numeric(use6 == 1)), u30 = fifelse(is.na(use30), 0, as.numeric(use30 == 1)))]
  setkey(B, record_id, wave)
  B[, `:=`(ever_all = cummax(u6), ever30 = cummax(u30),
           ever_w3 = cummax(fifelse(wave >= 3, u6, 0)), ever_w5 = cummax(fifelse(wave >= 5, u6, 0))), by = record_id]
  B[, cur6 := fifelse(is.na(use6), NA_real_, as.numeric(use6 == 1))]
  # a friend who skipped a wave: last answer given, else the next one (only used for past-6-month status)
  fill <- function(x) { f <- x; for (i in seq_along(f)[-1]) if (is.na(f[i])) f[i] <- f[i - 1]
    b <- x; for (i in rev(seq_along(b))[-1]) if (is.na(b[i])) b[i] <- b[i + 1]; fifelse(is.na(f), b, f) }
  B[, cur6_fill := fill(cur6), by = record_id]

  # Student x semester rows and the two outcomes
  PP <- V[, .(gp = GPS), by = .(vid, school, record_id, cohort, school_phys)]
  PP[, wave := gp_to_wave(school, gp)][, wave_prev := wave - 1L]
  PP <- merge(PP, B[, .(record_id, wave, use6, use30)], by = c("record_id", "wave"), all.x = TRUE)
  PP <- merge(PP, B[, .(record_id, wave_prev = wave, use6_prev = use6, use30_prev = use30,
                        ever_all_prev = ever_all, ever30_prev = ever30)], by = c("record_id", "wave_prev"), all.x = TRUE)
  setorder(PP, vid, gp)
  PP[, prior6  := shift(cummax(fifelse(is.na(use6), 0, as.numeric(use6 == 1))), fill = 0), by = vid]
  PP[, prior30 := shift(cummax(fifelse(is.na(use30), 0, as.numeric(use30 == 1))), fill = 0), by = vid]
  # Improvement: a student "starts" only when we SEE the change: no use at t-1, use at t, and no use
  # reported at any earlier wave (grade 9 included). Students already using at the first analysed
  # semester, and semesters with a missing answer, are not counted as starts.
  # A3: past-6-month use.  A4: past-30-day use.
  PP[, risk_A3 := !is.na(use6_prev) & use6_prev == 0 & !is.na(use6) & prior6 == 0 & ever_all_prev == 0]
  PP[, y_A3 := as.integer(risk_A3 & use6 == 1)]
  PP[, risk_A4 := !is.na(use30_prev) & use30_prev == 0 & !is.na(use30) & prior30 == 0 & ever30_prev == 0]
  PP[, y_A4 := as.integer(risk_A4 & use30 == 1)]

  # Exposure to vaping content on social media, measured one semester before (t-1)
  sm <- L[, {
    smp <- as.matrix(.SD[, paste0("smp", 1:9), with = FALSE]); smp[!(smp %in% 1:5)] <- NA       # 99 = don't know
    smu <- as.matrix(.SD[, paste0("smu", 1:15), with = FALSE]); smu[!(smu %in% 0:5)] <- NA
    # S1 (the paper): mean over the platforms answered, reversed so that 1 = never ... 5 = several times a day
    S1 <- ifelse(rowSums(!is.na(smp)) == 0, NA, 6 - rowMeans(smp, na.rm = TRUE))
    # S3 (recommended): the 6 platforms asked at every wave (Facebook, Instagram, Twitter, YouTube, TikTok,
    # Twitch). The vaping-content item is asked only for platforms used weekly or more, so a platform used
    # less than weekly counts as "never sees vaping posts there" instead of being dropped. Needs >= 4 platforms.
    core <- smp[, 1:6]; use_core <- smu[, c(1, 2, 3, 7, 8, 11)]
    core[is.na(core) & !is.na(use_core) & use_core < 2] <- 5
    S3 <- ifelse(rowSums(!is.na(core)) < 4, NA, 6 - rowMeans(core, na.rm = TRUE))
    .(record_id = record_id, wave = wave + 1L, S1_lag = S1, S3_lag = S3, mdd_lag = mdd)
  }]
  PP <- merge(PP, sm, by = c("record_id", "wave"), all.x = TRUE)
  PP <- merge(PP, L[, .(record_id, wave, mdd)], by = c("record_id", "wave"), all.x = TRUE)

  # Improvement: time-invariant covariates from the first valid answer at ANY wave (not only the
  # baseline wave), and "prefer not to disclose" is missing (sexuality code 10 was counted as minority).
  first <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_ }
  X <- L[order(record_id, wave), .(gender = first(fifelse(gender == 3, NA_real_, gender)), eth = first(eth),
         race = first(fifelse(race == 8, NA_real_, race)), sexuality = first(fifelse(sexuality == 10, NA_real_, sexuality)),
         paredu = first(fifelse(paredu %in% 1:6, paredu, NA_real_))), by = record_id]
  X[, `:=`(female = as.integer(gender == 0), hispanic = as.integer(eth == 1), asian = as.integer(race == 2), sexmin = as.integer(sexuality != 1))]
  PP <- merge(PP, X[, .(record_id, female, hispanic, asian, sexmin, paredu)], by = "record_id", all.x = TRUE)

  # Visible network (both students answered): used for the out-/in-degree covariates, as before
  ids <- split(V$record_id, V$school)
  E1 <- rbindlist(lapply(SCHOOLS, function(s) { i <- ids[[as.character(s)]]
    unique(N[kind == "roster" & wave %in% school_waves(s) & ego %in% i & alter %in% i & ego != alter, .(wave, ego, alter)])[, school := s] }))
  DEG <- merge(E1[, .(outdeg = .N), by = .(school, wave, record_id = ego)], E1[, .(indeg = .N), by = .(school, wave, record_id = alter)],
               by = c("school", "wave", "record_id"), all = TRUE)
  DEG[is.na(outdeg), outdeg := 0L][is.na(indeg), indeg := 0L]

  # Improvement: the friends of a student are ALL the friends named in the own-school slots, from the
  # roster or typed by hand (typed names arrive hashed from script 00), whether or not they answered.
  # visible       = participant who answered that wave (status observed)
  # no_answer     = participant who skipped that wave
  # nonparticipant / typed = never in the survey (status unseen, imputed below)
  EN <- unique(N[col_school %in% SCHOOLS, .(wave, ego, school = col_school, kind, alter)])[!(kind == "roster" & alter == ego)]
  EN[, akey := fifelse(kind == "typed", paste0(school, "|", alter), alter)]
  EN[, type := fifelse(kind == "typed", "typed", fifelse(alter %in% unique(L$record_id), "participant", "nonparticipant"))]
  EN <- merge(EN, B[resp == TRUE, .(alter = record_id, wave, aresp = TRUE)], by = c("alter", "wave"), all.x = TRUE)
  EN[type == "participant" & is.na(aresp), type := "no_answer"][type == "participant", type := "visible"]

  # Respondents per school x wave and how many had used: the prior for unseen friends
  PREV <- B[resp == TRUE & schoolid %in% SCHOOLS,
            .(n_U1 = .N, u_U1 = sum(fifelse(is_c2(schoolid), ever_w5, ever_w3)),
              n_U3 = sum(!is.na(cur6)), u_U3 = sum(cur6, na.rm = TRUE)), by = .(school = schoolid, wave)]
  list(V = V, PP = PP, B = B, DEG = DEG, EN = EN, PREV = PREV, RESP = B[resp == TRUE, .(record_id, wave)])
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. One specification
# ══════════════════════════════════════════════════════════════════════════════
# outcome    "A3" past-6-month initiation (main) | "A4" past-30-day initiation (co-primary)
# status     "U1" friend had EVER used by t-1 (Valente; main) | "U3" friend used in the past 6 months at t-1
# imputation "homophily" (main) | "prevalence" | "model" | "const" (all unseen friends = q)
# sm         "S3" (main) | "S1" (the paper's average)
# covs       "X2" (main: depression and degrees at t) | "X3" (all covariates at t-1)
prepare_spec <- function(F, outcome = "A3", status = "U1", imputation = "homophily", q = NA_real_,
                         sm = "S3", covs = "X2", drop_school = NULL, drop_gp = NULL) {
  R <- F$PP[get(paste0("risk_", outcome)) == TRUE]
  # Improvement: friends and their use are taken at t-1, one semester BEFORE the possible start, so a
  # friend who starts in the same semester is not counted as an influence (no same-period co-adopters).
  R[, `:=`(y = get(paste0("y_", outcome)), wn = gp_to_wave(school, gp - 1L))]
  R <- R[F$RESP[, .(record_id, wn = wave)], on = .(record_id, wn), nomatch = 0L]    # the student answered at t-1
  R <- merge(R, F$DEG[, .(school, wave, record_id, out_t = outdeg, in_t = indeg)], by = c("school", "wave", "record_id"), all.x = TRUE)
  R <- merge(R, F$DEG[, .(school, wn = wave, record_id, in_tn = indeg)], by = c("school", "wn", "record_id"), all.x = TRUE)
  for (v in c("out_t", "in_t", "in_tn")) set(R, which(is.na(R[[v]])), v, 0L)
  R[, smv := get(paste0(sm, "_lag"))]
  if (covs == "X2") R[, `:=`(mddv = mdd, outdeg = out_t, indeg = in_t)] else R[, `:=`(mddv = mdd_lag, indeg = in_tn)]
  if (!is.null(drop_school)) R <- R[school_phys != drop_school]                     # leave-one-school-out
  if (!is.null(drop_gp)) R <- R[gp != drop_gp]                                      # leave-one-semester-out
  need <- c("y", "gp", "cohort", "female", "hispanic", "asian", "paredu", "mddv", "smv", "sexmin", if (covs == "X2") c("outdeg", "indeg") else "indeg")
  R <- R[complete.cases(R[, ..need])]
  setorder(R, vid, gp); R[, rid := .I]

  # Friends named at t-1, with their status at t-1
  ed <- F$EN[R[, .(rid, school, cohort, ego = record_id, wn)], on = .(school, wave = wn, ego), nomatch = 0L,
             allow.cartesian = TRUE, .(rid, school, cohort, wn = i.wn, alter, akey, type)]
  ed <- F$B[, .(alter = record_id, wn = wave, ever_w3, ever_w5, cur6, cur6_fill)][ed, on = .(alter, wn)]
  ed[, s := if (status == "U1") fifelse(cohort == "C2", ever_w5, ever_w3) else cur6]
  ed[type %in% c("nonparticipant", "typed"), s := NA_real_]
  ed[, fill := if (status == "U1") s else cur6_fill]
  # school x wave prior (counts kept so the bootstrap can redraw it)
  pv <- F$PREV[, .(school, wn = wave, n_p = get(paste0("n_", status)), u_p = get(paste0("u_", status)))]
  # Homophily: a friend who is not in the survey is often named by several students; the share of those
  # students who had used tells us about that friend. Shrunk towards the school prior (2 pseudo-students).
  nom <- unique(F$EN[type == "nonparticipant", .(school, wn = wave, akey, ego)])
  nom <- F$B[, .(ego = record_id, wn = wave, ever_w3, ever_w5, cur6_fill)][nom, on = .(ego, wn)]
  nom[, u := if (status == "U1") fifelse(is_c2(school), ever_w5, ever_w3) else cur6_fill][is.na(u), u := 0]
  ed <- nom[, .(Nn = .N, Un = sum(u)), by = .(school, wn, akey)][ed, on = .(school, wn, akey)]
  ed[, unk := is.na(s)]
  # Network model (robustness): logistic model trained on visible friends with what is also known about
  # unseen friends (how many students name them, and the share of those who had used)
  if (imputation == "model") {
    nm <- unique(F$EN[, .(school, wn = wave, akey, ego)])
    nm <- F$B[, .(ego = record_id, wn = wave, ever_w3, ever_w5)][nm, on = .(ego, wn)]
    nm[, u := fifelse(is_c2(school), ever_w5, ever_w3)][is.na(u), u := 0]
    ed <- nm[, .(Nall = .N, Uall = sum(u)), by = .(school, wn, akey)][ed, on = .(school, wn, akey)]
    ed[is.na(Nall), `:=`(Nall = 0L, Uall = 0)][, `:=`(share = fifelse(Nall > 0, Uall / Nall, 0), lnN = log1p(Nall))]
    tr <- unique(ed[type == "visible" & !is.na(s), .(school, wn, akey, s, share, lnN)])
    fm <- glm(s ~ share + lnN + factor(school) + factor(wn), binomial, tr)
    ed[, pr_model := predict(fm, newdata = ed, type = "response")]
  }
  list(R = R, ed = ed, pv = pv, outcome = outcome, status = status, imputation = imputation, q = q, covs = covs,
       key = ed[, paste(school, wn, akey)])
}

# Peer exposure E = expected share of named friends who had used.
#   draw_prior = TRUE : redraw each school x wave prior from its Beta distribution (bootstrap)
#   mi_seed           : one 0/1 draw per unseen friend (multiple imputation; same friend = same value)
assemble <- function(P, draw_prior = FALSE, mi_seed = NULL) {
  ed <- P$ed
  pv <- copy(P$pv)[, p := u_p / n_p]
  if (draw_prior) pv[, p := rbeta(.N, 1 + u_p, 1 + n_p - u_p)]
  p <- pv$p[match(paste(ed$school, ed$wn), paste(pv$school, pv$wn))]
  p[is.na(p)] <- mean(pv$p, na.rm = TRUE)
  unseen <- ed$type %in% c("nonparticipant", "typed")
  pr <- ed$s
  a <- ed$unk & !unseen                                   # participants with a missing answer: own history, else prior
  pr[a] <- fifelse(is.na(ed$fill[a]), p[a], ed$fill[a])
  pr[ed$unk & unseen] <- p[ed$unk & unseen]
  if (P$imputation == "homophily") {
    h <- ed$unk & ed$type == "nonparticipant" & !is.na(ed$Nn)
    pr[h] <- (ed$Un[h] + 2 * p[h]) / (ed$Nn[h] + 2)
  }
  if (P$imputation == "const") pr[ed$unk] <- P$q
  if (P$imputation == "model") pr[ed$unk] <- ed$pr_model[ed$unk]
  if (!is.null(mi_seed)) {
    set.seed(mi_seed)
    u <- data.table(kk = P$key[ed$unk], pr = pr[ed$unk])[, .(pr = pr[1]), by = kk][, v := as.numeric(runif(.N) < pr)]
    pr[ed$unk] <- u$v[match(P$key[ed$unk], u$kk)]
  }
  agg <- data.table(rid = ed$rid, x = pr)[, .(n = .N, k = sum(x)), by = rid]
  D <- merge(P$R, agg, by = "rid", all.x = TRUE)
  D[is.na(n), `:=`(n = 0L, k = 0)]
  # Improvement: a student who named nobody gets E = 0 AND a flag (iso), so "no friends" is not
  # mistaken for "friends who do not vape".
  D[, `:=`(E = fifelse(n > 0, k / n, 0), nu = n - k, iso = as.integer(n == 0))]
  if (P$covs == "X3") D[, outdeg := n]
  D
}
make_data <- function(F, ...) assemble(prepare_spec(F, ...))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Models (discrete-time logit, SE clustered by student)
# ══════════════════════════════════════════════════════════════════════════════
cov_terms <- function(D, drop = NULL) {
  v <- c("factor(gp)", "cohort", "female", "hispanic", "asian", "paredu", "mddv", "outdeg", "indeg", "sexmin", "factor(school_phys)")
  if (any(D$iso == 1)) v <- c(v, "iso")                       # flag for students who named nobody
  if (uniqueN(D$cohort) < 2) v <- setdiff(v, "cohort")        # one-cohort samples
  if (uniqueN(D$school_phys) < 2) v <- setdiff(v, "factor(school_phys)")
  paste(setdiff(v, drop), collapse = " + ")
}
glm_fit <- function(f, D) suppressWarnings(glm(as.formula(f), binomial, D))
clustered <- function(fit, D, terms) { V <- sandwich::vcovCL(fit, cluster = D$record_id)
  data.table(term = terms, b = unname(coef(fit)[terms]), se = unname(sqrt(diag(V)[terms]))) }
add_or <- function(T) T[, `:=`(OR = exp(b), OR_lo = exp(b - 1.96 * se), OR_hi = exp(b + 1.96 * se), p = 2 * pnorm(-abs(b / se)))][]
center <- function(D) D[, `:=`(smc = smv - mean(smv), E25 = E / 0.25)]

# Additive model: OR per 1 point of social-media exposure and per +25 percentage points of friends who used
fit_additive <- function(D) { center(D)
  f <- glm_fit(paste("y ~ smc + E25 +", cov_terms(D)), D)
  add_or(clustered(f, D, c("smc", "E25")))[, term := c("Social media (per 1 point)", "Friends who used (per +25 pp)")][] }

# Interaction model (the direct test of the two-step-flow narrative).
# R = OR of social media when no friend used / OR of social media when half the friends used = exp(-2 b_int).
# R > 1: social media matters more for students with few vaping friends.
fit_interaction <- function(D, coef_only = FALSE) { center(D)
  f <- glm_fit(paste("y ~ smc * E25 +", cov_terms(D)), D)
  if (coef_only) return(unname(coef(f)["smc:E25"]))
  t <- clustered(f, D, "smc:E25")
  data.table(OR_int = exp(t$b), b = t$b, se = t$se, R = exp(-2 * t$b), R_lo = exp(-2 * (t$b + 1.96 * t$se)),
             R_hi = exp(-2 * (t$b - 1.96 * t$se)), p = 2 * pnorm(-abs(t$b / t$se)),
             n = nrow(D), events = sum(D$y), students = uniqueN(D$record_id)) }

# The paper's question in its own form: social media and LOW-THRESHOLD starts (E <= 1/4 at t-1)
fit_lowthreshold <- function(D, cut = 0.25) {
  D[, y_low := as.integer(y == 1 & E <= cut + 1e-9)]
  f <- glm_fit(paste("y_low ~ smv +", cov_terms(D)), D)
  add_or(clustered(f, D, "smv"))[, `:=`(term = "Social media, low-threshold starts (E <= 1/4)", events = sum(D$y_low))][] }

# Stratified (robustness): social-media OR for students with at most 1 in 7 friends who used vs the rest; R = ratio
fit_stratified <- function(D, cut = 1/7) {
  D[, low := as.integer(E <= cut + 1e-9)]
  f <- glm_fit(paste("y ~ low + smv:factor(low) +", cov_terms(D)), D)
  V <- sandwich::vcovCL(f, cluster = D$record_id); a <- "smv:factor(low)1"; h <- "smv:factor(low)0"
  b <- unname(coef(f)[a] - coef(f)[h]); se <- sqrt(V[a, a] + V[h, h] - 2 * V[a, h])
  data.table(R = exp(b), R_lo = exp(b - 1.96 * se), R_hi = exp(b + 1.96 * se), p = 2 * pnorm(-abs(b / se))) }

# Counts (Reviewer 1): number of friends who used and number who did not, each interacting with social media.
# Out-degree is left out because it is the sum of the two counts.
fit_counts <- function(D) { center(D); D[, `:=`(Uc = k, NUc = nu)]
  f <- glm_fit(paste("y ~ smc * Uc + smc * NUc +", cov_terms(D, drop = "outdeg")), D)
  add_or(clustered(f, D, c("Uc", "NUc", "smc:Uc", "smc:NUc")))[, term := c("Friends who used (per friend)",
    "Friends who did not use (per friend)", "Social media x friends who used", "Social media x friends who did not use")][] }

# ══════════════════════════════════════════════════════════════════════════════
# 4. Inference that carries the imputation uncertainty
# ══════════════════════════════════════════════════════════════════════════════
# Cluster bootstrap: resample students (all their semesters) and redraw each school x wave prior from its
# Beta distribution, then recompute the exposure and refit. Percentile CI and two-sided p for R.
bootstrap_R <- function(P, B = 1000L, seed = 2026L) {
  ids <- unique(P$R$record_id)
  est <- unlist(pmap(seq_len(B), function(b) {
    set.seed(seed + b)
    D <- assemble(P, draw_prior = TRUE)
    pick <- data.table(record_id = sample(ids, length(ids), TRUE))[, bid := .I]
    D <- D[pick, on = "record_id", allow.cartesian = TRUE, nomatch = 0L]
    tryCatch(fit_interaction(D, coef_only = TRUE), error = function(e) NA_real_)
  }))
  est <- est[is.finite(est)]
  data.table(R_boot_lo = exp(-2 * unname(quantile(est, .975))), R_boot_hi = exp(-2 * unname(quantile(est, .025))),
             p_boot = 2 * min(mean(est <= 0), mean(est >= 0)), B_ok = length(est))
}
# Multiple imputation (robustness): m random 0/1 completions of the unseen friends, Rubin's rules
mi_R <- function(P, m = 50L, seed = 3000L) {
  est <- rbindlist(pmap(seq_len(m), function(i) fit_interaction(assemble(P, mi_seed = seed + i))[, .(b, se)]))
  b <- mean(est$b); se <- sqrt(mean(est$se^2) + (1 + 1 / m) * var(est$b))
  data.table(R = exp(-2 * b), R_lo = exp(-2 * (b + 1.96 * se)), R_hi = exp(-2 * (b - 1.96 * se)), p = 2 * pnorm(-abs(b / se)), m = m)
}

# Predicted chance of starting in a semester (g-computation from the interaction model): every student set
# to a given social-media level and a given share of friends who used, then averaged.
predicted_grid <- function(D, E_values = c(0, 2/7), sm_q = c(typical = .5, high = .9)) { center(D)
  f <- glm_fit(paste("y ~ smc * E25 +", cov_terms(D)), D); m <- mean(D$smv)
  lv <- quantile(D$smv, sm_q, type = 1); names(lv) <- names(sm_q)
  rbindlist(lapply(names(sm_q), function(nm) rbindlist(lapply(E_values, function(e) {
    nd <- copy(D)[, `:=`(smc = lv[[nm]] - m, E25 = e / 0.25)]
    data.table(social_media = nm, sm_value = unname(lv[[nm]]), E = e, prob_pct = 100 * mean(predict(f, nd, type = "response")))
  })))) }
