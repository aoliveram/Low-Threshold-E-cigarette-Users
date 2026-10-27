# 11-frame.R — person-period frame, edges and alter statuses for every option in the sweep.
# Vertices follow the paper's construction (one vertex per school x student, union of the school's
# respondents over its analysis waves) so that the paper's tables can be reproduced exactly.
# Outputs (data/): frame.rds  list(V, PP, E1, EN, ST, PREV)
source("paneo/00-config.R")
L <- readRDS(file.path(DATA, "panel_long.rds"))
N <- readRDS(file.path(DATA, "noms.rds"))
setkey(L, record_id, wave)

# ---- vertices -----------------------------------------------------------------
V <- rbindlist(lapply(SCHOOLS, function(s) {
  ids <- sort(unique(L[wave %in% school_waves(s) & schoolid == s, record_id]))
  data.table(school = s, record_id = ids)
}))
V[, vid := paste0(school, "_", record_id)]
V[, cohort := ifelse(is_c2(school), "C2", "C1")]
V[, `:=`(school_phys = physical_school(school), school_c1fe = school_c1fe(school))]
msg("vertices: %d (students %d)", nrow(V), uniqueN(V$record_id))

# ---- record x wave behaviour (respondent rows only, as in the paper) ------------
grid <- CJ(record_id = unique(L$record_id), wave = 1:10)
B <- L[grid, .(record_id, wave, use6, use30, resp = !is.na(schoolid), schoolid), on = .(record_id, wave)]
B[is.na(resp), resp := FALSE]
B[, `:=`(u6 = fifelse(is.na(use6), 0, as.numeric(use6 == 1)), u30 = fifelse(is.na(use30), 0, as.numeric(use30 == 1)))]
setkey(B, record_id, wave)
B[, ever_all := cummax(u6), by = record_id]                                    # ever used, W1 onward
B[, ever30 := cummax(u30), by = record_id]
B[, ever_w3  := cummax(fifelse(wave >= 3, u6, 0)), by = record_id]            # within window, C1
B[, ever_w5  := cummax(fifelse(wave >= 5, u6, 0)), by = record_id]            # within window, C2
# current use with own-history fill (I5): last observed, else next observed
B[, cur6 := fifelse(is.na(use6), NA_real_, as.numeric(use6 == 1))]
B[, cur30 := fifelse(is.na(use30), NA_real_, as.numeric(use30 == 1))]
locf_nocb <- function(x) { f <- x; for (i in seq_along(f)[-1]) if (is.na(f[i])) f[i] <- f[i - 1]
  b <- x; for (i in rev(seq_along(b))[-1]) if (is.na(b[i])) b[i] <- b[i + 1]; fifelse(is.na(f), b, f) }
B[, cur6_fill := locf_nocb(cur6), by = record_id]
B[, cur30_fill := locf_nocb(cur30), by = record_id]
ST <- B[, .(record_id, wave, resp, ever_all, ever_w3, ever_w5, cur6, cur30, cur6_fill, cur30_fill)]

# ---- person-period base: vertex x GP 3..8 ----------------------------------------
PP <- V[, .(gp = GPS), by = .(vid, school, record_id, cohort, school_phys, school_c1fe)]
PP[, wave := gp_to_wave(school, gp)]
PP <- merge(PP, B[, .(record_id, wave, use6, use30, resp)], by = c("record_id", "wave"), all.x = TRUE)
PP[, wave_prev := wave - 1L]
PP <- merge(PP, B[, .(record_id, wave_prev = wave, use6_prev = use6, use30_prev = use30, resp_prev = resp)],
            by = c("record_id", "wave_prev"), all.x = TRUE)
PP <- merge(PP, B[, .(record_id, wave = wave - 1L, use6_next = use6)], by = c("record_id", "wave"), all.x = TRUE)
setorder(PP, vid, gp)

# A1 — the paper: first observed past-6-month use within GP3–GP8 (monotone), risk rows gp <= toa
PP[, beh := fifelse(is.na(use6), NA_real_, as.numeric(use6 == 1))]
PP[, toa_A1 := { x <- which(beh == 1); if (length(x)) gp[x[1]] else NA_integer_ }, by = vid]
PP[, risk_A1 := is.na(toa_A1) | gp <= toa_A1]
PP[, y_A1 := as.integer(!is.na(toa_A1) & gp == toa_A1)]
PP[, obs_A1 := !is.na(use6) & use6 %in% c(0, 1)]           # the 261015 observed-outcome fix
# prior use: within window before gp, and before the window (pre-window waves)
PP <- merge(PP, ST[, .(record_id, wave_prev = wave, ever_all_prev = ever_all)], by = c("record_id", "wave_prev"), all.x = TRUE)
PP[, win_start := fifelse(cohort == "C2", 5L, 3L)]
PP[, prior_in_window := shift(cummax(fifelse(is.na(beh), 0, beh)), fill = 0), by = vid]
# A2 — observed transition non-use -> use between consecutive waves (window history only)
PP[, risk_A2 := !is.na(use6_prev) & use6_prev == 0 & !is.na(use6) & prior_in_window == 0]
PP[, y_A2 := as.integer(risk_A2 & use6 == 1)]
# A3 — A2 and no use reported at any earlier wave (including pre-window waves)
PP[, risk_A3 := risk_A2 & ever_all_prev == 0]
PP[, y_A3 := as.integer(risk_A3 & use6 == 1)]
# A4 — as A3 with past-30-day use
PP[, prior30 := shift(cummax(fifelse(is.na(use30), 0, as.numeric(use30 == 1))), fill = 0), by = vid]
PP <- merge(PP, B[, .(record_id, wave_prev = wave, ever30_prev = ever30)], by = c("record_id", "wave_prev"), all.x = TRUE)
PP[, risk_A4 := !is.na(use30_prev) & use30_prev == 0 & !is.na(use30) & prior30 == 0 & ever30_prev == 0]
PP[, y_A4 := as.integer(risk_A4 & use30 == 1)]
# A5 — sustained: A3 adoption with use again at t+1 (missing t+1 -> row dropped)
PP[, risk_A5 := risk_A3 & !(y_A3 == 1 & is.na(use6_next))]
PP[, y_A5 := as.integer(risk_A5 & y_A3 == 1 & use6_next %in% 1)]

# ---- social media (record x wave), current and lagged ------------------------------
smv <- L[, {
  smp <- as.matrix(.SD[, paste0("smp", 1:9), with = FALSE]); smp[!(smp %in% 1:5)] <- NA
  smu <- as.matrix(.SD[, paste0("smu", 1:15), with = FALSE]); smu[!(smu %in% 0:5)] <- NA
  present <- which(colSums(!is.na(smp)) > 0)
  S1 <- ifelse(rowSums(!is.na(smp)) == 0, NA, 6 - rowMeans(smp, na.rm = TRUE))
  core <- smp[, SMPOST_CORE, drop = FALSE]
  S2 <- ifelse(rowSums(!is.na(core)) == 0, NA, 6 - rowMeans(core, na.rm = TRUE))
  use_core <- smu[, SMPOST_TO_SOCIALMED[as.character(SMPOST_CORE)], drop = FALSE]
  core3 <- core; core3[is.na(core3) & !is.na(use_core) & use_core < 2] <- 5      # used < weekly -> "never"
  S3 <- ifelse(rowSums(!is.na(core3)) < 4, NA, 6 - rowMeans(core3, na.rm = TRUE))
  S4 <- ifelse(rowSums(!is.na(smp)) == 0, NA, as.numeric(rowSums(smp <= 3, na.rm = TRUE) > 0))
  S5 <- ifelse(rowSums(!is.na(smp)) == 0, NA, 6 - apply(smp, 1, function(r) suppressWarnings(min(r, na.rm = TRUE))))
  S6 <- ifelse(rowSums(!is.na(smp)) == 0, NA, rowSums(smp <= 3, na.rm = TRUE))
  smu_core <- smu[, SOCIALMED_CORE, drop = FALSE]
  .(record_id = record_id, wave = wave, S1 = S1, S2 = S2, S3 = S3, S4 = S4, S5 = S5, S6 = S6,
    S7 = 6 - smp[, 2], S8 = 6 - smp[, 8],
    smuse = ifelse(rowSums(!is.na(smu_core)) == 0, NA, rowSums(smu_core >= 3, na.rm = TRUE)),
    mdd = mdd, pfu = fifelse(pfu %in% 0:5, pfu, NA_real_), sni1 = sni1)
}]
SMV <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "smuse", "mdd", "pfu")
PP <- merge(PP, smv[, c("record_id", "wave", SMV), with = FALSE], by = c("record_id", "wave"), all.x = TRUE)
lagv <- smv[, c("record_id", "wave", SMV), with = FALSE]; setnames(lagv, SMV, paste0(SMV, "_lag"))
lagv[, wave := wave + 1L]
PP <- merge(PP, lagv, by = c("record_id", "wave"), all.x = TRUE)
# the paper's lag is the previous GP row of the same vertex, i.e. wave - 1: identical by construction

# ---- covariates ---------------------------------------------------------------------
first_valid <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_ }
# X1 — the paper: baseline school wave only; missing coded 0; sexuality 10 counted as minority
base <- V[, .(vid, record_id, school, bw = ifelse(is_c2(school), 5L, 3L))]
base <- merge(base, L[, .(record_id, bw = wave, gender, eth, race, sexuality)], by = c("record_id", "bw"), all.x = TRUE)
base[, `:=`(female_X1 = as.integer(!is.na(gender) & gender == 0), hispanic_X1 = as.integer(!is.na(eth) & eth == 1),
            asian_X1 = as.integer(!is.na(race) & race == 2), sexmin_X1 = fifelse(is.na(sexuality), NA_integer_, as.integer(sexuality != 1)))]
pe <- merge(V[, .(vid, record_id, school)], L[, .(record_id, wave, paredu)], by = "record_id", allow.cartesian = TRUE)
pe <- pe[wave %in% 3:10][, ok := wave %in% school_waves(school[1]), by = vid][ok == TRUE]
setorder(pe, vid, wave)
pe <- pe[, .(paredu_X1 = first_valid(fifelse(paredu %in% 1:6, paredu, NA_real_))), by = vid]
# X2 — corrected: first non-missing report at any wave; "prefer not to disclose" -> missing
Lo <- L[order(record_id, wave)]
x2 <- Lo[, .(gender = first_valid(fifelse(gender == 3, NA_real_, gender)), eth = first_valid(eth),
             race = first_valid(fifelse(race == 8, NA_real_, race)), sexuality = first_valid(fifelse(sexuality == 10, NA_real_, sexuality)),
             paredu_X2 = first_valid(fifelse(paredu %in% 1:6, paredu, NA_real_))), by = record_id]
x2[, `:=`(female_X2 = fifelse(is.na(gender), NA_integer_, as.integer(gender == 0)), hispanic_X2 = fifelse(is.na(eth), NA_integer_, as.integer(eth == 1)),
          asian_X2 = fifelse(is.na(race), NA_integer_, as.integer(race == 2)), sexmin_X2 = fifelse(is.na(sexuality), NA_integer_, as.integer(sexuality != 1)))]
V <- merge(V, base[, .(vid, female_X1, hispanic_X1, asian_X1, sexmin_X1)], by = "vid")
V <- merge(V, pe, by = "vid", all.x = TRUE)
V <- merge(V, x2[, .(record_id, female_X2, hispanic_X2, asian_X2, sexmin_X2, paredu_X2)], by = "record_id", all.x = TRUE)

# ---- edges ---------------------------------------------------------------------------
# E1 — the paper's network: every roster friend cell at wave w, ego and alter both in the school's vertex set
ids_by_school <- split(V$record_id, V$school)
E1 <- rbindlist(lapply(SCHOOLS, function(s) {
  ids <- ids_by_school[[as.character(s)]]
  e <- N[kind == "roster" & wave %in% school_waves(s) & ego %in% ids & alter %in% ids & ego != alter,
         .(wave, ego, alter)]
  unique(e)[, school := s]
}))
# EN — named network: the ego's own-school slots (roster + typed), typed alters keyed by school
part_ids <- unique(L$record_id)
EN <- N[col_school %in% SCHOOLS, .(wave, ego, school = col_school, kind, alter)]
EN <- EN[!(kind == "roster" & alter == ego)]
EN <- unique(EN)
EN[, akey := fifelse(kind == "typed", paste0(school, "|", alter), alter)]
resp_w <- B[resp == TRUE, .(record_id, wave)]
EN[, type := fifelse(kind == "typed", "typed", fifelse(!(alter %in% part_ids), "nonpart", "participant"))]
EN <- merge(EN, resp_w[, .(alter = record_id, wave, aresp = TRUE)], by = c("alter", "wave"), all.x = TRUE)
EN[is.na(aresp), aresp := FALSE]
EN[type == "participant" & aresp == FALSE, type := "part_noresp"]
EN[type == "participant", type := "visible"]
msg("named edges W2-W10: %d | %s", nrow(EN), paste(names(table(EN$type)), table(EN$type), sep = "=", collapse = " "))

# ---- prevalence by school x wave (respondents; for I3/I4) ----------------------------
PREV <- B[resp == TRUE & schoolid %in% SCHOOLS, .(p_ever_all = mean(ever_all), p_ever_w3 = mean(ever_w3), p_ever_w5 = mean(ever_w5),
                                                   p_cur6 = mean(cur6, na.rm = TRUE), p_cur30 = mean(cur30, na.rm = TRUE)), by = .(school = schoolid, wave)]

saveRDS(list(V = V, PP = PP, E1 = E1, EN = EN, ST = ST, PREV = PREV, SMV_RAW = smv), file.path(DATA, "frame.rds"))
msg("PP: %d vertex-GP rows | events A1 %d, A2 %d, A3 %d, A4 %d, A5 %d", nrow(PP), sum(PP$y_A1), sum(PP$y_A2), sum(PP$y_A3), sum(PP$y_A4), sum(PP$y_A5))
