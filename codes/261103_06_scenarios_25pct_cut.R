# =============================================================================
# 261103_06_scenarios_25pct_cut.R
# MANUSCRIPT TABLES 1-3: three ways to keep the 25% cut, with the measures of script 01 (same frame, same
# covariates). Table 2 = scenario A (types of starter); Table 3 = scenarios B (Panel A) and C (Panel B);
# Table 1 = characteristics of low- and high-threshold starters.
#
#   A  Types of starter (the submitted paper's Table 2): logit of "starts this way" vs everyone else, for
#      any start, no visible using friend, low threshold (E <= 25%), high threshold (E > 25%); plus a
#      multinomial model (no start / low-threshold start / high-threshold start) that tests whether the
#      social-media OR differs between the two kinds of start (SE clustered by student, sandwich by hand).
#   B  The 25% cut as a moderator: any start ~ social media x (E > 25%). Social-media OR within each group.
#   C  Continuous exposure (script 01): any start ~ social media x E. Social-media OR at E = 0 and E = 50%.
#
# Also: mean social media (S3) by exposure group, and one figure (social-media OR along E, B vs C).
#
# Outputs (../outputs_261023/06_scenarios_<time>/):
#   table1_adopters.csv                characteristics of low- and high-threshold starters (manuscript Table 1)
#   scenarios_tables.csv               scenarios A, B, C for both outcomes (manuscript Tables 2 and 3)
#   social_media_by_exposure_group.csv mean S3 by exposure group
#   scenarios_sm_or_by_exposure.pdf    figure; 06_scenarios_log.txt
# How to run (from codes/, after script 00):   Rscript 261103_06_scenarios_25pct_cut.R
# Run time: under 1 minute.
# =============================================================================
rm(list = ls())
suppressWarnings(source("261023_functions_network_exposure.R"))
suppressMessages({ library(nnet); library(ggplot2) })
run <- start_run("06_scenarios")
FIG <- file.path(run$dir, "scenarios_sm_or_by_exposure.pdf")
F <- build_frame(readRDS(DATA_FILE))
OUT <- c(A4 = "30-day use (main)", A3 = "6-month use (secondary)")

pfmt <- function(p) ifelse(p < .001, "<.001", sprintf("%.3f", p))
orci <- function(b, se) sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se))
line <- function(o, scen, row, b, se, events = NA) data.table(outcome = o, scenario = scen, row, events,
                                                              OR = orci(b, se), p = pfmt(2 * pnorm(-abs(b / se))))

# Multinomial logit with SE clustered by student (sandwich: bread = inverse Hessian, meat = summed scores)
multinom_cl <- function(f, D) {
  m <- multinom(f, D, trace = FALSE, maxit = 1000, Hess = TRUE)
  X <- model.matrix(m); keep <- colnames(X)
  P <- fitted(m); Y <- model.matrix(~ D$cat3 - 1)
  lv <- m$lev[-1]
  S <- do.call(cbind, lapply(lv, function(j) (Y[, paste0("D$cat3", j)] - P[, j]) * X))   # scores, order = class, var
  colnames(S) <- unlist(lapply(lv, function(j) paste0(j, ":", keep)))
  H <- m$Hessian[colnames(S), colnames(S)]
  bread <- MASS::ginv(H)
  Sc <- rowsum(S, D$record_id); G <- nrow(Sc)
  V <- bread %*% crossprod(Sc) %*% bread * G / (G - 1); dimnames(V) <- list(colnames(S), colnames(S))
  list(b = setNames(as.vector(t(coef(m))), colnames(S)), V = V)
}

TAB <- list(); CURVE <- list(); STEP <- list(); DESC <- list(); T1 <- list()
msd <- function(x) sprintf("%.2f (%.2f)", mean(x), sd(x))
pct <- function(x) sprintf("%.1f", 100 * mean(x))
for (o in names(OUT)) {
  D <- make_data(F, outcome = o); center(D)
  vis <- prepare_spec(F, outcome = o)$ed[type == "visible", .(kv = sum(s, na.rm = TRUE)), by = rid]
  D <- merge(D, vis, by = "rid", all.x = TRUE)[is.na(kv), kv := 0]
  D[, high := as.integer(E > 0.25 + 1e-9)]
  # Table 1: starters, one row per start, by threshold group
  S <- D[y == 1]
  T1[[o]] <- rbindlist(lapply(c(0, 1), function(h) { s <- S[high == h]
    data.table(outcome = o, group = if (h == 0) "Low threshold (E <= 25%)" else "High threshold (E > 25%)",
      characteristic = c("Starts, n (%)", "Cohort 2, %", "Female, %", "Sexual minority, %", "Asian, %", "Hispanic/Latine, %",
                         "Parent education, mean (SD)", "Depressive symptoms (MDD), mean (SD)", "Out-degree, mean (SD)",
                         "In-degree, mean (SD)", "Lagged social media (S3), mean (SD)", paste0("Started in GS", 3:8, ", %")),
      value = c(sprintf("%d (%.1f%%)", nrow(s), 100 * nrow(s) / nrow(S)), pct(s$cohort == "C2"), pct(s$female), pct(s$sexmin),
                pct(s$asian), pct(s$hispanic), msd(s$paredu), msd(s$mddv), msd(s$outdeg), msd(s$indeg), msd(s$smv),
                sapply(3:8, function(g) pct(s$gp == g)))) }))
  DESC[[o]] <- D[, .(rows = .N, starts = sum(y), S3_mean = round(mean(smv), 2), pct_S3_above_1 = round(100 * mean(smv > 1), 1)),
                 by = .(group = fifelse(high == 1, "E > 25%", "E <= 25%"))][, outcome := o]

  # ── A. Types of starter vs everyone else ──
  for (r in list(list("Any start", D$y == 1), list("No visible using friend", D$y == 1 & D$kv == 0 & D$iso == 0),
                 list("Low threshold (E <= 25%)", D$y == 1 & D$high == 0), list("High threshold (E > 25%)", D$y == 1 & D$high == 1))) {
    D[, yy := as.integer(r[[2]])]
    f <- glm_fit(paste("yy ~ smc +", cov_terms(D)), D); t <- clustered(f, D, "smc")
    TAB[[length(TAB) + 1]] <- line(o, "A", r[[1]], t$b, t$se, sum(D$yy))
  }
  D[, cat3 := factor(fifelse(y == 0, "none", fifelse(high == 1, "high", "low")), levels = c("none", "low", "high"))]
  M <- multinom_cl(as.formula(paste("cat3 ~ smc +", cov_terms(D))), D)
  d <- c(`low:smc` = 1, `high:smc` = -1); bd <- sum(M$b[names(d)] * d); sd_ <- sqrt(drop(t(d) %*% M$V[names(d), names(d)] %*% d))
  TAB[[length(TAB) + 1]] <- line(o, "A", "Multinomial: low-threshold start vs no start", M$b["low:smc"], sqrt(M$V["low:smc", "low:smc"]))
  TAB[[length(TAB) + 1]] <- line(o, "A", "Multinomial: high-threshold start vs no start", M$b["high:smc"], sqrt(M$V["high:smc", "high:smc"]))
  TAB[[length(TAB) + 1]] <- line(o, "A", "Multinomial: ratio low / high", bd, sd_)

  # ── B. The 25% cut as a moderator ──
  f <- glm_fit(paste("y ~ smc * high +", cov_terms(D)), D); V <- sandwich::vcovCL(f, cluster = D$record_id); b <- coef(f)
  bh <- b["smc"] + b["smc:high"]; sh <- sqrt(V["smc", "smc"] + V["smc:high", "smc:high"] + 2 * V["smc", "smc:high"])
  fa <- glm_fit(paste("y ~ smc + high +", cov_terms(D)), D); ta <- clustered(fa, D, c("smc", "high"))
  TAB[[length(TAB) + 1]] <- rbind(
    line(o, "B", "Social media (additive)", ta$b[1], ta$se[1], sum(D$y)),
    line(o, "B", "Peers: E > 25% vs <= 25% (additive)", ta$b[2], ta$se[2]),
    line(o, "B", "Social media if E <= 25%", b["smc"], sqrt(V["smc", "smc"]), sum(D$y & !D$high)),
    line(o, "B", "Social media if E > 25%", bh, sh, sum(D$y & D$high)),
    line(o, "B", "Interaction", b["smc:high"], sqrt(V["smc:high", "smc:high"])),
    line(o, "B", "R = low / high", -b["smc:high"], sqrt(V["smc:high", "smc:high"])))
  STEP[[o]] <- data.table(outcome = o, x0 = c(0, 0.25), x1 = c(0.25, 0.6), b = c(b["smc"], bh), se = c(sqrt(V["smc", "smc"]), sh))

  # ── C. Continuous exposure ──
  f <- glm_fit(paste("y ~ smc * E25 +", cov_terms(D)), D); V <- sandwich::vcovCL(f, cluster = D$record_id); b <- coef(f)
  at <- function(e) { w <- c(1, e / .25); k <- c("smc", "smc:E25"); c(sum(b[k] * w), sqrt(drop(t(w) %*% V[k, k] %*% w))) }
  fa <- glm_fit(paste("y ~ smc + E25 +", cov_terms(D)), D); ta <- clustered(fa, D, c("smc", "E25"))
  TAB[[length(TAB) + 1]] <- rbind(
    line(o, "C", "Social media (additive)", ta$b[1], ta$se[1], sum(D$y)),
    line(o, "C", "Peers, per +25 pp (additive)", ta$b[2], ta$se[2]),
    line(o, "C", "Social media if no friend used (E = 0)", at(0)[1], at(0)[2]),
    line(o, "C", "Social media if half used (E = 50%)", at(.5)[1], at(.5)[2]),
    line(o, "C", "Interaction, per +25 pp", b["smc:E25"], sqrt(V["smc:E25", "smc:E25"])),
    line(o, "C", "R = E 0% / E 50%", -2 * b["smc:E25"], 2 * sqrt(V["smc:E25", "smc:E25"])))
  g <- seq(0, 0.6, by = 0.01)
  CURVE[[o]] <- data.table(outcome = o, E = g, t(sapply(g, at)))[, setnames(.SD, c("V1", "V2"), c("b", "se"))]
}

TAB <- rbindlist(TAB)
for (s in c("A", "B", "C")) {
  section(paste("Scenario", s))
  w <- dcast(TAB[scenario == s], row ~ outcome, value.var = c("events", "OR", "p"))
  w <- w[match(unique(TAB[scenario == s]$row), row)]
  setcolorder(w, c("row", "events_A4", "OR_A4", "p_A4", "events_A3", "OR_A3", "p_A3"))
  print(w, row.names = FALSE)
}
section("Social media (S3) by exposure group"); print(rbindlist(DESC), row.names = FALSE)
section("Table 1: characteristics of starters")
T1 <- rbindlist(T1)
for (o in names(OUT)) { cat("\n", OUT[o], "\n", sep = "")
  print(dcast(T1[outcome == o], characteristic ~ group, value.var = "value")[match(unique(T1$characteristic), characteristic)], row.names = FALSE) }
fwrite(T1, file.path(run$dir, "table1_adopters.csv"))
fwrite(TAB, file.path(run$dir, "scenarios_tables.csv"))
fwrite(rbindlist(DESC), file.path(run$dir, "social_media_by_exposure_group.csv"))

# ── Figure: social-media OR along peer exposure, continuous (C) vs the 25% cut (B) ──
lab <- function(o) factor(OUT[o], levels = OUT)
CV <- rbindlist(CURVE)[, `:=`(panel = lab(outcome), OR = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
ST <- rbindlist(STEP)[, `:=`(panel = lab(outcome), OR = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
COL <- c(C = "#2a78d6", B = "#eb6834")
p <- ggplot() +
  geom_hline(yintercept = 1, colour = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = 0.25, colour = "grey75", linewidth = 0.4, linetype = "dotted") +
  geom_ribbon(data = CV, aes(E, ymin = lo, ymax = hi), fill = COL["C"], alpha = 0.15) +
  geom_line(data = CV, aes(E, OR, colour = "C"), linewidth = 0.8) +
  geom_rect(data = ST, aes(xmin = x0, xmax = x1, ymin = lo, ymax = hi), fill = COL["B"], alpha = 0.12) +
  geom_segment(data = ST, aes(x = x0, xend = x1, y = OR, yend = OR, colour = "B"), linewidth = 0.8, linetype = "22") +
  facet_wrap(~ panel) +
  scale_colour_manual(NULL, values = COL, breaks = c("C", "B"),
                      labels = c(C = "C: continuous exposure", B = "B: 25% cut (two groups)")) +
  scale_x_continuous("Share of named friends who had used (E), semester before", labels = function(x) paste0(100 * x, "%"),
                     breaks = c(0, .25, .5)) +
  scale_y_log10("Social-media OR per point (log scale, 95% CI)", breaks = c(0.5, 1, 1.5, 2, 3)) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
ggsave(FIG, p, width = 6.5, height = 3, device = cairo_pdf)
say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
