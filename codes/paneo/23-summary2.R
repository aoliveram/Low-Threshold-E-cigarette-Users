# 23-summary2.R — tables and figures for the second stage.
source("paneo/00-config.R")
suppressMessages({ library(ggplot2); library(patchwork) })
ink <- "#52514e"; C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"; C5 <- "#e87ba4"
D2 <- file.path(OUT, "stage2")
S2 <- fread(file.path(D2, "stage2_results.csv")); L3 <- fread(file.path(OUT, "layer3", "layer3_results.csv"))
S2 <- S2[!is.na(model)]; L3 <- L3[!is.na(model)]
isR <- function(q) q == "R" | grepl("^R@", q)
th <- theme_minimal(base_size = 9) + theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = ink),
                                           plot.subtitle = element_text(colour = ink), legend.position = "bottom")
REF <- quote(A == "A3" & U == "U1" & T == "T3" & NI == "N4_I7" & Z == "Z3" & SM == "S3_lag")
A4S <- quote(A == "A4" & U == "U1" & T == "T3" & NI == "N4_I35" & Z == "Z3" & SM == "S3_lag")

# ---- 1. cut families ------------------------------------------------------------
cuts <- rbind(L3[model %in% c("MA", "MC", "MD") & isR(quantity), .(A, U, T, NI, Z, SM, label, model, quantity, est, est_lo, est_hi, p)],
              S2[stage == "cuts" & isR(quantity), .(A, U, T, NI, Z, SM, label, model, quantity, est, est_lo, est_hi, p)])
cuts[, family := fifelse(grepl("count", quantity), "Count", fifelse(grepl("pct_adopt", quantity), "Percentile (adopters)",
                 fifelse(grepl("pct_risk", quantity), "Percentile (at risk)", "Fraction")))]
cuts[, cut := sub("^R@", "", quantity)]
cuts[, cut := gsub("^c", "", gsub("count", "k<=", gsub("pct_adopt", "P", gsub("pct_risk", "P", cut))))]
tab_cuts <- cuts[, .(tests = .N, sig_above_1 = sum(p < .05 & est > 1), sig_below_1 = sum(p < .05 & est < 1)), by = .(family, model)]
fwrite(tab_cuts, file.path(D2, "cuts_summary.csv")); print(tab_cuts)
two <- rbind(cuts[eval(REF)][, spec := "Reference (A3, imputation draws)"], cuts[eval(A4S)][, spec := "30-day adoption (A4, prevalence imputation)"])
two[, cut := factor(cut, levels = unique(cut[order(family, cut)]))]
g1 <- ggplot(two[model %in% c("MC", "MD")], aes(cut, est, colour = model)) + geom_hline(yintercept = 1, colour = "#c3c2b7") +
  geom_pointrange(aes(ymin = est_lo, ymax = est_hi), position = position_dodge(0.5), size = 0.25) +
  facet_grid(spec ~ family, scales = "free_x", space = "free_x") + scale_y_log10() +
  scale_colour_manual(values = c(MC = C1, MD = C2), labels = c(MC = "Stratified (MC)", MD = "Multinomial (MD)"), name = NULL) +
  labs(title = "R across low-threshold cut families", subtitle = "R > 1 = substitution. Fractions: 0, 1/7, 1/4, 2/7; counts: k <= 0, 1, 2; percentiles of exposure.",
       x = NULL, y = "R (log scale)") + th + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
ggsave(file.path(D2, "fig_cut_families.png"), g1, width = 10, height = 5.5, dpi = 150)

# ---- 2. friend status U1..U5 ----------------------------------------------------
allU <- rbind(L3[, .(A, U, T, NI, Z, SM, label, model, quantity, est, est_lo, est_hi, p)],
              S2[stage == "U", .(A, U, T, NI, Z, SM, label, model, quantity, est, est_lo, est_hi, p)])
keyq <- data.table(model = c("M0", "MA", "MB", "MC", "MD"), quantity = c("OR_SM", "OR_SM_low@c0.250", "R", "R@c0.250", "R@c0.250"),
                   what = c("SM, any adoption", "SM, low-threshold (1/4)", "R interaction (MB)", "R stratified (MC, 1/4)", "R multinomial (MD, 1/4)"))
u <- merge(allU, keyq, by = c("model", "quantity"))
tab_u <- u[, .(specs = .N, median = round(median(est), 2), sig_above_1 = sum(p < .05 & est > 1), sig_below_1 = sum(p < .05 & est < 1)), by = .(what, U)][order(what, U)]
fwrite(tab_u, file.path(D2, "U_summary.csv")); print(tab_u)
u[, Ulab := factor(U, levels = c("U1", "U2", "U3", "U4", "U5"), labels = c("U1 ever used", "U2 ever incl. pre-window", "U3 current (6 m)", "U4 current (30 d)", "U5 perceived"))]
g2 <- ggplot(u, aes(Ulab, est, colour = label)) + geom_hline(yintercept = 1, colour = "#c3c2b7") +
  geom_jitter(width = 0.2, height = 0, size = 0.8, alpha = 0.7) + facet_wrap(~ what, nrow = 1, scales = "free_y") + scale_y_log10() +
  scale_colour_manual(values = c(D = C1, Disc = C2, I = "#8f8e88"), name = "Spec label") +
  labs(title = "Estimates across the multiverse, by how a friend counts as a user", x = NULL, y = "Estimate (log scale)") +
  th + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7))
ggsave(file.path(D2, "fig_U.png"), g2, width = 11, height = 4.8, dpi = 150)

# ---- 3. q grid (unseen friends' use) ----------------------------------------------
q <- merge(S2[stage == "qgrid"], data.table(model = c("MA", "MB", "MC"), quantity = c("OR_SM_low@c0.250", "R", "R@c0.143"),
                                             what = c("SM, low-threshold adoption (1/4)", "R interaction (MB)", "R stratified (MC, 1/7)")), by = c("model", "quantity"))
q[, base := paste(A, U)]
fwrite(q[, .(base, q, what, est, est_lo, est_hi, p)], file.path(D2, "qgrid.csv"))
g3 <- ggplot(q, aes(q, est, colour = base, fill = base)) + geom_hline(yintercept = 1, colour = "#c3c2b7") +
  geom_ribbon(aes(ymin = est_lo, ymax = est_hi), alpha = 0.08, colour = NA) + geom_line(linewidth = 0.7) + geom_point(size = 0.9) +
  facet_wrap(~ what, nrow = 1) + scale_y_log10() +
  scale_colour_manual(values = c(C1, C2, C3, C4), name = "Adoption, friend status") + scale_fill_manual(values = c(C1, C2, C3, C4), guide = "none") +
  labs(title = "Tipping point: share of unseen friends assumed to vape", subtitle = "Named network, isolates flagged, S3 lagged. Observed prevalence (median by school and wave): 0.12 ever used, 0.06 current use.",
       x = "q = share of unseen friends who vape", y = "Estimate (log scale)") + th
ggsave(file.path(D2, "fig_qgrid.png"), g3, width = 10, height = 4.2, dpi = 150)

# ---- 4. power ------------------------------------------------------------------
pw <- fread(file.path(D2, "power_simulation.csv"))
g4 <- ggplot(pw, aes(R, power, colour = model)) + geom_hline(yintercept = 0.8, colour = "#c3c2b7", linetype = 2) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.4) + facet_wrap(~ base) +
  scale_colour_manual(values = c(MB = C1, MC = C2), labels = c(MB = "Interaction (MB)", MC = "Stratified (MC, 1/7)"), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
  labs(title = "Power to detect substitution", subtitle = "Simulated on the observed risk sets; dashed line = 80%.", x = "True R", y = "Power") + th
ggsave(file.path(D2, "fig_power.png"), g4, width = 8.5, height = 3.8, dpi = 150)

# ---- 5. friendship substitution ------------------------------------------------------
fs <- fread(file.path(D2, "friendship_substitution.csv"))[sample == "All students"]
fs[, outcome := factor(outcome, levels = c("smuse", "S1", "S3"), labels = c("SM platforms used daily", "Vaping content (S1, paper)", "Vaping content (S3)"))]
fs[, friends := factor(friends, levels = c("all", "roster", "visible"), labels = c("All named", "Roster only", "Visible only"))]
g5 <- ggplot(fs, aes(est, test, colour = friends)) + geom_vline(xintercept = 0, colour = "#c3c2b7") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(0.6), size = 0.25) + facet_wrap(~ outcome, scales = "free_x") +
  scale_colour_manual(values = c(C1, C2, C3), name = "Friends counted") +
  labs(title = "Do friendships and social media substitute?", subtitle = "Substitution predicts negative after gaining friends and positive after losing them.", x = "Change per friend", y = NULL) + th
ggsave(file.path(D2, "fig_friendship.png"), g5, width = 10, height = 3.8, dpi = 150)
msg("Stage 2 figures written to %s", D2)
