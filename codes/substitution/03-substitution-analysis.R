# 03-substitution-analysis.R — does social media substitute for friends?
#   Original direction: few friends -> several (P25 -> P75): does social-media use fall?
#   Reverse direction:  several -> few: does social-media use rise?
#   Chain: the same for exposure to vaping content (sm_post, W4–W8), and within-student SM use -> vaping content.
suppressMessages({ library(fixest); library(ggplot2); library(patchwork)
  library(ggalluvial) })
OUT <- "../outputs_substitution"; dir.create(OUT, showWarnings = FALSE)
P <- readRDS(file.path(OUT, "panel_W4-W10.rds")); P <- P[!P$incomplete_zero, ]
qf <- quantile(P$named, c(.25, .75), type = 1); qs <- quantile(P$sm_daily, c(.25, .75), type = 1, na.rm = TRUE)
cat(sprintf("Friends: few <= %d (P25), several >= %d (P75) | SM platforms used daily: low <= %d, high >= %d\n", qf[1], qf[2], qs[1], qs[2]))
P$fr  <- ifelse(P$named <= qf[1], "few", ifelse(P$named >= qf[2], "several", "middle"))
P$smc <- factor(ifelse(P$sm_daily <= qs[1], "Low", ifelse(P$sm_daily >= qs[2], "High", "Middle")), levels = c("Low", "Middle", "High"))
P$never <- P$ever == 0

nxt <- P; nxt$wave <- nxt$wave - 1L
X <- merge(P, nxt[, c("record_id", "wave", "named", "fr", "sm_daily", "smc", "vape_mean", "vape_weekly", "never")],
           by = c("record_id", "wave"), suffixes = c("0", "1"))
X$group <- NA_character_
X$group[X$fr0 == "few" & X$fr1 == "several"]     <- "Few -> several"
X$group[X$fr0 == "few" & X$fr1 == "few"]         <- "Few -> few"
X$group[X$fr0 == "several" & X$fr1 == "few"]     <- "Several -> few"
X$group[X$fr0 == "several" & X$fr1 == "several"] <- "Several -> several"
X$d_named <- X$named1 - X$named0; X$gain_n <- pmax(X$d_named, 0); X$loss_n <- pmax(-X$d_named, 0)
X$d_sm <- X$sm_daily1 - X$sm_daily0; X$d_vape <- X$vape_mean1 - X$vape_mean0; X$d_vw <- X$vape_weekly1 - X$vape_weekly0
saveRDS(X, file.path(OUT, "pairs_W4-W10.rds"))
cat("pairs:", nrow(X), "\n"); print(table(X$group, useNA = "ifany"))

# ---- estimates ---------------------------------------------------------------
est <- list()
add <- function(fit, term, test, outcome, sample, contrast) {
  ct <- coeftable(fit); if (!term %in% rownames(ct)) return(invisible())
  b <- ct[term, 1]; se <- ct[term, 2]
  est[[length(est) + 1]] <<- data.frame(test, outcome, sample, contrast, estimate = b, lo = b - 1.96 * se, hi = b + 1.96 * se,
                                        p = ct[term, 4], n = nobs(fit), students = length(unique(fit$obs_selection$obsRemoved %||% 0)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
for (smp in c("All students", "Never-users")) {
  Xs <- if (smp == "All students") X else X[X$never1 %in% TRUE, ]
  Ps <- if (smp == "All students") P else P[P$never %in% TRUE, ]
  for (oc in c("d_sm", "d_vape", "d_vw")) {
    lab <- c(d_sm = "SM platforms used daily", d_vape = "Vaping-content exposure (paper scale)", d_vw = "Sees vaping content weekly+ (0/1)")[[oc]]
    a <- Xs[Xs$group %in% c("Few -> several", "Few -> few"), ]; a$g <- as.integer(a$group == "Few -> several")
    if (sum(!is.na(a[[oc]])) > 50) add(feols(as.formula(paste(oc, "~ g | wave^cohort")), a, cluster = ~record_id), "g",
                                       "Group contrast", lab, smp, "Gains friends (few->several) vs stays few")
    b <- Xs[Xs$group %in% c("Several -> few", "Several -> several"), ]; b$g <- as.integer(b$group == "Several -> few")
    if (sum(!is.na(b[[oc]])) > 50) add(feols(as.formula(paste(oc, "~ g | wave^cohort")), b, cluster = ~record_id), "g",
                                       "Group contrast", lab, smp, "Loses friends (several->few) vs stays several")
    f <- feols(as.formula(paste(oc, "~ gain_n + loss_n | wave^cohort")), Xs, cluster = ~record_id)
    add(f, "gain_n", "First differences", lab, smp, "Per friend gained")
    add(f, "loss_n", "First differences", lab, smp, "Per friend lost")
  }
  for (lv in c("sm_daily", "vape_mean")) {
    f <- feols(as.formula(paste(lv, "~ named | record_id + wave^cohort")), Ps, cluster = ~record_id)
    add(f, "named", "Within-student FE", c(sm_daily = "SM platforms used daily", vape_mean = "Vaping-content exposure (paper scale)")[[lv]], smp, "Per extra friend named")
  }
  f <- feols(vape_mean ~ sm_daily | record_id + wave^cohort, Ps, cluster = ~record_id)
  add(f, "sm_daily", "Within-student FE", "Vaping-content exposure (paper scale)", smp, "Per extra SM platform used daily (chain)")
}
E <- do.call(rbind, est); E$students <- NULL
E[, c("estimate", "lo", "hi")] <- round(E[, c("estimate", "lo", "hi")], 3); E$p <- signif(E$p, 3)
write.csv(E, file.path(OUT, "substitution_estimates.csv"), row.names = FALSE)
options(width = 200); print(E, row.names = FALSE)

# descriptive means by group (all students)
cat("\nMean SM platforms used daily, before -> after, by group:\n")
print(aggregate(cbind(sm_daily0, sm_daily1) ~ group, X, function(x) round(mean(x), 2)))
cat("\nMean vaping-content exposure, before -> after, by group:\n")
print(aggregate(cbind(vape_mean0, vape_mean1) ~ group, X, function(x) round(mean(x), 2)))

# ---- figure 1: alluvial ------------------------------------------------------
ink <- "#52514e"; blues <- c(Low = "#9ec5f4", Middle = "#3987e5", High = "#184f95")
A <- X[!is.na(X$group) & !is.na(X$smc0) & !is.na(X$smc1), ]
A <- aggregate(list(n = rep(1, nrow(A))), A[, c("group", "smc0", "smc1")], sum)
A$share <- A$n / ave(A$n, A$group, FUN = sum)
ng <- tapply(A$n, A$group, sum)
up <- with(A, tapply(n * (as.integer(smc1) > as.integer(smc0)), group, sum) / ng)
dn <- with(A, tapply(n * (as.integer(smc1) < as.integer(smc0)), group, sum) / ng)
lvl <- c("Few -> several", "Few -> few", "Several -> few", "Several -> several")
A$panel <- factor(A$group, levels = lvl, labels = sprintf("%s  (n = %s)\nSM use up %d%%, down %d%%", sub(" -> ", " → ", lvl),
                                                         trimws(format(ng[lvl], big.mark = ",")), round(100 * up[lvl]), round(100 * dn[lvl])))
g1 <- ggplot(A, aes(axis1 = smc0, axis2 = smc1, y = share)) +
  geom_alluvium(aes(fill = smc0), width = 0.28, alpha = 0.85, colour = "#fcfcfb", linewidth = 0.3) +
  geom_stratum(width = 0.28, fill = "#f0efec", colour = "#fcfcfb", linewidth = 0.6) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, colour = "#0b0b0b") +
  scale_x_discrete(limits = c("Before (t)", "After (t + 1)"), expand = c(0.12, 0.12)) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  scale_fill_manual(values = blues, name = "SM use before") + facet_wrap(~ panel, nrow = 2) +
  labs(title = "Social-media use before and after a change in friendships",
       subtitle = sprintf("Consecutive waves W4–W10. Friends: few ≤ %d, several = %d (P25/P75). SM use = platforms used daily: low ≤ %d, high ≥ %d.", qf[1], qf[2], qs[1], qs[2]),
       x = NULL, y = "Share of student transitions") +
  theme_minimal(base_size = 10) + theme(panel.grid = element_blank(), axis.text = element_text(colour = ink),
                                        plot.subtitle = element_text(colour = ink, size = 8.5), legend.position = "bottom", strip.text = element_text(face = "bold"))
cairo_pdf(file.path(OUT, "fig1_alluvial_sm_use.pdf"), width = 9, height = 8); print(g1); dev.off()
png(file.path(OUT, "fig1_alluvial_sm_use.png"), width = 1350, height = 1200, res = 150); print(g1); dev.off()

# ---- figure 2: estimates -------------------------------------------------------
F <- E[E$test != "Within-student FE" | grepl("named", E$contrast) | grepl("friend", E$contrast), ]
F <- E[E$outcome %in% c("SM platforms used daily", "Vaping-content exposure (paper scale)") & E$contrast != "Per extra SM platform used daily (chain)", ]
F$contrast <- factor(F$contrast, levels = rev(c("Gains friends (few->several) vs stays few", "Per friend gained",
                                                "Loses friends (several->few) vs stays several", "Per friend lost", "Per extra friend named")))
g2 <- ggplot(F, aes(estimate, contrast, colour = sample)) +
  geom_vline(xintercept = 0, colour = "#c3c2b7") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ outcome, scales = "free_x") +
  scale_colour_manual(values = c("All students" = "#2a78d6", "Never-users" = "#eb6834"), name = NULL) +
  labs(title = "Change in social-media outcomes when friendships change (95% CI)",
       subtitle = "Substitution predicts: negative after gaining friends, positive after losing them.",
       x = "Difference (outcome units)", y = NULL) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                                        axis.text = element_text(colour = ink), plot.subtitle = element_text(colour = ink))
cairo_pdf(file.path(OUT, "fig2_estimates.pdf"), width = 9.5, height = 4.8); print(g2); dev.off()
png(file.path(OUT, "fig2_estimates.png"), width = 1425, height = 720, res = 150); print(g2); dev.off()
cat("\nSaved figures and substitution_estimates.csv in", OUT, "\n")
