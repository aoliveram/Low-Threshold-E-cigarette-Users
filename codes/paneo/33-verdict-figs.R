# 33-verdict-figs.R — figures for the verdict document.
source("paneo/00-config.R")
suppressMessages({ library(ggplot2) })
ink <- "#52514e"; BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; GRAY <- "#a3a29c"
th <- theme_minimal(base_size = 9) + theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = ink),
                                           plot.subtitle = element_text(colour = ink), legend.position = "bottom")
D <- file.path(OUT, "verdict")
# 1. predicted probabilities
P <- fread(file.path(D, "predicted_probabilities.csv"))[sm_level %in% c("P50", "P90")]
P[, sm_lab := factor(sm_level, levels = c("P50", "P90"), labels = c("Typical student (median)", "High exposure (90th percentile)"))]
P[, E_lab := factor(round(E, 3), labels = c("0% (none)", "14% (1 of 7)", "29% (2 of 7)"))]
P[, spec := factor(spec, levels = c("Paper (Table 2 data)", "Proposed R0 (A3)", "S* (A4, p = .047)"),
                   labels = c("Paper (Table 2 data, S1)", "Proposed R0 (past-6-month adoption, S3)", "S* (30-day adoption, S3; p = .047)"))]
rat <- dcast(P, spec + E_lab ~ sm_level, value.var = "prob")[, lab := sprintf("x%.2f\n+%.1f pp", P90 / P50, 100 * (P90 - P50))]
g1 <- ggplot(P, aes(E_lab, 100 * prob, fill = sm_lab)) + geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(data = rat, aes(E_lab, 100 * P90, label = lab), inherit.aes = FALSE, vjust = -0.3, size = 2.6, colour = ink) +
  facet_wrap(~ spec, nrow = 1) + scale_fill_manual(values = c(GRAY, BLUE), name = "Social-media exposure") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Chance of starting to vape in a semester (model-based average)",
       subtitle = "Labels: high vs typical social-media exposure, as a ratio and as percentage points. Peer exposure = share of named friends who had used.",
       x = "Peer exposure in the previous semester", y = "Predicted probability (%)") + th
ggsave(file.path(D, "fig_probabilities.png"), g1, width = 10, height = 4.2, dpi = 150)

# 2. robustness of S* and R0
R <- fread(file.path(D, "verdict_results.csv"))[model == "MB" & quantity == "R"]
R[, group := fifelse(dim == "base", "Base", fifelse(dim == "drop_school", "Leave one school out", fifelse(dim == "drop_gp", "Leave one semester out",
            fifelse(dim %in% c("I7_seed") | option %like% "I7_s", "Imputation seeds (multiple imputation)", paste("Change:", dim)))))]
R[, lab := fifelse(dim == "base", "base", option)]
R[, sig := factor(p < .05, levels = c(TRUE, FALSE), labels = c("p < .05", "p >= .05"))]
R[, base := factor(base, levels = c("Sstar", "R0"), labels = c("S* (30-day adoption, prevalence imputation)", "R0 (proposed reference)"))]
setorder(R, base, group, est); R[, row := factor(paste(group, lab, sep = " | "), levels = unique(paste(group, lab, sep = " | ")))]
g2 <- ggplot(R, aes(est, row, colour = sig)) + geom_vline(xintercept = 1, colour = "#c3c2b7") +
  geom_errorbarh(aes(xmin = est_lo, xmax = est_hi), height = 0, linewidth = 0.4) + geom_point(size = 1.4) +
  facet_wrap(~ base, scales = "free_y") + scale_x_log10() + scale_colour_manual(values = c(BLUE, GRAY), name = NULL) +
  labs(title = "How stable is the interaction R?", subtitle = "R > 1: social media matters more when peer exposure is low. Each row changes one thing.",
       x = "R (interaction, log scale)", y = NULL) + th + theme(axis.text.y = element_text(size = 5.5))
ggsave(file.path(D, "fig_robustness.png"), g2, width = 10, height = 9, dpi = 150)
msg("verdict figures written")
