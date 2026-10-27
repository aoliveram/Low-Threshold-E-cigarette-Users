# 18-summary.R — flag every specification (rule P6) and draw the Layer 2 heatmaps and Layer 3
# specification curves.
source("paneo/00-config.R")
suppressMessages({ library(ggplot2); library(patchwork) })
ink <- "#52514e"; BLUE <- "#1c5cab"; RED <- "#c23b3a"; MID <- "#f0efec"
LABCOL <- c(D = "#2a78d6", Disc = "#eb6834", I = "#8f8e88")

KEY <- data.table(
  qkey = c("sm_any", "low0", "low25", "R_MB", "R_MC0", "R_MC25", "R_MD25", "R_MA25", "ME_corr", "MF_U", "MF_NU"),
  model = c("M0", "MA", "MA", "MB", "MC", "MC", "MD", "MA", "ME", "MF", "MF"),
  quantity = c("OR_SM", "OR_SM_low@c0.000", "OR_SM_low@c0.250", "R", "R@c0.000", "R@c0.250", "R@c0.250", "R@c0.250",
               "slope_corrected", "OR_smc:Uc", "OR_NUc"),
  title = c("SM, any adoption (M0)", "SM, no-exposure adoption (MA, 0)", "SM, low-threshold adoption (MA, 1/4)",
            "R interaction (MB)", "R stratified (MC, 0)", "R stratified (MC, 1/4)", "R multinomial (MD, 1/4)", "R event-type (MA, 1/4)",
            "Threshold slope, corrected (ME)", "SM x using friends (MF)", "Non-using friends (MF)"))

# Non-exclusive flags. R > 1 or a negative threshold slope = substitution (the paper's mechanism).
flags <- function(res, idcols) {
  r <- res[!is.na(model)]
  r[, `:=`(sig = !is.na(p) & p < 0.05, up = est > 1, isR = quantity == "R" | grepl("^R@", quantity))]
  f <- r[, .(
    substitution  = any(isR & sig & up) | any(quantity == "slope_corrected" & sig & !up) | any(quantity == "OR_smc:Uc" & sig & !up),
    substitution_clean = any(model %in% c("MB", "MC") & isR & sig & up),
    amplification = any(isR & sig & !up),
    paper_like    = any(model == "MA" & grepl("^OR_SM_low", quantity) & sig & up),
    additive      = any(model == "M0" & quantity == "OR_SM" & sig & up),
    protective_nonusers = any(quantity == "OR_NUc" & sig & !up)), by = idcols]
  f[, interesting := substitution | paper_like | amplification | protective_nonusers | additive]
  f[, main := fifelse(substitution, "Substitution", fifelse(amplification, "Alternative: amplification",
              fifelse(paper_like, "As in the paper", fifelse(protective_nonusers | additive, "Alternative: additive / protective", "Not informative"))))]
  f
}
key_table <- function(res) {
  k <- merge(res[!is.na(model)], KEY, by = c("model", "quantity"))
  k[, cell := sprintf("%.2f%s", est, fifelse(p < .001, "***", fifelse(p < .01, "**", fifelse(p < .05, "*", ""))))]
  k
}

# ---------------- Layer 2 ----------------
L2 <- fread(file.path(OUT, "layer2", "layer2_results.csv"))
c2 <- flags(L2, c("id", "base", "dim", "option", "label"))
fwrite(c2, file.path(OUT, "layer2", "layer2_flags.csv"))
k2 <- key_table(L2)
fwrite(dcast(k2, base + dim + option + label ~ qkey, value.var = "cell"), file.path(OUT, "layer2", "layer2_key_table.csv"))
show <- c("sm_any", "low0", "low25", "R_MB", "R_MC25", "R_MD25", "MF_NU")
show_lab <- KEY[match(show, KEY$qkey), title]
for (bb in unique(k2$base)) {
  d <- k2[base == bb & qkey %in% show]
  d[, col := factor(qkey, levels = show, labels = show_lab)]
  rows <- unique(d[order(dim, option), paste0(dim, ": ", option)])
  d[, row := factor(paste0(dim, ": ", option), levels = rev(rows))]
  d[, lfill := pmax(pmin(log(est), log(2)), -log(2))]
  g <- ggplot(d, aes(col, row, fill = lfill)) + geom_tile(colour = "#fcfcfb", linewidth = 0.6) +
    geom_text(aes(label = cell), size = 2.3, colour = "#0b0b0b") +
    scale_fill_gradient2(low = BLUE, mid = MID, high = RED, midpoint = 0, limits = c(-log(2), log(2)),
                         breaks = log(c(0.5, 0.75, 1, 1.5, 2)), labels = c("0.5", "0.75", "1", "1.5", "2"), name = "Estimate") +
    scale_x_discrete(position = "top", labels = function(x) gsub(" \\(", "\n(", x)) +
    labs(title = sprintf("Layer 2: one decision at a time, from %s", if (bb == "R0") "the reference spec (R0)" else "the paper's Table 2 (H1)"),
         subtitle = "Odds ratios or R; *p<.05 **p<.01 ***p<.001. R > 1 = substitution (SM matters more when peer exposure is low).",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 8) + theme(panel.grid = element_blank(), axis.text = element_text(colour = ink),
                                         plot.subtitle = element_text(colour = ink))
  h <- 1.6 + 0.17 * length(rows)
  ggsave(file.path(OUT, "layer2", sprintf("layer2_heatmap_%s.pdf", bb)), g, width = 10, height = h, device = cairo_pdf)
  ggsave(file.path(OUT, "layer2", sprintf("layer2_heatmap_%s.png", bb)), g, width = 10, height = h, dpi = 150)
}
msg("Layer 2 flags (non-exclusive), by base:")
print(c2[, lapply(.SD, sum), by = base, .SDcols = c("substitution", "substitution_clean", "amplification", "paper_like", "additive", "protective_nonusers", "interesting")])

# ---------------- Layer 3 ----------------
f3 <- file.path(OUT, "layer3", "layer3_results.csv")
if (file.exists(f3)) {
  L3 <- fread(f3)
  idc <- c("id", "A", "U", "T", "NI", "Z", "SM", "label")
  c3 <- flags(L3, idc)
  fwrite(c3, file.path(OUT, "layer3", "layer3_flags.csv"))
  msg("Layer 3 flags (non-exclusive), by spec label:")
  print(c3[, c(.(specs = .N), lapply(.SD, sum)), by = label, .SDcols = c("substitution", "substitution_clean", "amplification", "paper_like", "additive", "protective_nonusers", "interesting")])
  msg("Layer 3 main category by label:"); print(dcast(c3[, .N, by = .(label, main)], main ~ label, value.var = "N", fill = 0))
  k3 <- merge(L3[!is.na(model)], KEY, by = c("model", "quantity"))
  fwrite(dcast(key_table(L3), A + U + T + NI + Z + SM + label ~ qkey, value.var = "cell"), file.path(OUT, "layer3", "layer3_key_table.csv"))
  curve <- function(kk) {
    d <- k3[qkey == kk][order(est)]; d[, rank := .I]
    top <- ggplot(d, aes(rank, est, colour = label)) + geom_hline(yintercept = 1, colour = "#c3c2b7") +
      geom_linerange(aes(ymin = est_lo, ymax = est_hi), linewidth = 0.35, alpha = 0.6) + geom_point(size = 1.1) +
      scale_y_log10() + scale_colour_manual(values = LABCOL, name = "Spec label") +
      labs(title = KEY[qkey == kk, title], subtitle = sprintf("%d specifications; %d with p < .05 above 1, %d below 1", nrow(d),
           sum(d$p < .05 & d$est > 1), sum(d$p < .05 & d$est < 1)), x = NULL, y = "Estimate (log scale)") +
      theme_minimal(base_size = 8) + theme(panel.grid.minor = element_blank(), axis.text.x = element_blank(),
                                           legend.position = "top", plot.title = element_text(face = "bold"))
    m <- melt(d[, .(rank, A, U, T, NI, Z, SM)], id.vars = "rank", variable.name = "decision", value.name = "option")
    m[, row := paste0(decision, ": ", option)]
    bot <- ggplot(m, aes(rank, row)) + geom_point(shape = 15, size = 0.8, colour = ink) +
      labs(x = "Specifications, sorted by estimate", y = NULL) + theme_minimal(base_size = 7) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_blank())
    top / bot + plot_layout(heights = c(2, 1.4))
  }
  for (kk in c("low25", "low0", "sm_any", "R_MB", "R_MC0", "R_MC25", "R_MD25", "MF_NU")) {
    ggsave(file.path(OUT, "layer3", sprintf("layer3_curve_%s.pdf", kk)), curve(kk), width = 9, height = 6.5, device = cairo_pdf)
    ggsave(file.path(OUT, "layer3", sprintf("layer3_curve_%s.png", kk)), curve(kk), width = 9, height = 6.5, dpi = 150)
  }
}
msg("Summary written to %s", OUT)
