# 01-nominations-distribution.R — how many friends do students name per wave?
# Defines "few" (<= P25) and "several" (>= P75) for the social-media substitution analysis.
# Counts every named friend: roster selections + typed names, in the student's own-school slots.
suppressMessages({ library(readxl); library(ggplot2) })
DATA <- "../Cleaned_Data"
XL <- c(w18 = file.path(DATA, "ADVANCE_W1-W8.xlsx"),
        w910 = file.path(DATA, "ADVANCE_W9-W10.xlsx"))
OUT <- "../outputs_substitution"; dir.create(OUT, showWarnings = FALSE)
SCHOOLS <- c(101:107, 112:114, 201, 212:214)          # analysis schools (108 excluded, as in 261015)
EMPTY <- c("", "NA", "-999", "-99", "0")
WAVES <- 4:10                                          # waves with general social-media use (socialmed_*)

read_cols <- function(f, pattern) {
  nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
  ct <- rep("skip", length(nm)); ct[grepl(pattern, nm) | nm == "record_id"] <- "text"
  d <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal")
  names(d) <- tolower(names(d)); d
}
pat <- sprintf("^w(%s)_(schoolid|friend[1-7]_(other_)?[0-9]{3}|socialmed_[0-9]+)$", paste(WAVES, collapse = "|"))
D <- list(w18 = read_cols(XL[["w18"]], pat), w910 = read_cols(XL[["w910"]], pat))

rows <- list()
for (w in WAVES) {
  d <- if (w <= 8) D$w18 else D$w910
  s <- suppressWarnings(as.integer(d[[sprintf("w%d_schoolid", w)]]))
  ok <- which(!is.na(s) & s %in% SCHOOLS)
  sm_cols <- grep(sprintf("^w%d_socialmed_[0-9]+$", w), names(d), value = TRUE)
  for (i in ok) {
    sch <- s[i]
    r <- unlist(d[i, intersect(sprintf("w%d_friend%d_%d", w, 1:7, sch), names(d))])
    o <- unlist(d[i, intersect(sprintf("w%d_friend%d_other_%d", w, 1:7, sch), names(d))])
    r <- !is.na(r) & !(r %in% EMPTY); o <- !is.na(o) & !(trimws(o) %in% EMPTY)
    k <- length(r); if (length(o) == k && k > 0) named <- sum(r | o) else named <- sum(r) + sum(o)
    sm_any <- if (length(sm_cols)) any(!is.na(suppressWarnings(as.numeric(unlist(d[i, sm_cols]))))) else NA
    rows[[length(rows) + 1]] <- data.frame(record_id = d$record_id[i], wave = w, school = sch,
                                           named = min(named, 7L), sm_answered = sm_any)
  }
}
nom <- do.call(rbind, rows)
saveRDS(nom, file.path(OUT, "nominations_W4-W10.rds"))

cat("Respondent-waves:", nrow(nom), " students:", length(unique(nom$record_id)), "\n\n")
cat("Distribution of named friends (pooled W4-W10):\n")
tab <- table(factor(nom$named, levels = 0:7)); print(rbind(n = tab, pct = round(100 * prop.table(tab), 1)))
cat("\nQuantiles (type 7):\n"); print(quantile(nom$named, c(.25, .5, .75)))
cat("Quantiles (type 1, observed values):\n"); print(quantile(nom$named, c(.25, .5, .75), type = 1))
cat("\nP25 / median / P75 by wave (type 1):\n")
print(do.call(rbind, lapply(split(nom$named, nom$wave), function(x) c(n = length(x), quantile(x, c(.25, .5, .75), type = 1), mean = round(mean(x), 2)))))
cat("\nZero nominations: share who answered the social-media use items (survey-completion proxy):\n")
z <- nom[nom$named == 0, ]; cat(sprintf("  %d of %d (%.1f%%)\n", sum(z$sm_answered, na.rm = TRUE), nrow(z), 100 * mean(z$sm_answered, na.rm = TRUE)))

q <- quantile(nom$named, c(.25, .75), type = 1)
pd <- as.data.frame(prop.table(table(wave = nom$wave, named = factor(nom$named, levels = 0:7)), margin = 1), responseName = "share")
pd$wave <- factor(paste0("W", pd$wave), levels = paste0("W", WAVES))
pall <- data.frame(named = 0:7, share = as.numeric(prop.table(tab)))
ink <- "#52514e"; bar <- "#2a78d6"
g1 <- ggplot(pall, aes(factor(named), share)) +
  geom_col(fill = bar, width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * share)), vjust = -0.4, size = 3.2, colour = ink) +
  annotate("text", x = q[1] + 1, y = max(pall$share) * 1.12, label = sprintf("P25 = %d", q[1]), colour = ink, size = 3.4, fontface = "bold") +
  annotate("text", x = q[2] + 1, y = max(pall$share) * 1.12, label = sprintf("P75 = %d", q[2]), colour = ink, size = 3.4, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Friends named per survey wave (W4–W10, pooled)",
       subtitle = sprintf("%s respondent-waves; roster + typed names, own-school slots (max 7)", format(nrow(nom), big.mark = ",")),
       x = "Friends named", y = "Share of respondent-waves") +
  theme_minimal(base_size = 11) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                        axis.text = element_text(colour = ink), plot.subtitle = element_text(colour = ink))
g2 <- ggplot(pd, aes(named, share)) + geom_col(fill = bar, width = 0.7) + facet_wrap(~ wave, nrow = 1) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Same distribution by wave", x = "Friends named", y = NULL) +
  theme_minimal(base_size = 10) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                        axis.text = element_text(colour = ink, size = 7))
cairo_pdf(file.path(OUT, "nominations_distribution.pdf"), width = 9, height = 7); gridExtra::grid.arrange(g1, g2, heights = c(3, 2)); dev.off()
png(file.path(OUT, "nominations_distribution.png"), width = 1350, height = 1050, res = 150); gridExtra::grid.arrange(g1, g2, heights = c(3, 2)); dev.off()
cat("\nSaved:", file.path(OUT, "nominations_distribution.{pdf,png}"), "\n")
