# 14-sni-vs-named.R — does "How many close friends do you have?" (sni_1, W5/W9) agree with the name generator?
source("paneo/00-config.R")
L <- readRDS(file.path(DATA, "panel_long.rds")); N <- readRDS(file.path(DATA, "noms.rds"))
FR <- readRDS(file.path(DATA, "frame.rds")); EN <- FR$EN
lab <- c(`1` = "None", `2` = "1-2", `3` = "3-5", `4` = "6-9", `5` = "10+", `6` = "Unknown")
out <- list()
for (w in c(5, 9)) {
  R <- L[wave == w & schoolid %in% SCHOOLS, .(record_id, school = schoolid, sni1)]
  nm <- EN[wave == w, .(named = .N, roster = sum(kind == "roster"), typed = sum(kind == "typed"),
                        visible = sum(type == "visible")), by = .(record_id = ego, school)]
  R <- merge(R, nm, by = c("record_id", "school"), all.x = TRUE)
  for (v in c("named", "roster", "typed", "visible")) set(R, which(is.na(R[[v]])), v, 0L)
  R[, sni := factor(lab[as.character(sni1)], levels = lab)]
  msg("\n=== W%d: %d respondents in analysis schools; sni_1 answered: %d ===", w, nrow(R), sum(!is.na(R$sni1)))
  msg("Rows = close friends reported (sni_1); columns = friends named (roster + typed)")
  print(addmargins(table(sni = R$sni, named = R$named, useNA = "ifany")))
  ok <- R[sni1 %in% 1:5]
  msg("Spearman(sni_1, named) = %.3f | Spearman(sni_1, visible) = %.3f",
      cor(ok$sni1, ok$named, method = "spearman"), cor(ok$sni1, ok$visible, method = "spearman"))
  z <- ok[named == 0]
  msg("Named nobody but reported close friends: %d of %d (%.1f%%); reported none: %d",
      sum(z$sni1 >= 2), nrow(z), 100 * mean(z$sni1 >= 2), sum(z$sni1 == 1))
  # consistency band: named within the reported category, allowing the cap of 7
  ok[, lo := c(0, 1, 3, 6, 10)[sni1]][, hi := c(0, 2, 5, 9, Inf)[sni1]]
  ok[, agree := named >= pmin(lo, 7) & (named <= hi | named == 7)]
  ok[, below := named < pmin(lo, 7)][, above := named > hi & named < 7]
  msg("Named count consistent with reported category: %.1f%% | fewer named than reported: %.1f%% | more named than reported: %.1f%%",
      100 * mean(ok$agree), 100 * mean(ok$below), 100 * mean(ok$above))
  out[[length(out) + 1]] <- ok[, .(wave = w, record_id, school, sni1, named, roster, typed, visible)]
}
fwrite(rbindlist(out), file.path(OUT, "checks", "sni_vs_named.csv"))
