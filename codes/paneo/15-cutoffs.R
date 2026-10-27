# 15-cutoffs.R — where adopters fall on the exposure scale under each network rule (descriptive; no models).
source("paneo/12-engine.R")
cand <- c(0, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 3/5, 2/3)
lab  <- c("0", "1/7", "1/6", "1/5", "1/4", "2/7", "1/3", "2/5", "3/7", "1/2", "3/5", "2/3")
cases <- list(
  paper_N1        = default_spec(A = "A1", U = "U1", N = "N1", I = "I0", Z = "Z1", S = "S1", X = "X1", F = "F1", CL = "vid", paperlag = TRUE, T = "T3"),
  N4_unseen_0     = default_spec(I = "I0"),
  N4_I3I5_expect  = default_spec(I = "I35"),
  N4_I7_draws     = default_spec(I = "I7"))
res <- rbindlist(lapply(names(cases), function(nm) {
  ds <- build_data(cases[[nm]])
  rbindlist(lapply(seq_along(ds), function(d) { D <- ds[[d]]; A <- D[y == 1]
    data.table(case = nm, draw = d, cut = lab, adopters = nrow(A),
               share_adopters = sapply(cand, function(c) mean(A$E <= c + 1e-9)),
               share_atrisk = sapply(cand, function(c) mean(D$E <= c + 1e-9)),
               median_n = median(A$n)) }))
}))
S <- res[, .(adopters = round(mean(adopters)), share_adopters = round(100 * mean(share_adopters), 1),
             share_atrisk = round(100 * mean(share_atrisk), 1), median_n = median(median_n)), by = .(case, cut)]
W <- dcast(S, cut ~ case, value.var = "share_adopters"); W <- W[match(lab, cut)]
msg("Share of adopters (%%) at or below each fraction of friends using:")
print(W, row.names = FALSE)
msg("adopters per case: %s | median friends in denominator among adopters: %s",
    paste(unique(S[, paste(case, adopters)]), collapse = "; "), paste(unique(S[, paste(case, median_n)]), collapse = "; "))
fwrite(S, file.path(OUT, "checks", "cutoff_distributions.csv"))
