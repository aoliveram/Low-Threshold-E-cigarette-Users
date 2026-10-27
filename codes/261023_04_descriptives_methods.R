# =============================================================================
# 261023_04_descriptives_methods.R
# DESCRIPTIVE NUMBERS quoted in the Methods (optional; no models).
#
#   1. Who are the named friends? visible / skipped the wave / not in the survey / typed by hand
#   2. How much do we know about friends who are not in the survey? (how many students name each one)
#   3. Students who named nobody: do they report close friends? (wX_sni_1, asked at W5 and W9)
#   4. Social-media items: how many are missing only because the platform is used less than weekly
#   5. Distribution of the peer exposure E and of starts across E
#
# Only aggregate tables are written (no student-level rows).
# Outputs (../outputs_261023/04_descriptives_<time>/): methods_descriptives.csv, 04_descriptives_log.txt
#
# How to run (from codes/, after script 00):   Rscript 261023_04_descriptives_methods.R      (seconds)
# =============================================================================
rm(list = ls())
source("261023_functions_network_exposure.R")
run <- start_run("04_descriptives")
slim <- readRDS(DATA_FILE); F <- build_frame(slim)
P <- lapply(c(A3 = "A3", A4 = "A4"), function(o) prepare_spec(F, outcome = o))
D <- lapply(P, assemble)
OUTT <- list(); keep <- function(block, item, value) OUTT[[length(OUTT) + 1]] <<- data.table(block, item, value = as.character(value))

# ══════════════════════════════════════════════════════════════════════════════
# 1. Named friends, by type (friendships used in the A3 models, named at t-1)
# ══════════════════════════════════════════════════════════════════════════════
section("1. Named friends by type")
ed <- P$A3$ed
tt <- ed[, .N, by = type][, pct := round(100 * N / sum(N), 1)][order(-N)]
print(tt, row.names = FALSE)
for (i in seq_len(nrow(tt))) keep("Named friends (A3 models)", tt$type[i], sprintf("%s (%.1f%%)", format(tt$N[i], big.mark = ","), tt$pct[i]))
say("Unseen (not in the survey or typed): %.1f%% of named friendships", 100 * mean(ed$type %in% c("nonparticipant", "typed")))
keep("Named friends (A3 models)", "unseen = nonparticipant + typed, %", round(100 * mean(ed$type %in% c("nonparticipant", "typed")), 1))
EN48 <- F$EN[wave %in% 4:8]
say("All nominations W4-W8 (any student): %.1f%% unseen", 100 * mean(EN48$type %in% c("nonparticipant", "typed")))
keep("All nominations W4-W8", "unseen, %", round(100 * mean(EN48$type %in% c("nonparticipant", "typed")), 1))

# ══════════════════════════════════════════════════════════════════════════════
# 2. Friends not in the survey: how many students name each one?
# ══════════════════════════════════════════════════════════════════════════════
section("2. Information available for friends not in the survey (homophily)")
np <- unique(ed[type == "nonparticipant", .(school, wn, akey, Nn)])
tab <- np[, .N, by = .(named_by = fifelse(Nn >= 4, "4+", as.character(Nn)))][order(named_by)][, pct := round(100 * N / sum(N), 1)]
print(tab, row.names = FALSE)
say("Median number of students naming a non-participant friend: %d; named by 2 or more: %.1f%%", as.integer(median(np$Nn)), 100 * mean(np$Nn >= 2))
keep("Non-participant friends", "named by 2 or more students, %", round(100 * mean(np$Nn >= 2), 1))
keep("Non-participant friends", "median students naming each", median(np$Nn))

# ══════════════════════════════════════════════════════════════════════════════
# 3. Students who named nobody: close friends reported (wX_sni_1)
# ══════════════════════════════════════════════════════════════════════════════
section("3. Named nobody vs 'How many close friends do you have?' (W5, W9)")
lab <- c("None", "1-2", "3-5", "6-9", "10+")
for (w in c(5, 9)) {
  R <- slim$panel[wave == w & schoolid %in% SCHOOLS & sni1 %in% 1:5, .(record_id, school = schoolid, sni1)]
  nm <- F$EN[wave == w, .(named = .N), by = .(record_id = ego, school)]
  R <- merge(R, nm, by = c("record_id", "school"), all.x = TRUE)[is.na(named), named := 0L]
  z <- R[named == 0]
  say("W%d: %d named nobody; %.1f%% of them report at least one close friend. Spearman(sni_1, named) = %.2f",
      w, nrow(z), 100 * mean(z$sni1 >= 2), cor(R$sni1, R$named, method = "spearman"))
  print(table(close_friends = factor(lab[z$sni1], levels = lab)))
  keep("Named nobody", sprintf("W%d: report >= 1 close friend, %%", w), round(100 * mean(z$sni1 >= 2), 1))
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. Social-media items skipped by design
# ══════════════════════════════════════════════════════════════════════════════
section("4. Vaping-content items (wX_sm_post_k) missing because the platform is used less than weekly")
L <- slim$panel[wave %in% 4:8]                      # the vaping-content items exist only at W4-W8
core <- as.matrix(L[, paste0("smp", 1:6), with = FALSE]); core[!(core %in% 1:5)] <- NA
use  <- as.matrix(L[, paste0("smu", c(1, 2, 3, 7, 8, 11)), with = FALSE]); use[!(use %in% 0:5)] <- NA
L_ok <- rowSums(!is.na(use)) > 0; core <- core[L_ok, ]; use <- use[L_ok, ]      # answered the general-use items
miss <- is.na(core); skip <- miss & !is.na(use) & use < 2
say("Platform items missing: %.1f%%; of those, %.1f%% belong to a platform used less than weekly (skip logic)",
    100 * mean(miss), 100 * sum(skip) / sum(miss))
s1_n <- rowSums(!is.na(core))
say("Platforms answered per student-wave (paper's S1 averages over these): %s", paste(sprintf("%d: %.0f%%", 0:6, 100 * tabulate(s1_n + 1, 7) / length(s1_n)), collapse = "  "))
keep("Social media items", "missing items due to skip logic, %", round(100 * sum(skip) / sum(miss), 1))
# Reliability of S3 (Reviewer 2): Cronbach's alpha of the 6 platform items, skipped platforms coded "never"
s3 <- core; s3[is.na(s3) & !is.na(use) & use < 2] <- 5; s3 <- 6 - s3[rowSums(!is.na(s3)) == 6, ]
alpha <- function(X) { k <- ncol(X); k / (k - 1) * (1 - sum(apply(X, 2, var)) / var(rowSums(X))) }
say("S3 items complete for all 6 platforms: %d student-waves; Cronbach's alpha = %.2f", nrow(s3), alpha(s3))
keep("Social media items", "S3 Cronbach alpha (6 platforms)", round(alpha(s3), 2))
keep("Social media items", "student-waves answering 1 platform only, %", round(100 * mean(s1_n == 1), 1))

# ══════════════════════════════════════════════════════════════════════════════
# 5. Peer exposure E and starts
# ══════════════════════════════════════════════════════════════════════════════
section("5. Peer exposure E (share of named friends who had used, t-1)")
for (o in names(D)) {
  d <- D[[o]]
  b <- d[, .(band = cut(E, c(-Inf, 0, 1/7, 1/4, 2/7, 1/2, 1), labels = c("0", "(0, 1/7]", "(1/7, 1/4]", "(1/4, 2/7]", "(2/7, 1/2]", "(1/2, 1]")))]
  tb <- d[, .(student_semesters = .N, starts = sum(y)), by = .(band = b$band)][order(band)]
  tb[, `:=`(pct_rows = round(100 * student_semesters / sum(student_semesters), 1), start_rate_pct = round(100 * starts / student_semesters, 2))]
  say("\n%s: E quantiles P25/P50/P75/P90 = %s; among starters = %s", o,
      paste(round(quantile(d$E, c(.25, .5, .75, .9)), 3), collapse = "/"), paste(round(quantile(d$E[d$y == 1], c(.25, .5, .75, .9)), 3), collapse = "/"))
  print(tb, row.names = FALSE)
  keep(sprintf("Exposure %s", o), "starts with E <= 1/4, %", round(100 * mean(d$E[d$y == 1] <= 0.25 + 1e-9), 1))
  keep(sprintf("Exposure %s", o), "student-semesters with E = 0, %", round(100 * mean(d$E == 0), 1))
}
fwrite(rbindlist(OUTT), file.path(run$dir, "methods_descriptives.csv"))
say("\nOutputs written to %s", normalizePath(run$dir))
end_run(run)
