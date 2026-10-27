# Descriptive checks for the sweep plan (no models). Never prints typed names.
suppressMessages(library(readxl))
f <- "../Cleaned_Data/ADVANCE_W1-W8.xlsx"
nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
cat("W7/W8 past-30 e-cig columns:", paste(grep("^w[78]_past30.*(_3|_3_any)$", nm, value = TRUE), collapse = " "), "\n")
cat("W7/W8 other 30-day vaping columns:", paste(head(grep("^w[78]_.*(vape|ecig).*30|^w[78]_.*30.*(vape|ecig)", nm, value = TRUE), 12), collapse = " "), "\n")
ct <- rep("skip", length(nm)); ct[grepl("^w[3-8]_friend[1-7]_(other_)?[0-9]{3}$|^w[3-8]_schoolid$", nm) | nm == "record_id"] <- "text"
d <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal"); names(d) <- tolower(names(d))
SCH <- c(101:107, 112:114, 201, 212:214); EMPTY <- c("", "NA", "-999", "-99", "0")
res <- list(); unm <- list()
for (w in 4:8) {
  s <- suppressWarnings(as.integer(d[[sprintf("w%d_schoolid", w)]]))
  resp <- d$record_id[!is.na(s)]
  for (sch in SCH) {
    ii <- which(s == sch)
    R <- as.matrix(d[ii, intersect(sprintf("w%d_friend%d_%d", w, 1:7, sch), names(d))]); R[R %in% EMPTY] <- NA
    O <- as.matrix(d[ii, intersect(sprintf("w%d_friend%d_other_%d", w, 1:7, sch), names(d))]); O[trimws(O) %in% EMPTY] <- NA
    inpanel <- matrix(R %in% d$record_id, nrow(R)); responded <- matrix(R %in% resp, nrow(R))
    res[[length(res) + 1]] <- data.frame(wave = w, school = sch, ego = d$record_id[ii],
      roster = rowSums(!is.na(R)), roster_participant_resp = rowSums(!is.na(R) & inpanel & responded),
      roster_participant_noresp = rowSums(!is.na(R) & inpanel & !responded), roster_nonpart = rowSums(!is.na(R) & !inpanel),
      typed = rowSums(!is.na(O)))
    v <- R[!is.na(R) & !inpanel]; if (length(v)) unm[[length(unm) + 1]] <- data.frame(wave = w, school = sch, alter = v)
  }
}
X <- do.call(rbind, res); U <- do.call(rbind, unm)
tot <- colSums(X[, c("roster_participant_resp", "roster_participant_noresp", "roster_nonpart", "typed")])
cat("\nNamed friends W4–W8 by type (all egos):\n"); print(round(100 * tot / sum(tot), 1)); cat("total named:", sum(tot), "\n")
X$named <- X$roster + X$typed; X$unseen <- X$roster_participant_noresp + X$roster_nonpart + X$typed
cat("\nEgos who named >=1 friend:", sum(X$named > 0), "| with >=1 unseen friend:", sum(X$unseen > 0 & X$named > 0),
    sprintf("(%.1f%%)", 100 * mean(X$unseen[X$named > 0] > 0)), "| all named friends unseen:", sum(X$unseen == X$named & X$named > 0), "\n")
cat("Mean share unseen among egos naming >=1:", round(mean((X$unseen / X$named)[X$named > 0]), 3), "\n")
cat("\nNon-participant roster IDs: format examples (numeric?):", mean(grepl("^[0-9]+$", U$alter)), "share numeric;",
    "distinct IDs:", length(unique(paste(U$school, U$alter))), "\n")
k <- table(paste(U$wave, U$school, U$alter)); cat("Nominators per non-participant (same wave): mean", round(mean(k), 2), "| named by >=2 egos:", round(100 * mean(k >= 2), 1), "%\n")
