# Check: panel_long / noms built from the workbooks match Yuchan's slim RDS on overlapping columns.
source("paneo/00-config.R")
L <- readRDS(file.path(DATA, "panel_long.rds")); N <- readRDS(file.path(DATA, "noms.rds"))
s <- readRDS("../Cleaned_Data/ADVANCE_slim_v1.rds")
msg("W1 schoolid values in panel: %s", paste(names(table(L[wave == 1, schoolid])), collapse = " "))
bad <- 0
for (w in 1:10) {
  d <- as.data.table(if (w <= 8) s$w18 else s$w910); setnames(d, tolower(names(d)))
  sid <- d[[sprintf("w%d_schoolid", w)]]; if (is.null(sid)) next
  ref <- d[!is.na(sid)]; mine <- L[wave == w]
  m <- merge(ref[, .(record_id = as.character(record_id), use6_ref = as.numeric(get(sprintf("w%d_past_6mo_use_3", w))),
                     mdd_ref = if (sprintf("w%d_rcads_mdd_mean", w) %in% names(ref)) as.numeric(get(sprintf("w%d_rcads_mdd_mean", w))) else NA_real_)],
             mine[, .(record_id, use6, mdd)], by = "record_id", all = TRUE)
  d1 <- m[!(is.na(use6_ref) & is.na(use6)) & (is.na(use6_ref) | is.na(use6) | use6_ref != use6)]
  d2 <- m[!(is.na(mdd_ref) & is.na(mdd)) & (is.na(mdd_ref) | is.na(mdd) | abs(mdd_ref - mdd) > 1e-9)]
  msg("W%d: slim respondents %d, mine %d | use6 mismatches %d | mdd mismatches %d", w, nrow(ref), nrow(mine), nrow(d1), nrow(d2))
  bad <- bad + nrow(d1) + nrow(d2)
  # roster friend cells
  fc <- grep(sprintf("^w%d_friend[0-9]+_[0-9]+$", w), names(d), value = TRUE)
  if (length(fc)) {
    ref_e <- rbindlist(lapply(fc, function(cl) { v <- d[[cl]]; ok <- !is.na(v) & as.character(v) != ""
      data.table(ego = as.character(d$record_id[ok]), alter = trimws(as.character(v[ok])), cl = cl) }))
    mine_e <- N[wave == w & kind == "roster"]
    msg("     roster nominations: slim %d, mine %d (mine excludes empty codes)", nrow(ref_e), nrow(mine_e))
  }
}
msg("TOTAL variable mismatches: %d", bad)
