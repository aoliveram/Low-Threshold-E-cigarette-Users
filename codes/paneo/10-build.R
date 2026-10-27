# 10-build.R — read the raw ADVANCE workbooks once.
# Outputs (data/):  panel_long.rds  person x wave variables (respondents: non-missing schoolid)
#                   noms.rds        every friend nomination (roster ID or hashed typed name)
# Typed names are normalised and hashed on read; raw names are never stored or printed.
source("paneo/00-config.R")
suppressMessages(library(readxl))
t0 <- Sys.time()

VARS_NUM <- c("schoolid", "past_6mo_use_3", "past30_sub_3_any", "past30_sub_3", "friends_use_ecig",
              "dem_gender", "dem_sexuality", "race", "eth", "dem_high_par_edu", "rcads_mdd_mean", "sni_1")
read_book <- function(f, waves) {
  nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
  wv <- paste(waves, collapse = "|")
  num <- grepl(sprintf("^w(%s)_(%s)$", wv, paste(VARS_NUM, collapse = "|")), nm) |
         grepl(sprintf("^w(%s)_(sm_post|socialmed)_[0-9]+$", wv), nm)
  txt <- grepl(sprintf("^w(%s)_friend[1-7]_(other_)?[0-9]{3}$", wv), nm)
  ct <- rep("skip", length(nm)); ct[num] <- "numeric"; ct[txt] <- "text"; ct[nm == "record_id"] <- "text"
  d <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal")
  names(d) <- tolower(names(d)); as.data.table(d)
}
B <- list(w18 = read_book(XL_W18, 1:8), w910 = read_book(XL_W910, 9:10))
msg("read workbooks: %d x %d and %d x %d (%.0fs)", nrow(B$w18), ncol(B$w18), nrow(B$w910), ncol(B$w910),
    as.numeric(difftime(Sys.time(), t0, units = "secs")))

# ---- person x wave panel --------------------------------------------------------
long <- rbindlist(lapply(1:10, function(w) {
  d <- if (w <= 8) B$w18 else B$w910
  col <- function(v) { x <- d[[sprintf("w%d_%s", w, v)]]; if (is.null(x)) rep(NA_real_, nrow(d)) else as.numeric(x) }
  sid <- col("schoolid")
  if (w == 1) sid <- c(`1` = 101, `2` = 104, `3` = 102, `4` = 103, `5` = 105)[as.character(sid)]  # W1 coding
  out <- data.table(record_id = d$record_id, wave = w, schoolid = as.numeric(sid),
                    use6 = col("past_6mo_use_3"),
                    use30 = if (w %in% 7:8) as.numeric(col("past30_sub_3") > 0) else col("past30_sub_3_any"),
                    pfu = col("friends_use_ecig"), gender = col("dem_gender"), sexuality = col("dem_sexuality"),
                    race = col("race"), eth = col("eth"), paredu = col("dem_high_par_edu"),
                    mdd = col("rcads_mdd_mean"), sni1 = col("sni_1"))
  for (k in 1:9)  out[[paste0("smp", k)]] <- col(paste0("sm_post_", k))
  for (k in 1:15) out[[paste0("smu", k)]] <- col(paste0("socialmed_", k))
  out[!is.na(schoolid)]
}))
saveRDS(long, file.path(DATA, "panel_long.rds"))
msg("panel_long: %d respondent-waves, %d students", nrow(long), uniqueN(long$record_id))

# ---- nominations (W2–W10; W1 friend cells are REDCap codes) ----------------------
noms <- rbindlist(lapply(2:10, function(w) {
  d <- if (w <= 8) B$w18 else B$w910
  cols <- grep(sprintf("^w%d_friend[1-7]_(other_)?[0-9]{3}$", w), names(d), value = TRUE)
  rbindlist(lapply(cols, function(cl) {
    v <- d[[cl]]; ok <- !is.na(v) & !(trimws(v) %in% EMPTY)
    if (!any(ok)) return(NULL)
    typed <- grepl("_other_", cl)
    data.table(wave = w, ego = d$record_id[ok], col_school = as.integer(sub(".*_", "", cl)),
               slot = as.integer(sub(sprintf("^w%d_friend([1-7]).*", w), "\\1", cl)),
               kind = if (typed) "typed" else "roster",
               alter = if (typed) paste0("T:", hash_names(normalize_name(v[ok]))) else trimws(v[ok]))
  }))
}))
noms <- noms[!(kind == "typed" & alter == "T:NA")]
saveRDS(noms, file.path(DATA, "noms.rds"))
msg("noms: %d nominations (%d roster, %d typed) | %.0fs total", nrow(noms), sum(noms$kind == "roster"),
    sum(noms$kind == "typed"), as.numeric(difftime(Sys.time(), t0, units = "secs")))
