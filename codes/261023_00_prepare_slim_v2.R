# =============================================================================
# 261023_00_prepare_slim_v2.R
# ONE-TIME DATA PREP for the recommended models (run once, before scripts 01-04).
#
# Reads the two ADVANCE workbooks and writes ADVANCE_slim_v2.rds with:
#   panel : one row per student x wave (respondents only: non-missing schoolid)
#   noms  : one row per friend nomination (roster ID, or a HASHED typed name)
#
# What is new compared with ADVANCE_slim_v1.rds:
#   - past-30-day e-cigarette use (wX_past30_sub_3_any; W7-W8 only have days, wX_past30_sub_3)
#   - general social-media use per platform (wX_socialmed_k) and every vaping-content item (wX_sm_post_k)
#   - typed friend names (wX_friendK_other_SCH), HASHED here: raw names are never stored or printed
#   - "How many close friends do you have?" (wX_sni_1), used only as a validation check
#
# How to use: put this file in codes/ and run it from codes/ (same layout as the earlier scripts).
#   Rscript 261023_00_prepare_slim_v2.R
# =============================================================================
rm(list = ls())
suppressMessages({ library(readxl); library(data.table) })

in_path  <- "../Cleaned_Data"
out_file <- file.path(in_path, "ADVANCE_slim_v2.rds")
XL_W18   <- file.path(in_path, "ADVANCE_W1-W8.xlsx")
XL_W910  <- file.path(in_path, "ADVANCE_W9-W10.xlsx")
stopifnot(file.exists(XL_W18), file.exists(XL_W910))
EMPTY <- c("", "NA", "-999", "-99", "0")

# Typed names are normalised and hashed with a random salt that is never saved:
# the hash only needs to tell whether two students typed the same name in this build.
SALT <- paste(sample(c(letters, 0:9), 40, TRUE), collapse = "")
hash_names <- function(x) {
  x <- tolower(trimws(x)); x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\s+", " ", trimws(gsub("[^a-z ]", "", x)))
  out <- rep(NA_character_, length(x)); ok <- !is.na(x) & nzchar(x)
  out[ok] <- substr(vapply(paste0(SALT, x[ok]), digest::digest, "", algo = "sha256", serialize = FALSE), 1, 16)
  out
}

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
cat("Reading the ADVANCE workbooks...\n")
B <- list(w18 = read_book(XL_W18, 1:8), w910 = read_book(XL_W910, 9:10))

# ---- student x wave panel ----------------------------------------------------------
panel <- rbindlist(lapply(1:10, function(w) {
  d <- if (w <= 8) B$w18 else B$w910
  col <- function(v) { x <- d[[sprintf("w%d_%s", w, v)]]; if (is.null(x)) rep(NA_real_, nrow(d)) else as.numeric(x) }
  sid <- col("schoolid")
  if (w == 1) sid <- c(`1` = 101, `2` = 104, `3` = 102, `4` = 103, `5` = 105)[as.character(sid)]   # W1 school codes
  out <- data.table(record_id = d$record_id, wave = w, schoolid = as.numeric(sid),
                    use6 = col("past_6mo_use_3"),
                    use30 = if (w %in% 7:8) as.numeric(col("past30_sub_3") > 0) else col("past30_sub_3_any"),
                    pfu = col("friends_use_ecig"), gender = col("dem_gender"), sexuality = col("dem_sexuality"),
                    race = col("race"), eth = col("eth"), paredu = col("dem_high_par_edu"),
                    mdd = col("rcads_mdd_mean"), sni1 = col("sni_1"))
  for (k in 1:9)  out[[paste0("smp", k)]] <- col(paste0("sm_post_", k))      # vaping content seen, platform k
  for (k in 1:15) out[[paste0("smu", k)]] <- col(paste0("socialmed_", k))    # general use, platform k
  out[!is.na(schoolid)]
}))

# ---- nominations (W2-W10; W1 friend cells are internal REDCap codes) ----------------------
noms <- rbindlist(lapply(2:10, function(w) {
  d <- if (w <= 8) B$w18 else B$w910
  cols <- grep(sprintf("^w%d_friend[1-7]_(other_)?[0-9]{3}$", w), names(d), value = TRUE)
  rbindlist(lapply(cols, function(cl) {
    v <- d[[cl]]; ok <- !is.na(v) & !(trimws(v) %in% EMPTY)
    if (!any(ok)) return(NULL)
    typed <- grepl("_other_", cl)
    data.table(wave = w, ego = d$record_id[ok], col_school = as.integer(sub(".*_", "", cl)),
               kind = if (typed) "typed" else "roster",
               alter = if (typed) paste0("T:", hash_names(v[ok])) else trimws(v[ok]))
  }))
}))
noms <- noms[!(kind == "typed" & alter == "T:NA")]

saveRDS(list(panel = panel, noms = noms), out_file)
cat(sprintf("Saved %s\n  panel: %d student-waves (%d students)\n  noms: %d nominations (%d typed, hashed)\n",
            out_file, nrow(panel), uniqueN(panel$record_id), nrow(noms), sum(noms$kind == "typed")))
