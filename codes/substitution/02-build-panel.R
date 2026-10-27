# 02-build-panel.R — student x wave panel (W4–W10) for the social-media substitution analysis.
suppressMessages(library(readxl))
DATA <- "../Cleaned_Data"
XL <- c(w18 = file.path(DATA, "ADVANCE_W1-W8.xlsx"),
        w910 = file.path(DATA, "ADVANCE_W9-W10.xlsx"))
OUT <- "../outputs_substitution"; dir.create(OUT, showWarnings = FALSE)
WAVES <- 4:10
read_cols <- function(f, pattern) {
  nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
  ct <- rep("skip", length(nm)); ct[grepl(pattern, nm)] <- "numeric"; ct[nm == "record_id"] <- "text"
  d <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal"); names(d) <- tolower(names(d)); d
}
pat <- "^w([1-9]|10)_(past_6mo_use_3|socialmed_[0-9]+|sm_post_[0-9]+)$"
D <- list(w18 = read_cols(XL[["w18"]], pat), w910 = read_cols(XL[["w910"]], pat))
items <- function(d, w, stem) sub(sprintf("^w%d_%s_", w, stem), "", grep(sprintf("^w%d_%s_[0-9]+$", w, stem), names(d), value = TRUE))
cat("socialmed items by wave:\n"); for (w in WAVES) { d <- if (w <= 8) D$w18 else D$w910; cat(" W", w, ": ", paste(items(d, w, "socialmed"), collapse = " "), "\n", sep = "") }
CORE <- Reduce(intersect, lapply(WAVES, function(w) items(if (w <= 8) D$w18 else D$w910, w, "socialmed")))
CORE <- setdiff(CORE, "14")                                   # 14 = "another social media site"
cat("core platforms (present W4–W10):", paste(CORE, collapse = " "), "\n")
vals <- unlist(lapply(WAVES, function(w) { d <- if (w <= 8) D$w18 else D$w910; unlist(d[sprintf("w%d_socialmed_%s", w, CORE)]) }))
cat("socialmed raw values:\n"); print(table(vals, useNA = "ifany"))

rows <- list()
for (w in WAVES) {
  d <- if (w <= 8) D$w18 else D$w910
  sm <- as.matrix(d[sprintf("w%d_socialmed_%s", w, CORE)]); sm[!(sm %in% 0:5)] <- NA        # 9 = don't know
  sm_daily <- ifelse(rowSums(!is.na(sm)) == 0, NA, rowSums(sm >= 3, na.rm = TRUE))            # daily or more
  vp_cols <- grep(sprintf("^w%d_sm_post_[0-9]+$", w), names(d), value = TRUE)
  if (length(vp_cols)) { vp <- as.matrix(d[vp_cols]); vp[!(vp %in% 1:5)] <- NA
    vape_mean <- ifelse(rowSums(!is.na(vp)) == 0, NA, 6 - rowMeans(vp, na.rm = TRUE))         # paper's measure (1 never .. 5 several/day)
    vape_weekly <- ifelse(rowSums(!is.na(vp)) == 0, NA, as.integer(rowSums(vp <= 3, na.rm = TRUE) > 0))
  } else { vape_mean <- vape_weekly <- rep(NA_real_, nrow(d)) }
  rows[[length(rows) + 1]] <- data.frame(record_id = d$record_id, wave = w, sm_daily, vape_mean, vape_weekly)
}
P <- do.call(rbind, rows)
# own e-cig use ever reported up to each wave (W1..w), for the never-user restriction
use <- do.call(rbind, lapply(1:10, function(w) { d <- if (w <= 8) D$w18 else D$w910; v <- sprintf("w%d_past_6mo_use_3", w)
  if (!v %in% names(d)) return(NULL); data.frame(record_id = d$record_id, wave = w, use = d[[v]]) }))
use <- use[order(use$record_id, use$wave), ]
use$ever <- ave(ifelse(is.na(use$use), 0, use$use == 1), use$record_id, FUN = cummax)
nom <- readRDS(file.path(OUT, "nominations_W4-W10.rds"))
P <- merge(nom, P, by = c("record_id", "wave"), all.x = TRUE)
P <- merge(P, use[, c("record_id", "wave", "ever")], by = c("record_id", "wave"), all.x = TRUE)
P$incomplete_zero <- P$named == 0 & !P$sm_answered
P$cohort <- ifelse(P$school >= 200, "C2", "C1")
saveRDS(P, file.path(OUT, "panel_W4-W10.rds"))
cat("\npanel rows:", nrow(P), "| incomplete zeros dropped later:", sum(P$incomplete_zero), "\n")
cat("sm_daily distribution (valid rows):\n"); print(round(100 * prop.table(table(P$sm_daily[!P$incomplete_zero])), 1))
cat("quantiles sm_daily (type 1):\n"); print(quantile(P$sm_daily[!P$incomplete_zero], c(1/3, 2/3, .25, .5, .75), type = 1, na.rm = TRUE))
cat("vape_mean non-NA (W4–W8):", sum(!is.na(P$vape_mean)), "; vape_weekly share:", round(mean(P$vape_weekly, na.rm = TRUE), 3), "\n")
cat("cor(named, sm_daily) pooled:", round(cor(P$named, P$sm_daily, use = "complete.obs"), 3), "\n")
