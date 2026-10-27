# Corrected breakdown of "no matched friend at t-1": reads W3–W9 (diag5 read only W3–W7).
suppressMessages({library(netdiffuseR); library(readxl)})
S <- Sys.getenv("S"); load(file.path(S, "run_yuchan", "all_objects.RData"))
DATA <- "../Cleaned_Data"
rd <- function(f, pat) { nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
  ct <- rep("skip", length(nm)); ct[grepl(pat, nm) | nm == "record_id"] <- "text"
  x <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal"); names(x) <- tolower(names(x)); x }
X18 <- rd(file.path(DATA, "ADVANCE_W1-W8.xlsx"), "^w[3-8]_(schoolid|friend[1-7]_(other_)?[0-9]{3})$")
X9  <- rd(file.path(DATA, "ADVANCE_W9-W10.xlsx"), "^w9_(schoolid|friend[1-7]_(other_)?[0-9]{3})$")
EMPTY <- c("", "NA", "-999", "-99", "0")
d <- focus_models[["overall_sm_low_threshold"]]$analytic_data
d <- merge(d, .gp_degree, by = c("id", "grade_period")); z <- d[d$matched_degree_previous_GP == 0, ]
z$sch <- as.integer(sub("_.*", "", z$id)); z$rid <- sub("^[^_]+_", "", z$id)
z$w_prev <- ifelse(z$sch %in% c2_schools, z$grade_period + 1L, z$grade_period - 1L)
cat("t-1 waves among these rows:\n"); print(table(z$w_prev))
cls <- character(nrow(z))
for (i in seq_len(nrow(z))) {
  w <- z$w_prev[i]; s <- z$sch[i]; X <- if (w <= 8) X18 else X9; j <- match(z$rid[i], X$record_id)
  sc <- if (is.na(j)) NA else suppressWarnings(as.integer(X[[sprintf("w%d_schoolid", w)]][j]))
  if (is.na(sc) || sc != s) { cls[i] <- "ego recorded in another school at t-1 (moved)"; next }
  r <- unlist(X[j, intersect(sprintf("w%d_friend%d_%d", w, 1:7, s), names(X))]); o <- unlist(X[j, intersect(sprintf("w%d_friend%d_other_%d", w, 1:7, s), names(X))])
  r <- r[!is.na(r) & !(r %in% EMPTY)]; o <- o[!is.na(o) & !(trimws(o) %in% EMPTY)]
  cls[i] <- if (!length(r) && !length(o)) "nominated nobody" else if (!length(r)) "only typed names" else if (!length(o)) "roster friends, none visible" else "roster friends none visible + typed"
}
t <- table(cls, z$outcome); out <- data.frame(group = rownames(t), n = rowSums(t), adoptions = t[, "1"], rate = round(100 * t[, "1"] / rowSums(t), 1))
print(out[order(-out$n), ], row.names = FALSE)
