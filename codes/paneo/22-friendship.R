# 22-friendship.R — friendship substitution (M-G) with the sweep's friend definitions.
source("paneo/00-config.R")
suppressMessages(library(fixest)); setFixest_nthreads(1)
FR <- readRDS(file.path(DATA, "frame.rds")); EN <- FR$EN; SMV <- FR$SMV_RAW; ST <- FR$ST
L <- readRDS(file.path(DATA, "panel_long.rds"))
P <- L[wave %in% 4:10 & schoolid %in% SCHOOLS, .(record_id, wave, school = schoolid)]
cnt <- EN[wave %in% 4:10, .(all = .N, roster = sum(kind == "roster"), visible = sum(type == "visible")), by = .(record_id = ego, school, wave)]
P <- merge(P, cnt, by = c("record_id", "school", "wave"), all.x = TRUE)
for (v in c("all", "roster", "visible")) set(P, which(is.na(P[[v]])), v, 0L)
P <- merge(P, SMV[, .(record_id, wave, smuse, S1, S3)], by = c("record_id", "wave"), all.x = TRUE)
P <- merge(P, ST[, .(record_id, wave, ever_all)], by = c("record_id", "wave"), all.x = TRUE)
P <- P[!(all == 0 & is.na(smuse))]                       # incomplete surveys
P[, cohort := ifelse(is_c2(school), "C2", "C1")]
setorder(P, record_id, wave)
res <- list()
for (smp in c("All students", "Never-users")) for (fd in c("all", "roster", "visible")) for (y in c("smuse", "S1", "S3")) {
  d <- if (smp == "Never-users") P[ever_all == 0] else P
  d <- copy(d); d[, fr := get(fd)]
  f <- feols(as.formula(paste(y, "~ fr | record_id + wave^cohort")), d, cluster = ~record_id, notes = FALSE)
  res[[length(res) + 1]] <- data.table(sample = smp, friends = fd, outcome = y, test = "within FE, per friend",
                                       est = coef(f)[["fr"]], se = se(f)[["fr"]], n = nobs(f))
  d[, `:=`(fr_prev = shift(fr), y_prev = shift(get(y)), w_prev = shift(wave)), by = record_id]
  d <- d[w_prev == wave - 1]
  d[, `:=`(dy = get(y) - y_prev, gain = pmax(fr - fr_prev, 0), loss = pmax(fr_prev - fr, 0))]
  f2 <- feols(dy ~ gain + loss | wave^cohort, d, cluster = ~record_id, notes = FALSE)
  for (tm in c("gain", "loss")) res[[length(res) + 1]] <- data.table(sample = smp, friends = fd, outcome = y,
      test = paste("first differences, per friend", ifelse(tm == "gain", "gained", "lost")), est = coef(f2)[[tm]], se = se(f2)[[tm]], n = nobs(f2))
}
R <- rbindlist(res)[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))]
fwrite(R, file.path(OUT, "stage2", "friendship_substitution.csv"))
print(R[, .(sample, friends, outcome, test, est = round(est, 3), lo = round(lo, 3), hi = round(hi, 3), p = signif(p, 2), n)], row.names = FALSE)
