# 42-loso-table.R — S* leave-one-school-out with the characteristics of each school removed.
source("paneo/12-engine.R")
D <- build_data(default_spec(A = "A4", I = "I35"))[[1]]
type_of <- function(s) ifelse(s %in% c(103, 105, 112, 113), "Asian-majority", ifelse(s %in% c(102, 106, 107, 114), "Hispanic-majority", "Other"))
ids <- c(`101` = "101 + 201", `112` = "112 + 212", `113` = "113 + 213", `114` = "114 + 214")
ch <- D[, .(schools = ifelse(as.character(school_phys[1]) %in% names(ids), ids[as.character(school_phys[1])], as.character(school_phys[1])),
            type = type_of(school_phys[1]), cohorts = paste(sort(unique(cohort)), collapse = "+"),
            students = uniqueN(record_id), person_periods = .N, events = sum(y),
            pct_female = round(100 * mean(female, na.rm = TRUE)), pct_hispanic = round(100 * mean(hispanic, na.rm = TRUE)),
            pct_asian = round(100 * mean(asian, na.rm = TRUE)), mean_SM = round(mean(sm), 2), mean_E = round(mean(E), 3),
            pct_named_nobody = round(100 * mean(iso))), by = school_phys]
V <- fread(file.path(OUT, "verdict", "verdict_results.csv"))[base == "Sstar" & dim == "drop_school" & model == "MB" & quantity == "R"]
V[, school_phys := as.integer(option)]
T <- merge(ch, V[, .(school_phys, R = round(est, 2), R_lo = round(est_lo, 2), R_hi = round(est_hi, 2), p = round(p, 3), n_without = n, events_without = events)], by = "school_phys")
setorder(T, p); fwrite(T, file.path(OUT, "sstar", "loso_table.csv")); options(width = 250); print(T, row.names = FALSE)
msg("all schools: %d students, %d person-periods, %d events", uniqueN(D$record_id), nrow(D), sum(D$y))
