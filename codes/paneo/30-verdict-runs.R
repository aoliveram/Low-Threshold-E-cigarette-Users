# 30-verdict-runs.R — robustness of the two candidate specifications for the verdict:
#   S*  = the only defensible significant spec (A4, U1, T3, N4, I35, Z3, S3 lagged): R = 1.72, p = .047
#   R0  = the proposed reference (A3, U1, T3, N4, I7, Z3, S3 lagged)
# Jobs: one-decision-at-a-time around S*; leave-one-school-out and leave-one-semester-out for both;
# imputation seeds for the multiple-imputation versions.
source("paneo/12-engine.R")
dir.create(file.path(OUT, "verdict", "specs"), recursive = TRUE, showWarnings = FALSE)
SSTAR <- list(A = "A4", I = "I35"); R0 <- list()
VV <- list(
  I = list(I0 = list(I = "I0"), I3 = list(I = "I3"), I6 = list(I = "I6"), I4_f1.5 = list(I = "I4", f = 1.5),
           I7_s1 = list(I = "I7", seed = 1L), I7_s2 = list(I = "I7", seed = 2L), I7_s3 = list(I = "I7", seed = 3L),
           I7_s4 = list(I = "I7", seed = 4L), I7_s5 = list(I = "I7", seed = 5L)),
  A = list(A3 = list(A = "A3"), A5 = list(A = "A5")),
  U = list(U2 = list(U = "U2"), U3 = list(U = "U3"), U4 = list(U = "U4")),
  T = list(T1 = list(T = "T1"), T2 = list(T = "T2")),
  N = list(N1 = list(N = "N1"), N2 = list(N = "N2"), N3 = list(N = "N3")),
  Z = list(Z1 = list(Z = "Z1"), Z2 = list(Z = "Z2")),
  S = c(setNames(lapply(c("S1", "S2", "S4", "S5", "S6", "S7", "S8"), function(x) list(S = x, Stime = "lag")), paste0(c("S1", "S2", "S4", "S5", "S6", "S7", "S8"), "_lag")),
        list(S3_cur = list(S = "S3", Stime = "cur"))),
  X = list(X1 = list(X = "X1"), X3 = list(X = "X3"), X4 = list(X = "X4")),
  F = list(F1 = list(F = "F1")), CL = list(CL_vid = list(CL = "vid")),
  sample = list(C1 = list(sample = "C1"), C2 = list(sample = "C2"), no_cap = list(sample = "no_cap")),
  typed = list(W0 = list(typed = "W0"), W1 = list(typed = "W1"), W2 = list(typed = "W2")))
reg <- rbindlist(lapply(names(VV), function(d) data.table(base = "Sstar", dim = d, option = names(VV[[d]]))))
reg <- rbind(data.table(base = "Sstar", dim = "base", option = "base"), reg)
phys <- sort(unique(physical_school(SCHOOLS)))
for (b in c("Sstar", "R0")) {
  reg <- rbind(reg, data.table(base = b, dim = "drop_school", option = as.character(phys)),
               data.table(base = b, dim = "drop_gp", option = as.character(3:8)))
}
reg <- rbind(reg, data.table(base = "R0", dim = "base", option = "base"),
             data.table(base = "R0", dim = "I7_seed", option = paste0("s", 2:5)))
reg[, id := sprintf("V_%03d_%s_%s_%s", .I, base, dim, option)]
spec_of <- function(i) {
  r <- reg[i]; b <- if (r$base == "Sstar") SSTAR else R0
  o <- switch(r$dim, base = list(), drop_school = list(drop_school = as.integer(r$option)), drop_gp = list(drop_gp = as.integer(r$option)),
              I7_seed = list(seed = as.integer(sub("s", "", r$option))), VV[[r$dim]][[r$option]])
  do.call(default_spec, modifyList(b, o))
}
reg[, heavy := sapply(seq_len(.N), function(i) spec_of(i)$I == "I7")]; setorder(reg, -heavy); reg[, heavy := NULL]
msg("Verdict runs: %d jobs on %d cores", nrow(reg), NCORES); t0 <- Sys.time()
invisible(parallel::mclapply(seq_len(nrow(reg)), function(i) {
  f <- file.path(OUT, "verdict", "specs", paste0(reg$id[i], ".rds")); if (file.exists(f)) return(TRUE)
  r <- tryCatch(run_spec(spec_of(i), models = c("M0", "MA", "MB", "MC", "MD")), error = function(e) data.table(error = conditionMessage(e)))
  r[, `:=`(id = reg$id[i], base = reg$base[i], dim = reg$dim[i], option = reg$option[i])]; saveRDS(r, f); TRUE
}, mc.cores = NCORES, mc.preschedule = FALSE))
res <- rbindlist(lapply(list.files(file.path(OUT, "verdict", "specs"), full.names = TRUE), readRDS), fill = TRUE)
fwrite(res, file.path(OUT, "verdict", "verdict_results.csv"))
msg("Verdict runs done: %d jobs, %.1f min", uniqueN(res$id), as.numeric(difftime(Sys.time(), t0, units = "mins")))
if ("error" %in% names(res)) print(res[!is.na(error), .(id, error)])
