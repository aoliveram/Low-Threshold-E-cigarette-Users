# 20-stage2.R — second stage of the sweep, one job registry on NCORES cores:
#   cuts : the Layer 3 grid with count and percentile cut families (models MA, MC, MD)
#   U    : the Layer 3 grid with U2 and U4, plus U5 (perceived friends' use) (all models)
#   qgrid: unseen friends' use fixed at q = 0, 0.05, ..., 1 on the defensible bases (all models)
source("paneo/12-engine.R")
dir.create(file.path(OUT, "stage2", "specs"), recursive = TRUE, showWarnings = FALSE)

NI <- list(N1_I0 = list(N = "N1", I = "I0"), N4_I0 = list(N = "N4", I = "I0"),
           N4_I35 = list(N = "N4", I = "I35"), N4_I7 = list(N = "N4", I = "I7"))
SM <- list(S1_lag = list(S = "S1", Stime = "lag"), S3_lag = list(S = "S3", Stime = "lag"), S1_cur = list(S = "S1", Stime = "cur"))
CUTSETS <- list(count = c(0, 1, 2), pct_adopt = c(.5, .75, .9), pct_risk = c(.5, .75, .9))

g <- CJ(A = c("A1", "A3", "A4"), U = c("U1", "U3"), T = c("T1", "T3"), NI = names(NI), Z = c("Z1", "Z3"), SM = names(SM))
cuts <- g[, .(stage = "cuts", A, U, T, NI, Z, SM, q = NA_real_)]
gU <- CJ(A = c("A1", "A3", "A4"), U = c("U2", "U4"), T = c("T1", "T3"), NI = names(NI), Z = c("Z1", "Z3"), SM = names(SM))
u5 <- CJ(A = c("A1", "A3", "A4"), U = "U5", T = c("T1", "T3"), NI = "none", Z = "none", SM = names(SM))
Us <- rbind(gU, u5)[, .(stage = "U", A, U, T, NI, Z, SM, q = NA_real_)]
qg <- CJ(A = c("A3", "A4"), U = c("U1", "U3"), q = seq(0, 1, by = 0.05))[, .(stage = "qgrid", A, U, T = "T3", NI = "N4_I2", Z = "Z3", SM = "S3_lag", q)]
reg <- rbind(cuts, Us, qg)
reg[, id := sprintf("S2_%04d_%s_%s_%s_%s_%s_%s_%s%s", .I, stage, A, U, T, NI, Z, SM, ifelse(is.na(q), "", sprintf("_q%.2f", q)))]

spec_of <- function(i) {
  r <- reg[i]
  base <- list(A = r$A, U = r$U, T = r$T)
  if (r$NI %in% names(NI)) base <- c(base, NI[[r$NI]])
  if (r$NI == "N4_I2") base <- c(base, list(N = "N4", I = "I2", q = r$q))
  if (r$Z %in% c("Z1", "Z3")) base$Z <- r$Z
  base <- c(base, SM[[r$SM]])
  if (r$stage == "cuts") base$cutsets <- CUTSETS
  do.call(default_spec, base)
}
models_of <- function(i) {
  r <- reg[i]
  if (r$stage == "cuts") return(c("MA", "MC", "MD"))
  if (r$U == "U5") return(c("M0", "MA", "MB", "MC", "MD", "ME"))
  c("M0", "MA", "MB", "MC", "MD", "ME", "MF")
}
label_spec <- function(sp) {
  inc <- sp$A == "A1" || sp$T == "T1" || (sp$U != "U5" && sp$N %in% c("N3", "N4") && sp$I == "I0") ||
         (sp$U != "U5" && sp$Z == "Z1") || sp$X == "X1" || sp$F == "F1"
  disc <- (sp$U != "U5" && sp$N %in% c("N1", "N2")) || sp$S == "S1" || sp$Z == "Z2" || sp$Stime == "cur" || sp$U == "U5"
  if (inc) "I" else if (disc) "Disc" else "D"
}
reg[, heavy := sapply(seq_len(.N), function(i) { sp <- spec_of(i); sp$I == "I7" && sp$U != "U5" })]
setorder(reg, -heavy); reg[, heavy := NULL]
msg("Stage 2: %d jobs (cuts %d, U %d, qgrid %d) on %d cores", nrow(reg), sum(reg$stage == "cuts"), sum(reg$stage == "U"), sum(reg$stage == "qgrid"), NCORES)
t0 <- Sys.time()
invisible(parallel::mclapply(seq_len(nrow(reg)), function(i) {
  f <- file.path(OUT, "stage2", "specs", paste0(reg$id[i], ".rds"))
  if (file.exists(f)) return(TRUE)
  sp <- spec_of(i)
  r <- tryCatch(run_spec(sp, models = models_of(i)), error = function(e) data.table(error = conditionMessage(e)))
  r[, `:=`(id = reg$id[i], stage = reg$stage[i], A = reg$A[i], U = reg$U[i], T = reg$T[i], NI = reg$NI[i],
           Z = reg$Z[i], SM = reg$SM[i], q = reg$q[i], label = label_spec(sp))]
  saveRDS(r, f); TRUE
}, mc.cores = NCORES, mc.preschedule = FALSE))
res <- rbindlist(lapply(list.files(file.path(OUT, "stage2", "specs"), full.names = TRUE), readRDS), fill = TRUE)
fwrite(res, file.path(OUT, "stage2", "stage2_results.csv"))
msg("Stage 2 done: %d jobs, %d rows, %.1f min", uniqueN(res$id), nrow(res), as.numeric(difftime(Sys.time(), t0, units = "mins")))
if ("error" %in% names(res)) print(res[!is.na(error), .(id, error)])
