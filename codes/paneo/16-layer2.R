# 16-layer2.R — Layer 2: change one decision at a time, from the reference spec (R0) and from the
# paper's primary spec (H1, Table 2). Runs on NCORES cores; one RDS per spec in out/layer2/specs/.
source("paneo/12-engine.R")
dir.create(file.path(OUT, "layer2", "specs"), recursive = TRUE, showWarnings = FALSE)

BASE <- list(
  R0 = list(),
  H1 = list(A = "A1", U = "U1", N = "N1", I = "I0", Z = "Z1", S = "S1", X = "X1", F = "F1", CL = "vid", paperlag = TRUE, T = "T3", fix = TRUE))
VARS <- list(
  A = lapply(c(A1 = "A1", A2 = "A2", A3 = "A3", A4 = "A4", A5 = "A5"), function(v) list(A = v)),
  U = lapply(c(U1 = "U1", U2 = "U2", U3 = "U3", U4 = "U4"), function(v) list(U = v)),
  T = lapply(c(T1 = "T1", T2 = "T2", T3 = "T3"), function(v) list(T = v)),
  N = lapply(c(N1 = "N1", N2 = "N2", N3 = "N3", N4 = "N4"), function(v) list(N = v)),
  I = c(lapply(c(I0 = "I0", I1 = "I1", I3 = "I3", I35 = "I35", I6 = "I6", I7 = "I7"), function(v) list(I = v)),
        list(I2_q0.1 = list(I = "I2", q = 0.1), I2_q0.3 = list(I = "I2", q = 0.3), I2_q0.5 = list(I = "I2", q = 0.5),
             I4_f1.5 = list(I = "I4", f = 1.5), I4_f2 = list(I = "I4", f = 2))),
  Z = lapply(c(Z1 = "Z1", Z2 = "Z2", Z3 = "Z3"), function(v) list(Z = v)),
  S = unlist(lapply(paste0("S", 1:8), function(s) setNames(list(list(S = s, Stime = "lag"), list(S = s, Stime = "cur")),
                                                       paste0(s, c("_lag", "_cur")))), recursive = FALSE),
  X = lapply(c(X1 = "X1", X2 = "X2", X3 = "X3", X4 = "X4"), function(v) list(X = v)),
  F = lapply(c(F1 = "F1", F2 = "F2"), function(v) list(F = v)),
  CL = lapply(c(CL_vid = "vid", CL_record = "record"), function(v) list(CL = v)),
  sample = lapply(c(all = "all", C1 = "C1", C2 = "C2", no_cap = "no_cap"), function(v) list(sample = v)),
  typed = lapply(c(W0 = "W0", W1 = "W1", W2 = "W2", W3 = "W3"), function(v) list(typed = v)))

reg <- rbindlist(lapply(names(BASE), function(b) rbindlist(lapply(names(VARS), function(d) {
  if (b == "H1" && d %in% c("I", "typed")) return(NULL)        # no unseen friends in the paper's network
  rbindlist(lapply(names(VARS[[d]]), function(o) data.table(base = b, dim = d, option = o)))
}))))
reg[, id := sprintf("L2_%03d_%s_%s_%s", .I, base, dim, option)]
spec_of <- function(i) do.call(default_spec, modifyList(BASE[[reg$base[i]]], VARS[[reg$dim[i]]][[reg$option[i]]]))

label_spec <- function(sp) {
  inc <- sp$A == "A1" || sp$T == "T1" || (sp$N %in% c("N3", "N4") && sp$I == "I0") || sp$Z == "Z1" || sp$X == "X1" || sp$F == "F1"
  disc <- sp$N %in% c("N1", "N2") || sp$S == "S1" || sp$Z == "Z2" || sp$Stime == "cur"
  if (inc) "I" else if (disc) "Disc" else "D"
}

# heavier specs (imputation draws) first, for load balancing
reg[, heavy := sapply(seq_len(.N), function(i) spec_of(i)$I == "I7")]
setorder(reg, -heavy); reg[, heavy := NULL]
msg("Layer 2: %d specs on %d cores", nrow(reg), NCORES)
t0 <- Sys.time()
done <- parallel::mclapply(seq_len(nrow(reg)), function(i) {
  f <- file.path(OUT, "layer2", "specs", paste0(reg$id[i], ".rds"))
  if (file.exists(f)) return(TRUE)
  sp <- spec_of(i)
  r <- tryCatch(run_spec(sp), error = function(e) data.table(error = conditionMessage(e)))
  r[, `:=`(id = reg$id[i], base = reg$base[i], dim = reg$dim[i], option = reg$option[i], label = label_spec(sp))]
  saveRDS(r, f); TRUE
}, mc.cores = NCORES, mc.preschedule = FALSE)
res <- rbindlist(lapply(list.files(file.path(OUT, "layer2", "specs"), full.names = TRUE), readRDS), fill = TRUE)
fwrite(res, file.path(OUT, "layer2", "layer2_results.csv"))
msg("Layer 2 done: %d specs, %d result rows, %.1f min", uniqueN(res$id), nrow(res), as.numeric(difftime(Sys.time(), t0, units = "mins")))
if ("error" %in% names(res)) print(res[!is.na(error), .(id, error)])
