# 17-layer3.R — Layer 3: bounded multiverse (full product over six decisions; everything else at the
# reference). Runs on NCORES cores; one RDS per spec in out/layer3/specs/.
source("paneo/12-engine.R")
dir.create(file.path(OUT, "layer3", "specs"), recursive = TRUE, showWarnings = FALSE)

NI <- list(N1_I0 = list(N = "N1", I = "I0"), N4_I0 = list(N = "N4", I = "I0"),
           N4_I35 = list(N = "N4", I = "I35"), N4_I7 = list(N = "N4", I = "I7"))
SM <- list(S1_lag = list(S = "S1", Stime = "lag"), S3_lag = list(S = "S3", Stime = "lag"), S1_cur = list(S = "S1", Stime = "cur"))
reg <- CJ(A = c("A1", "A3", "A4"), U = c("U1", "U3"), T = c("T1", "T3"), NI = names(NI), Z = c("Z1", "Z3"), SM = names(SM))
reg[, id := sprintf("L3_%03d_%s_%s_%s_%s_%s_%s", .I, A, U, T, NI, Z, SM)]
spec_of <- function(i) do.call(default_spec, c(list(A = reg$A[i], U = reg$U[i], T = reg$T[i], Z = reg$Z[i]), NI[[reg$NI[i]]], SM[[reg$SM[i]]]))
label_spec <- function(sp) {
  inc <- sp$A == "A1" || sp$T == "T1" || (sp$N %in% c("N3", "N4") && sp$I == "I0") || sp$Z == "Z1" || sp$X == "X1" || sp$F == "F1"
  disc <- sp$N %in% c("N1", "N2") || sp$S == "S1" || sp$Z == "Z2" || sp$Stime == "cur"
  if (inc) "I" else if (disc) "Disc" else "D"
}
reg[, heavy := NI == "N4_I7"]; setorder(reg, -heavy); reg[, heavy := NULL]
msg("Layer 3: %d specs on %d cores", nrow(reg), NCORES)
t0 <- Sys.time()
done <- parallel::mclapply(seq_len(nrow(reg)), function(i) {
  f <- file.path(OUT, "layer3", "specs", paste0(reg$id[i], ".rds"))
  if (file.exists(f)) return(TRUE)
  sp <- spec_of(i)
  r <- tryCatch(run_spec(sp), error = function(e) data.table(error = conditionMessage(e)))
  r[, `:=`(id = reg$id[i], A = reg$A[i], U = reg$U[i], T = reg$T[i], NI = reg$NI[i], Z = reg$Z[i], SM = reg$SM[i], label = label_spec(sp))]
  saveRDS(r, f); TRUE
}, mc.cores = NCORES, mc.preschedule = FALSE)
res <- rbindlist(lapply(list.files(file.path(OUT, "layer3", "specs"), full.names = TRUE), readRDS), fill = TRUE)
fwrite(res, file.path(OUT, "layer3", "layer3_results.csv"))
msg("Layer 3 done: %d specs, %d result rows, %.1f min", uniqueN(res$id), nrow(res), as.numeric(difftime(Sys.time(), t0, units = "mins")))
if ("error" %in% names(res)) print(res[!is.na(error), .(id, error)])
