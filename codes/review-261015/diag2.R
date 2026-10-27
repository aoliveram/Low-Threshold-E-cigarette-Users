suppressMessages({library(netdiffuseR); library(sandwich); library(lmtest); library(Matrix)})
S <- Sys.getenv("S")
load(file.path(S, "run_yuchan", "all_objects.RData"))
idv <- vertex_ids_clean
sch_of <- as.integer(sub("_.*","",idv)); rid_of <- sub("^[^_]+_","",idv)
getv <- function(w, stem) { d <- if (w<=8) d_w18 else d_w910; v <- paste0("w",w,"_",stem)
  if (!v %in% names(d)) return(rep(NA_real_, length(idv)))
  suppressWarnings(as.numeric(d[[v]][match(rid_of, d$record_id)])) }
cat("\n### sex_min=1 because of code 10 ###\n")
bw <- sapply(sch_of, function(s) school_waves(s)[1])
sexb <- sapply(seq_along(idv), function(i) { d <- if (bw[i]<=8) d_w18 else d_w910; suppressWarnings(as.numeric(d[[paste0("w",bw[i],"_dem_sexuality")]][match(rid_of[i], d$record_id)])) })
cat("students coded sex_min=1:", sum(sexb!=1, na.rm=TRUE), " of which code 10 (prefer not to say):", sum(sexb==10, na.rm=TRUE), "\n")

cat("\n### social-media items per wave ###\n")
for (w in 4:10) { d <- if (w<=8) d_w18 else d_w910; cat("W",w,": ", paste(grep(paste0("^w",w,"_sm_post_[0-9]+$"), names(d), value=TRUE), collapse=" "), "\n", sep="") }
x <- unlist(lapply(4:8, function(w) unlist(d_w18[grep(paste0("^w",w,"_sm_post_[0-9]+$"), names(d_w18))])))
cat("raw value distribution of sm_post items (W4-W8):\n"); print(table(x, useNA="ifany"))

cat("\n### prior use before the analysis window (GP3) ###\n")
pre_waves <- function(s) { if (s %in% c1_early_schools) return(1:2); if (s %in% c1_late_schools) return(2); if (s %in% c2_schools) return(3:4); integer(0) }
prior_use <- sapply(seq_along(idv), function(i) { ws <- pre_waves(sch_of[i]); if (!length(ws)) return(NA)
  u <- sapply(ws, function(w) getv(w,"past_6mo_use_3")[i]); if (all(is.na(u))) return(NA); any(u==1, na.rm=TRUE) })
names(prior_use) <- idv
toa_v <- as.integer(toa_vals); names(toa_v) <- idv
cat("students with use observed before GP3:", sum(prior_use, na.rm=TRUE), "/", sum(!is.na(prior_use)), "with pre-window data\n")
for (nm in names(focus_models)) { d <- focus_models[[nm]]$analytic_data; ev <- d$id[d$outcome==1]
  cat(sprintf("  %-26s events=%d, events with prior (pre-GP3) use=%d; risk-set students with prior use=%d\n", nm, length(ev), sum(prior_use[ev], na.rm=TRUE), sum(prior_use[unique(d$id)], na.rm=TRUE))) }
d <- tom_lagnet_lagSM_noexp_allSM_res$analytic_data
cat("\n### own e-cig status at t-1 among events (lagged overall SM, no-exposure) ###\n")
ev <- d[d$outcome==1,]
st_prev <- mapply(function(id, gp) { i <- match(id, idv); s <- sch_of[i]; w <- if (s %in% c2_schools) gp+2-1 else gp-1; getv(w,"past_6mo_use_3")[i] }, ev$id, ev$grade_period)
print(table(prev_status=st_prev, useNA="ifany"))
ov <- tom_overall_adoption_allSM_res$analytic_data
cat("\n### current (paper V4) models: GP3 events and same-period co-adopters ###\n")
cur <- tom_current_noexp_allSM_res$analytic_data; cl <- tom_current_lowthr_allSM_res$analytic_data
cat("current no-exposure events:", sum(cur$outcome==1), " GP3:", sum(cur$outcome==1 & cur$grade_period==3), "\n")
cat("current low-threshold events:", sum(cl$outcome==1), " GP3:", sum(cl$outcome==1 & cl$grade_period==3), "\n")
cat("overall adoption (current SM) events:", sum(ov$outcome==1), " GP3:", sum(ov$outcome==1 & ov$grade_period==3), "\n")
# strictly-prior exposure with the current-period graph: A_t %*% cumadopt_{t-1}
ca <- diffnet_all$cumadopt
prior_cnt <- matrix(NA_real_, nrow(ca), ncol(ca)); prior_prop <- prior_cnt
for (gi in seq_len(ncol(ca))) { A <- as(diffnet_all$graph[[gi]], "dgCMatrix"); prev <- if (gi>1) ca[,gi-1] else rep(0, nrow(ca))
  num <- as.vector(A %*% prev); den <- Matrix::rowSums(A); prior_cnt[,gi] <- num; prior_prop[,gi] <- ifelse(den>0, num/den, 0) }
at <- cl[cl$grade_period==cl$toa & !is.na(cl$toa),]
ix <- match(at$id, idv); gi <- at$grade_period - 2
pc <- prior_cnt[cbind(ix,gi)]; pp <- prior_prop[cbind(ix,gi)]; cc <- as.numeric(expo[cbind(ix,gi)]); tp <- as.numeric(thr_vals)[ix]
cat("adopters at TOA in current low-thr sample:", nrow(at), "\n")
cat("  no-exposure (netdiffuseR, incl. same-period friends):", sum(cc==0), " | strictly-prior friends:", sum(pc==0), "\n")
cat("  <=25% (netdiffuseR):", sum(tp<=.25), " | strictly-prior friends:", sum(pp<=.25), "\n")
cat("  adopters whose exposure comes ONLY from same-period co-adopters:", sum(cc>0 & pc==0), "\n")

cat("\n### alters counted as 'adopted' who were not current users ###\n")
# observed status matrix by vertex x GP
obs <- matrix(NA_real_, length(idv), length(all_gps))
for (k in seq_along(all_gps)) { gp <- all_gps[k]; for (s in unique(sch_of)) { ii <- which(sch_of==s); w <- if (s %in% c2_schools) gp+2 else gp; obs[ii,k] <- getv(w,"past_6mo_use_3")[ii] } }
tot <- 0; quit <- 0; unk <- 0
for (k in 2:length(all_gps)) { A <- as(diffnet_all$graph[[k-1]], "dgCMatrix"); s <- summary(A); a <- s$j
  adopted <- ca[a, k-1]==1; tot <- tot + sum(adopted); quit <- quit + sum(adopted & obs[a,k-1] %in% 0); unk <- unk + sum(adopted & is.na(obs[a,k-1])) }
cat(sprintf("ties to 'adopted' alters at t-1: %d; alter reported NON-use at t-1: %d (%.1f%%); alter status missing at t-1: %d (%.1f%%)\n", tot, quit, 100*quit/tot, unk, 100*unk/tot))
