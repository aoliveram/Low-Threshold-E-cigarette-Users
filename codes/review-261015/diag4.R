suppressMessages({library(netdiffuseR); library(sandwich); library(lmtest); library(Matrix)})
S <- Sys.getenv("S")
load(file.path(S, "run_yuchan", "all_objects.RData"))
idv <- vertex_ids_clean; sch_of <- as.integer(sub("_.*","",idv)); rid_of <- sub("^[^_]+_","",idv)
resp <- matrix(FALSE, length(idv), length(all_gps))
for (k in seq_along(all_gps)) for (s in unique(sch_of)) { ii <- which(sch_of==s); w <- if (s %in% c2_schools) all_gps[k]+2 else all_gps[k]
  d <- if (w<=8) d_w18 else d_w910; sc <- suppressWarnings(as.integer(d[[paste0("w",w,"_schoolid")]][match(rid_of[ii], d$record_id)])); resp[ii,k] <- !is.na(sc) & sc==s }
ne <- 0; alt_nr <- 0; ego_nr <- 0
for (k in seq_along(all_gps)) { sm <- summary(as(diffnet_all$graph[[k]], "dgCMatrix")); ne <- ne + nrow(sm); alt_nr <- alt_nr + sum(!resp[sm$j,k]); ego_nr <- ego_nr + sum(!resp[sm$i,k]) }
cat(sprintf("edges=%d; alter did not respond that wave: %d (%.1f%%); ego did not respond that wave: %d\n", ne, alt_nr, 100*alt_nr/ne, ego_nr))
# physical-school FE sensitivity for the four primary models
phys <- function(x) ifelse(x==201,101L, ifelse(x==212,112L, ifelse(x==213,113L, ifelse(x==214,114L, as.integer(x)))))
for (nm in names(focus_models)) { o <- focus_models[[nm]]; d <- o$analytic_data; d$school_phys <- phys(d$school)
  v <- if (grepl("instagram", nm)) "insta_rev_lag" else "sm_post_avg_rev_lag"
  f <- update(formula(o$model), . ~ . - factor(school_c1fe) + factor(school_phys))
  m <- glm(f, data=d, family=binomial); ct <- coeftest(m, vcov.=vcovCL(m, cluster=d$id))
  c0 <- coeftest(o$model, vcov.=vcovCL(o$model, cluster=d$id))
  cat(sprintf("%-26s SM OR original=%.3f (p=%.3f) | physical-school FE=%.3f (p=%.3f) | cohortC2 %.2f -> %.2f\n", nm, exp(c0[v,1]), c0[v,4], exp(ct[v,1]), ct[v,4], exp(c0["cohortC2",1]), exp(ct["cohortC2",1])))
}
