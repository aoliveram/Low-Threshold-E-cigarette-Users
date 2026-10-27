suppressMessages({library(netdiffuseR); library(sandwich); library(lmtest)})
S <- Sys.getenv("S")
load(file.path(S, "run_yuchan", "all_objects.RData"))
cat("\n### 1. sexuality codes (baseline waves) ###\n")
for (w in c(3,5)) { v <- paste0("w",w,"_dem_sexuality"); print(table(d_w18[[v]], useNA="ifany")) }
cat("cov_df sex_min table:\n"); print(table(cov_df$sex_min, useNA="ifany"))
cat("\n### 2. demographics from baseline wave only ###\n")
cat("cov_df rows:", nrow(cov_df), "\n")
# students absent at their school's baseline wave
base_missing <- sapply(seq_len(nrow(cov_df)), function(i) {
  id <- cov_df$id[i]; sch <- as.integer(sub("_.*","",id)); rid <- sub("^[^_]+_","",id)
  w <- school_waves(sch)[1]; d <- if (w<=8) d_w18 else d_w910
  j <- match(rid, d$record_id); is.na(j) || is.na(d[[paste0("w",w,"_schoolid")]][j]) || is.na(d[[paste0("w",w,"_dem_gender")]][j])
})
cat("students with no gender at baseline wave:", sum(base_missing), sprintf("(%.1f%%)\n", 100*mean(base_missing)))
cat("  of them coded female=0:", sum(cov_df$female[base_missing]==0), "; hispanic=0:", sum(cov_df$hispanic[base_missing]==0), "; sex_min NA:", sum(is.na(cov_df$sex_min[base_missing])), "\n")
# first non-missing gender across all waves for those
fnm <- function(id, stem) { sch <- as.integer(sub("_.*","",id)); rid <- sub("^[^_]+_","",id)
  for (w in school_waves(sch)) { d <- if (w<=8) d_w18 else d_w910; v <- paste0("w",w,"_",stem)
    if (!v %in% names(d)) next; x <- suppressWarnings(as.numeric(d[[v]][match(rid,d$record_id)])); if (!is.na(x)) return(x) }
  NA_real_ }
g2 <- sapply(cov_df$id[base_missing], fnm, stem="dem_gender")
cat("  ...first non-missing gender available later:", sum(!is.na(g2)), "; of which female (0):", sum(g2==0, na.rm=TRUE), "\n")

cat("\n### 3. analytic samples of the four primary (lagged) models ###\n")
for (nm in names(focus_models)) {
  d <- focus_models[[nm]]$analytic_data
  ev <- d[d$outcome==1,]
  cat(sprintf("%-26s N=%d events=%d | GP of events: %s\n", nm, nrow(d), nrow(ev), paste(names(table(ev$grade_period)), table(ev$grade_period), sep=":", collapse=" ")))
}
d <- tom_lagnet_lagSM_noexp_allSM_res$analytic_data
d <- merge(d, .gp_degree, by=c("id","grade_period"), all.x=TRUE)
ev <- d[d$outcome==1,]
cat("\nNo-exposure events (lagged overall SM):", nrow(ev), "\n")
cat("  out-degree at t-1 == 0 among no-exposure events:", sum(ev$matched_degree_previous_GP==0), sprintf("(%.1f%%)\n", 100*mean(ev$matched_degree_previous_GP==0)))
cat("  current out_degree == 0 among no-exposure events:", sum(ev$out_degree==0), "\n")
cat("  distribution of t-1 degree among no-exposure events:\n"); print(table(ev$matched_degree_previous_GP))

cat("\n### 4. Non-focal adopters coded as 0 in event-type models ###\n")
d2 <- tom_lagnet_lagSM_lowthr_allSM_res$analytic_data
atoa <- !is.na(d2$toa) & d2$grade_period==d2$toa
cat("rows at TOA in low-threshold data:", sum(atoa), " of which outcome=1:", sum(d2$outcome[atoa]==1), " outcome=0 (other adopters as controls):", sum(d2$outcome[atoa]==0), "\n")
