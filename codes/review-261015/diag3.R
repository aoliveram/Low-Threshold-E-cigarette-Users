suppressMessages({library(netdiffuseR); library(sandwich); library(lmtest)})
S <- Sys.getenv("S")
load(file.path(S, "run_yuchan", "all_objects.RData"))
.d <- focus_models[["overall_sm_low_threshold"]]$analytic_data
.d <- merge(.d, .gp_degree, by=c("id","grade_period"), all.x=TRUE)
.toa <- as.integer(toa_vals); names(.toa) <- vertex_ids_clean
.d$outcome <- as.integer(!is.na(.toa[.d$id]) & .d$grade_period == .toa[.d$id])
.d$peer_prop <- .d$network_threshold_lag
.d$grp <- ifelse(.d$matched_degree_previous_GP==0, "0 sin amigos emparejados", ifelse(.d$peer_prop==0, "1 amigos, ninguno usa", "2 al menos un amigo usa"))
cat("rows/events by group:\n"); print(with(.d, table(grp, outcome)))
cat("raw adoption rate by group and SM tercile:\n")
.d$smq <- cut(.d$sm_post_avg_rev_lag, breaks=c(0,1,2,5), labels=c("SM=1 (nunca)","1<SM<=2","SM>2"), include.lowest=TRUE)
print(round(100*with(.d, tapply(outcome, list(grp, smq), mean)),1))
print(with(.d, table(grp, smq)))
terms <- "factor(grade_period) + cohort + female + hispanic + asian + par_edu + mdd + out_degree + in_degree + sex_min + factor(school_c1fe)"
for (g in c("no_users","some_users")) {
  sub <- if (g=="no_users") .d[.d$peer_prop==0,] else .d[.d$peer_prop>0,]
  f <- glm(as.formula(paste("outcome ~ sm_post_avg_rev_lag +", terms)), data=sub, family=binomial)
  ct <- coeftest(f, vcov.=vcovCL(f, cluster=sub$id))["sm_post_avg_rev_lag",]
  cat(sprintf("%-10s N=%d events=%d  SM OR=%.3f [%.3f, %.3f] p=%.3f\n", g, nrow(sub), sum(sub$outcome), exp(ct[1]), exp(ct[1]-1.96*ct[2]), exp(ct[1]+1.96*ct[2]), ct[4]))
}
