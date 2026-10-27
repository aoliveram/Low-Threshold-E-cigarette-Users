# ============================================================================
# Minimal reproducible example: hazard_rate() fails on as_diffnet()-built objects
# netdiffuseR 1.26.0 (dev).  Error: "attempt to set an attribute on NULL"
# ============================================================================
library(netdiffuseR)
cat("netdiffuseR version:", as.character(packageVersion("netdiffuseR")), "\n\n")

grab <- tempfile(fileext = ".pdf"); pdf(grab)        # discard plots
status_of <- function(dn) if (is.null(dn$status)) "NULL" else
  paste0(class(dn$status)[1], " ", paste(dim(dn$status), collapse = "x"))
test <- function(label, dn) {
  r <- tryCatch({ hazard_rate(dn); "hazard_rate OK" },
                error = function(e) paste0("hazard_rate ERROR: ", conditionMessage(e)))
  cat(sprintf("%-26s | $status = %-12s | %s\n", label, status_of(dn), r))
}

## (1) rdiffnet object -------------------------------------------------------
set.seed(1)
dn_r <- rdiffnet(60, 5, seed.graph = "small-world",
                 rgraph.args = list(k = 4, p = .3))

## (2) as_diffnet object (Yuchan's construction path) ------------------------
mk_as_diffnet <- function(ids, Tper = 4, seed = 1) {
  set.seed(seed); n <- length(ids)
  arr <- array(0L, dim = c(n, n, Tper),
               dimnames = list(ids, ids, as.character(seq_len(Tper))))
  for (t in seq_len(Tper)) {
    m <- matrix(rbinom(n * n, 1, .15), n, n); diag(m) <- 0
    arr[, , t] <- m
  }
  toa <- sample(c(NA, seq_len(Tper)), n, replace = TRUE)
  names(toa) <- ids
  as_diffnet(graph = arr, toa = toa, t0 = 1, t1 = Tper)
}
dn_a  <- mk_as_diffnet(paste0("A_", 1:30), seed = 1)

## (3) combined diffnet via c() — Yuchan's do.call(c, diffnet_list) ----------
dn_a2 <- mk_as_diffnet(paste0("B_", 1:25), seed = 2)
dn_c  <- tryCatch(c(dn_a, dn_a2),
                  error = function(e) { cat("c() failed:", conditionMessage(e), "\n"); NULL })

cat("Does the object carry a $status field, and does hazard_rate() work?\n")
cat("-----------------------------------------------------------------------\n")
test("rdiffnet",            dn_r)
test("as_diffnet",          dn_a)
if (!is.null(dn_c)) test("c(as_diffnet,as_diffnet)", dn_c)

## Root-cause confirmation: the offending field ------------------------------
cat("\nFields present on each object:\n")
cat("  rdiffnet   :", paste(names(dn_r),  collapse = ", "), "\n")
cat("  as_diffnet :", paste(names(dn_a),  collapse = ", "), "\n")
cat("\nWhere is the adoption status actually stored on as_diffnet?\n")
cat("  is.null($status)   :", is.null(dn_a$status), "\n")
cat("  has $cumadopt      :", !is.null(dn_a$cumadopt),
    " dim:", paste(dim(dn_a$cumadopt), collapse = "x"), "\n")
cat("  has $adopt         :", !is.null(dn_a$adopt),
    " dim:", paste(dim(dn_a$adopt), collapse = "x"), "\n")
dev.off()
