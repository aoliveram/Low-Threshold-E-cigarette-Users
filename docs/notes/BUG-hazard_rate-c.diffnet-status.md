# Bug: `hazard_rate()` fails on combined diffnets — `c.diffnet` drops the new `$status` field

**Package:** netdiffuseR 1.26.0 (dev, GitHub)
**Works on:** 1.25.0 (CRAN) · single `rdiffnet`/`as_diffnet` objects
**Fails on:** any diffnet produced by `c()` / `do.call(c, ...)`
**Severity:** breaks `hazard_rate()` (and any consumer of `$status`) for the *combine* workflow

---

## Symptom

```
Error in dimnames(obj) <- dn : attempt to set an attribute on NULL
Calls: hazard_rate
Execution halted
```

Triggered by the common pattern of building one diffnet per group and merging:

```r
diffnet_all <- do.call(c, diffnet_list)   # list of per-school diffnets
hazard_rate(diffnet_all)                   # <- errors
```

---

## Root cause

`hazard_rate()` (1.26.0) reads the **new** `$status` field and immediately sets dimnames on it:

```r
hazard_rate <- function (obj, no.plot = FALSE, include.grid = TRUE, ...) {
    if (inherits(obj, "diffnet")) {
        dn  <- with(obj$meta, list(ids, pers))
        obj <- obj$status          # <- NULL for combined diffnets
        dimnames(obj) <- dn        # <- "attempt to set an attribute on NULL"
    }
    ...
}
```

`$status` is populated by the single-object constructors (`rdiffnet`, `as_diffnet`) but
**`c.diffnet` never rebuilds it.** Its reconstructed object lists only the legacy fields:

```r
# c.diffnet (1.26.0), abridged
toa      <- do.call(c,     lapply(diffnets, function(x) x$toa))
adopt    <- do.call(rbind, lapply(diffnets, function(x) x$adopt))
cumadopt <- do.call(rbind, lapply(diffnets, function(x) x$cumadopt))
graph <- structure(list(graph = A, toa = toa, adopt = adopt,
    cumadopt = cumadopt, vertex.static.attrs = vertex.static.attrs,
    ...))                          # <- no `status =` entry
```

Field inventory confirms the regression:

| object | fields | `$status` |
|---|---|---|
| `rdiffnet(...)`            | graph, toa, adopt, cumadopt, **status**, vertex.\*, meta | `60x5` matrix |
| `as_diffnet(...)`          | graph, toa, adopt, cumadopt, **status**, vertex.\*, meta | `30x4` matrix |
| `c(as_diffnet, as_diffnet)`| graph, toa, adopt, cumadopt, vertex.\*, meta              | **NULL** |

So `c.diffnet` was not updated when `$status` was introduced; it carries `adopt`/`cumadopt`
forward but silently omits `status`. On 1.25.0 `hazard_rate` read `cumadopt` (still present
after combine), which is why the stable release is unaffected.

---

## Minimal reproducer (self-contained, no external data)

```r
library(netdiffuseR)

mk <- function(ids, T = 4, s = 1) {
  set.seed(s); n <- length(ids)
  arr <- array(rbinom(n * n * T, 1, .15), c(n, n, T),
               dimnames = list(ids, ids, as.character(1:T)))
  for (t in 1:T) diag(arr[, , t]) <- 0
  toa <- sample(c(NA, 1:T), n, replace = TRUE); names(toa) <- ids
  as_diffnet(graph = arr, toa = toa, t0 = 1, t1 = T)
}

a <- mk(paste0("A_", 1:30), s = 1)
b <- mk(paste0("B_", 1:25), s = 2)
combined <- c(a, b)

is.null(a$status)         # FALSE  — single object has it
is.null(combined$status)  # TRUE   — c.diffnet dropped it

hazard_rate(a)            # OK
hazard_rate(combined)     # Error in dimnames(obj) <- dn : attempt to set an attribute on NULL
```

---

## Suggested fix

Two layers; (1) is the real fix, (2) hardens the reader.

**(1) `c.diffnet` — carry `status` forward like `adopt`/`cumadopt`:**

```r
status <- do.call(rbind, lapply(diffnets, function(x) x$status))
# ...and add `status = status,` to the structure(list(...)) that builds the object
```

If `status` is meant to be derived rather than stored, derive it in the combined
constructor the same way the single-object constructor does, so the two paths stay in parity.

**(2) `hazard_rate` — defensive fallback (helps any pre-existing object lacking `$status`):**

```r
if (inherits(obj, "diffnet")) {
    dn  <- with(obj$meta, list(ids, pers))
    st  <- if (!is.null(obj$status)) obj$status else obj$cumadopt   # fallback
    obj <- st
    dimnames(obj) <- dn
}
```

**Regression test to add:**

```r
test_that("c.diffnet preserves $status and hazard_rate works on combined diffnets", {
  a <- mk(paste0("A_", 1:10)); b <- mk(paste0("B_", 1:10), s = 2)
  cc <- c(a, b)
  expect_false(is.null(cc$status))
  expect_equal(dim(cc$status), c(20L, dim(a$status)[2]))
  expect_silent(hazard_rate(cc))
})
```

A grep for other `$status` consumers is worthwhile — anything assuming `$status` exists will
hit the same NULL on combined objects.

---

## Workaround (until patched)

`hazard_rate()` is only used for a diagnostic plot; the regression pipeline does not depend on
it. Either skip the call, restore `$status` post-combine
(`diffnet_all$status <- diffnet_all$cumadopt`), or run on netdiffuseR 1.25.0.

*Reproduced on macOS, R 4.5.0, netdiffuseR 1.26.0 (commit installed from GitHub), 2026-06-12.*
