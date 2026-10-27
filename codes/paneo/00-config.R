# 00-config.R — shared constants and helpers for the sweep (codes/paneo).
# Run every script from codes/:  Rscript paneo/<script>.R
suppressMessages({ library(data.table) })

PANEO   <- "paneo"
DATA    <- "../outputs_paneo/data"                # student-level intermediate files: never in git
OUT     <- "../outputs_paneo/out"                 # the tables and figures the documents cite are copied to ../results/paneo
for (d in c(DATA, file.path(OUT, c("checks", "layer1", "layer2", "layer3", "stage2", "verdict", "sstar"))))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
XL_W18  <- "../Cleaned_Data/ADVANCE_W1-W8.xlsx"     # the copy the submitted paper was built from
XL_W910 <- "../Cleaned_Data/ADVANCE_W9-W10.xlsx"
SALT_FILE <- file.path(DATA, "hash_salt.txt")     # local salt for hashing typed friend names
NCORES  <- 8L                                     # performance cores on the M4 Pro

C1_SCHOOLS <- c(101:107, 112:114)                 # 108 excluded, as in the paper
C2_SCHOOLS <- c(201, 212:214)
SCHOOLS    <- c(C1_SCHOOLS, C2_SCHOOLS)
GPS        <- 3:8                                 # analysis window: fall 10th .. spring 12th grade
is_c2      <- function(s) s %in% C2_SCHOOLS
gp_to_wave <- function(s, gp) ifelse(is_c2(s), gp + 2L, gp)
wave_to_gp <- function(s, w)  ifelse(is_c2(s), w - 2L, w)
school_waves <- function(s) if (is_c2(s)) 5:10 else 3:8
pre_waves    <- function(s) if (is_c2(s)) 3:4 else 1:2
physical_school <- function(s) ifelse(s == 201, 101L, ifelse(s %in% 212:214, as.integer(s) - 100L, as.integer(s)))
school_c1fe     <- function(s) ifelse(is_c2(s), 101L, as.integer(s))   # the paper's convention (F1)

# Social-media items: sm_post_k (vaping content) and the matching socialmed_j (general use) item.
SMPOST_CORE <- 1:6                                # Facebook, Instagram, Twitter, YouTube, TikTok, Twitch
SMPOST_TO_SOCIALMED <- c(`1` = 1, `2` = 2, `3` = 3, `4` = 7, `5` = 8, `6` = 11, `7` = 12, `8` = 6, `9` = 15)
SOCIALMED_CORE <- c(1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13)   # general-use items present W4–W10

EMPTY <- c("", "NA", "-999", "-99", "0")

normalize_name <- function(x) {
  x <- tolower(trimws(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^a-z ]", "", x)
  gsub("\\s+", " ", trimws(x))
}
hash_names <- function(x) {
  if (!file.exists(SALT_FILE)) writeLines(paste(sample(c(letters, 0:9), 40, TRUE), collapse = ""), SALT_FILE)
  salt <- readLines(SALT_FILE, n = 1)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x) & nzchar(x)
  out[ok] <- substr(vapply(paste0(salt, x[ok]), digest::digest, "", algo = "sha256", serialize = FALSE), 1, 16)
  out
}

msg <- function(...) cat(sprintf(...), "\n", sep = "")
