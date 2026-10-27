suppressMessages(library(readxl))
f <- "../Cleaned_Data/ADVANCE_W1-W8.xlsx"
nm <- tolower(names(read_excel(f, n_max = 1, .name_repair = "minimal")))
ct <- rep("skip", length(nm)); ct[grepl("^w5_friend[1-7]_(other_)?[0-9]{3}$|^w5_schoolid$", nm) | nm == "record_id"] <- "text"
d <- read_excel(f, col_types = ct, na = c("", "NA", "."), .name_repair = "minimal"); names(d) <- tolower(names(d))
ids <- d$record_id
rc <- grep("^w5_friend[1-7]_[0-9]{3}$", names(d), value = TRUE); oc <- grep("^w5_friend[1-7]_other_[0-9]{3}$", names(d), value = TRUE)
r <- unlist(d[rc]); r <- r[!is.na(r)]; o <- unlist(d[oc]); o <- o[!is.na(o)]
cat("W5 roster cells non-NA:", length(r), " | look like a record_id in the panel:", sum(r %in% ids), sprintf("(%.1f%%)", 100*mean(r %in% ids)), "\n")
cat("Most frequent roster values:\n"); print(head(sort(table(r), decreasing = TRUE), 12))
ph <- c("none","no","n/a","na","nobody","no one","noone","-","x","idk","no friends")
cat("\nW5 typed cells non-NA:", length(o), "| placeholder-like values:", sum(tolower(trimws(o)) %in% ph), "\n")  # typed names are personal data: never print them
