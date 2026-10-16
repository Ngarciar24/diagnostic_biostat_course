# Build flashcards.csv from flashcards_src.txt (chapter|front|back). Keeps rehearsal-added cards.
src <- readLines("flashcards_src.txt", warn = FALSE); src <- src[nzchar(src)]
parts <- strsplit(src, "|", fixed = TRUE)
bad <- which(lengths(parts) != 3); if (length(bad)) stop("Bad lines: ", paste(bad, collapse = ", "))
cards <- data.frame(front = sapply(parts, `[`, 2), back = sapply(parts, `[`, 3),
                    chapter = as.integer(sapply(parts, `[`, 1)), stringsAsFactors = FALSE)
stopifnot(!anyNA(cards$chapter), !anyDuplicated(cards$front))
keep <- if (file.exists("flashcards.csv")) { o <- read.csv("flashcards.csv", colClasses = "character"); o <- o[startsWith(o$front, "[Rehearsal]"), ]; o$chapter <- as.integer(o$chapter); o } else NULL
out <- rbind(cards, keep)
write.csv(out, "flashcards.csv", row.names = FALSE)
cat(nrow(cards), "base cards;", ifelse(is.null(keep), 0, nrow(keep)), "rehearsal cards kept\n")
print(table(cards$chapter))
