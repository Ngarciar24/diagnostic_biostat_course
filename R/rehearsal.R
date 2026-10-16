# Timed-rehearsal helpers (Day 10). Questions: interview_questions.csv. Cards: flashcards.csv

# Draw n random questions (optionally at least one per day when n >= number of days).
draw_questions <- function(q, n = 30, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(n <= nrow(q))
  days <- sort(unique(q$day))
  first <- if (n >= length(days)) sapply(days, function(d) sample(which(q$day == d), 1)) else integer(0)
  rest <- setdiff(seq_len(nrow(q)), first)
  idx <- c(first, sample(rest, n - length(first)))
  q[sample(idx), ]
}

# scores: numeric 0 (missed), 1 (partial), 2 (solid) aligned to `drawn`.
# Misses (< `miss_below`) become flashcards unless the front already exists. Returns a summary list.
score_rehearsal <- function(drawn, scores, flashcards = "flashcards.csv", miss_below = 2) {
  stopifnot(length(scores) == nrow(drawn), all(scores %in% 0:2))
  cards <- if (file.exists(flashcards)) read.csv(flashcards, stringsAsFactors = FALSE)
           else data.frame(front = character(), back = character(), chapter = integer())
  missed <- drawn[scores < miss_below, ]
  new <- data.frame(front = paste0("[Rehearsal] ", missed$question), back = missed$answer,
                    chapter = missed$day, stringsAsFactors = FALSE)
  new <- new[!(new$front %in% cards$front), ]
  write.csv(rbind(cards, new), flashcards, row.names = FALSE)
  list(total = sum(scores), max = 2 * length(scores), pct = 100 * sum(scores) / (2 * length(scores)),
       by_day = tapply(scores, drawn$day, mean), added = nrow(new))
}
