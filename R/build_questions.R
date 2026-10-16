# Combine questions/day*.yml into interview_questions.csv (run from repo root).
suppressMessages(library(yaml))
files <- sort(list.files("questions", pattern = "^day[0-9]+\\.yml$", full.names = TRUE))
rows <- lapply(files, function(f) {
  day <- as.integer(sub("^day0*([0-9]+)\\.yml$", "\\1", basename(f)))
  qs <- yaml.load_file(f)
  do.call(rbind, lapply(qs, function(q) data.frame(
    day = day, topic = q$topic, question = trimws(q$q), answer = trimws(q$a),
    hardest = isTRUE(q$hard), stringsAsFactors = FALSE)))
})
out <- do.call(rbind, rows)
write.csv(out, "interview_questions.csv", row.names = FALSE)
cat(sprintf("%d questions across %d days\n", nrow(out), length(unique(out$day))))
