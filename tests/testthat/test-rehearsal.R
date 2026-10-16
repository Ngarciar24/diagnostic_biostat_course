root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "helpers.R")); source(file.path(root, "R", "rehearsal.R"))

mkq <- function() data.frame(day = rep(1:5, each = 8), topic = "t", question = paste("Q", 1:40),
                             answer = paste("A", 1:40), hardest = FALSE, stringsAsFactors = FALSE)

test_that("draw_questions draws n unique questions covering every day", {
  d <- draw_questions(mkq(), 30, seed = 1)
  expect_equal(nrow(d), 30); expect_equal(anyDuplicated(d$question), 0)
  expect_setequal(unique(d$day), 1:5)
})

test_that("draw_questions is reproducible with a seed", {
  expect_equal(draw_questions(mkq(), 10, seed = 3)$question, draw_questions(mkq(), 10, seed = 3)$question)
})

test_that("score_rehearsal appends misses once and reports the score", {
  f <- tempfile(fileext = ".csv")
  d <- draw_questions(mkq(), 10, seed = 2)
  s <- c(2, 2, 1, 0, 2, 2, 0, 2, 2, 2)
  r <- score_rehearsal(d, s, f)
  expect_equal(r$added, 3); expect_equal(r$total, 15); expect_equal(r$pct, 75)
  r2 <- score_rehearsal(d, s, f)           # same misses again: no duplicates
  expect_equal(r2$added, 0)
  expect_equal(nrow(read.csv(f)), 3)
})

test_that("score_rehearsal validates input", {
  d <- draw_questions(mkq(), 5, seed = 1)
  expect_error(score_rehearsal(d, c(0, 1, 3, 2, 2), tempfile()))
  expect_error(score_rehearsal(d, c(1, 1), tempfile()))
})
