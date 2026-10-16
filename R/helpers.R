if (!exists("%||%", baseenv())) `%||%` <- function(x, y) if (is.null(x)) y else x  # R < 4.4 compat (lme4 printing)
# Shared helpers for the course labs. Each function is unit-tested in tests/testthat.

# ---- Proportion confidence intervals -------------------------------------
ci_wilson <- function(x, n, conf = 0.95) {
  stopifnot(n > 0, x >= 0, x <= n)
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(est = p, lower = max(0, centre - half), upper = min(1, centre + half))
}

ci_clopper_pearson <- function(x, n, conf = 0.95) {
  stopifnot(n > 0, x >= 0, x <= n)
  a <- 1 - conf
  lo <- if (x == 0) 0 else qbeta(a / 2, x, n - x + 1)
  hi <- if (x == n) 1 else qbeta(1 - a / 2, x + 1, n - x)
  c(est = x / n, lower = lo, upper = hi)
}

# ---- 2x2 diagnostic report ------------------------------------------------
# truth and test are logical/0-1 vectors (1 = condition present / test positive)
report_2x2 <- function(test, truth, conf = 0.95, ci = ci_wilson) {
  test <- as.logical(test); truth <- as.logical(truth)
  tp <- sum(test & truth); fn <- sum(!test & truth)
  fp <- sum(test & !truth); tn <- sum(!test & !truth)
  row <- function(name, x, n) {
    r <- ci(x, n, conf)
    data.frame(measure = name, x = x, n = n, est = r[["est"]],
               lower = r[["lower"]], upper = r[["upper"]])
  }
  rbind(row("Sensitivity", tp, tp + fn),
        row("Specificity", tn, tn + fp),
        row("PPV", tp, tp + fp),
        row("NPV", tn, tn + fn))
}

# PPV/NPV at a chosen prevalence from sensitivity and specificity (Bayes)
predictive_values <- function(sens, spec, prev) {
  ppv <- sens * prev / (sens * prev + (1 - spec) * (1 - prev))
  npv <- spec * (1 - prev) / (spec * (1 - prev) + (1 - sens) * prev)
  c(PPV = ppv, NPV = npv, LRpos = sens / (1 - spec), LRneg = (1 - sens) / spec)
}

# ---- EP05-style nested precision (balanced) -------------------------------
# Balanced design: day (D) > run (R) > replicate (K). Returns variance components
# by the classical ANOVA method of moments (what VCA::anovaVCA also does).
ep05_anova <- function(y, day, run, nrep) {
  d <- data.frame(y = y, day = factor(day), run = factor(paste(day, run)))
  D <- nlevels(d$day); R <- nlevels(d$run) / D; K <- nrep
  grand <- mean(d$y)
  ybar_day <- tapply(d$y, d$day, mean)
  ybar_run <- tapply(d$y, d$run, mean)
  ss_day <- R * K * sum((ybar_day - grand)^2)
  run_day <- sapply(levels(d$run), function(r) sub(" .*", "", r))
  ss_run <- K * sum((ybar_run - ybar_day[run_day])^2)
  ss_err <- sum((d$y - ybar_run[as.character(d$run)])^2)
  ms_day <- ss_day / (D - 1)
  ms_run <- ss_run / (D * (R - 1))
  ms_err <- ss_err / (D * R * (K - 1))
  v_rep <- ms_err
  v_run <- max(0, (ms_run - ms_err) / K)
  v_day <- max(0, (ms_day - ms_run) / (R * K))
  c(day = v_day, run = v_run, repeatability = v_rep,
    within_lab = v_day + v_run + v_rep)
}

# ---- EP17-style detection capability --------------------------------------
lob_param <- function(blank, z = 1.645) mean(blank) + z * sd(blank)
lod_param <- function(lob, low, z = 1.645) lob + z * sd(low)
lob_nonparam <- function(blank, alpha = 0.05) {
  # 95th percentile of the ranked blank results (rank-based, interpolated)
  unname(quantile(blank, 1 - alpha, type = 6))
}

# ---- Sample size for sensitivity / specificity (Buderer 1996) --------------
# Total n so that the CI for sens (or spec) has half-width `d`, given prevalence.
n_buderer <- function(p, d, prev, conf = 0.95, what = c("sens", "spec")) {
  what <- match.arg(what)
  z <- qnorm(1 - (1 - conf) / 2)
  n_cases <- z^2 * p * (1 - p) / d^2
  if (what == "sens") ceiling(n_cases / prev) else ceiling(n_cases / (1 - prev))
}

# ---- Two-cut-off classification --------------------------------------------
# Returns factor: "rule-out" (<= lo), "intermediate", "rule-in" (>= hi)
classify_two_cut <- function(x, lo, hi) {
  stopifnot(lo <= hi)
  factor(ifelse(x <= lo, "rule-out", ifelse(x >= hi, "rule-in", "intermediate")),
         levels = c("rule-out", "intermediate", "rule-in"))
}

# ---- Course plumbing -------------------------------------------------------
# Interview questions live in interview_questions.csv (day, topic, question, answer, hardest).
read_questions <- function(path = "interview_questions.csv") {
  q <- read.csv(path, stringsAsFactors = FALSE)
  stopifnot(all(c("day", "topic", "question", "answer", "hardest") %in% names(q)))
  q
}

# Print the questions of one day as markdown (use inside a chunk with results='asis').
show_questions <- function(day, path = "interview_questions.csv") {
  q <- read_questions(path)
  q <- q[q$day == day, ]
  for (i in seq_len(nrow(q))) {
    star <- if (isTRUE(as.logical(q$hardest[i]))) " ★" else ""
    cat(sprintf("**Q%d.%s** %s\n\n> %s\n\n", i, star, q$question[i], gsub("\n", "\n> ", q$answer[i])))
  }
}

# ---- Probit LoD (hit-rate design) ------------------------------------------
# hits/n at each concentration; returns the concentration with `target` detection prob.
lod_probit <- function(conc, hits, n, target = 0.95) {
  fit <- glm(cbind(hits, n - hits) ~ log10(conc), family = binomial(link = "probit"))
  b <- coef(fit)
  10^((qnorm(target) - b[[1]]) / b[[2]])
}

# ---- Linearity: deviation of the linear fit from a polynomial fit ----------
# Returns F-test p-value for the quadratic+cubic terms and %deviation at each level.
linearity_check <- function(x, y, degree = 3) {
  lin <- lm(y ~ x)
  poly_fit <- lm(y ~ poly(x, degree, raw = TRUE))
  a <- anova(lin, poly_fit)
  lv <- sort(unique(x))
  dev <- 100 * (predict(lin, data.frame(x = lv)) - predict(poly_fit, data.frame(x = lv))) /
    predict(poly_fit, data.frame(x = lv))
  list(p_nonlinear = a$`Pr(>F)`[2], F = a$F[2], deviation_pct = setNames(dev, lv))
}
