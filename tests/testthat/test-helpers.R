source(file.path(rprojroot_dir <- normalizePath(file.path("..", "..")), "R", "helpers.R"))

test_that("Wilson and Clopper-Pearson agree with binom package", {
  skip_if_not_installed("binom")
  w <- binom::binom.confint(37, 50, methods = "wilson")
  cp <- binom::binom.confint(37, 50, methods = "exact")
  expect_equal(unname(ci_wilson(37, 50)[2:3]), c(w$lower, w$upper), tolerance = 1e-8)
  expect_equal(unname(ci_clopper_pearson(37, 50)[2:3]), c(cp$lower, cp$upper), tolerance = 1e-8)
})

test_that("CP handles boundaries", {
  expect_equal(ci_clopper_pearson(0, 20)[["lower"]], 0)
  expect_equal(ci_clopper_pearson(20, 20)[["upper"]], 1)
})

test_that("report_2x2 counts are right", {
  truth <- c(rep(1, 10), rep(0, 20)); test <- c(rep(1, 8), rep(0, 2), rep(1, 3), rep(0, 17))
  r <- report_2x2(test, truth)
  expect_equal(r$est[r$measure == "Sensitivity"], 0.8)
  expect_equal(r$est[r$measure == "Specificity"], 17 / 20)
  expect_equal(r$est[r$measure == "PPV"], 8 / 11)
})

test_that("predictive values match hand calculation", {
  pv <- predictive_values(0.9, 0.8, 0.1)
  expect_equal(pv[["PPV"]], 0.09 / (0.09 + 0.18))
  expect_equal(pv[["LRpos"]], 4.5)
})

test_that("ep05_anova recovers truth on a large balanced simulation", {
  set.seed(1)
  D <- 400; R <- 2; K <- 2
  d <- expand.grid(rep = 1:K, run = 1:R, day = 1:D)
  d$y <- 100 + rnorm(D, 0, 3)[d$day] + rnorm(D * R, 0, 2)[(d$day - 1) * R + d$run] + rnorm(nrow(d), 0, 1.5)
  v <- ep05_anova(d$y, d$day, d$run, K)
  expect_equal(unname(v[c("day", "run", "repeatability")]), c(9, 4, 2.25), tolerance = 0.15)
})

test_that("ep05_anova matches VCA::anovaVCA", {
  skip_if_not_installed("VCA")
  set.seed(2)
  D <- 20; R <- 2; K <- 2
  d <- expand.grid(rep = 1:K, run = 1:R, day = 1:D)
  d$y <- 50 + rnorm(D, 0, 2)[d$day] + rnorm(D * R, 0, 1)[(d$day - 1) * R + d$run] + rnorm(nrow(d))
  d$day <- factor(d$day); d$run <- factor(d$run)
  fit <- VCA::anovaVCA(y ~ day/run, d)
  vca <- fit$aov.tab[c("day", "day:run", "error"), "VC"]
  mine <- ep05_anova(d$y, d$day, d$run, K)
  expect_equal(unname(mine[c("day", "run", "repeatability")]), unname(vca), tolerance = 1e-6)
})

test_that("LoB/LoD parametric formulas", {
  b <- c(1, 2, 3, 4, 5); lob <- lob_param(b)
  expect_equal(lob, 3 + 1.645 * sd(b))
  expect_equal(lod_param(lob, c(10, 12, 14)), lob + 1.645 * 2)
})

test_that("Buderer sample size matches hand calc", {
  # sens 0.9, d 0.05, prev 0.2: 1.96^2*.09/.0025 = 138.3 cases -> /0.2 = 691.5 -> 692
  expect_equal(n_buderer(0.9, 0.05, 0.2), 692)
  expect_equal(n_buderer(0.9, 0.05, 0.2, what = "spec"), ceiling(138.2976 / 0.8))
})

test_that("two-cut classification", {
  f <- classify_two_cut(c(1, 5, 9), 2, 8)
  expect_equal(as.character(f), c("rule-out", "intermediate", "rule-in"))
  expect_error(classify_two_cut(1, 5, 2))
})

test_that("lod_probit recovers the 95% point and matches MASS::dose.p", {
  conc <- c(2, 5, 10, 20, 40); b0 <- -2; b1 <- 2   # true model: probit(p) = -2 + 2*log10(conc)
  p <- pnorm(b0 + b1 * log10(conc)); n <- rep(1e6, 5); hits <- round(p * n)
  true_lod <- 10^((qnorm(0.95) - b0) / b1)
  expect_equal(lod_probit(conc, hits, n), true_lod, tolerance = 1e-3)
  skip_if_not_installed("MASS")
  fit <- glm(cbind(hits, n - hits) ~ log10(conc), family = binomial("probit"))
  dp <- MASS::dose.p(fit, p = 0.95)
  expect_equal(10^as.numeric(dp), lod_probit(conc, hits, n), tolerance = 1e-8)
})

test_that("linearity_check flags curvature and not a straight line", {
  x <- rep(1:9, each = 3)
  set.seed(3)
  expect_gt(linearity_check(x, 2 * x + rnorm(27, 0, 0.3))$p_nonlinear, 0.01)
  expect_lt(linearity_check(x, 2 * x - 0.08 * x^2 + rnorm(27, 0, 0.1))$p_nonlinear, 1e-6)
})
