# Tests for Bayesian State-Space Sampling

test_that("BayesianSSMforAS - analysis uses time-sorted data", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$ssm_count <- "count"
  options$ssm_sampleSize <- "n"
  options$ssm_total <- "N"
  options$ssm_time <- "time"
  options$ssm_scaleModel <- "WithoutPredictor"

  options$ssm_statePlot <- FALSE
  options$ssm_predictionPlot <- FALSE
  options$ssm_posteriorDistPlot <- FALSE
  options$ssm_plotBeta <- FALSE
  options$ssm_showMcmcSummary <- FALSE

  options$ssm_advancedMcmcBurnin <- 100
  options$ssm_advancedMcmcSamples <- 100
  options$ssm_advancedMcmcChains <- 1
  options$ssm_advancedMcmcThin <- 1
  options$ssm_advancedMcmcSeed <- 123
  options$ssm_advancedMcmcAdaptDelta <- 0.9
  options$ssm_advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(3, 1, 2),
    n     = c(20, 20, 20),
    N     = c(100, 100, 100),
    time  = c(30, 10, 20)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)

  expect_equal(results$status, "complete", info = results$results$errorMessage)
  expect_true("summaryTable" %in% names(results$results))
  expect_equal(results$results$summaryTable$data[[1]]$t, 30)
  expect_equal(results$results$summaryTable$data[[1]]$y, 3)
})

test_that("BayesianSSMforAS - options load", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  expect_true(is.list(options))
  expect_true("ssm_count" %in% names(options))
  expect_true("ssm_scaleModel" %in% names(options))
})

test_that("BayesianSSMforAS - summary table is generated", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$ssm_count <- "count"
  options$ssm_sampleSize <- "n"
  options$ssm_total <- "N"
  options$ssm_time <- "time"
  options$ssm_scaleModel <- "WithoutPredictor"

  options$ssm_statePlot <- FALSE
  options$ssm_predictionPlot <- FALSE
  options$ssm_posteriorDistPlot <- FALSE
  options$ssm_plotBeta <- FALSE
  options$ssm_showMcmcSummary <- FALSE

  # Keep test runtime short while still exercising Stan sampling and result wiring.
  options$ssm_advancedMcmcBurnin <- 100
  options$ssm_advancedMcmcSamples <- 100
  options$ssm_advancedMcmcChains <- 1
  options$ssm_advancedMcmcThin <- 1
  options$ssm_advancedMcmcSeed <- 123
  options$ssm_advancedMcmcAdaptDelta <- 0.9
  options$ssm_advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5,
    temp  = c(20, 21, 19, 22, 20)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)

  expect_equal(results$status, "complete", info = results$results$errorMessage)
  expect_true("summaryTable" %in% names(results$results))

  summaryRows <- results$results$summaryTable$data
  expect_equal(length(summaryRows), 1)
  expect_true(summaryRows[[1]]$decision %in% c("Consider to Accept", "Consider to Reject", "Consider to Continue"))
  expect_true(is.numeric(summaryRows[[1]]$massAQL))
  expect_true(is.numeric(summaryRows[[1]]$massRQL))
})

test_that("BayesianSSMforAS - tail correction is deterministic across reruns", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$ssm_count <- "count"
  options$ssm_sampleSize <- "n"
  options$ssm_total <- "N"
  options$ssm_time <- "time"
  options$ssm_scaleModel <- "WithoutPredictor"

  options$ssm_statePlot <- FALSE
  options$ssm_predictionPlot <- FALSE
  options$ssm_posteriorDistPlot <- FALSE
  options$ssm_plotBeta <- FALSE
  options$ssm_showMcmcSummary <- FALSE
  options$ssm_postHocCorrection <- TRUE
  options$ssm_postHocCorrectionWeight <- 5

  options$ssm_advancedMcmcBurnin <- 100
  options$ssm_advancedMcmcSamples <- 100
  options$ssm_advancedMcmcChains <- 1
  options$ssm_advancedMcmcThin <- 1
  options$ssm_advancedMcmcSeed <- 123
  options$ssm_advancedMcmcAdaptDelta <- 0.9
  options$ssm_advancedMcmcMaxTreeDepth <- 8

  ssmData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(ssmData, csvPath, row.names = FALSE)

  results1 <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)
  results2 <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)

  summary1 <- results1$results$summaryTable$data[[1]]
  summary2 <- results2$results$summaryTable$data[[1]]

  expect_equal(summary1$pred, summary2$pred)
  expect_equal(summary1$massAQL, summary2$massAQL)
  expect_equal(summary1$massRQL, summary2$massRQL)
  expect_equal(summary1$decision, summary2$decision)
})

test_that("BayesianSSMforAS - invalid lot inputs fail gracefully before Stan", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$ssm_count <- "count"
  options$ssm_sampleSize <- "n"
  options$ssm_total <- "N"
  options$ssm_time <- "time"
  options$ssm_scaleModel <- "WithoutPredictor"

  options$ssm_statePlot <- FALSE
  options$ssm_predictionPlot <- FALSE
  options$ssm_posteriorDistPlot <- FALSE
  options$ssm_plotBeta <- FALSE
  options$ssm_showMcmcSummary <- FALSE

  options$ssm_advancedMcmcBurnin <- 100
  options$ssm_advancedMcmcSamples <- 100
  options$ssm_advancedMcmcChains <- 1
  options$ssm_advancedMcmcThin <- 1
  options$ssm_advancedMcmcSeed <- 123
  options$ssm_advancedMcmcAdaptDelta <- 0.9
  options$ssm_advancedMcmcMaxTreeDepth <- 8

  invalidData <- data.frame(
    count = c(10, 1),
    n     = c(5, 5),
    N     = c(20, 20),
    time  = 1:2
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(invalidData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)

  expect_equal(results$status, "complete", info = results$results$errorMessage)
  expect_true("placeholder" %in% names(results$results))
  expect_false("summaryTable" %in% names(results$results))
})

test_that("BayesianSSMforAS - constant predictor fails gracefully before Stan", {
  options <- jaspTools::analysisOptions("BayesianSSMforAS")

  options$ssm_count <- "count"
  options$ssm_sampleSize <- "n"
  options$ssm_total <- "N"
  options$ssm_time <- "time"
  options$ssm_scaleModel <- "WithPredictor"
  options$ssm_predictor <- "temp"

  options$ssm_statePlot <- FALSE
  options$ssm_predictionPlot <- FALSE
  options$ssm_posteriorDistPlot <- FALSE
  options$ssm_plotBeta <- FALSE
  options$ssm_showMcmcSummary <- FALSE

  options$ssm_advancedMcmcBurnin <- 100
  options$ssm_advancedMcmcSamples <- 100
  options$ssm_advancedMcmcChains <- 1
  options$ssm_advancedMcmcThin <- 1
  options$ssm_advancedMcmcSeed <- 123
  options$ssm_advancedMcmcAdaptDelta <- 0.9
  options$ssm_advancedMcmcMaxTreeDepth <- 8

  invalidData <- data.frame(
    count = c(1, 0, 2, 1, 1),
    n     = c(20, 20, 20, 20, 20),
    N     = c(100, 100, 100, 100, 100),
    time  = 1:5,
    temp  = rep(20, 5)
  )

  csvPath <- tempfile(fileext = ".csv")
  utils::write.csv(invalidData, csvPath, row.names = FALSE)

  results <- jaspTools::runAnalysis("BayesianSSMforAS", csvPath, options)

  expect_equal(results$status, "complete", info = results$results$errorMessage)
  expect_true("placeholder" %in% names(results$results))
  expect_false("summaryTable" %in% names(results$results))
})
