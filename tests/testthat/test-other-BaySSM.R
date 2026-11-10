context("Other: BaySSM")

# Generated from the saved JASP analyses, then reviewed for headless execution and completion checks.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

# Stan's sampler gives slightly different draws per platform and compiler (the same seed gives
# a posterior mean of 0.846 on macOS, 0.850 on Windows and 0.853 on Linux). The exact tables and
# plots were made on macOS (arm64) and are only compared there; every platform checks the cells
# that come from the data exactly and the sampled ones within a tolerance.
exactStanResults <- identical(Sys.info()[["sysname"]], "Darwin")

expectStanRow <- function(row, ref, tolerance) {
  for (name in names(ref)) {
    if (is.numeric(ref[[name]]) && !is.null(tolerance[[name]]))
      expect_equal(row[[name]], ref[[name]], tolerance = tolerance[[name]], scale = 1, label = name)
    else
      expect_identical(row[[name]], ref[[name]], label = name)
  }
}

summaryTolerance <- list(massAQL = 0.005, massRQL = 0.03, pred = 3)

test_that("BayesianSSMforAS with temperature results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")

  predictorRows <- results[["results"]][["predictorSummary"]][["data"]]
  expect_length(predictorRows, 1)
  expectStanRow(predictorRows[[1]],
                list(predictor = "temperature", lower = 0.497402696413993, mean = 0.846355022604599,
                     median = 0.833704390306833, upper = 1.24517861504467),
                list(lower = 0.05, mean = 0.05, median = 0.05, upper = 0.1))

  summaryRows <- results[["results"]][["summaryTable"]][["data"]]
  expect_length(summaryRows, 1)
  expectStanRow(summaryRows[[1]],
                list(decision = "Consider to Reject", t = "30", y = 8, massAQL = 4e-04,
                     massRQL = 0.878333333333333, pred = 121),
                summaryTolerance)

  if (exactStanResults) {
    plotName <- results[["results"]][["predPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-predictive")

    jaspTools::expect_equal_tables(predictorRows,
      list(0.497402696413993, 0.846355022604599, 0.833704390306833, "temperature",
       1.24517861504467))

    plotName <- results[["results"]][["ssm_statePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_defect-rate-over-time")

    jaspTools::expect_equal_tables(summaryRows,
      list("Consider to Reject", 4e-04, 0.878333333333333, 121, 30, 8))
  }

})

test_that("BayesianSSMforAS without predictors results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "BaySSM.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("BayesianSSMforAS", dataset, opts, view = FALSE)
  expect_identical(results$status, "complete")

  summaryRows <- results[["results"]][["summaryTable"]][["data"]]
  expect_length(summaryRows, 1)
  expectStanRow(summaryRows[[1]],
                list(decision = "Consider to Reject", t = "30", y = 8, massAQL = 0.00126666666666667,
                     massRQL = 0.902733333333333, pred = 138),
                summaryTolerance)

  if (exactStanResults) {
    plotName <- results[["results"]][["predPlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_posterior-predictive")

    plotName <- results[["results"]][["ssm_statePlot"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_defect-rate-over-time")

    jaspTools::expect_equal_tables(summaryRows,
      list("Consider to Reject", 0.00126666666666667, 0.902733333333333,
       138, 30, 8))
  }

})
