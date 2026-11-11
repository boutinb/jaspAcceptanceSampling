# Small defect rates must remain distinct in the plotted data. These numerical
# references do not depend on AcceptanceSampling's OC implementations.
.asExpectCurvePrecision <- function(results, container, proportion, probability, outgoingQuality) {
  output <- results[["results"]][[container]][["collection"]]
  plots <- lapply(c("ocCurve", "aoqCurve"), function(name) {
    plotName <- output[[paste0(container, "_", name)]][["data"]]
    results[["state"]][["figures"]][[plotName]][["obj"]]
  })
  ocPlot <- plots[[1]]
  aoqPlot <- plots[[2]]
  expect_equal(ocPlot$data$PD, proportion, tolerance = 1e-12)
  expect_equal(ocPlot$data$PA, probability, tolerance = 1e-12)
  expect_equal(aoqPlot$data$PD, proportion, tolerance = 1e-12)
  expect_equal(aoqPlot$data$AOQ, outgoingQuality, tolerance = 1e-12)
  expect_length(unique(ocPlot$data$PD), length(proportion))
  expect_true(all(aoqPlot$data$AOQ > 0))

  rendered <- ggplot2::ggplot_build(aoqPlot)
  expect_equal(rendered$data[[3]]$yintercept, max(outgoingQuality), tolerance = 1e-12)
  labelValue <- as.numeric(sub("AOQL: ", "", rendered$data[[4]]$label, fixed = TRUE))
  expect_gt(labelValue, 0)
  expect_lt(abs(labelValue / max(outgoingQuality) - 1), .005)
}

.asPrecisionAttributeOptions <- function() {
  options <- jaspTools::analysisOptions("AnalyzeAttributePlan")
  options$singlePlanLotSize <- 10000
  options$singlePlanSampleSize <- 100
  options$singlePlanAcceptanceNumber <- 0
  options$singlePlanRejectionNumber <- 1
  options$singlePlanDistribution <- "binomial"
  options$singlePlanAssessment <- FALSE
  options$singlePlanSummaryTable <- FALSE
  options$singlePlanOperatingCharacteristicCurve <- TRUE
  options$singlePlanAverageOutgoingQualityCurve <- TRUE
  options$singlePlanAverageTotalInspectionCurve <- FALSE
  options$singlePlanAverageSampleNumberCurve <- FALSE
  options$multiplePlanStages <- list()
  options$singlePlanProportionNonconformingFrom <- .0001
  options$singlePlanProportionNonconformingTo <- .0004
  options$singlePlanProportionNonconformingStep <- .0001
  options
}

test_that("single attribute curves retain low-rate and ordinary numerical precision", {
  options <- .asPrecisionAttributeOptions()

  for (step in c(.0001, .01)) {
    options$singlePlanProportionNonconformingFrom <- step
    options$singlePlanProportionNonconformingTo <- 4 * step
    options$singlePlanProportionNonconformingStep <- step
    results <- jaspTools::runAnalysis("AnalyzeAttributePlan", "test.csv", options)
    proportion <- seq(step, 4 * step, by = step)
    probability <- (1 - proportion)^100
    outgoingQuality <- probability * proportion * .99
    .asExpectCurvePrecision(results, "singleContainer", proportion, probability, outgoingQuality)
  }
})

test_that("multistage attribute curves retain precision in stage-weighted AOQ", {
  options <- .asPrecisionAttributeOptions()
  options$multiplePlanLotSize <- 10000
  options$multiplePlanStagesCount <- 2
  options$multiplePlanStages <- list(
    list(acceptanceNumber = 0, rejectionNumber = 2, sampleSize = 50, value = "1"),
    list(acceptanceNumber = 1, rejectionNumber = 2, sampleSize = 50, value = "2"))
  options$multiplePlanDistribution <- "binomial"
  options$multiplePlanProportionNonconformingFrom <- .0001
  options$multiplePlanProportionNonconformingTo <- .0004
  options$multiplePlanProportionNonconformingStep <- .0001
  options$multiplePlanAssessment <- FALSE
  options$multiplePlanSummaryTable <- FALSE
  options$multiplePlanOperatingCharacteristicCurve <- TRUE
  options$multiplePlanAverageOutgoingQualityCurve <- TRUE
  options$multiplePlanAverageTotalInspectionCurve <- FALSE
  options$multiplePlanAverageSampleNumberCurve <- FALSE
  results <- jaspTools::runAnalysis("AnalyzeAttributePlan", "test.csv", options)

  proportion <- seq(.0001, .0004, by = .0001)
  # Accept immediately with no defectives, or after exactly one in the first
  # sample and none in the second. All other paths reject the lot.
  firstStage <- (1 - proportion)^50
  secondStage <- 50 * proportion * (1 - proportion)^99
  probability <- firstStage + secondStage
  outgoingQuality <- (firstStage * .995 + secondStage * .99) * proportion
  .asExpectCurvePrecision(results, "multContainer", proportion, probability, outgoingQuality)
})

test_that("variable curves retain low-rate precision", {
  options <- jaspTools::analysisOptions("AnalyzeVariablePlan")
  options$lotSize <- 10000
  options$sampleSize <- 24
  options$kValue <- 1.309
  options$historicalSdKnown <- TRUE
  options$proportionNonconformingFrom <- .0001
  options$proportionNonconformingTo <- .0004
  options$proportionNonconformingStep <- .0001
  options$planAssessment <- FALSE
  options$summaryTable <- FALSE
  options$operatingCharacteristicCurve <- TRUE
  options$averageOutgoingQualityCurve <- TRUE
  options$averageTotalInspectionCurve <- FALSE
  results <- jaspTools::runAnalysis("AnalyzeVariablePlan", "test.csv", options)

  proportion <- seq(.0001, .0004, by = .0001)
  probability <- stats::pnorm(sqrt(24) * (stats::qnorm(1 - proportion) - 1.309))
  outgoingQuality <- probability * proportion * (10000 - 24) / 10000
  .asExpectCurvePrecision(results, "analyzeVarContainer", proportion, probability, outgoingQuality)
})
