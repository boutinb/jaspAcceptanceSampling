# Label the existing finite-lot approximation without changing its calculation.
.aoqAttributeOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("AnalyzeAttributePlan")
  options$singlePlanSampleSize <- 10
  options$singlePlanAcceptanceNumber <- 0
  options$singlePlanRejectionNumber <- 1
  options$multiplePlanStagesCount <- 2
  options$multiplePlanStages <- list(
    list(value = "1", sampleSize = 5, acceptanceNumber = 0, rejectionNumber = 2),
    list(value = "2", sampleSize = 5, acceptanceNumber = 1, rejectionNumber = 2))
  for (prefix in c("singlePlan", "multiplePlan")) {
    options[[paste0(prefix, "LotSize")]] <- 20
    options[[paste0(prefix, "Distribution")]] <- "hypergeometric"
    options[[paste0(prefix, "ProportionNonconformingFrom")]] <- 0
    options[[paste0(prefix, "ProportionNonconformingTo")]] <- 0.5
    options[[paste0(prefix, "ProportionNonconformingStep")]] <- 0.05
    options[[paste0(prefix, "Assessment")]] <- FALSE
    options[[paste0(prefix, "SummaryTable")]] <- FALSE
    options[[paste0(prefix, "OperatingCharacteristicCurve")]] <- FALSE
    options[[paste0(prefix, "AverageOutgoingQualityCurve")]] <- TRUE
    options[[paste0(prefix, "AverageTotalInspectionCurve")]] <- FALSE
    options[[paste0(prefix, "AverageSampleNumberCurve")]] <- FALSE
  }
  options
}

.aoqExpectLabelForTest <- function(result, containerName, approximate) {
  container <- result$results[[containerName]]
  expect_false(is.null(container))
  expect_null(container[["error"]])
  collection <- container$collection
  curve <- collection[[paste0(containerName, "_aoqCurve")]]
  note <- collection[[paste0(containerName, "_aoqApproximation")]]
  expect_false(is.null(curve))
  expect_null(curve[["error"]])
  expect_identical(curve$status, "complete")
  if (approximate) {
    expect_match(curve$title, "Approximation", fixed = TRUE)
    expect_false(is.null(note))
    expect_null(note[["error"]])
    text <- paste(note$rawtext, collapse = "")
    expect_match(text, "Finite-lot approximation", fixed = TRUE)
    expect_match(text, "large fraction of the lot", fixed = TRUE)
    expect_match(text, "not an exact finite-lot calculation", fixed = TRUE)
    expect_match(text, "including for multiple-stage plans", fixed = TRUE)
  } else {
    expect_identical(curve$title, "AOQ (Average Outgoing Quality) Curve")
    expect_null(note)
  }
}

test_that("Analyzed attribute plans label AOQ only for hypergeometric sampling", {
  options <- .aoqAttributeOptionsForTest()
  for (distribution in c("hypergeometric", "binomial", "poisson")) {
    options$singlePlanDistribution <- options$multiplePlanDistribution <- distribution
    result <- jaspTools::runAnalysis("AnalyzeAttributePlan", dataset = NULL, options = options, view = FALSE)
    for (container in c("singleContainer", "multContainer")) {
      .aoqExpectLabelForTest(result, container, approximate = distribution == "hypergeometric")
      curve <- result$results[[container]]$collection[[paste0(container, "_aoqCurve")]]
      data <- result$state$figures[[curve$data]]$obj$data
      expect_gt(nrow(data), 0L)
      expect_true(all(is.finite(data$AOQ)))
    }
  }
})

test_that("Generated attribute plans label AOQ only for hypergeometric sampling", {
  options <- jaspTools::analysisOptions("CreateAttributePlan")
  options$lotSize <- 1000
  options$acceptableQualityLevel <- 0.05
  options$rejectableQualityLevel <- 0.15
  options$producerRisk <- 0.05
  options$consumerRisk <- 0.10
  options$summaryTable <- FALSE
  options$operatingCharacteristicCurve <- FALSE
  options$averageTotalInspectionCurve <- FALSE
  options$proportionNonconformingFrom <- 0
  options$proportionNonconformingTo <- 0.2
  options$proportionNonconformingStep <- 0.05
  options$averageOutgoingQualityCurve <- TRUE
  for (distribution in c("hypergeometric", "binomial", "poisson")) {
    options$distribution <- distribution
    result <- jaspTools::runAnalysis("CreateAttributePlan", dataset = NULL, options = options, view = FALSE)
    .aoqExpectLabelForTest(result, "createContainer", approximate = distribution == "hypergeometric")
  }
})

test_that("Variable plans do not receive the hypergeometric AOQ qualification", {
  for (analysis in c("CreateVariablePlan", "AnalyzeVariablePlan")) {
    options <- jaspTools::analysisOptions(analysis)
    options$historicalSdKnown <- TRUE
    options$lotSize <- 1000
    if (analysis == "CreateVariablePlan") {
      options$acceptableQualityLevel <- 0.05
      options$rejectableQualityLevel <- 0.15
      options$producerRisk <- 0.05
      options$consumerRisk <- 0.10
    } else {
      options$sampleSize <- 24
      options$kValue <- 1.309
      options$planAssessment <- FALSE
    }
    options$summaryTable <- FALSE
    options$operatingCharacteristicCurve <- FALSE
    options$averageTotalInspectionCurve <- FALSE
    options$proportionNonconformingFrom <- 0.05
    options$proportionNonconformingTo <- 0.15
    options$proportionNonconformingStep <- 0.05
    options$averageOutgoingQualityCurve <- TRUE
    result <- jaspTools::runAnalysis(analysis, dataset = NULL, options = options, view = FALSE)
    container <- if (analysis == "CreateVariablePlan") "createVarContainer" else "analyzeVarContainer"
    .aoqExpectLabelForTest(result, container, approximate = FALSE)
  }
})

test_that("AOQ qualifications follow the distribution and curve visibility in the same analysis", {
  options <- .aoqAttributeOptionsForTest()
  original <- get("AnalyzeAttributePlan", asNamespace("jaspAcceptanceSampling"))
  observed <- list()
  wrapper <- function(jaspResults, dataset = NULL, options, ...) {
    private <- jaspResults$.__enclos_env__$private
    capture <- function() {
      private$complete()
      result <- jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      for (container in c("singleContainer", "multContainer")) {
        curve <- jaspResults[[container]][["aoqCurve"]]
        if (!is.null(curve))
          result[[paste0(container, "Data")]] <- curve$plotObject$data
      }
      result
    }
    original(jaspResults, dataset, options, ...)
    observed[[1L]] <<- capture()
    changes <- list(
      list(singlePlanDistribution = "binomial", multiplePlanDistribution = "binomial"),
      list(singlePlanDistribution = "hypergeometric", multiplePlanDistribution = "hypergeometric"),
      list(singlePlanAverageOutgoingQualityCurve = FALSE, multiplePlanAverageOutgoingQualityCurve = FALSE),
      list(singlePlanAverageOutgoingQualityCurve = TRUE, multiplePlanAverageOutgoingQualityCurve = TRUE))
    for (change in changes) {
      options[names(change)] <- change
      private$changeOptions(as.character(jsonlite::toJSON(options, digits = NA)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, dataset, options, ...)
      observed[[length(observed) + 1L]] <<- capture()
    }
  }
  testthat::with_mocked_bindings(
    jaspTools::runAnalysis("AnalyzeAttributePlan", dataset = NULL, options = options, view = FALSE),
    AnalyzeAttributePlan = wrapper, .package = "jaspAcceptanceSampling")
  expect_length(observed, 5L)
  for (container in c("singleContainer", "multContainer")) {
    for (i in c(1L, 3L, 5L))
      .aoqExpectLabelForTest(observed[[i]], container, approximate = TRUE)
    .aoqExpectLabelForTest(observed[[2L]], container, approximate = FALSE)
    hidden <- observed[[4L]]$results[[container]]$collection
    expect_null(hidden[[paste0(container, "_aoqCurve")]])
    expect_null(hidden[[paste0(container, "_aoqApproximation")]])
    dataKey <- paste0(container, "Data")
    expect_s3_class(observed[[1L]][[dataKey]], "data.frame")
    expect_equal(observed[[3L]][[dataKey]], observed[[1L]][[dataKey]])
    expect_equal(observed[[5L]][[dataKey]], observed[[1L]][[dataKey]])
  }
})
