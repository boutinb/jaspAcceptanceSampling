# Exercise option changes in the real result engine, retaining all affected outputs.
.variableSdOptionsForTest <- function(analysis) {
  options <- jaspTools::analysisOptions(analysis)
  options$lotSize <- 1000
  options$historicalSdKnown <- TRUE
  options$proportionNonconformingFrom <- .05
  options$proportionNonconformingTo <- .15
  options$proportionNonconformingStep <- .05
  options$summaryTable <- TRUE
  options$operatingCharacteristicCurve <- TRUE
  options$averageOutgoingQualityCurve <- TRUE
  options$averageTotalInspectionCurve <- TRUE
  if (analysis == "CreateVariablePlan") {
    options$acceptableQualityLevel <- .05
    options$rejectableQualityLevel <- .15
    options$producerRisk <- .05
    options$consumerRisk <- .1
  } else {
    options$sampleSize <- 24
    options$kValue <- 1.309099
    options$planAssessment <- TRUE
    options$planAssessmentAcceptableQualityLevel <- .05
    options$planAssessmentRejectableQualityLevel <- .15
    options$planAssessmentProducerRisk <- .05
    options$planAssessmentConsumerRisk <- .1
  }
  options
}

.variableSdSequenceForTest <- function(analysis, options, changes = logical()) {
  original <- get(paste0(analysis, "Internal"), asNamespace("jaspAcceptanceSampling"))
  containerKey <- if (analysis == "CreateVariablePlan") "createVarContainer" else "analyzeVarContainer"
  observed <- list()
  wrapper <- function(jaspResults, dataset = NULL, options, ...) {
    private <- jaspResults$.__enclos_env__$private
    capture <- function() {
      private$complete()
      result <- jsonlite::fromJSON(
        jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      container <- result$results[[containerKey]]
      expect_null(container$error)
      collection <- container$collection
      readOutput <- function(key) collection[[paste0(containerKey, "_", key)]]
      tables <- lapply(c("plan_table", "summaryTable", "riskTable"), function(key) readOutput(key)$data)
      text <- lapply(c("decision_info", "explanation"), function(key) readOutput(key)$rawtext)
      plots <- lapply(c("ocCurve", "aoqCurve", "atiCurve"), function(key) {
        expect_identical(readOutput(key)$status, "complete")
        jaspResults[[containerKey]][[key]]$plotObject$data
      })
      list(tables = tables, text = text, plots = plots)
    }
    original(jaspResults, dataset, options, ...)
    observed[[1L]] <<- capture()
    for (i in seq_along(changes)) {
      options$historicalSdKnown <- changes[[i]]
      private$changeOptions(as.character(jsonlite::toJSON(options, digits = NA)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, dataset, options, ...)
      observed[[i + 1L]] <<- capture()
    }
  }
  if (analysis == "CreateVariablePlan")
    testthat::local_mocked_bindings(CreateVariablePlanInternal = wrapper, .package = "jaspAcceptanceSampling")
  else
    testthat::local_mocked_bindings(AnalyzeVariablePlanInternal = wrapper, .package = "jaspAcceptanceSampling")
  result <- jaspTools::runAnalysis(analysis, "test.csv", options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_length(observed, length(changes) + 1L)
  observed
}

for (analysis in c("CreateVariablePlan", "AnalyzeVariablePlan")) {
  test_that(paste(analysis, "refreshes all outputs when the SD assumption changes"), {
    options <- .variableSdOptionsForTest(analysis)
    observed <- .variableSdSequenceForTest(analysis, options, c(FALSE, TRUE))
    known <- .variableSdSequenceForTest(analysis, options)[[1L]]
    options$historicalSdKnown <- FALSE
    estimated <- .variableSdSequenceForTest(analysis, options)[[1L]]
    expect_equal(observed[[1L]], known)
    expect_equal(observed[[2L]], estimated)
    expect_equal(observed[[3L]], known)
    expect_false(identical(known$tables[[2L]], estimated$tables[[2L]]))
    for (i in seq_along(known$plots))
      expect_false(identical(known$plots[[i]], estimated$plots[[i]]))
    if (analysis == "CreateVariablePlan") {
      expect_match(paste(known$text[[1L]], collapse = ""), "historical standard deviation", fixed = TRUE)
      expect_match(paste(estimated$text[[1L]], collapse = ""), "sample standard deviation", fixed = TRUE)
      expect_false(grepl("historical standard deviation", paste(estimated$text[[1L]], collapse = ""), fixed = TRUE))
    } else {
      expect_false(identical(known$tables[[3L]], estimated$tables[[3L]]))
    }
  })
}
