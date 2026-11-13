# Generated plans must fit within the specified lot without changing their risks.
.attributeFeasibilityOptionsForTest <- function() {
  options <- jaspTools::analysisOptions("CreateAttributePlan")
  options$acceptableQualityLevel <- 0.01
  options$rejectableQualityLevel <- 0.06
  options$producerRisk <- 0.05
  options$consumerRisk <- 0.10
  options$proportionNonconformingFrom <- 0
  options$proportionNonconformingTo <- 0.1
  options$proportionNonconformingStep <- 0.01
  options$summaryTable <- TRUE
  options$operatingCharacteristicCurve <- TRUE
  options$averageOutgoingQualityCurve <- TRUE
  options$averageTotalInspectionCurve <- TRUE
  options
}

.attributeExpectInfeasibleForTest <- function(container, sampleSize) {
  for (key in c("findPlanTable", "findProbTable")) {
    output <- container$collection[[paste0("createContainer_", key)]]
    expect_identical(output$status, "error")
    expect_identical(output[["error"]]$type, "badData")
  }
  message <- container$collection$createContainer_findPlanTable[["error"]]$errorMessage
  expect_match(paste(message, collapse = ""),
               sprintf("The generated sample size (%d) exceeds the lot size (20)", sampleSize), fixed = TRUE)
  for (key in c("description", "summaryTable", "ocCurve", "aoqCurve", "atiCurve"))
    expect_null(container$collection[[paste0("createContainer_", key)]])
}

test_that("Create attribute plan rejects samples larger than the lot and preserves feasible plans", {
  for (distribution in c("binomial", "poisson")) {
    options <- .attributeFeasibilityOptionsForTest()
    options$distribution <- distribution
    sampleSize <- if (distribution == "binomial") 110 else 112
    options$lotSize <- 20
    result <- jaspTools::runAnalysis("CreateAttributePlan", dataset = NULL, options = options, view = FALSE)
    .attributeExpectInfeasibleForTest(result$results$createContainer, sampleSize)

    for (lotSize in c(sampleSize, 1000)) {
      options$lotSize <- lotSize
      result <- jaspTools::runAnalysis("CreateAttributePlan", dataset = NULL, options = options, view = FALSE)
      container <- result$results$createContainer
      expect_null(container[["error"]])
      collection <- container$collection
      jaspTools::expect_equal_tables(collection$createContainer_findPlanTable$data,
                                    list("Sample Size", sampleSize, "Acceptance Number", 3))
      # Independent probability calculations retain the originally requested risk constraints.
      acceptance <- if (distribution == "binomial") {
        stats::pbinom(3, sampleSize, c(0.01, 0.06))
      } else {
        stats::ppois(3, sampleSize * c(0.01, 0.06))
      }
      jaspTools::expect_equal_tables(collection$createContainer_findProbTable$data,
                                    list("AQL", 0.01, acceptance[1], 1 - acceptance[1],
                                         "RQL", 0.06, acceptance[2], 1 - acceptance[2]))
      expect_gte(acceptance[1], 0.95)
      expect_lte(acceptance[2], 0.10)
      aoq <- result$state$figures[[collection$createContainer_aoqCurve$data]]$obj$data$AOQ
      ati <- result$state$figures[[collection$createContainer_atiCurve$data]]$obj$data$ATI
      expect_gt(length(aoq), 0L)
      expect_gt(length(ati), 0L)
      expect_true(all(aoq >= 0))
      expect_true(all(ati <= lotSize))
      if (lotSize == sampleSize) {
        expect_equal(aoq, rep(0, length(aoq)))
        expect_equal(ati, rep(lotSize, length(ati)))
      }
    }
  }
})

test_that("Create attribute plan clears an infeasible plan and recovers when the lot size changes", {
  options <- .attributeFeasibilityOptionsForTest()
  options$distribution <- "binomial"
  options$lotSize <- 1000
  original <- get("CreateAttributePlanInternal", asNamespace("jaspAcceptanceSampling"))
  observed <- list()
  wrapper <- function(jaspResults, dataset = NULL, options, ...) {
    private <- jaspResults$.__enclos_env__$private
    capture <- function() {
      private$complete()
      result <- jsonlite::fromJSON(jaspTools:::getJsonResultsFromJaspResults(jaspResults), simplifyVector = FALSE)
      container <- jaspResults[["createContainer"]]
      if (!is.null(container[["aoqCurve"]]))
        result$aoqData <- container[["aoqCurve"]]$plotObject$data
      if (!is.null(container[["atiCurve"]]))
        result$atiData <- container[["atiCurve"]]$plotObject$data
      result
    }
    original(jaspResults, dataset, options, ...)
    observed[[1L]] <<- capture()
    for (lotSize in c(20, 110, 1000)) {
      options$lotSize <- lotSize
      private$changeOptions(as.character(jsonlite::toJSON(options, digits = NA)))
      jaspBase:::emptyRecomputed()
      original(jaspResults, dataset, options, ...)
      observed[[length(observed) + 1L]] <<- capture()
    }
  }
  testthat::with_mocked_bindings(
    jaspTools::runAnalysis("CreateAttributePlan", dataset = NULL, options = options, view = FALSE),
    CreateAttributePlanInternal = wrapper, .package = "jaspAcceptanceSampling")
  expect_length(observed, 4L)
  .attributeExpectInfeasibleForTest(observed[[2L]]$results$createContainer, 110)
  for (i in c(1L, 3L, 4L)) {
    container <- observed[[i]]$results$createContainer
    expect_null(container[["error"]])
    for (key in c("findPlanTable", "findProbTable", "summaryTable", "ocCurve", "aoqCurve", "atiCurve")) {
      output <- container$collection[[paste0("createContainer_", key)]]
      expect_false(is.null(output))
      expect_null(output[["error"]])
      expect_identical(output$status, "complete")
    }
  }
  expect_null(observed[[2L]]$aoqData)
  expect_null(observed[[2L]]$atiData)
  expect_s3_class(observed[[3L]]$aoqData, "data.frame")
  expect_s3_class(observed[[3L]]$atiData, "data.frame")
  expect_true(all(observed[[3L]]$aoqData$AOQ == 0))
  expect_true(all(observed[[3L]]$atiData$ATI == 110))
  expect_equal(observed[[4L]]$aoqData, observed[[1L]]$aoqData)
  expect_equal(observed[[4L]]$atiData, observed[[1L]]$atiData)
  for (key in c("createContainer_findPlanTable", "createContainer_findProbTable", "createContainer_summaryTable"))
    expect_equal(observed[[4L]]$results$createContainer$collection[[key]]$data,
                 observed[[1L]]$results$createContainer$collection[[key]]$data)
})
