
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BayesianSSMforAS <- function(jaspResults, dataset = NULL, options, ...) {
  bayesianSSMforASInternal(jaspResults, dataset, options)
}

# Backward-compatible alias.
bayesianSSMforAS <- BayesianSSMforAS

bayesianSSMforASInternal <- function(jaspResults, dataset = NULL, options) {

  dataset <- .asSSM_readData(dataset, options)

  ready <- .asSSMReady(options, dataset)

  jaspResults[["placeholder"]] <- NULL

  if (ready)
    .asSSM_fitModel(jaspResults, dataset, options)

  if (!is.null(jaspResults[["placeholder"]]))
    return()

  .asSSM_createSummaryTable(jaspResults, dataset, options)
  .asSSM_createMcmcSummaryTable(jaspResults, dataset, options)

  if (ready && options[["ssm_statePlot"]])
    .asSSM_plotState(jaspResults, dataset, options)

  if (ready && options[["ssm_posteriorDistPlot"]])
    .asSSM_plotPosteriorDist(jaspResults, dataset, options)

  if (ready && options[["ssm_predictionPlot"]])
    .asSSM_plotPrediction(jaspResults, dataset, options)

  if (ready && options[["ssm_plotBeta"]])
    .asSSM_plotBetaCoefficient(jaspResults, dataset, options)
}

.asSSM_setAnalysisError <- function(jaspResults, message) {
  errorContainer <- createJaspContainer(title = "")
  errorContainer$setError(message)
  errorContainer$position <- 0
  jaspResults[["placeholder"]] <- errorContainer
}

.asSSMReady <- function(options, dataset) {
  if (options[["ssm_count"]] == "" || options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" || options[["ssm_time"]] == "") return(FALSE)
  if (options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] == "") return(FALSE)
  return(TRUE)
}

.asSSM_readData <- function(dataset, options) {

  if (options[["ssm_count"]] == "" ||
      options[["ssm_total"]] == "" ||
      options[["ssm_sampleSize"]] == "" ||
      options[["ssm_time"]] == "") {
    return(dataset)
  }

  colsNumeric <- c()
  if (options[["ssm_count"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_count"]])
  if (options[["ssm_total"]]      != "") colsNumeric <- c(colsNumeric, options[["ssm_total"]])
  if (options[["ssm_sampleSize"]] != "") colsNumeric <- c(colsNumeric, options[["ssm_sampleSize"]])
  if (options[["ssm_time"]]       != "") colsNumeric <- c(colsNumeric, options[["ssm_time"]])
  if (options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != "")
    colsNumeric <- c(colsNumeric, options[["ssm_predictor"]])

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = colsNumeric)
  }

  # A preloaded dataset gives an ordinal or nominal column (e.g. an integer Lot Number) as a factor:
  # use the values of its labels, not the factor codes.
  if (!is.null(dataset)) {
    for (col in intersect(colsNumeric, colnames(dataset))) {
      if (is.factor(dataset[[col]]))
        dataset[[col]] <- suppressWarnings(as.numeric(as.character(dataset[[col]])))
    }
  }

  if (!is.null(dataset) && options[["ssm_time"]] != "" && options[["ssm_time"]] %in% colnames(dataset)) {
    dataset <- dataset[order(dataset[[options[["ssm_time"]]]]), , drop = FALSE]
  }

  return(dataset)
}

.asSSM_isWholeNumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

.asSSM_validateInputs <- function(dataset, options) {
  if (is.null(dataset) || nrow(dataset) == 0L) {
    return(gettext("The dataset is empty."))
  }

  usePredictor <- options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != ""
  requiredCols <- c(
    options[["ssm_count"]],
    options[["ssm_sampleSize"]],
    options[["ssm_total"]],
    options[["ssm_time"]]
  )
  if (usePredictor)
    requiredCols <- c(requiredCols, options[["ssm_predictor"]])
  requiredCols <- unique(requiredCols[nzchar(requiredCols)])

  missingCols <- setdiff(requiredCols, names(dataset))
  if (length(missingCols) > 0L) {
    return(gettextf(
      "The following assigned variables are missing from the dataset: %1$s.",
      paste(missingCols, collapse = ", ")
    ))
  }

  count <- dataset[[options[["ssm_count"]]]]
  sampleSize <- dataset[[options[["ssm_sampleSize"]]]]
  lotSize <- dataset[[options[["ssm_total"]]]]
  time <- dataset[[options[["ssm_time"]]]]

  validateNumeric <- function(x, label, mustBeWhole = FALSE, mustBeNonNegative = FALSE) {
    if (any(!is.finite(x))) {
      return(gettextf("%1$s cannot contain missing or non-finite values.", label))
    }
    if (mustBeWhole && any(!.asSSM_isWholeNumber(x))) {
      return(gettextf("%1$s must contain whole numbers only.", label))
    }
    if (mustBeNonNegative && any(x < 0)) {
      return(gettextf("%1$s cannot contain negative values.", label))
    }
    NULL
  }

  msg <- validateNumeric(count, gettext("Defect Count"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(sampleSize, gettext("Sample Size"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(lotSize, gettext("Lot Size"), mustBeWhole = TRUE, mustBeNonNegative = TRUE)
  if (!is.null(msg)) return(msg)

  msg <- validateNumeric(time, gettext("Time"))
  if (!is.null(msg)) return(msg)

  if (usePredictor) {
    predictor <- dataset[[options[["ssm_predictor"]]]]
    msg <- validateNumeric(predictor, gettext("Predictor"))
    if (!is.null(msg)) return(msg)
    predictorSD <- stats::sd(predictor)
    if (!is.finite(predictorSD) || predictorSD <= .Machine$double.eps^0.5) {
      return(gettext("Predictor must vary across lots when using the predictor model."))
    }
  }

  if (any(count > sampleSize)) {
    return(gettext("Observed defects cannot exceed the sample size for any lot."))
  }

  if (any(sampleSize > lotSize)) {
    return(gettext("Sample size cannot exceed the lot size for any lot."))
  }

  NULL
}

.asSSM_fitModel <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["fit"]])) {
    fitContainer <- createJaspState()
    fitContainer$dependOn(c(
      "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcBurnin",
      "ssm_advancedMcmcChains", "ssm_advancedMcmcSamples",
      "ssm_advancedMcmcThin", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))
    jaspResults[["fit"]] <- fitContainer
  }

  if (!is.null(jaspResults[["fit"]]$object)) return()

  validationError <- .asSSM_validateInputs(dataset, options)
  if (!is.null(validationError)) {
    .asSSM_setAnalysisError(jaspResults, validationError)
    return()
  }

  stanData <- list(
    T = nrow(dataset),
    n = as.integer(dataset[[options[["ssm_sampleSize"]]]]),  # tested items
    s = as.integer(dataset[[options[["ssm_total"]]]]),       # lot size
    y = as.integer(dataset[[options[["ssm_count"]]]]),       # defects in sample
    prior_theta1_a = options[["ssm_priorTheta1Shape1"]],
    prior_theta1_b = options[["ssm_priorTheta1Shape2"]],
    prior_sigma_sd = options[["ssm_priorSigmaSD"]]
  )

  usePredictor <- options[["ssm_scaleModel"]] == "WithPredictor" && options[["ssm_predictor"]] != ""

  if (usePredictor) {
    stanData$temperature      <- as.numeric(scale(dataset[[options[["ssm_predictor"]]]]))
    stanData$prior_beta_scale <- options[["ssm_priorBetaScale"]]
    model_obj <- jaspAcceptanceSampling:::.asSSM_getStanModel("complex")
  } else {
    model_obj <- jaspAcceptanceSampling:::.asSSM_getStanModel("simple")
  }

  thisSeed  <- options[["ssm_advancedMcmcSeed"]]
  thisDelta <- options[["ssm_advancedMcmcAdaptDelta"]]
  thisDepth <- options[["ssm_advancedMcmcMaxTreeDepth"]]
  thisChains <- as.integer(options[["ssm_advancedMcmcChains"]])

  availableCores <- suppressWarnings(parallel::detectCores())
  if (!is.finite(availableCores) || availableCores < 1)
    availableCores <- 1L
  thisCores <- min(thisChains, as.integer(availableCores))

  controlList <- list()
  if (is.numeric(thisDelta) && length(thisDelta) == 1L &&
      is.finite(thisDelta) && thisDelta > 0 && thisDelta < 1)
    controlList$adapt_delta <- thisDelta

  if (is.numeric(thisDepth) && length(thisDepth) == 1L &&
      is.finite(thisDepth) && thisDepth > 0)
    controlList$max_treedepth <- as.integer(thisDepth)

  jaspBase::startProgressbar(1, gettext("Sampling..."))

  tryCatch({
    stanFit <- rstan::sampling(
      object     = model_obj,
      data       = stanData,
      chains     = thisChains,
      iter       = options[["ssm_advancedMcmcBurnin"]] + options[["ssm_advancedMcmcSamples"]],
      warmup     = options[["ssm_advancedMcmcBurnin"]],
      thin       = options[["ssm_advancedMcmcThin"]],
      cores      = thisCores,
      refresh    = 0,
      seed       = thisSeed,
      control    = controlList
    )

    samples <- rstan::extract(stanFit)
    summary <- rstan::summary(stanFit)$summary

    jaspResults[["fit"]]$object <- list(
      samples = samples,
      summary = summary
    )

  }, error = function(e) {
    msg <- gettextf("Estimation failed: %s", e$message)
    .asSSM_setAnalysisError(jaspResults, msg)
  })

  jaspBase::progressbarTick()
}


.asSSM_createSummaryTable <- function(jaspResults, dataset, options) {
  table <- jaspResults[["summaryTable"]]
  if (is.null(table)) {
    table <- createJaspTable(title = gettext("Posterior Summary (Current Lot)"))
    table$dependOn(c(
      "ssm_postHocCorrection", "ssm_postHocCorrectionWeight",
      "ssm_controlLimitsLower", "ssm_controlLimitsUpper",
      "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcBurnin", "ssm_advancedMcmcSamples",
      "ssm_advancedMcmcChains", "ssm_advancedMcmcThin",
      "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth",
      "ssm_decisionAcceptThreshold", "ssm_decisionRejectThreshold"
    ))
    table$position <- 0

    table$addColumnInfo(name = "t",        title = gettext("Lot Nr."),              type = "number")
    table$addColumnInfo(name = "y",        title = gettext("Obs. Defects"),         type = "integer")
    table$addColumnInfo(name = "pred",     title = gettext("Median Pred. Defects"), type = "number")
    table$addColumnInfo(name = "massAQL",  title = gettext("Mass < AQL"),           type = "number", format = "dp:3")
    table$addColumnInfo(name = "massRQL",  title = gettext("Mass > RQL"),           type = "number", format = "dp:3")
    table$addColumnInfo(name = "decision", title = gettext("Decision"),                 type = "string")

    jaspResults[["summaryTable"]] <- table
  }

  table$setData(list(
    t = integer(0),
    y = integer(0),
    pred = numeric(0),
    massAQL = numeric(0),
    massRQL = numeric(0),
    decision = character(0)
  ))

  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  samples <- fit$object$samples

  T_idx <- nrow(dataset)
  tT <- dataset[[options[["ssm_time"]]]][T_idx]
  sT <- dataset[[options[["ssm_total"]]]][T_idx]        # total lot size
  nT <- dataset[[options[["ssm_sampleSize"]]]][T_idx]   # sample size
  yT <- dataset[[options[["ssm_count"]]]][T_idx]

  if (is.na(tT) || is.na(sT) || is.na(yT)) {
    table$setError(gettext("The Lot Number, Lot Size, or Defect Count is missing (NA) for the last lot."))
    return()
  }

  y_pred <- samples$y_pred
  pmf <- .asSSM_predictivePMF(y_pred, sT)

  if (options[["ssm_postHocCorrection"]]) {
    pmf <- .asSSM_correctedPredictivePMF(y_pred, sT, options[["ssm_postHocCorrectionWeight"]])
  }

  limit_AQL <- options[["ssm_controlLimitsLower"]] * sT
  limit_RQL <- options[["ssm_controlLimitsUpper"]] * sT

  mass_below_AQL <- .asSSM_pmfMassBelow(pmf, limit_AQL)
  mass_above_RQL <- .asSSM_pmfMassAbove(pmf, limit_RQL)

  acceptThreshold <- options[["ssm_decisionAcceptThreshold"]]
  rejectThreshold <- options[["ssm_decisionRejectThreshold"]]

  decision <- gettext("Consider to Continue")
  if (!is.na(mass_above_RQL) && mass_above_RQL < acceptThreshold) {
    decision <- gettext("Consider to Accept")
  } else if (!is.na(mass_above_RQL) && !is.na(mass_below_AQL) &&
             mass_above_RQL > acceptThreshold && mass_below_AQL < rejectThreshold) {
    decision <- gettext("Consider to Reject")
  }

  table$setData(list(
    t = tT,
    y = yT,
    pred = .asSSM_pmfMedian(pmf),
    massAQL = mass_below_AQL,
    massRQL = mass_above_RQL,
    decision = decision
  ))
}

.asSSM_plotState <- function(jaspResults, dataset, options) {
  plot <- jaspResults[["ssm_statePlot"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Defect Rate Over Time"), width = 600, height = 400)
    plot$dependOn(c(
      "ssm_statePlot",
      "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))
    plot$position <- 1
    jaspResults[["ssm_statePlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta <- samples$theta * 100
  timeValues <- dataset[[options[["ssm_time"]]]]

  df <- data.frame(
    time  = timeValues,
    mean  = colMeans(theta),
    lower = apply(theta, 2, quantile, 0.025),
    upper = apply(theta, 2, quantile, 0.975)
  )

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$time)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df$lower, df$upper))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = "#c2c2c2", alpha = 0.6) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      x = gettext("Lot"),
      y = gettextf("Defect Rate (%%)")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotPrediction <- function(jaspResults, dataset, options) {
  depVars <- c(
    "ssm_predictionPlot",
    "ssm_postHocCorrection", "ssm_postHocCorrectionWeight",
    "ssm_count", "ssm_total", "ssm_sampleSize",
    "ssm_controlLimitsLower", "ssm_controlLimitsUpper",
    "ssm_scaleModel", "ssm_predictor",
    "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
    "ssm_priorSigmaSD", "ssm_priorBetaScale",
    "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
    "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
  )

  plot <- jaspResults[["predPlot"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Posterior Predictive"), width = 500, height = 400)
    plot$dependOn(depVars)
    plot$position <- 2
    jaspResults[["predPlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  y_pred_raw <- samples$y_pred

  sT <- dataset[[options[["ssm_total"]]]][nrow(dataset)]
  pmf <- .asSSM_predictivePMF(y_pred_raw, sT)
  if (options[["ssm_postHocCorrection"]])
    pmf <- .asSSM_correctedPredictivePMF(y_pred_raw, sT, options[["ssm_postHocCorrectionWeight"]])

  limit_AQL <- options[["ssm_controlLimitsLower"]] * sT
  limit_RQL <- options[["ssm_controlLimitsUpper"]] * sT

  # Posterior probabilities for AQL / RQL regions
  mass_below_AQL <- .asSSM_pmfMassBelow(pmf, limit_AQL)
  mass_above_RQL <- .asSSM_pmfMassAbove(pmf, limit_RQL)

  # Plot range is based on the uncorrected predictive draws so the distribution
  # remains visible even when correction adds diffuse mass across the full lot size.
  raw_q <- stats::quantile(y_pred_raw, probs = c(0.001, 0.999), na.rm = TRUE, names = FALSE)
  x_min <- floor(min(raw_q[1], limit_AQL, limit_RQL))
  x_max <- ceiling(max(raw_q[2], limit_AQL, limit_RQL))
  x_min <- max(0, x_min)
  if (!is.finite(x_max) || x_max <= x_min)
    x_max <- x_min + 1

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(x_min, x_max, limit_AQL, limit_RQL))
  if (length(xBreaks) < 2L)
    xBreaks <- c(x_min, x_max)
  xLower <- min(xBreaks)
  xUpper <- max(xBreaks)

  # Aggregate the deterministic PMF into coarser display bins to keep the
  # predictive plot visually similar to the original histogram-style output.
  pmf_plot <- pmf[pmf$defects >= xLower & pmf$defects <= xUpper, , drop = FALSE]
  if (nrow(pmf_plot) == 0L)
    pmf_plot <- pmf

  breaks <- seq(xLower, xUpper, length.out = 31)
  bin_ids <- cut(pmf_plot$defects, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
  mids <- head(breaks, -1L) + diff(breaks) / 2
  probs <- tapply(pmf_plot$prob, bin_ids, sum)
  df_hist <- data.frame(
    x = mids,
    prob = 0
  )
  if (length(probs) > 0L)
    df_hist$prob[as.integer(names(probs))] <- as.numeric(probs)

  binWidth <- if (length(breaks) > 1L) (breaks[2] - breaks[1]) else 1

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(df_hist$prob)))

  p <- ggplot2::ggplot(df_hist, ggplot2::aes(x = .data$x, y = .data$prob)) +
    ggplot2::geom_col(width = binWidth, fill = "#595959", colour = "white", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = limit_AQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = limit_RQL, linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = limit_AQL, y = max(df_hist$prob),
                      label = "AQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = limit_RQL, y = max(df_hist$prob),
                      label = "RQL", vjust = -0.5, hjust = -0.1, size = 3) +
    ggplot2::labs(
      x = gettext("Defects in Current Lot"),
      y = gettext("Posterior Predictive Probability")
    ) +
    ggplot2::scale_x_continuous(
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    ggplot2::scale_y_continuous(
      breaks = yBreaks,
      limits = range(yBreaks)
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p

  if (isTRUE(options[["ssm_postHocCorrection"]])) {
    jaspResults[["predPlotNote"]] <- createJaspHtml(
      text = gettext("Note. Tail correction adds a uniform sudden-jump component over the full lot-size range (0 to N), so this figure is zoomed to the uncorrected predictive range to keep the posterior shape readable."),
      position = 2.1,
      dependencies = depVars
    )
  } else {
    jaspResults[["predPlotNote"]] <- NULL
  }
}


.asSSM_plotPosteriorDist <- function(jaspResults, dataset, options) {
  plot <- jaspResults[["postDist"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Posterior Defect Rate"), width = 500, height = 400)
    plot$dependOn(c(
      "ssm_posteriorDistPlot",
      "ssm_count", "ssm_total", "ssm_sampleSize",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))
    plot$position <- 3
    jaspResults[["postDist"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  theta   <- samples$theta
  df    <- data.frame(x = theta[, ncol(theta)] * 100)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df$x)

  densVals <- stats::density(df$x)
  yBreaks  <- jaspGraphs::getPrettyAxisBreaks(c(0, max(densVals$y)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#b3b3b3") +
    ggplot2::labs(
      x = gettextf("Defect Rate (%%)"),
      y = gettext("Density")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_plotBetaCoefficient <- function(jaspResults, dataset, options) {
  if (options[["ssm_scaleModel"]] != "WithPredictor") return()
  plot <- jaspResults[["betaPlot"]]
  if (is.null(plot)) {
    plot <- createJaspPlot(title = gettext("Beta Coefficient"), width = 500, height = 400)
    plot$dependOn(c(
      "ssm_plotBeta",
      "ssm_predictor",
      "ssm_count", "ssm_total", "ssm_sampleSize",
      "ssm_scaleModel",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcSamples", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))

    plot$position <- 4
    jaspResults[["betaPlot"]] <- plot
  }

  if (is.null(jaspResults[["fit"]]$object)) return()

  samples <- jaspResults[["fit"]]$object$samples
  beta    <- samples$beta_temperature

  df      <- data.frame(x = beta)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(beta, 0))

  densVals <- stats::density(beta)
  yBreaks  <- jaspGraphs::getPrettyAxisBreaks(c(0, max(densVals$y)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_density(fill = "#b3b3b3") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
    ggplot2::labs(
      x = gettext("Predictor Coefficient \u03B2"),
      y = gettext("Density")
    ) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
}

.asSSM_predictivePMF <- function(y_pred_draws, sT) {
  sMax <- floor(sT)
  if (!is.finite(sMax) || sMax < 0) {
    return(data.frame(defects = integer(0), prob = numeric(0)))
  }

  draws <- as.numeric(y_pred_draws)
  draws <- draws[is.finite(draws)]
  if (length(draws) == 0L) {
    return(data.frame(defects = 0:sMax, prob = rep(0, sMax + 1L)))
  }

  draws <- as.integer(round(draws))
  draws <- pmin(pmax(draws, 0L), as.integer(sMax))

  probs <- tabulate(draws + 1L, nbins = sMax + 1L)
  probs <- probs / sum(probs)

  data.frame(defects = 0:sMax, prob = probs)
}

.asSSM_correctedPredictivePMF <- function(y_pred_draws, sT, weightPercent = 0) {
  pmf <- .asSSM_predictivePMF(y_pred_draws, sT)
  if (nrow(pmf) == 0L)
    return(pmf)

  x <- weightPercent / 100
  if (!is.finite(x))
    x <- 0
  x <- max(0, min(1, x))

  pmf$prob <- (1 - x) * pmf$prob + x / nrow(pmf)
  pmf
}

.asSSM_pmfMassBelow <- function(pmf, threshold) {
  if (nrow(pmf) == 0L)
    return(NA_real_)
  sum(pmf$prob[pmf$defects < threshold])
}

.asSSM_pmfMassAbove <- function(pmf, threshold) {
  if (nrow(pmf) == 0L)
    return(NA_real_)
  sum(pmf$prob[pmf$defects > threshold])
}

.asSSM_pmfMedian <- function(pmf) {
  if (nrow(pmf) == 0L)
    return(NA_real_)

  cumProb <- cumsum(pmf$prob)
  idx <- which(cumProb >= 0.5)[1]
  if (is.na(idx))
    return(NA_real_)

  pmf$defects[idx]
}

.asSSM_createMcmcSummaryTable <- function(jaspResults, dataset, options) {

  if (!isTRUE(options[["ssm_showMcmcSummary"]])) return()

  table <- jaspResults[["mcmcSummary"]]
  if (is.null(table)) {
    table <- createJaspTable(title = gettext("MCMC Diagnostics"))
    table$dependOn(c(
      "ssm_showMcmcSummary",
      "ssm_showTheta", "ssm_showSigma", "ssm_showYpred", "ssm_showBeta",
      "ssm_scaleModel", "ssm_predictor",
      "ssm_count", "ssm_total", "ssm_sampleSize", "ssm_time",
      "ssm_priorTheta1Shape1", "ssm_priorTheta1Shape2",
      "ssm_priorSigmaSD", "ssm_priorBetaScale",
      "ssm_advancedMcmcBurnin", "ssm_advancedMcmcSamples",
      "ssm_advancedMcmcChains", "ssm_advancedMcmcThin", "ssm_advancedMcmcSeed",
      "ssm_advancedMcmcAdaptDelta", "ssm_advancedMcmcMaxTreeDepth"
    ))
    table$position <- 5   # after the other tables/plots
    table$addColumnInfo(name = "parameter", title = gettext("Parameter"), type = "string")
    table$addColumnInfo(name = "mean",      title = gettext("Mean"),      type = "number")
    table$addColumnInfo(name = "sd",        title = gettext("SD"),        type = "number")
    table$addColumnInfo(name = "n_eff",     title = gettext("ESS"),       type = "number")
    table$addColumnInfo(name = "Rhat",      title = gettext("R-hat"),     type = "number")

    jaspResults[["mcmcSummary"]] <- table
  }

  table$setData(list(
    parameter = character(0),
    mean = numeric(0),
    sd = numeric(0),
    n_eff = numeric(0),
    Rhat = numeric(0)
  ))

  fit <- jaspResults[["fit"]]
  if (is.null(fit) || is.null(fit$object)) return()

  sm <- fit$object$summary
  allNames <- rownames(sm)
  pars <- character(0)

  if (isTRUE(options[["ssm_showTheta"]])) {
    pars <- c(pars, grep("^theta\\[", allNames, value = TRUE))
  }
  if (isTRUE(options[["ssm_showSigma"]])) {
    if ("sigma_eta" %in% allNames)
      pars <- c(pars, "sigma_eta")
  }
  if (isTRUE(options[["ssm_showYpred"]])) {
    if ("y_pred" %in% allNames)
      pars <- c(pars, "y_pred")
  }
  if (options[["ssm_scaleModel"]] == "WithPredictor" && isTRUE(options[["ssm_showBeta"]])) {
    if ("beta_temperature" %in% allNames)
      pars <- c(pars, "beta_temperature")
  }

  if (length(pars) == 0L) {
    table$setError(gettext("No parameters selected for the MCMC diagnostics table."))
    return()
  }

  smSel <- sm[pars, , drop = FALSE]

  table$setData(list(
    parameter = rownames(smSel),
    mean = smSel[, "mean"],
    sd = smSel[, "sd"],
    n_eff = smSel[, "n_eff"],
    Rhat = smSel[, "Rhat"]
  ))
}
