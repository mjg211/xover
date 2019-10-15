#' Analyse a binary outcome in a cross-over dataset
#'
#' Facilitates the analysis of binary outcome data in a cross-over dataset.
#'
#' \code{an_bin()} supports the analysis of binary outcome data from a
#' cross-over trial. Precisely, a dataset of class \code{xover_data} is provided
#' (see \code{data}), along with a binary outcome variable within this dataset
#' (see \code{outcome}). The value of \code{method}, and several other input
#' arguments, then determines the type of analysis performed on the data. At
#' present, the following values of \code{method} are supported:
#'
#' \itemize{
#' \item \code{"conditional_logistic"}: To analyse using conditional logistic
#' regression;
#' \item \code{"gee"}: To analyse using a generalised estimating equation;
#' \item \code{"glmm"}: To analyse using a generalised linear mixed model;
#' \item \code{"mainland_gart"}: To analyse using the Mainland-Gart test;
#' \item \code{"mcnemar"}: To analyse using McNemar's test;
#' \item \code{"prescott"}: To analyse using Prescott's test.
#' }
#'
#' See the package vignette for further dettails.
#'
#' Note that datasets can be converted to class \code{xover_data} using
#' \code{\link[xover]{as_xover_data}}.
#'
#' @param data A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @param outcome The chosen binary outcome variable from \code{data} for which
#' the analysis will be performed. Should be stated absolutely, i.e., not
#' as a \code{\link[base]{character}} string.
#' @param method A \code{\link[base]{character}} string indicating which method
#' to use for analysis. Must be one of \code{"conditional_logistic"},
#' \code{"gee"}, \code{"glmm"}, \code{"mainland_gart"}, \code{"mcnemar"}, and
#' \code{"prescott"}.
#' @param alternative \code{\link[base]{character}} string indicating whether to
#' perform a one- or two-sided test. Must be one of \code{"one_sided"} and
#' \code{"two_sided"}.
#' @param complete_case A \code{\link[base]{logical}} variable indicating
#' whether to reduce \code{data} to its complete case form.
#' @param exact A \code{\link[base]{logical}} variable indicating whether to use
#' the exact or approximate version of the chosen method.
#' @param correct Only used in the case that \code{method = "mainland_gart"},
#' where it determines the type of correction to apply to the test statistic.
#' Should be a single \code{\link[base]{numeric}} integer between one and four
#' inclusive. See thepackage vignette for further details.
#' @param conf_level The confidence level to use in confidence interval
#' construction. Should be a single \code{\link[base]{numeric}} strictly between
#' zero and one.
#' @param carryover Only used in the case that \code{method} is one of
#' \code{"conditional_logistic"}, \code{"gee"}, or \code{"glmm"}, when it
#' indicates whether carryover effects should be included in the model. Should
#' be a \code{\link[base]{logical}} variable.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @param ... Additional arguments to pass to \code{\link[elrm]{elrm}} in the
#' case that \code{method = "conditional_logistic"} and \code{exact = T}.
#' @return A \code{\link[base]{list}} or class \code{xover_an} containing in
#' particular a \code{\link[base]{list}} \code{test_results} describing the
#' results of the chosen test.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_data}} for converting datasets to class
#' \code{xover_data}.
#' @export
an_bin <- function(data, outcome, method = "glmm", alternative = "two_sided",
                   complete_case = T, exact = T, correct = 1, conf_level = 0.95,
                   carryover = F, summary = T, ...) {

  ##### Input checking #########################################################

  check_belong(method, "method", c("conditional_logistic", "gee", "glmm",
                                   "mainland_gart", "mcnemar", "prescott"), 1)
  check_logical(complete_case, "complete_case")
  check_logical(summary, "summary")
  check_logical(exact, "exact")
  if (all(any(method %in% c("mcnemar", "mainland_gart", "prescott"),
              all(method == "conditional_logistic", exact)),
          !complete_case)) {
    warning("A complete-case analysis is required with the exact conditional",
            " logistic approach, as well as with McNemar's, the Mainland-Gart,",
            " and Prescott's test. Proceeding with complete case analysis.")
    complete_case    <- T
  }
  if (missing(outcome)) {
    stop("outcome must be supplied.")
  }
  outcome            <- substitute(outcome)
  internal_data      <- data
  if (method %in% c("conditional_logistic", "gee", "glmm")) {
    check_data(internal_data, outcome, "binary", output = F)
  } else {
    check_data(internal_data, outcome, "binary", 2, 2, 2, F)
    sequences        <- levels(internal_data$sequence)
    if (!all(strsplit(sequences[1], split = "")[[1]] ==
             rev(strsplit(sequences[2], split = "")[[1]]))) {
      stop("For chosen method, data must relate to a trial with an AB/BA",
           " type design.")
    }
  }
  if (complete_case) {
    internal_data_cc <- complete_case_data(internal_data, outcome, F)
    if (summary) {
      message("   Converted the data to complete-case form. This removed ",
              nrow(internal_data) - nrow(internal_data_cc),
              " rows from the dataset.")
    }
    if (nrow(internal_data_cc) == 0) {
      stop("The complete-case dataset consists of zero outcomes.")
    }
    internal_data <- internal_data_cc
  }
  check_belong(alternative, "alternative", c("two_sided", "one_sided"), 1)
  if (alternative == "two_sided") {
    alternative   <- "two.sided"
  } else {
    alternative   <- "greater"
  }
  check_belong(correct, "correct", 1:4, 1)
  check_real_range_strict(conf_level, "conf_level", c(0, 1), 1)
  check_logical(carryover, "carryover")
  if (all(method %in% c("mainland_gart", "mcnemar"), !exact,
          alternative == "greater")) {
    warning("One-sided testing cannot be performed using the approximate ",
            "version of McNemar's or the Mainland-Gart test. Proceeding with ",
            "two-sided testing.")
    alternative   <- "two.sided"
  } else if (all(method == "conditional_logistic", exact)) {
    warning("One-sided testing cannot be performed using the exact version of ",
            "conditional logistic regression. Proceeding with two-sided ",
            "testing.")
    alternative   <- "two.sided"
  }
  if (all(method != "mainland_gart", correct != 1)) {
    warning("correct has been changed from default but will not be used based ",
            "on the choice of method.")
  }
  if (all(method %in% c("gee", "glmm"), !exact)) {
    warning("exact has been changed from default but will not be used based on",
            "the choices of method and exact.")
  }
  if (all(carryover, !(method %in% c("conditional_logistic", "gee", "glmm")))) {
    warning("carryover has been changed from default but will not be used",
            " based on the choice of method.")
  }
  if (all(exact, method == "conditional_logistic")) {
    if (!requireNamespace("elrm", quietly = T)) {
      stop("Package \"elrm\" is needed for the exact version of conditional ",
           "logistic regression. Please install it locally by first downloading ",
           "it from https://cran.r-project.org/src/contrib/Archive/elrm/.",
           call. = F)
    }
  }

  #### Print summary ###########################################################

  if (summary) {
    message("\n-----")
    message("Analysis of a binary outcome in cross-over trial data")
    message("-----\n")
    name <- c("conditional logistic regression.", "a GEE.", "a GLMM.",
              "the Mainland-Gart test.", "McNemar's test.",
              "Prescott's test")[which(c("conditional_logistic", "gee", "glmm",
                                          "mainland_gart", "mcnemar",
                                          "prescott") == method)]
    message("   The outcome variable '", deparse(outcome),
            "' will be analysed using ", name)
    if (!(method %in% c("gee", "glmm"))) {
      if (exact) {
        message("\n   The exact version of this test will be used.")
      } else {
        message("\n   The approximate version of this test will be used.")
      }
    }
    if (method == "mainland_gart") {
      if (correct %in% 1:2) {
        message("\n   The point estimate and confidence interval for the ",
                "treatment effect will be calculated with no correction ",
                "applied to the contingency table.")
      } else if (correct == 3) {
        message("\n   The point estimate and confidence interval for the ",
                "treatment effect will be calculated after adding one half ",
                "to all preference cells.")
      } else {
        message("\n   The point estimate and confidence interval for the ",
                "treatment effect will be calculated after adding one half ",
                "to all zero cells.")
      }
      if (!exact) {
        if (correct %in% 1:2) {
          message("\n   A conventional \u03c7\u00B2 test will be conducted, ",
                  "with no correction applied to the contingency table.")
        } else if (correct == 2) {
          message("\n   A conventional \u03c7\u00B2 test will be conducted, ",
                  "with Yates' correction applied to the contingency table.")
        } else if (correct == 3) {
          message("\n   A Wald test will be conducted using the estimate and",
                  " variance obtained after adding one half to all preference",
                  " cells.")
        } else {
          message("\n   A Wald test will be conducted using the estimate and",
                  " variance obtained after adding one half to all zero cells.")
        }
      }
    }
    add_on <- paste(", and ", round(100*conf_level), "% confidence intervals ",
                    "will be constructed where possible.", sep = "")
    if (alternative == "two.sided") {
      message("\n   A two-sided hypothesis test will be conducted", add_on,
              "\n")
    } else{
      message("\n   A one-sided hypothesis test will be conducted", add_on,
              "\n")
    }
  }

  #### Main computations #######################################################

  if (summary) {
    message("   Beginning the analysis. Outputs from functions called",
            " from other packages may follow...")
  }
  contingency_table <- contingency_table_data(internal_data, outcome, F)
  if (method == "mcnemar") {
    test_results    <- an_bin_mcnemar(contingency_table, exact, conf_level,
                                      alternative)
  } else if (method == "mainland_gart") {
    test_results    <- an_bin_mainland_gart(contingency_table, exact, correct,
                                            conf_level, alternative)
  } else if (method == "prescott") {
    test_results    <- an_bin_prescott(contingency_table, exact, alternative)
  } else if (method == "conditional_logistic") {
    test_results    <- an_bin_clogistic(internal_data, outcome, exact,
                                        alternative, conf_level, carryover, ...)
  } else if (method == "glmm") {
    test_results    <- an_bin_glmm(internal_data, outcome, alternative,
                                   conf_level, carryover)
  } else if (method == "gee") {
    test_results    <- an_bin_gee(internal_data, outcome, alternative,
                                  conf_level, carryover)
  }
  if (summary) {
    message("...completed the analysis. Preparing outputs...")
  }
  if (alternative == "two.sided") {
    alternative     <- "two_sided"
  } else {
    alternative     <- "one_sided"
  }

  ##### Outputting #############################################################

  output            <- list(test_results = test_results,
                           contingency_table = contingency_table, data = data,
                           outcome = outcome, method = method,
                           alternative = alternative,
                           complete_case = complete_case,
                           exact = exact, correct = correct,
                           conf_level = conf_level, carryover = carryover,
                           summary = summary)
  class(output)     <- c(class(output), "xover_an")
  if (summary) {
    message("...outputting.")
  }
  return(output)

}
