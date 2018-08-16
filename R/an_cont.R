#' Analyse a continuous outcome in a cross-over dataset
#'
#' Facilitates the analysis of continuous outcome data in a cross-over dataset.
#'
#' \code{an_cont()} supports the analysis of continuous outcome data from a
#' cross-over trial. Precisely, a dataset of class \code{xover_data} is provided
#' (see \code{data}), along with a continuous outcome variable within this
#' dataset (see \code{outcome}). The value of \code{method}, and several other
#' input arguments, then determines the type of analysis performed on the data.
#' At present, the following values of \code{method} are supported:
#'
#' \itemize{
#' \item \code{"anova"}: To analyse using an analysis of variance approach;
#' \item \code{"lmm"}: To analyse using a linear mixed model;
#' \item \code{"t_test"}: To analyse using t-tests.
#' }
#'
#' See the package vignette for further dettails.
#'
#' Note that datasets can be converted to class \code{xover_data} using
#' \code{\link[xover]{as_xover_data}}.
#'
#' @param data A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @param outcome The chosen continuous outcome variable from \code{data} for
#' which the analysis will be performed. Should be stated absolutely, i.e., not
#' as a \code{\link[base]{character}} string.
#' @param method A \code{\link[base]{character}} string indicating which method
#' to use for analysis. Must be one of \code{"anova"}, \code{"lmm"}, and
#' \code{"t_test"}.
#' @param alternative \code{\link[base]{character}} string indicating whether to
#' perform a one- or two-sided test. Must be one of \code{"one_sided"} and
#' \code{"two_sided"}.
#' @param complete_case A \code{\link[base]{logical}} variable indicating
#' whether to reduce \code{data} to its complete case form.
#' @param conf_level The confidence level to use in confidence interval
#' construction. Should be a single \code{\link[base]{numeric}} strictly between
#' zero and one.
#' @param carryover Only used in the case that \code{method} is
#' \code{"lmm"}, when it indicates whether carryover effects should be included
#' in the model. Should be a \code{\link[base]{logical}} variable.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[base]{list}} or class \code{xover_an} containing in
#' particular a \code{\link[base]{list}} \code{test_results} describing the
#' results of the chosen test.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_data}} for converting datasets to class
#' \code{xover_data}.
#' @export
an_cont <- function(data, outcome, method = "lmm", alternative = "two_sided",
                   complete_case = T, conf_level = 0.95, carryover = F,
                   summary = T, ...) {

  ##### Input checking #########################################################

  check_belong(method, "method", c("anova", "lmm", "t_test"), 1)
  check_logical(complete_case, "complete_case")
  check_logical(summary, "summary")
  if (all(method != "lmm", !complete_case)) {
    warning("A complete-case analysis is required with the ANOVA and t-test
            approaches. Proceeding with complete case analysis.")
    complete_case    <- T
  }
  if (missing(outcome)) {
    stop("outcome must be supplied.")
  }
  outcome            <- substitute(outcome)
  internal_data      <- data
  if (method == "lmm") {
    check_data(internal_data, outcome, "continuous", output = F)
  } else {
    check_data(internal_data, outcome, "continuous", 2, 2, 2, F)
    sequences        <- levels(internal_data$sequence)
    if (!all(strsplit(sequences[1], split = "")[[1]] ==
             rev(strsplit(sequences[2], split = "")[[1]]))) {
      stop("For chosen method, data must relate to a trial with an AB/BA",
           " type design.")
    }
  }
  if (complete_case) {
    internal_data_cc <- internal_complete_case_data(internal_data, outcome)
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
  check_real_range_strict(conf_level, "conf_level", c(0, 1), 1)
  check_logical(carryover, "carryover")
  if (all(method == "anova", alternative == "one_sided")) {
    warning("One-sided testing cannot be performed using the ANOVA approach.",
            " Proceeding with two-sided testing.")
    alternative   <- "two_sided"
  }
  if (all(carryover, method != "lmm")) {
    warning("carryover has been changed from default but will not be used",
            " based on the choice of method.")
  }

  #### Print summary ###########################################################

  if (summary) {
    message("\n-----")
    message("Analysis of a continuous outcome in cross-over trial data")
    message("-----\n")
    name <- c("ANOVA.", "a LMM.",
              "t-tests.")[which(c("anova","lmm", "t_test") == method)]
    message("   The outcome variable '", deparse(outcome),
            "' will be analysed using ", name)
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
    message("   Beginning the analysis...")
  }
  if (method == "anova") {
    test_results <- an_cont_anova(internal_data, outcome)
  } else if (method == "lmm") {
    test_results <- an_cont_lmm(internal_data, outcome, alternative,
                                conf_level, carryover)
  } else {
    test_results <- an_cont_t_test(internal_data, outcome, alternative,
                                   conf_level)
  }
  if (summary) {
    message("...completed the analysis. Preparing outputs...")
  }

  ##### Outputting #############################################################

  output         <- list(test_results = test_results, data = data,
                         outcome = outcome, method = method,
                         alternative = alternative,
                         complete_case = complete_case, conf_level = conf_level,
                         carryover = carryover, summary = summary)
  class(output)  <- c(class(output), "xover_an")
  if (summary) {
    message("...outputting.")
  }
  return(output)

}
