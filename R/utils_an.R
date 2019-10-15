an_bin_clogistic <- function(data, outcome, exact, alternative, conf_level,
                             carryover, ...) {
  if (deparse(outcome) != "outcome") {
    data$outcome       <- eval(outcome, data)
  }
  if (all(carryover, !("carryover" %in% colnames(data)))) {
    data               <- add_carryover_data(data, F)
  }
  if (exact) {
    data$n             <- 1
    data$outcome       <- as.numeric(as.character(data$outcome))
    if (carryover) {
      test             <- elrm::elrm(formula  = outcome/n ~ period + treatment +
                                                              carryover +
                                                              subject,
                                     interest = ~ period + treatment +
                                                    carryover,
                                     dataset  = data, ...)
    } else {
      test             <- elrm::elrm(formula  = outcome/n ~ period + treatment +
                                                              subject,
                                     interest = ~ period + treatment,
                                     dataset  = data, ...)
    }
    estimate           <- test$coeffs[which(names(test$coeffs) != "joint")]
    p_value            <- test$p.values[which(names(test$coeffs) != "joint")]
    return(list(estimate = estimate, p_value = p_value))
  } else {
    if (carryover) {
      test             <- Epi::clogistic(formula = outcome ~ period +
                                                               treatment +
                                                               carryover,
                                         strata = subject, data = internal_data)
    } else {
      test             <- Epi::clogistic(formula = outcome ~ period + treatment,
                                         strata = subject, data = internal_data)
    }
    estimate           <- test$coefficients
    cov_estimate       <- test$var
    se_estimate        <- sqrt(diag(cov_estimate))
    statistic          <- estimate/se_estimate
    conf_int           <- matrix(0, nrow = 2, ncol = length(estimate))
    for (i in 1:length(estimate)) {
      conf_int[, i]    <- estimate[i] + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
                                          sqrt(se_estimate[i])
    }
    conf_int           <- tibble::as_tibble(conf_int)
    colnames(conf_int) <- names(estimate)
    if (alternative == "two.sided") {
      p_value          <- stats::pnorm(abs(statistic), lower.tail = F) +
                            stats::pnorm(-abs(statistic), lower.tail = T)
    } else {
      p_value          <- stats::pnorm(statistic, lower.tail = F)
    }
    return(list(conf_int = conf_int, cov_estimate = cov_estimate,
                estimate = estimate, p_value = p_value,
                se_estimate = se_estimate, statistic = statistic))
  }
}

an_bin_gee <- function(data, outcome, alternative, conf_level, carryover) {
  if (deparse(outcome) != "outcome") {
    data$outcome     <- eval(outcome, data)
  }
  if (all(carryover, !("carryover" %in% colnames(data)))) {
    data             <- add_carryover_data(data, F)
  }
  if (carryover) {
    test             <- gee::gee(formula = outcome ~ period + treatment +
                                   carryover,
                                 id = subject, data = data, family = binomial)
  } else {
    test             <- gee::gee(formula = outcome ~ period + treatment,
                                 id = subject, data = data, family = binomial)
  }
  estimate           <- test$coefficients
  cov_estimate       <- test$robust.variance
  se_estimate        <- sqrt(diag(cov_estimate))
  statistic          <- estimate/se_estimate
  conf_int           <- matrix(0, nrow = 2, ncol = length(estimate))
  for (i in 1:length(estimate)) {
    conf_int[, i]    <- estimate[i] + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
      sqrt(se_estimate[i])
  }
  conf_int           <- tibble::as_tibble(conf_int)
  colnames(conf_int) <- names(estimate)
  if (alternative == "two.sided") {
    p_value          <- stats::pnorm(abs(statistic), lower.tail = F) +
      stats::pnorm(-abs(statistic), lower.tail = T)
  } else {
    p_value          <- stats::pnorm(statistic, lower.tail = F)
  }
  return(list(conf_int = conf_int, cov_estimate = cov_estimate,
              estimate = estimate, p_value = p_value, se_estimate = se_estimate,
              statistic = statistic))
}

an_bin_glmm <- function(data, outcome, alternative, conf_level, carryover) {
  if (deparse(outcome) != "outcome") {
    data$outcome     <- eval(outcome, data)
  }
  if (all(carryover, !("carryover" %in% colnames(data)))) {
    data             <- add_carryover_data(data, F)
  }
  if (carryover) {
    test_full        <- lme4::glmer(formula = outcome ~ treatment + period +
                                                          carryover +
                                                          (1|subject),
                                    family = binomial, data = data)
    test_reduced_tr  <- lme4::glmer(formula = outcome ~ period + carryover +
                                                          (1|subject),
                                    family = binomial, data = data)
    test_reduced_per <- lme4::glmer(formula = outcome ~ treatment + carryover +
                                                          (1|subject),
                                    family = binomial, data = data)
    test_reduced_co  <- lme4::glmer(formula = outcome ~ treatment + period +
                                                          (1|subject),
                                    family = binomial, data = data)
  } else {
    test_full        <- lme4::glmer(formula = outcome ~ treatment + period +
                                                          (1|subject),
                                    family = binomial, data = data)
    test_reduced_tr  <- lme4::glmer(formula = outcome ~ period + (1|subject),
                                    family = binomial, data = data)
    test_reduced_per <- lme4::glmer(formula = outcome ~ treatment + (1|subject),
                                    family = binomial, data = data)
  }
  estimate           <- lme4::fixef(test_full)
  cov_estimate       <- as.matrix(lme4::vcov.merMod(test_full))
  se_estimate        <- sqrt(diag(cov_estimate))
  conf_int           <- matrix(0, nrow = 2, ncol = length(estimate))
  for (i in 1:length(estimate)) {
    conf_int[, i]    <- estimate[i] + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
                                        sqrt(se_estimate[i])
  }
  conf_int           <- tibble::as_tibble(conf_int)
  colnames(conf_int) <- names(estimate)
  statistic          <- estimate/se_estimate
  if (alternative == "two.sided") {
    p_value          <- stats::pnorm(abs(statistic), lower.tail = F) +
                          stats::pnorm(-abs(statistic), lower.tail = T)
  } else {
    p_value          <- stats::pnorm(statistic, lower.tail = F)
  }
  df_full            <- attr(logLik(test_full), "df")
  df_tr              <- attr(logLik(test_reduced_tr), "df")
  df_per             <- attr(logLik(test_reduced_per), "df")
  diff_l_tr          <- as.numeric(-2*(logLik(test_reduced_tr) -
                                         logLik(test_full)))
  diff_l_per         <- as.numeric(-2*(logLik(test_reduced_per) -
                                         logLik(test_full)))
  statistic_global   <- c("period" = diff_l_per, "treatment" = diff_l_tr)
  df_global          <- df_full - c(df_per, df_tr)
  if (carryover) {
    df_global        <- c(df_global,
                          df_full - attr(logLik(test_reduced_co), "df"))
    statistic_global <- c(statistic_global,
                          "carryover" = as.numeric(-2*(logLik(test_reduced_co) -
                                                         logLik(test_full))))
  }
  p_value_global     <- stats::pchisq(statistic_global, df_global,
                                      lower.tail = F)
  names(df_global)   <- names(statistic_global)
  sigma_b            <- c("sigma_b" =
                            sqrt(as.numeric(lme4::VarCorr(test_full))))
  return(list(conf_int = conf_int, cov_estimate = cov_estimate,
              df_global = df_global, estimate = estimate, p_value = p_value,
              p_value_global = p_value_global, se_estimate = se_estimate,
              sigma_b = sigma_b, statistic = statistic,
              statistic_global = diff_l))
}

an_bin_mainland_gart <- function(contingency_table, exact, correct, conf_level,
                                 alternative) {
  n12                <- contingency_table$`(0, 1)`[1]
  n13                <- contingency_table$`(1, 0)`[1]
  n22                <- contingency_table$`(0, 1)`[2]
  n23                <- contingency_table$`(1, 0)`[2]
  if (correct == 3) {
    n12              <- n12 + 0.5
    n13              <- n13 + 0.5
    n22              <- n22 + 0.5
    n23              <- n23 + 0.5
  } else if (correct == 4) {
    n12              <- ifelse(n12 == 0, n12 + 0.5, n12)
    n13              <- ifelse(n13 == 0, n13 + 0.5, n13)
    n22              <- ifelse(n22 == 0, n22 + 0.5, n22)
    n23              <- ifelse(n23 == 0, n23 + 0.5, n23)
  }
  estimate_tr        <- 0.5*log(n12*n23/(n13*n22))
  se_estimate_tr     <- sqrt(0.25*(1/n12 + 1/n13 + 1/n22 + 1/n23))
  conf_int_tr        <- estimate_tr + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
    se_estimate_tr
  if (exact) {
    test             <- stats::fisher.test(contingency_table[1:2, 4:5],
                                           alternative = alternative,
                                           conf.level  = conf_level)
    p_value_tr       <- test$p.value
  } else {
    m1               <- n12 + n13
    m2               <- n22 + n23
    n.2              <- n12 + n22
    n.3              <- n13 + n23
    m.               <- m1 + m2
    if (correct == 1) {
      statistic_tr   <- (n12*n23 - n22*n13)^2*m./(n.2*n.3*m1*m2)
    } else if (correct == 2) {
      statistic_tr   <- (abs(n12*n23 - n22*n13) - 0.5*m.)^2*m./(n.2*n.3*m1*m2)
    } else {
      statistic_tr   <- estimate_tr^2/variance
    }
    p_value_tr       <- stats::pchisq(statistic_tr, df = 1, lower.tail = F)
  }
  n22_old            <- n22
  n22                <- n23
  n23                <- n22_old
  estimate_per       <- 0.5*log(n12*n23/(n13*n22))
  se_estimate_per    <- sqrt(0.25*(1/n12 + 1/n13 + 1/n22 + 1/n23))
  conf_int_per       <- estimate_per + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
    se_estimate_per
  if (exact) {
    test             <- stats::fisher.test(rbind(contingency_table[1, 4:5],
                                                 contingency_table[2, 5:4]),
                                           alternative = alternative,
                                           conf.level = conf_level)
    p_value_per      <- test$p.value
  } else {
    m2               <- n22 + n23
    n.2              <- n12 + n22
    n.3              <- n13 + n23
    m.               <- m1 + m2
    if (correct == 1) {
      statistic_per  <- (n12*n23 - n22*n13)^2*m./(n.2*n.3*m1*m2)
    } else if (correct == 2) {
      statistic_per  <- (abs(n12*n23 - n22*n13) - 0.5*m.)^2*m./(n.2*n.3*m1*m2)
    } else {
      statistic_per  <- estimate_per^2/variance
    }
    p_value_per      <- stats::pchisq(statistic_per, df = 1, lower.tail = F)
  }
  conf_int           <- tibble::tibble(period2 = conf_int_per,
                                       treatmentB = conf_int_tr)
  estimate           <- c(estimate_per, estimate_tr)
  p_value            <- c(p_value_per, p_value_tr)
  se_estimate        <- c(se_estimate_per, se_estimate_tr)
  colnames(conf_int) <- names(estimate) <- names(p_value) <-
    names(se_estimate) <- c("period2",
                            paste("treatment",
                                  strsplit(contingency_table$sequence[1],
                                           split = "")[[1]][2], sep = ""))
  if (!exact) {
    statistic        <- c(statistic_per, statistic_tr)
    names(statistic) <- names(estimate)
    return(list(conf_int = conf_int, estimate = estimate, p_value = p_value,
                se_estimate = se_estimate, statistic = statistic))
  } else {
    return(list(conf_int = conf_int, estimate = estimate, p_value = p_value,
                se_estimate = se_estimate))
  }

}

an_bin_mcnemar <- function(contingency_table, exact, conf_level, alternative) {
  nB               <- contingency_table$`(0, 1)`[1] +
    contingency_table$`(1, 0)`[2]
  nP               <- contingency_table$`(0, 1)`[3] +
    contingency_table$`(1, 0)`[3]
  nA               <- nP - nB
  estimate_tr      <- nB/nP
  se_estimate_tr   <- sqrt(estimate_tr*(1 - estimate_tr)/nP)
  conf_int_tr      <- ci_fixed_wald(nB, nP, 1 - conf_level)
  if (exact) {
    test           <- stats::binom.test(nB, nP, alternative = alternative)
    statistic_tr   <- nB
    p_value_tr     <- test$p.value
  } else {
    statistic_tr   <- (nA - nB)^2/nP
    p_value_tr     <- stats::pchisq(statistic_tr, df = 1, lower.tail = F)
  }
  n2               <- contingency_table$`(0, 1)`[1] +
    contingency_table$`(0, 1)`[2]
  n1               <- nP - n2
  estimate_per     <- n2/nP
  se_estimate_per  <- sqrt(estimate_per*(1 - estimate_per)/nP)
  conf_int_per     <- ci_fixed_wald(n2, nP, 1 - conf_level)
  if (exact) {
    test           <- stats::binom.test(n2, nP, alternative = alternative)
    statistic_per  <- n2
    p_value_per    <- test$p.value
  } else {
    statistic_per  <- (n1 - n2)^2/nP
    p_value_per    <- stats::pchisq(statistic_per, df = 1, lower.tail = F)
  }
  conf_int         <- tibble::tibble(period2 = conf_int_per,
                                     treatmentB = conf_int_tr)
  estimate         <- c(estimate_per, estimate_tr)
  se_estimate      <- c(se_estimate_per, se_estimate_tr)
  p_value          <- c(p_value_per, p_value_tr)
  statistic        <- c(statistic_per, statistic_tr)
  names(estimate)  <- names(p_value)     <- names(se_estimate) <-
    names(statistic) <- colnames(conf_int) <-
    c("period2", paste("treatment", strsplit(contingency_table$sequence[1],
                                             split = "")[[1]][2], sep = ""))
  return(list(conf_int = conf_int, estimate = estimate, p_value = p_value,
              se_estimate = se_estimate, statistic = statistic))
}

an_bin_prescott <- function(contingency_table, exact, alternative) {
  collapsed_contingency <-
    tibble::tibble(`(0, 1)`            = contingency_table$`(0, 1)`,
                   `(0, 0) and (1, 1)` = contingency_table$`(0, 0)` +
                     contingency_table$`(1, 1)`,
                   `(1, 0)`            = contingency_table$`(1, 0)`,
                   Total               = contingency_table$total)
  if (exact) {
    ex_tables           <-
      as.matrix(expand.grid(prefB  = as.numeric(collapsed_contingency[1, 1]):
                              as.numeric(collapsed_contingency[3, 1]),
                            nopref =
                              0:as.numeric(collapsed_contingency[3, 2]),
                            prefA  =
                              0:as.numeric(collapsed_contingency[3, 3])))
    ex_tables           <- ex_tables[rowSums(ex_tables) ==
                                       collapsed_contingency$Total[1], ,
                                     drop = F]
    Te                  <- as.numeric(collapsed_contingency[1, 1] -
                                        collapsed_contingency[1, 3])
    if (alternative == "greater") {
      ex_tables         <- ex_tables[which(ex_tables[, 1] -
                                             ex_tables[, 3] >= Te), , drop = F]
    } else {
      ex_tables         <- ex_tables[which((ex_tables[, 1] -
                                              ex_tables[, 3] <= -abs(Te)) |
                                             ex_tables[, 1] -
                                             ex_tables[, 3] >= abs(Te)), ,
                                     drop = F]
    }
    p_value_tr          <- 0
    for (i in 1:nrow(ex_tables)) {
      p_value_tr        <- p_value_tr +
        prod(choose(as.numeric(collapsed_contingency[3, 1:3]), ex_tables[i, ]))
    }
    p_value_tr          <-
      p_value_tr/choose(as.numeric(collapsed_contingency$Total[3]),
                        as.numeric(collapsed_contingency$Total[1]))
  } else {
    if (alternative == "greater") {
      alternative       <- "increasing"
    }
    test                <-
      DescTools::CochranArmitageTest(collapsed_contingency[1:2, 1:3],
                                     alternative = alternative)
    statistic_tr        <- as.numeric(test$statistic)
    p_value_tr          <- test$p.value
  }
  collapsed_contingency[2, c(1, 3)] <- collapsed_contingency[2, c(3, 1)]
  collapsed_contingency[3, ]        <- colSums(collapsed_contingency[1:2, ])
  if (exact) {
    ex_tables           <-
      as.matrix(expand.grid(pref2  = as.numeric(collapsed_contingency[1, 1]):
                              as.numeric(collapsed_contingency[3, 1]),
                            nopref =
                              0:as.numeric(collapsed_contingency[3, 2]),
                            pref1  =
                              0:as.numeric(collapsed_contingency[3, 3])))
    ex_tables           <- ex_tables[rowSums(ex_tables) ==
                                       collapsed_contingency$Total[1], ,
                                     drop = F]
    Te                  <- as.numeric(collapsed_contingency[1, 1] -
                                        collapsed_contingency[1, 3])
    if (alternative == "greater") {
      ex_tables         <- ex_tables[which(ex_tables[, 1] -
                                             ex_tables[, 3] >= Te), , drop = F]
    } else {
      ex_tables         <- ex_tables[which((ex_tables[, 1] -
                                              ex_tables[, 3] <= -abs(Te)) |
                                             ex_tables[, 1] -
                                             ex_tables[, 3] >= abs(Te)), ,
                                     drop = F]
    }
    p_value_per         <- 0
    for (i in 1:nrow(ex_tables)) {
      p_value_per       <- p_value_per +
        prod(choose(as.numeric(collapsed_contingency[3, 1:3]), ex_tables[i, ]))
    }
    p_value_per         <-
      p_value_per/choose(as.numeric(collapsed_contingency$Total[3]),
                         as.numeric(collapsed_contingency$Total[1]))
  } else {
    test                <-
      DescTools::CochranArmitageTest(collapsed_contingency[1:2, 1:3],
                                     alternative = alternative)
    statistic_per       <- as.numeric(test$statistic)
    p_value_per         <- test$p.value
  }
  p_value               <- c(p_value_per, p_value_tr)
  names(p_value)        <- c("period2",
                             paste("treatment",
                                   strsplit(contingency_table$sequence[1],
                                            split = "")[[1]][2], sep = ""))
  if (!exact) {
    statistic           <- c(statistic_per, statistic_tr)
    names(statistic)    <- names(p_value)
    return(list(p_value = p_value, statistic = statistic))
  } else {
    return(list(p_value = p_value))
  }
}

an_cont_anova <- function(data, outcome) {
  summaries      <- dplyr::summarise(dplyr::group_by(data, subject),
                                     total    = sum(eval(outcome)),
                                     diff     = diff(eval(outcome)),
                                     sequence = sequence[1])
  sequences      <- levels(data$sequence)
  n1             <- length(which(summaries$sequence == sequences[1]))
  n2             <- length(which(summaries$sequence == sequences[2]))
  m_inv          <- n1*n2/(n1 + n2)
  bar_y11.       <- mean(eval(outcome,
                              dplyr::filter(data, `sequence index` == 1 &
                                                    period == 1)))
  bar_y12.       <- mean(eval(outcome,
                              dplyr::filter(data, `sequence index` == 1 &
                                                    period == 2)))
  bar_y21.       <- mean(eval(outcome,
                              dplyr::filter(data, `sequence index` == 2 &
                                                    period == 1)))
  bar_y22.       <- mean(eval(outcome,
                              dplyr::filter(data, `sequence index` == 2 &
                                                    period == 2)))
  bar_y1..       <- 0.5*(bar_y11. + bar_y12.)
  bar_y2..       <- 0.5*(bar_y21. + bar_y22.)
  SS_Co          <- 2*m_inv*(bar_y1.. - bar_y2..)^2
  SS_BSR         <- 0.5*(sum(summaries$total^2) -
                           sum(dplyr::filter(summaries,
                                             sequence ==
                                               sequences[1])$total)^2/n1 -
                           sum(dplyr::filter(summaries,
                                             sequence ==
                                               sequences[2])$total)^2/n2)
  SS_Tr          <- 0.5*m_inv*(bar_y11. - bar_y12. - bar_y21. + bar_y22.)^2
  SS_Per         <- 0.5*m_inv*(bar_y11. - bar_y12. + bar_y21. - bar_y22.)^2
  SS_WSR         <- sum(eval(outcome, data)^2) - 0.5*sum(summaries$total^2) -
                      sum(eval(outcome,
                               dplyr::filter(data, period == 1 &
                                                     sequence ==
                                                        sequences[1])))^2/n1 -
                      sum(eval(outcome,
                               dplyr::filter(data, period == 2 &
                                                     sequence ==
                                                       sequences[1])))^2/n1 -
                      sum(eval(outcome,
                               dplyr::filter(data, period == 1 &
                                                     sequence ==
                                                       sequences[2])))^2/n2 -
                      sum(eval(outcome,
                               dplyr::filter(data, period == 2 &
                                                     sequence ==
                                                       sequences[2])))^2/n2 +
                      0.5*sum(eval(outcome,
                                   dplyr::filter(data, sequence ==
                                                         sequences[1])))^2/n1 +
                      0.5*sum(eval(outcome,
                                   dplyr::filter(data, sequence ==
                                                         sequences[2])))^2/n2
  SS_Tot         <- sum(eval(outcome, data)^2) -
                      0.5*sum(eval(outcome, data))^2/(n1 + n2)
  anova_table    <-
    tibble::tibble(Source    = c("B-S: Carry-over", "B-S: B-S residual",
                                 "W-S: Direct treatments (adj. for periods)",
                                 "W-S: Periods (adj. for treatments)",
                                 "W-S: W-S residual", "Total"),
                   df        = c(1, n1 + n2 - 2, 1, 1, n1 + n2 - 2,
                                 2*(n1 + n2) - 1),
                   SS        = c(SS_Co, SS_BSR, SS_Tr, SS_Per, SS_WSR, SS_Tot),
                   MS        = c((SS/df)[1:5], NA),
                   F         = c(MS[1]/MS[2], NA, MS[3]/MS[5], MS[4]/MS[5], NA,
                                 NA),
                   `p-value` = c(pf(F[1], 1, df[2], lower.tail = F), NA,
                                 pf(F[3], 1, df[5], lower.tail = F),
                                 pf(F[4], 1, df[5], lower.tail = F), NA, NA))
  statistic      <- anova_table$F[c(4, 3, 1)]
  p_value        <- anova_table$`p-value`[c(4, 3, 1)]
  sigma_e        <- c("sigma_e" = sqrt(SS_WSR))
  sigma_b        <- c("sigma_b" = sqrt(0.5*(SS_BSR - SS_WSR)))
  names(p_value) <- names(statistic) <- c("period", "treatment", "carryover")
  return(list(anova_table = anova_table, p_value = p_value, sigma_b = sigma_b,
              sigma_e = sigma_e, statistic = statistic))
}

an_cont_lmm <- function(data, outcome, alternative, conf_level, carryover) {
  if (deparse(outcome) != "outcome") {
    data$outcome     <- eval(outcome, data)
  }
  if (all(carryover, !("carryover" %in% colnames(data)))) {
    data             <- add_carryover_data(data, F)
  }
  if (carryover) {
    test_full        <- lme4::lmer(formula = outcome ~ treatment + period +
                                                         carryover +
                                                         (1|subject),
                                   data = data)
    test_reduced_tr  <- lme4::lmer(formula = outcome ~ period + carryover +
                                                         (1|subject),
                                   data = data)
    test_reduced_per <- lme4::lmer(formula = outcome ~ treatment + carryover +
                                                         (1|subject),
                                   data = data)
    test_reduced_co  <- lme4::lmer(formula = outcome ~ treatment + period +
                                                         (1|subject),
                                   data = data)
  } else {
    test_full        <- lme4::lmer(formula = outcome ~ treatment + period +
                                                         (1|subject),
                                   data = data)
    test_reduced_tr  <- lme4::lmer(formula = outcome ~ period + (1|subject),
                                   data = data)
    test_reduced_per <- lme4::lmer(formula = outcome ~ treatment + (1|subject),
                                   data = data)
  }
  estimate           <- lme4::fixef(test_full)
  cov_estimate       <- as.matrix(lme4::vcov.merMod(test_full))
  se_estimate        <- sqrt(diag(cov_estimate))
  conf_int           <- matrix(0, nrow = 2, ncol = length(estimate))
  for (i in 1:length(estimate)) {
    conf_int[, i]    <- estimate[i] + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
                                        sqrt(se_estimate[i])
  }
  conf_int           <- tibble::as_tibble(conf_int)
  colnames(conf_int) <- names(estimate)
  statistic          <- estimate/se_estimate
  if (alternative == "two_sided") {
    p_value          <- stats::pnorm(abs(statistic), lower.tail = F) +
                          stats::pnorm(-abs(statistic), lower.tail = T)
  } else {
    p_value          <- stats::pnorm(statistic, lower.tail = F)
  }
  df_full            <- attr(logLik(test_full), "df")
  df_tr              <- attr(logLik(test_reduced_tr), "df")
  df_per             <- attr(logLik(test_reduced_per), "df")
  diff_l_tr          <- as.numeric(-2*(logLik(test_reduced_tr) -
                                         logLik(test_full)))
  diff_l_per         <- as.numeric(-2*(logLik(test_reduced_per) -
                                         logLik(test_full)))
  statistic_global   <- c("period" = diff_l_per, "treatment" = diff_l_tr)
  df_global          <- df_full - c(df_per, df_tr)
  if (carryover) {
    df_global        <- c(df_global,
                          df_full - attr(logLik(test_reduced_co), "df"))
    statistic_global <- c(statistic_global,
                          "carryover" = as.numeric(-2*(logLik(test_reduced_co) -
                                                         logLik(test_full))))
  }
  p_value_global     <- stats::pchisq(statistic_global, df_global,
                                      lower.tail = F)
  names(df_global)   <- names(statistic_global)
  sigma_b            <- c("sigma_b" =
                            sqrt(as.numeric(lme4::VarCorr(test_full))))
  sigma_e            <- c("sigma_e" = attr(lme4::VarCorr(test_full), "sc"))
  return(list(conf_int = conf_int, cov_estimate = cov_estimate,
              df_global = df_global, estimate = estimate, p_value = p_value,
              p_value_global = p_value_global, se_estimate = se_estimate,
              sigma_b = sigma_b, sigma_e = sigma_e, statistic = statistic,
              statistic_global = statistic_global))
}

an_cont_t_test <- function(data, outcome, alternative, conf_level) {
  summaries          <- dplyr::summarise(dplyr::group_by(data, subject),
                                         total    = sum(eval(outcome)),
                                         diff     = diff(eval(outcome)),
                                         sequence = sequence[1])
  sequences          <- levels(data$sequence)
  n1                 <- length(which(summaries$sequence == sequences[1]))
  n2                 <- length(which(summaries$sequence == sequences[2]))
  m                  <- (n1 + n2)/(n1*n2)
  bar_t1.            <- mean(as.numeric(dplyr::filter(summaries,
                                                      sequence ==
                                                        sequences[1])$total))
  bar_t2.            <- mean(as.numeric(dplyr::filter(summaries,
                                                      sequence ==
                                                        sequences[2])$total))
  hat_sigma_T2       <- (sum((dplyr::filter(summaries,
                                            sequence ==
                                              sequences[1])$total -
                                bar_t1.)^2) +
                          sum((dplyr::filter(summaries,
                                             sequence ==
                                               sequences[2])$total -
                                 bar_t2.)^2))/
                          (n1 + n2 - 2)
  estimate_co        <- bar_t2. - bar_t1.
  se_estimate_co     <- sqrt(hat_sigma_T2*m)
  bar_d1.            <- mean(as.numeric(dplyr::filter(summaries,
                                                      sequence ==
                                                        sequences[1])$diff))
  bar_d2.            <- mean(as.numeric(dplyr::filter(summaries,
                                                      sequence ==
                                                        sequences[2])$diff))
  hat_sigma_D2       <- (sum((dplyr::filter(summaries,
                                            sequence ==
                                              sequences[1])$diff - bar_d1.)^2) +
                           sum((dplyr::filter(summaries,
                                              sequence ==
                                                sequences[2])$diff -
                                  bar_d2.)^2))/
                           (n1 + n2 - 2)
  estimate_tr        <- 0.5*(bar_d2. - bar_d1.)
  se_estimate_tr     <- se_estimate_per <- sqrt(hat_sigma_D2*m/4)
  estimate_per       <- 0.5*(-bar_d2. - bar_d1.)
  estimate           <- c(estimate_tr, estimate_per, estimate_co)
  se_estimate        <- c(se_estimate_tr, se_estimate_per, se_estimate_co)
  statistic          <- estimate/se_estimate
  conf_int           <- matrix(0, nrow = 2, ncol = length(estimate))
  for (i in 1:length(estimate)) {
    conf_int[, i]    <- estimate[i] + c(-1, 1)*qnorm(1 - (1 - conf_level)/2)*
                                        sqrt(se_estimate[i])
  }
  conf_int           <- tibble::as_tibble(conf_int)
  colnames(conf_int) <- names(estimate) <- names(se_estimate) <-
    c("period2", paste(c("treatment", "carryover"),
                       strsplit(sequences[1], split = "")[[1]][2], sep = ""))
  if (alternative == "two_sided") {
    p_value          <- stats::pnorm(abs(statistic), lower.tail = F) +
      stats::pnorm(-abs(statistic), lower.tail = T)
  } else {
    p_value          <- stats::pnorm(statistic, lower.tail = F)
  }
  sigma_e            <- c("sigma_e" = sqrt(0.5*hat_sigma_D2))
  sigma_b            <- c("sigma_b" = sqrt(0.5*(0.5*hat_sigma_T2 - sigma_e^2)))
  return(list(conf_int = conf_int, estimate = estimate, p_value = p_value,
              se_estimate = se_estimate, sigma_b = sigma_b, sigma_e = sigma_e,
              statistic = statistic))
}

ci_fixed_wald <- function(s, m, alpha) {
  if (s == 0) {
    Clow   <- Cupp <- 0
  } else if (s == m) {
    Clow   <- Cupp <- 1
  } else {
    pi_hat <- s/m
    factor <- stats::qnorm(1 - alpha/2)*sqrt(pi_hat*(1 - pi_hat)/m)
    Clow   <- pi_hat - factor
    Cupp   <- pi_hat + factor
    Clow   <- ifelse(Clow < 0, 0, ifelse(Clow > 1, 1, Clow))
    Cupp   <- ifelse(Cupp < 0, 0, ifelse(Cupp > 1, 1, Cupp))
  }
  return(c(Clow, Cupp))
}

an_bin_lc_f_ib_10 <- function(a, a_plus, t) {
  return(exp(sum(lchoose(t, c(a[1], a_plus[1] - a[1], a[2], a_plus[3] - a[3],
                              a_plus[2] - a[2], a[3]))))/
           exp(sum(lchoose(c(t[1] + t[2], t[3] + t[5], t[4] + t[6]), a_plus))))
}

an_bin_lc_f_ib_20 <- function(a_st, a_plus, t) {
  return(exp(sum(lchoose(t, c(a_st[1], a_plus[3] - a_st[3], a_st[2],
                              a_plus[2] - a_st[2], a_st[3],
                              a_plus[2] - a_st[2]))))/
           exp(sum(lchoose(c(t[1] + t[6], t[3] + t[4], t[2] + t[5]), a_plus))))
}

an_bin_lc_f_ib_21 <- function(a_stst, a_plus, t) {
  return(exp(sum(lchoose(t, c(a_plus[1] - a_stst[2], a_stst[1], a_stst[2],
                              a_plus[2] - a_stst[1], a_stst[3],
                              a_plus[3] - a_stst[3]))))/
           exp(sum(lchoose(c(t[1] + t[3], t[2] + t[4], t[5] + t[6]), a_plus))))
}

#hux_table                                                   <-
#  huxtable::add_colnames(huxtable::hux(contingency_table))
#huxtable::top_border(hux_table)[1, ]                        <- 1
#huxtable::bottom_border(hux_table)[c(1, nrow(hux_table)), ] <- 1
#huxtable::print_screen(hux_table, colnames = F)
