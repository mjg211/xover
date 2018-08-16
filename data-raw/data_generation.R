##### mental_fatigue ###########################################################
mental_fatigue                  <- readr::read_csv("mental_fatigue.csv")
mental_fatigue$period           <- as.factor(mental_fatigue$period)
mental_fatigue$treatment        <- as.factor(mental_fatigue$treatment)
mental_fatigue$subject          <- as.factor(mental_fatigue$subject)
mental_fatigue$sequence         <- as.factor(mental_fatigue$sequence)
mental_fatigue$`sequence index` <- as.factor(mental_fatigue$`sequence index`)
mental_fatigue$accuracy         <- as.double(mental_fatigue$accuracy)
save(mental_fatigue, file = "data_mental_fatigue.RData")
##### Source:
################################################################################

##### raynaud ##################################################################
raynaud                  <- readr::read_csv("raynaud.csv")
raynaud$period           <- as.factor(raynaud$period)
raynaud$treatment        <- factor(raynaud$treatment, c("P", "N"))
raynaud$subject          <- as.factor(raynaud$subject)
raynaud$sequence         <- factor(raynaud$sequence, c("PN", "NP"))
raynaud$`sequence index` <- as.factor(raynaud$`sequence index`)
save(raynaud, file = "data_raynaud.RData")
##### Source:
################################################################################

##### sauter ###################################################################
sauter                  <- readr::read_csv("sauter.csv")
sauter$period           <- as.factor(sauter$period)
sauter$treatment        <- as.factor(sauter$treatment)
sauter$subject          <- as.factor(sauter$subject)
sauter$sequence         <- as.factor(sauter$sequence)
sauter$`sequence index` <- as.factor(sauter$`sequence index`)
save(sauter, file = "data_sauter.RData")
##### Source:
################################################################################

##### erythromycin #############################################################
erythromycin                  <- readr::read_csv("erythromycin.csv")
erythromycin$period           <- as.factor(erythromycin$period)
erythromycin$treatment        <- as.factor(erythromycin$treatment)
erythromycin$subject          <- as.factor(erythromycin$subject)
erythromycin$sequence         <- as.factor(erythromycin$sequence)
erythromycin$`sequence index` <- as.factor(erythromycin$`sequence index`)
save(erythromycin, file = "data_erythromycin.RData")
##### Source:
################################################################################

##### eib ######################################################################
eib                  <- readr::read_csv("eib.csv")
eib$period           <- as.factor(eib$period)
eib$treatment        <- factor(eib$treatment, c("P", "F", "S"))
eib$subject          <- as.factor(eib$subject)
eib$sequence         <- factor(eib$sequence, c("PFS", "PSF", "FPS",
                                               "FSP", "SPF", "SFP"))
eib$`sequence index` <- factor(match(eib$sequence, levels(eib$sequence)), 1:6)
eib$baseline         <- as.double(eib$baseline)
save(eib, file = "data_eib.RData")
##### Senn (2002). The data in the book is an extension of 'Russia' from
##### http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm
################################################################################

##### asthma ###################################################################
asthma                  <- readr::read_csv("asthma.csv")
asthma$period           <- as.factor(asthma$period)
asthma$treatment        <- factor(asthma$treatment, c("P", "I6", "I12", "I24",
                                                      "M6", "M12", "M24"))
asthma$subject          <- as.factor(asthma$subject)
asthma$sequence         <- factor(asthma$sequence,
                                  c("P-I6-M6-M24-I12", "P-I24-M24-I6-M12",
                                    "P-M24-M12-M6-I24", "I6-I12-P-M24-M12",
                                    "I6-M6-M24-I12-I24", "I6-M12-I12-M6-P",
                                    "I12-P-M24-M12-M6", "I12-I24-M12-P-I6",
                                    "I12-M6-P-I24-M24", "I24-I6-I12-P-M24",
                                    "I24-M12-P-I6-M6", "I24-M24-I6-M12-I12",
                                    "M6-P-I24-M24-I6", "M6-I24-I6-I12-P",
                                    "M6-M24-I12-I24-M12", "M12-P-I6-M6-M24",
                                    "M12-I12-M6-P-I24", "M12-M6-I24-I6-I12",
                                    "M24-I6-M12-I12-M6", "M24-I12-I24-M12-P",
                                    "M24-M12-M6-I24-I6"))
asthma$`sequence index` <- factor(match(asthma$sequence,
                                        levels(asthma$sequence)), 1:21)
save(asthma, file = "data_asthma.RData")
##### Source: 'Selipati' from
##### 'http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm
################################################################################

##### phenytoin ################################################################
phenytoin                  <- readr::read_csv("phenytoin.csv")
phenytoin$period           <- as.factor(phenytoin$period)
phenytoin$treatment        <- as.factor(phenytoin$treatment)
phenytoin$subject          <- as.factor(phenytoin$subject)
phenytoin$sequence         <- as.factor(phenytoin$sequence)
phenytoin$`sequence index` <- as.factor(phenytoin$`sequence index`)
save(phenytoin, file = "data_phenytoin.RData")
##### Source: 'Phenytoin' from
##### http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm
################################################################################

##### Table 3.1 ################################################################
# Senn and Auclair (1990) reported on the results of an AB/BA crossover trial
# comparing the effects of two treatments in children with moderate or severe
# asthma. Precisely, they are measurements of peak expiratory flow (PEF), a
# measure of lung function. A single inhaled dose of 200 μg of salbutamol
# (treatment S), a well-established bronchodilator, was compared to 12 μg of
# formoterol (treatment F), a more recently developed bronchodilator. In one
# group children were given formoterol in the morning and observed for 8h in the
# clinic. they have travelled home where they of their parents took further
# measurements 10, 11 and 12 h after treatment. On a subsequent occassion after
# a wash-ot of at least one day they presented at the clinic again and were
# given a single dose of salbutamol. measurements in the clinica follows as
# before and were again succeeded by measurements at home. for the second
# sequence group, the porceudre was as for the first except the order of
# treatments were reversed.
table.3.1 <- tibble::tibble(PEF = c(310, 270, 310, 260, 370, 300, 410, 390, 250,
                                    210, 380, 350, 330, 365, 370, 385, 310, 400,
                                    380, 410, 290, 320, 260, 340, 90, 220),
                            period = factor(rep(1:2, 13), 1:2),
                            treatment = factor(c(rep(c("F", "S"), 7),
                                                 rep(c("S", "F"), 6)),
                                               c("F", "S")),
                            subject = factor(c(1, 1, 4, 4, 6, 6, 7, 7, 10, 10,
                                               11, 11, 14, 14, 2, 2, 3, 3, 5, 5,
                                               9, 9, 12, 12, 13, 13),
                                             (1:14)[-8]),
                            sequence = factor(c(rep("FS", 14), rep("SF", 12)),
                                              c("FS", "SF")),
                            `sequence index` = factor(c(rep(1, 14), rep(2, 12)),
                                                      1:2),
                            Sex = factor(c(rep("Male", 2), rep("Female", 2),
                                           rep("Female", 2), rep("Male", 2),
                                           rep("Male", 2), rep("Female", 2),
                                           rep("Male", 2), rep("Male", 2),
                                           rep("Male", 2), rep("Female", 2),
                                           rep("Male", 2), rep("Male", 2),
                                           rep("Male", 2)),
                                         c("Male", "Female")))

##### Table 4.2 ################################################################
# Patients opinion of efficacy (visual analogue scale scores in millimetre: 0
# good, 100 bad) for a trial in asthma. the treatments compared in the trial
# were a single dose of formoterol solution aerosol 12 mu-g and a single dose of
# salbutamol aerosol 200 mu-g. Patients were allocated at random in equal
# numbers to one of two treatment sequences: formoterol followed after a
# wash-out of 7 days by salbutamol or slbutamol followed after a wash-out of
# seven days by formoterol. the main purpose to the trial was to study the
# protective effect of these bronchodilators on methacholine-induced
# bronchoconstriction, and during each treatment day the patients were given
# increasing doses of methacholine until a given response was observed. patients
# were asked to give their opnion on efficacy using a VAS scale with 'bad' on
# the right and 'good' on the left. thus high scores are good and low scores are
# bad.
# A VAS score is at the best of times not easy to interpret. when transformed in
# logit manner this task appears even more daunting. an alternative
# transformation which is sometimes considered for VAS scales is the arc sine
# transformation. Example used by Senn to consider various transformatinos that
# can be used in the analysis of crossover data, and also in the exploration of
# non-parametric methods.
# the main outcome variable was PD20, defined as the cumulative dose of
# methacholine required to produce a 20% fall in FEV1. the data are given here,
# and are partially explored in Leuenberger and Gebre-Michel (1989) who also
# discuss the trial in 'survival' terms. the methacholine was delivered in
# discrete steps. since in such a trial the highest dose given to the patient
# will not produce a fall of exactly 20%, the dose which would produce such a
# fall has been estimated by interpolation using the last two doses. for two
# patients, problems in recording data mean that the values are missing. for two
# other patients on one of the two treatmen days the PD20 recorded is the actual
# highest possible dose delivered during the manouevere. these values may be
# regarded as being censored and are marked accordingly in another variable.
# used by senn to explore analysis of survival data in crossover trials.
table.4.2 <- tibble::tibble(VAS = c(63, 82, 24, 84, 32, 66, 33, 51, 24, 75, 30, 38,
                            40, 68, 5, 4, 21, 57, 5, 23, 32, 56, 4, 53),
                    PD20 = c(7.29, 6.68, 7.84, 2.42, 4.65, 1.93, 12.40, 9.47,
                             10.57, 8.96, 8.17, 7.60, 5.06, 2.33, NA, NA, 12.40, 10.33,
                             NA, NA, 7.33, 6.97, 11.11, 10.80),
                    Period = factor(rep(1:2, 12), levels = 1:2),
                    Treatment = factor(c(rep(c("F", "S"), 6),
                                         rep(c("S", "F"), 6)),
                                       levels = c("F", "S")),
                    Subject = factor(c(1, 1, 3, 3, 5, 5, 6, 6, 10, 10, 12, 12,
                                       2, 2, 4, 4, 7, 7, 8, 8, 9, 9, 11, 11),
                                     levels = 1:12),
                    Sequence = factor(rep(c("FS", "SF"), each = 12),
                                      levels = c("FS", "SF")),
                    `Sequence Index` = factor(rep(1:2, each = 12), levels = 1:2),
                    PD20.censored = factor(c(rep(0, 6), 1, rep(0, 7), NA, NA, 1, 0, NA, NA,
                                             rep(0, 4)),
                                           levels = 0:1))

in a double-blind multiple-dose AB/BA crosover in stable exertional angine pectoris 60
patients were allocated at random to one or two treatment sequences AB or BA, where
A standard for metoprolol oros 14/190 and B standard for lopresor sr 200 mu-g. The duration of
each treatment period was four weeks. There was no wash-out between treatments. At the end
of the treatment period, patients performed an exercise test conssiting of 3 minutes work at
50 W with 25 W increases every 2 minuted up to a possible maximum of 200W. the exercise test
was terminated as soon as the patient suffered an anginal attack or was otherwise unable to continue
due to exhaustion or dyspnoea. The trial was carriedout in 4 centres, report here on the
results from centre 3 for heart rate in beats/minute (bpm) at the end of the exercise
test. Senn used this to explain the relationship between the means of the basic
estimators and the Mann-Whitney U statistic.

table.4.9 <- tibble(`Heart rate` = c(88, 95, 130, 125, 115, 130, 125, 110, 140, 118, 110, 110,
                            110, 115, 80, 105, 120, 125, 140, 100, 100, 98, 90, 80),
                    Period = factor(rep(1:2, 12), levels = 1:2),
                    Treatment = factor(c(rep(c("M", "L"), 6),
                                         rep(c("L", "M"), 6)),
                                       levels = c("M", "L")),
                    Subject = factor(c(301, 301, 304, 304, 305, 305, 307, 307, 310, 310,
                                       311, 311, 302, 302, 303, 303, 306, 306, 308, 308,
                                       309, 309, 312, 312),
                                     levels = 301:312),
                    Sequence = factor(rep(c("ML", "LM"), each = 12),
                                      levels = c("ML", "LM")),
                    `Sequence Index` = factor(rep(1:2, each = 12), levels = 1:2))

in an AB/BA double-blind crossover trial, children aged 7 to 11 suffering from exercise-induced
asthma were randomized to one or two treatment sequences. A single dose of 12 mu-g formoterol
solution aerosol followed by a single dose of 200 mu-g sabutamol solution aerosol or vice
versa. One an initial examination day a suitable exercise challenge was established for
each child. that is to say, an exercise work load was established which induced a given
response in terms of its effect in reducing forced expiratory volume in one second (FEV1),
a measure of lung function. for a given child the same challenge was used on each treatment
day and was carried out twice: one 2 hours after treatment and again 8 hours after treatment.
various lung function measurements were taken throughout the trial. amongst many variables recorded
was the investgiators overall subjective assessment of efficacy. this was recorded on a four-point
scale: 1 = poor, 2 = fair, 3 = moderate, 4 = good. the results are given. Used by Senn to
explore categorical data analysis, and through binary created scale (good vs not good), binary analysis.

table.4.12 <- tibble(`4PS` = c(4, 4, 3, 1, 4, 1, 4, 3, 4, 4, 4, 3, 4, 3, 4, 1,
                               4, 3, 4, 1, 4, 3, 4, 2, 2, 4, 3, 4, 4, 4, 4, 4,
                               4, 4, 4, 4, 4, 4, 4, 3, 3, 4, 2, 4, 2, 4, 3, 4),
                     `2PS` = c(1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0,
                               1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1),
                     Period = factor(rep(1:2, 24), levels = 1:2),
                     Treatment = factor(c(rep(c("F", "S"), 12),
                                          rep(c("S", "F"), 12)),
                                        levels = c("F", "S")),
                     Subject = factor(c(3, 3, 4, 4, 7, 7, 8, 8, 9, 9, 11, 11,
                                        15, 15, 16, 16, 19, 19, 20, 20, 22, 22,
                                        23, 23, 1, 1, 2, 2, 5, 5, 6, 6, 10, 10,
                                        12, 12, 13, 13, 14, 14, 17, 17, 18, 18,
                                        21, 21, 24, 24),
                                      levels = 1:24),
                     Sequence = factor(rep(c("FS", "SF"), each = 24),
                                       levels = c("FS", "SF")),
                     `Sequence Index` = factor(rep(1:2, each = 24), levels = 1:2))

a double-blind crossover trial in asthma compared the effect of the long-acting beta-agonist
salmeterol (50 mu-g twice daily) to placebo (Wilding et al 1997). One hundred and one
patients were treated for two six-month periods, having been randomized to one of two
sequences: either placebo followed by salmeterol or salmeterol followed by placebo. there
was a one-month wash-out between treatment periods. a number of different measures were compared
we concentrate here on the number of exacerbations of asthma. fiften patients did not
complete the trial, results for 86 who did given. was a phase IV trial involved studying
patients over an extended period in which they are given regular therapy. the risk of drop
out is much greater in such trials, and this is reflected in the number of patients
discontinuing in this study.

table.4.17 <- tibble(Outcome = c(rep(c(0, 0), 27), rep(c(0, 1), 3),
                                 rep(c(1, 0), 9),
                                 rep(c(2, 2), 1), rep(c(3, 0), 1),
                                 rep(c(0, 0), 24), rep(c(1, 0), 7),
                                 rep(c(0, 1), 10), rep(c(3, 1), 1),
                                 rep(c(2, 2), 1), rep(c(1, 4), 1),
                                 rep(c(1, 6), 1)),
                     Period = factor(rep(1:2, 86), levels = 1:2),
                     Treatment = factor(c(rep(c("P", "S"), 41),
                                          rep(c("S", "P"), 45)),
                                        levels = c("P", "S")),
                     Subject = factor(rep(1:86, each = 2),
                                      levels = 1:86),
                     Sequence = factor(c(rep("FS", 82), rep("SF", 90)),
                                       levels = c("FS", "SF")),
                     `Sequence Index` = factor(c(rep(1, 82), rep(2, 90)),
                                               levels = 1:2))

# Table 6.1
table.6.1 <- tibble(Outcome = c(0, 2, 42, 25, 3, 27, 55, NA, NA,
                                0, 68, 9, 9, 0, 11, 0, 8, 31, 81, 47,
                                6, 44, 2, 33, 18, 0, 22, 33, 39, 17,
                                24, 10, 36, 27, 39, 56, 0, 29, 38,
                                0, 0, 0, 41, 15, 18, 45, 66, 41, 59, 16,
                                24, 0, 33, 1, 1, 66, 58, 0, 100, 0, 11,
                                48, 25, 19, 10, 79, 33, 37, 38, 15, 0, 0,
                                49, 11, 12, 38, 77, 28, 1, 0, 36, 0, 53, NA,
                                9, 29, 71, 87, 2, 38, 51, 1, 10, 1, NA, NA,
                                85, 53, NA),
                    Period = factor(rep(1:3, 33), levels = 1:3),
                    Treatment = factor(c(rep(c("D1", "D2", "P"), 5),
                                         rep(c("P", "D2", "D1"), 6),
                                         rep(c("D2", "D1", "P"), 6),
                                         rep(c("D2", "P", "D1"), 6),
                                         rep(c("D1", "P", "D2"), 6),
                                         rep(c("P", "D1", "D2"), 4)),
                                       levels = c("P", "D1", "D2")),
                    Subject = factor(rep(1:33, each = 3),
                                     levels = 1:33),
                    Sequence = factor(c(rep("D1-D2-P", 15),
                                        rep("P-D2-D1", 18),
                                        rep("D2-D1-P", 18),
                                        rep("D2-P-D1", 18),
                                        rep("D1-P-D2", 18),
                                        rep("P-D1-D2", 12)),
                                      levels = c("D1-D2-P", "P-D2-D1",
                                                 "D2-D1-P", "D2-P-D1",
                                                 "D1-P-D2", "P-D1-D2")),
                    `Sequence Index` = factor(c(rep(1, 15),
                                                rep(2, 18),
                                                rep(3, 18),
                                                rep(4, 18),
                                                rep(5, 18),
                                                rep(6, 12)),
                                              levels = 1:6),
                    `Substituted Data` = factor(c(rep(0, 50), 1,
                                                  rep(0, 48)), levels = 0:1))

table.6.6 <- tibble(Breaks = c(0, 3, 1, rep(0, 34), 1, 0, 2, 2, rep(0, 9),
                               1, rep(0, 7), 1, 3, rep(0, 4), 1, rep(0, 11),
                               1, rep(0, 10), 1, rep(0, 20)),
                    n = c(rep(6, 46), 5, rep(6, 3), 5, 6, 3, rep(6, 3),
                          rep(3, 2), rep(6, 3), 3, rep(6, 3), 3, rep(6, 2),
                          2, 6, 2, rep(6, 3), 2, 6, 2, 6, 5, 4, 3, rep(1, 3),
                          rep(6, 2), 0, rep(6, 2), 0, 4, 5, 0, 4, 3, 0,
                          6, 0, 0, 6, 0, 0, 4, 0, 0, 3, 0, 0),
                    Period = factor(rep(1:3, 36), levels = 1:3),
                    Treatment = factor(c(rep(c("A", "B", "C"), 3),
                                         rep(c("A", "C", "B"), 3),
                                         rep(c("B", "A", "C"), 2),
                                         rep(c("B", "C", "A"), 3),
                                         rep(c("C", "A", "B"), 2),
                                         rep(c("C", "B", "A"), 2),
                                         rep(c("B", "C", "A"), 2),
                                         rep(c("B", "A", "C"), 2),
                                         rep(c("B", "C", "A"), 1),
                                         rep(c("C", "A", "B"), 2),
                                         rep(c("B", "C", "A"), 1),
                                         rep(c("C", "A", "B"), 1),
                                         rep(c("C", "B", "A"), 2),
                                         rep(c("C", "A", "B"), 1),
                                         rep(c("A", "B", "C"), 1),
                                         rep(c("B", "A", "C"), 1),
                                         rep(c("C", "B", "A"), 1),
                                         rep(c("B", "A", "C"), 1),
                                         rep(c("C", "B", "A"), 1),
                                         rep(c("A", "C", "B"), 1),
                                         rep(c("C", "B", "A"), 1),
                                         rep(c("A", "C", "B"), 1),
                                         rep(c("B", "A", "C"), 1)),
                                       levels = c("A", "B", "C")),
                    Subject = factor(rep(c(5, 17, 35, 6, 36, 39, 4, 8, 10,
                                           40, 43, 26, 42, 3, 29, 13, 32, 14,
                                           30, 20, 12, 31, 2, 1, 11, 19, 15,
                                           7, 38, 41, 22, 33, 16, 18, 25, 34), each = 3),
                                     levels = c(1:8, 10:20, 22, 25, 26, 29:36, 38:43)),
                    Sequence = factor(c(rep("ABC", 9), rep("ACB", 9),
                                        rep("BAC", 6), rep("BCA", 9),
                                        rep("CAB", 6), rep("CBA", 6),
                                        rep("BCA", 6), rep("BAC", 6),
                                        rep("BCA", 3), rep("CAB", 6),
                                        rep("BCA", 3), rep("CAB", 3),
                                        rep("CBA", 6), rep("CAB", 3),
                                        rep("ABC", 3), rep("BAC", 3),
                                        rep("CBA", 3), rep("BAC", 3),
                                        rep("CBA", 3), rep("ACB", 3),
                                        rep("CBA", 3), rep("ACB", 3),
                                        rep("BAC", 3)),
                                      levels = c("ABC", "ACB",
                                                 "BAC", "BCA",
                                                 "CAB", "CBA")),
                    `Sequence Index` = factor(c(rep(1, 9), rep(2, 9),
                                                rep(3, 6), rep(4, 9),
                                                rep(5, 6), rep(6, 6),
                                                rep(4, 6), rep(3, 6),
                                                rep(4, 3), rep(5, 6),
                                                rep(4, 3), rep(5, 3),
                                                rep(6, 6), rep(5, 3),
                                                rep(1, 3), rep(3, 3),
                                                rep(6, 3), rep(3, 3),
                                                rep(6, 3), rep(2, 3),
                                                rep(6, 3), rep(2, 3),
                                                rep(3, 3)),
                                              levels = 1:6),
                    Condoms = c(rep(18, 15*3), rep(17, 6), rep(15, 15),
                                rep(14, 12), rep(12, 3), rep(3, 3),
                                rep(12, 6), rep(9, 3), rep(7, 3), rep(6, 6),
                                rep(4, 3), rep(3, 3)),
                    Periods = factor(c(rep(3, 3*28), rep(2, 12),
                                       rep(1, 12)), levels = 1:3))


table.7.1 <- tibble(FEV1 = c(2.7, 1.7, 2.6, 2.2,
                             2.5, 2.4, 2.4, 2.4,
                             2.6, 2.5, 2.5, 2.4,
                             2.0, 2.2, 2.6, 2.6,
                             3.6, 3.7, 3.7, 3.6,
                             1.4, 2.4, 0.9, 1.1,
                             2.6, 2.6, 2.5, 2.4,
                             2.5, 2.2, 2.0, 2.7,
                             1.4, 1.3, 1.3, 1.3,
                             2.3, 2.3, 2.2, 2.2,
                             1.0, 2.7, 1.9, 1.8,
                             2.2, 2.1, 2.2, 1.9,
                             2.0, 1.7, 1.6, 1.7,
                             2.6, 2.2, 1.8, 1.9,
                             3.3, 3.3, 3.6, 3.7,
                             2.5, 2.2, 2.4, 2.3),
                    Period = factor(rep(1:4, 16), levels = 1:4),
                    Treatment = factor(c(rep(c("A", "B", "D", "C"), 4),
                                         rep(c("B", "C", "A", "D"), 4),
                                         rep(c("C", "D", "B", "A"), 4),
                                         rep(c("D", "A", "C", "B"), 4)),
                                       levels = c("A", "B", "C", "D")),
                    Subject = factor(rep(c(3, 5, 12, 13, 4, 6, 10, 16, 2, 8, 9,
                                           14, 1, 7, 11, 15), each = 4),
                                     levels = 1:16),
                    Sequence = factor(rep(c("ABDC", "BCAD", "CDBA", "DACB"),
                                          each = 16),
                                      levels = c("ABDC", "BCAD",
                                                 "CDBA", "DACB")),
                    `Sequence Index` = factor(rep(1:4, 16),
                                              levels = 1:4))

table.7.3 <- tibble(FEV1 = c(3.400, 2.500, 2.250, 1.925, 1.460, 1.260,
                             1.480, 0.880, 2.050, 2.100, 2.500, 3.500,
                             1.600, 2.650, 1.750, 2.190, 0.640, 0.840,
                             2.700, 2.250, 0.900, 0.925, 1.270, 1.010,
                             2.150, 2.100, 2.500, 2.340, 1.750, 1.725,
                             1.370, 1.120, 1.750, 1.350, 2.525, 2.150,
                             1.080, 0.840, 3.120, 2.310, 2.100, 3.100,
                             2.300, 2.700, 1.030, 1.870, 0.810, 0.940),
                    Period = factor(rep(1:2, 24), levels = 1:2),
                    Treatment = factor(c(rep(c("F12", "P"), 5),
                                         rep(c("P", "F12"), 4),
                                         rep(c("F24", "F12"), 4),
                                         rep(c("F12", "F24"), 3),
                                         rep(c("F24", "P"), 4),
                                         rep(c("P", "F24"), 4)),
                                       levels = c("P", "F12", "F24")),
                    Subject = factor(rep(c(4, 11, 14, 21, 35, 5, 9, 16, 19,
                                           2, 12, 13, 36, 6, 10, 15, 3, 7,
                                           18, 22, 1, 8, 17, 20), each = 2),
                                     levels = c(1:22, 35:36)),
                    Sequence = factor(c(rep("F12-P", 10),
                                        rep("P-F12", 8),
                                        rep("F24-F12", 8),
                                        rep("F12-F24", 6),
                                        rep("F24-P", 8),
                                        rep("P-F24", 8)),
                                      levels = c("P-F12", "P-F24",
                                                 "F12-P", "F12-F24",
                                                 "F24-P","F24-F12")),
                    `Sequence Index` = factor(c(rep(3, 10),
                                                rep(1, 8),
                                                rep(6, 8),
                                                rep(4, 6),
                                                rep(5, 8),
                                                rep(2, 8)),
                                              levels = 1:6))


table.10.5 <- tibble(Sleep = c(38.1, 23.8, 16.4, 19.7, 16.6, 15.3,
                               39.0, 25.3, 30.2, 31.7, 19.1, 17.6,
                               16.8, 21.8, 20.1, 22.3, 21.8, 15.1,
                               24.0, 19.5, 15.1, 17.8, 17.6, 21.9,
                               15.5, 20.0, 10.1),
                     Period = factor(rep(1:3, 9), levels = 1:3),
                     Treatment = factor(c(rep(c("O", "A", "O"), 5),
                                          rep(c("A", "O", "A"), 4)),
                                        levels = c("A", "O")),
                     Subject = factor(rep(1:9, each = 3),
                                      levels = c(1:22, 35:36)),
                     Sequence = factor(c(rep("OAO", 15),
                                         rep("AOA", 12)),
                                       levels = c("AOA", "OAO")),
                     `Sequence Index` = factor(c(rep(1, 15),
                                                 rep(2, 12)),
                                               levels = 1:2))

heartburn <- tibble(MD = c(7, 35, 5, 60, 10, 28, 14, 36, 60, 35,
                           5, 15, 2, 13, 32, 60, 60, 35, 25, 14,
                           15, 15, 26, 60, 60, 60, 60, 60, 15, 35,
                           11, 35, 6, 13, 4, 60, 9, 60, 6, 18,
                           60, 60, 60, 10, 3, 15, 60, 10, 10, 10,
                           10, 15, 60, 12, 6, 15, 60, 15, 15, 35,
                           8, 60, 60, 60, 60, 60, 4, 60, 7, 60,
                           7, 60, 60, 12, 60, 60, 9, 60, 18, 60,
                           28, 60, 32, 25, 15, 60, 11, 60, 32, 60,
                           60, 7, 60, 7, 60, 8, 32, 10, 60, 8,
                           60, 8, 60, 4, 25, 60, 32, 60, 60, 4,
                           12, 60, 9, 35, 10, 26, 60, 13, 30, 15),
                    CAT = factor(c(1, 2, 1, 4, 1, 2, 1, 4, 4, 2,
                                   1, 1, 1, 1, 2, 4, 4, 2, 2, 1,
                                   1, 1, 2, 4, 4, 4, 4, 4, 1, 2,
                                   1, 2, 1, 1, 1, 4, 1, 4, 1, 3,
                                   4, 4, 4, 1, 1, 1, 4, 1, 1, 1,
                                   1, 1, 4, 1, 1, 1, 4, 1, 1, 2,
                                   1, 4, 4, 4, 4, 4, 1, 4, 1, 4,
                                   1, 4, 4, 1, 4, 4, 1, 4, 3, 4,
                                   2, 4, 2, 2, 1, 4, 1, 4, 2, 4,
                                   4, 1, 4, 1, 4, 1, 2, 1, 4, 1,
                                   4, 1, 4, 1, 2, 4, 2, 4, 4, 1,
                                   1, 4, 1, 2, 1, 2, 4, 1, 2, 1),
                                 levels = 1:4),
                    Age = c(rep(55, 2), rep(35, 2), rep(36, 2), rep(44, 2),
                            rep(38, 2), rep(46, 2), rep(56, 2), rep(30, 2),
                            rep(42, 2), rep(39, 2), rep(30, 2), rep(34, 2),
                            rep(29, 2), rep(37, 2), rep(35, 2), rep(27, 2),
                            rep(33, 2), rep(29, 2), rep(32, 2), rep(38, 2),
                            rep(52, 2), rep(25, 2), rep(65, 2), rep(50, 2),
                            rep(28, 2), rep(39, 2), rep(50, 2), rep(52, 2),
                            rep(38, 2), rep(31, 2),
                            rep(31, 2), rep(30, 2), rep(28, 2), rep(48, 2),
                            rep(27, 2), rep(54, 2), rep(30, 2), rep(30, 2),
                            rep(31, 2), rep(27, 2), rep(30, 2), rep(29, 2),
                            rep(34, 2), rep(NA, 2), rep(48, 2), rep(32, 2),
                            rep(22, 2), rep(29, 2), rep(29, 2), rep(26, 2),
                            rep(30, 2), rep(47, 2), rep(33, 2), rep(38, 2),
                            rep(29, 2), rep(28, 2), rep(26, 2), rep(34, 2),
                            rep(32, 2), rep(31, 2)),
                    Sex = factor(c(rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(1, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(1, 2),
                                   rep(2, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(1, 2),
                                   rep(1, 2), rep(2, 2), rep(1, 2), rep(1, 2),
                                   rep(2, 2), rep(2, 2), rep(2, 2), rep(1, 2),
                                   rep(2, 2), rep(1, 2), rep(2, 2), rep(2, 2),
                                   rep(1, 2), rep(2, 2), rep(1, 2), rep(2, 2),
                                   rep(1, 2), rep(1, 2), rep(2, 2), rep(1, 2),
                                   rep(2, 2), rep(2, 2), rep(1, 2), rep(2, 2),
                                   rep(2, 2), rep(2, 2)), levels = c(1, 2)),
                    Freq = c(rep(7, 2), rep(3, 2), rep(4, 2), rep(4, 2),
                             rep(7, 2), rep(4, 2), rep(5, 2), rep(3, 2),
                             rep(2, 2), rep(2, 2), rep(4, 2), rep(3, 2),
                             rep(3, 2), rep(3, 2), rep(3, 2), rep(5, 2),
                             rep(4, 2), rep(2, 2), rep(4, 2), rep(3, 2),
                             rep(6, 2), rep(3, 2), rep(3, 2), rep(4, 2),
                             rep(2, 2), rep(3, 2), rep(2, 2), rep(4, 2),
                             rep(3, 2), rep(3, 2),
                             rep(4, 2), rep(3, 2), rep(4, 2), rep(5, 2),
                             rep(2, 2), rep(3, 2), rep(2, 2), rep(2, 2),
                             rep(2, 2), rep(3, 2), rep(3, 2), rep(5, 2),
                             rep(2, 2), rep(4, 2), rep(5, 2), rep(3, 2),
                             rep(2, 2), rep(5, 2), rep(5, 2), rep(4, 2),
                             rep(2, 2), rep(3, 2), rep(3, 2), rep(4, 2),
                             rep(5, 2), rep(3, 2), rep(2, 2), rep(2, 2),
                             rep(3, 2), rep(2, 2)),
                    Center = factor(c(rep(1, 2*30), rep(2, 2*30)),
                                    levels = c(1, 2)),
                    Subject = factor(rep(1:60, each = 2), levels = 1:60),
                    Sequence.index = factor(c(rep(c(1, 2), each = 2*15),
                                              rep(c(1, 2), each = 2*15)),
                                            levels = c(1, 2)),
                    Sequence = factor(c(rep(c("AP", "PA"), each = 2*15),
                                        rep(c("PA", "AP"), each = 2*15)),
                                      levels = c("AP", "PA")),
                    Treatment = factor(c(rep(c("A", "P"), 15),
                                         rep(c("P", "A"), 15),
                                         rep(c("A", "P"), 15),
                                         rep(c("P", "A"), 15)),
                                       levels = c("A", "P")),
                    Period = factor(rep(c(1, 2), 60), levels = c(1, 2)))

cerebral <- tibble(Outcome = factor(c(rep(c(0, 0), 6), rep(c(0, 1), 2),
                                      rep(c(1, 0), 1), rep(c(1, 1), 7),
                                      rep(c(0, 0), 4), rep(c(0, 1), 2),
                                      rep(c(1, 0), 3), rep(c(1, 1), 8),
                                      rep(c(0, 0), 6), rep(c(0, 1), 0),
                                      rep(c(1, 0), 6), rep(c(1, 1), 22),
                                      rep(c(0, 0), 9), rep(c(0, 1), 4),
                                      rep(c(1, 0), 2), rep(c(1, 1), 18)),
                                    levels = c(0, 1)),
                   Center = factor(c(rep(1, 2*33), rep(2, 2*67)),
                                   levels = c(1, 2)),
                   Subject = factor(rep(1:100, each = 2), levels = 1:100),
                   Sequence.index = factor(c(rep(1, 2*16), rep(2, 2*17),
                                             rep(1, 2*34), rep(2, 2*33)),
                                           levels = c(1, 2)),
                   Sequence = factor(c(rep("AB", 2*16), rep("BA", 2*17),
                                       rep("AB", 2*34), rep("BA", 2*33)),
                                     levels = c("AB", "BA")),
                   Treatment = factor(c(rep(c("A", "B"), 16),
                                        rep(c("B", "A"), 17),
                                        rep(c("A", "B"), 34),
                                        rep(c("B", "A"), 33)),
                                      levels = c("A", "B")),
                   Period = factor(rep(c(1, 2), 100), levels = c(1, 2)))

estradiol <- tibble(Outcome = c(4.399, 3.779, 4.748, 4.524, 4.202, 3.185,
                                4.487, 4.154, 4.614, 3.724, 2.376, 1.297,
                                4.123, 3.656, 3.463, 2.452, 3.123, 1.258,
                                3.455, 3.089, 4.132, 2.734, 3.539, 2.742,
                                3.166, 3.625, 3.714, 3.273, 3.209, 2.667,
                                2.692, 3.175, 4.406, 4.638, 4.890, 4.971,
                                3.470, 2.141, 3.732, 4.008, 4.644, 4.307,
                                3.792, 3.646, 3.605, 2.452),
                    IQ = c(rep(98.8, 2), rep(75.0, 2), rep(92.5, 2), rep(82.5, 2),
                           rep(100.0, 2), rep(117.5, 2), rep(90.0, 2), rep(103.8, 2),
                           rep(106.3, 2), rep(108.8, 2), rep(106.3, 2), rep(103.8, 2),
                           rep(117.5, 2), rep(118.8, 2), rep(88.8, 2), rep(112.5, 2),
                           rep(91.3, 2), rep(87.5, 2), rep(101.3, 2), rep(101.3, 2),
                           rep(101.3, 2), rep(107.5, 2), rep(103.8, 2)),
                    Subject = factor(rep(1:23, each = 2), levels = 1:23),
                    Sequence.index = factor(c(rep(1, 2*12), rep(2, 2*11)),
                                            levels = c(1, 2)),
                    Sequence = factor(c(rep("AB", 2*12), rep("BA", 2*11)),
                                      levels = c("AB", "BA")),
                    Treatment = factor(c(rep(c("A", "B"), 12),
                                         rep(c("B", "A"), 11)),
                                       levels = c("A", "B")),
                    Period = factor(rep(c(1, 2), 23), levels = c(1, 2)))

plaque <- tibble(Outcome = c(0.796, 0.790, 0.411, 0.339, 0.385, 0.596,
                             0.333, 0.333, 0.550, 0.550, 0.217, 0.800,
                             0.086, 0.569, 0.250, 0.589, 0.062, 0.458,
                             0.429, 1.339, 0.036, 0.143, 0.036, 0.661,
                             0.200, 0.275, 0.065, 0.226, 0.117, 0.435,
                             0.121, 0.224, 0.250, 1.271, 0.180, 0.460,
                             0.062, 0.000, 0.143, 0.000, 0.453, 0.344,
                             0.235, 0.059, 0.792, 0.937, 0.852, 0.024,
                             1.200, 0.033, 0.080, 0.000, 0.241, 0.019,
                             0.271, 0.687, 0.304, 0.000, 0.341, 0.136,
                             0.462, 0.000, 0.421, 0.395, 0.187, 0.167,
                             0.792, 0.917),
                 Subject = factor(rep(1:34, each = 2), levels = 1:34),
                 Sequence.index = factor(c(rep(1, 2*18), rep(2, 2*16)),
                                         levels = c(1, 2)),
                 Sequence = factor(c(rep("AB", 2*18), rep("BA", 2*16)),
                                   levels = c("AB", "BA")),
                 Treatment = factor(c(rep(c("A", "B"), 18),
                                      rep(c("B", "A"), 16)),
                                    levels = c("A", "B")),
                 Period = factor(rep(c(1, 2), 34), levels = c(1, 2)))



eg.6.1 <- tibble(Outcome = factor(c(rep(c(0,0,0,0), 1), rep(c(0,0,0,1), 0),
                                    rep(c(0,0,1,0), 1), rep(c(0,0,1,1), 1),
                                    rep(c(0,1,0,0), 1), rep(c(0,1,0,1), 1),
                                    rep(c(0,1,1,0), 1), rep(c(0,1,1,1), 0),
                                    rep(c(1,0,0,0), 1), rep(c(1,0,0,1), 1),
                                    rep(c(1,0,1,0), 1), rep(c(1,0,1,1), 2),
                                    rep(c(1,1,0,0), 1), rep(c(1,1,0,1), 0),
                                    rep(c(1,1,1,0), 2), rep(c(1,1,1,1), 4),
                                    rep(c(0,0,0,0), 0), rep(c(0,0,0,1), 1),
                                    rep(c(0,0,1,0), 1), rep(c(0,0,1,1), 0),
                                    rep(c(0,1,0,0), 1), rep(c(0,1,0,1), 1),
                                    rep(c(0,1,1,0), 1), rep(c(0,1,1,1), 1),
                                    rep(c(1,0,0,0), 0), rep(c(1,0,0,1), 1),
                                    rep(c(1,0,1,0), 0), rep(c(1,0,1,1), 0),
                                    rep(c(1,1,0,0), 1), rep(c(1,1,0,1), 2),
                                    rep(c(1,1,1,0), 3), rep(c(1,1,1,1), 9),
                                    rep(c(0,0,0,0), 1), rep(c(0,0,0,1), 1),
                                    rep(c(0,0,1,0), 0), rep(c(0,0,1,1), 0),
                                    rep(c(0,1,0,0), 1), rep(c(0,1,0,1), 1),
                                    rep(c(0,1,1,0), 1), rep(c(0,1,1,1), 1),
                                    rep(c(1,0,0,0), 1), rep(c(1,0,0,1), 0),
                                    rep(c(1,0,1,0), 1), rep(c(1,0,1,1), 0),
                                    rep(c(1,1,0,0), 1), rep(c(1,1,0,1), 2),
                                    rep(c(1,1,1,0), 3), rep(c(1,1,1,1), 5),
                                    rep(c(0,0,0,0), 1), rep(c(0,0,0,1), 0),
                                    rep(c(0,0,1,0), 1), rep(c(0,0,1,1), 0),
                                    rep(c(0,1,0,0), 0), rep(c(0,1,0,1), 2),
                                    rep(c(0,1,1,0), 2), rep(c(0,1,1,1), 0),
                                    rep(c(1,0,0,0), 0), rep(c(1,0,0,1), 0),
                                    rep(c(1,0,1,0), 0), rep(c(1,0,1,1), 1),
                                    rep(c(1,1,0,0), 0), rep(c(1,1,0,1), 4),
                                    rep(c(1,1,1,0), 0), rep(c(1,1,1,1), 10)),
                                  levels = c(0, 1)),
                 Subject = factor(rep(1:80, each = 4), levels = 1:80),
                 Sequence.index = factor(c(rep(1, 4*18), rep(2, 4*22),
                                           rep(3, 4*19), rep(4, 4*21)),
                                         levels = c(1, 2)),
                 Sequence = factor(c(rep("ABCD", 4*18), rep("BDAC", 4*22),
                                     rep("CADB", 4*19), rep("DCBA", 4*21)),
                                   levels = c("ABCD", "BDAC", "CADB", "DCBA")),
                 Treatment = factor(c(rep(c("A", "B", "C", "D"), 18),
                                      rep(c("B", "D", "A", "C"), 22),
                                      rep(c("C", "A", "D", "B"), 19),
                                      rep(c("D", "C", "B", "A"), 21)),
                                    levels = c("A", "B", "C", "D")),
                 Period = factor(rep(c(1, 2, 3, 4), 80), levels = c(1, 2, 3, 4)))

#eg from table 2.1 and table 2.2 in JK book
PEFR <- read_csv("pefr.csv")
PEFR$Severity <- as.factor(PEFR$Severity)
PEFR$Period <- as.factor(PEFR$Period)
PEFR$Treatment <- as.factor(PEFR$Treatment)
PEFR$Subject <- as.factor(PEFR$Subject)
PEFR$Sequence <- as.factor(PEFR$Sequence)
PEFR$`Sequence Index` <- as.factor(PEFR$`Sequence Index`)
PEFR$`Binary Baseline NAM` <- as.factor(PEFR$`Binary Baseline NAM`)

#JK book
table.2.13 <- tibble(FEV1 = c(1.28, 1.33, 1.60, 2.21, 2.46, 2.43, 1.41, 1.81,
                              1.40, 0.85, 1.12, 1.20, 0.90, 0.90, 2.41, 2.79,
                              3.06, 1.38, 2.68, 2.10, 2.60, 2.32, 1.48, 1.30,
                              2.08, 2.34, 2.72, 2.48, 1.94, 1.11, 3.35, 3.23,
                              1.16, 1.25),
                     Period = factor(rep(1:2, 17), levels = 1:2),
                     Treatment = factor(c(rep(c("A", "B"), 8),
                                          rep(c("B", "A"), 9)),
                                        levels = c("A", "B")),
                     Subject = factor(rep(1:17, each = 2),
                                      levels = 1:17),
                     Sequence = factor(c(rep("AB", 16),
                                         rep("BA", 18)),
                                       levels = c("AB", "BA")),
                     `Sequence Index` = factor(c(rep(1, 16),
                                                 rep(2, 18)),
                                               levels = 1:2),
                     Baseline = c(1.09, 1.24, 1.38, 1.90, 2.27, 2.19,
                                  1.34, 1.47, 1.31, 0.85, 0.96, 1.12,
                                  0.66, 0.78, 1.69, 1.90, 1.74, 1.54,
                                  2.41, 2.13, 3.05, 2.18, 1.20, 1.41,
                                  1.70, 2.21, 1.89, 2.05, 0.89, 0.72,
                                  2.41, 2.83, 0.96, 1.01))

# Table 2.30
table.2.30 <- tibble(Duration = c(720, 759, 840, 840, 614, 750, 1020, 780,
                                  510, 550, 780, 780, 740, 720, 720, 490,
                                  1280, 1170, 345, 320, 795, 720, 280, 280,
                                  325, 300, 370, 300, 720, 798, 540, 462,
                                  1020, 1020, 322, 510, 360, 540, 330, 505,
                                  840, 868, 780, 780, 780, 780, 180, 180,
                                  540, 540, 1020, 1020, 480, 540, 370, 406,
                                  220, 220, 285, 270),
                     Period = factor(rep(1:2, 30), levels = 1:2),
                     Treatment = factor(c(rep(c("A", "B"), 14),
                                          rep(c("B", "A"), 16)),
                                        levels = c("A", "B")),
                     Subject = factor(rep(1:30, each = 2),
                                      levels = 1:30),
                     Sequence = factor(c(rep("AB", 28),
                                         rep("BA", 32)),
                                       levels = c("AB", "BA")),
                     `Sequence Index` = factor(c(rep(1, 28),
                                                 rep(2, 32)),
                                               levels = 1:2),
                     Baseline = c(rep(540, 2), rep(1200, 2), rep(855, 2),
                                  rep(395, 2), rep(540, 2), rep(510, 2),
                                  rep(780, 2), rep(840, 2), rep(1100, 2),
                                  rep(400, 2), rep(640, 2), rep(285, 2),
                                  rep(405, 2), rep(390, 2), rep(900, 2),
                                  rep(475, 2), rep(1002, 2), rep(360, 2),
                                  rep(190, 2), rep(300, 2), rep(750, 2),
                                  rep(780, 2), rep(810, 2), rep(240, 2),
                                  rep(560, 2), rep(1020, 2), rep(540, 2),
                                  rep(450, 2), rep(270, 2), rep(240, 2)))

#p189
innovo <- tibble(kPa = c(6.1, 6.1, 5.6, 7.8,
                         11.4, 7.6, 10.8, 12.1,
                         10.7, 7.6, 10.1, 8.0,
                         2.9, 2.8, 2.7, 3.4,
                         9.9, 8.1, 6.0, 12.2,
                         5.9, 7.2, 9.0, 9.0,
                         6.7, 5.8, 6.5, 6.6,
                         9.6, 7.6, 12.6, 7.9,
                         6.6, 7.4, 7.3, 7.1,
                         9.8, 9.6, 12.9, 10.0,
                         7.4, NA, NA, NA,
                         17.8, 14.3, 12.6, 10.3,
                         4.6, 3.4, 4.1, 5.1),
                 Period = factor(rep(1:4, 13), levels = 1:4),
                 Treatment = factor(c("D", "C", "B", "A", "B", "C", "A", "D",
                                      "C", "D", "B", "A", "A", "B", "C", "D",
                                      "D", "C", "A", "B", "C", "A", "B", "D",
                                      "C", "D", "A", "B", "C", "B", "D", "A",
                                      "A", "B", "D", "C", "C", "A", "D", "B",
                                      "A", "C", "B", "D", "C", "A", "B", "D",
                                      "C", "B", "A", "D"),
                                    levels = c("A", "B", "C", "D")),
                     Subject = factor(rep(1:13, each = 4),
                                      levels = 1:13),
                     Sequence = factor(rep(c("DCBA", "BCAD", "CDBA", "ABCD",
                                         "DCAB", "CABD", "CDAB", "CBDA",
                                         "ABDC", "CADB", "ACBD", "CABD",
                                         "CBAD"), each = 4),
                                       levels = c("ABCD", "ABDC", "ACBD",
                                                  "BCAD", "CABD", "CADB",
                                                  "CBAD", "CBDA", "CDAB",
                                                  "CDBA", "DCAB", "DCBA")),
                     `Sequence Index` = factor(rep(c(12, 4, 10, 1, 11, 5, 9, 8,
                                                 2, 6, 3, 5, 7), each = 4),
                                               levels = 1:12),
                     Baseline = c(6.4, 4.5, 3.0, 5.1,
                                  11.1, 7.0, 8.1, 11.0,
                                  7.0, 5.9, 6.7, 6.8,
                                  2.6, 2.5, 2.3, 2.1,
                                  8.7, 8.3, 8.0, 5.5,
                                  5.8, 6.8, 7.1, 7.3,
                                  7.0, 6.4, 6.0, 6.1,
                                  7.4, 6.5, 6.3, 6.0,
                                  5.9, 6.0, 5.9, 5.9,
                                  6.5, 4.7, 4.1, 4.8,
                                  2.1, NA, NA, NA,
                                  7.8, 7.7, 8.3, 8.5,
                                  2.6, 4.8, 3.9, 2.5))

#p196
example.5.2 <- tibble(Outcome = c(5.15, 5.97, 3.19, 4.74, 6.59, 6.28,
                                  2.26, 4.12, 5.87, 2.99, 4.94, 3.71,
                                  3.81, 1.54, 6.18, 5.56, 2.37, 5.76,
                                  5.15, 5.87, 3.09, 1.44, 3.91, 4.32,
                                  4.32, 6.07, 4.94, 0.62, 2.68, 5.76,
                                  3.60, 1.85, 4.43, 5.15, 0.82, 0.62),
                 Period = factor(rep(1:2, 18), levels = 1:2),
                 Treatment = factor(c("C", "B", "B", "C",
                                      "A", "B", "C", "A",
                                      "B", "A", "A", "C",
                                      "C", "A", "A", "C",
                                      "A", "B", "C", "B",
                                      "B", "A", "B", "C",
                                      "A", "B", "B", "A",
                                      "C", "A", "B", "C",
                                      "C", "B", "A", "C"),
                                    levels = c("A", "B", "C")),
                 Subject = factor(rep(1:18, each = 2),
                                  levels = 1:18),
                 Sequence = factor(c(rep("CB", 2), rep("BC", 2), rep("AB", 2),
                                     rep("CA", 2), rep("BA", 2), rep("AC", 2),
                                     rep("CA", 2), rep("AC", 2), rep("AB", 2),
                                     rep("CB", 2), rep("BA", 2), rep("BC", 2),
                                     rep("AB", 2), rep("BA", 2), rep("CA", 2),
                                     rep("BC", 2), rep("CB", 2), rep("AC", 2)),
                                   levels = c("AB", "AC", "BA", "BC",
                                              "CA", "CB")),
                 `Sequence Index` = factor(c(rep(6, 2), rep(4, 2), rep(1, 2),
                                             rep(5, 2), rep(3, 2), rep(2, 2),
                                             rep(5, 2), rep(2, 2), rep(1, 2),
                                             rep(6, 2), rep(3, 2), rep(4, 2),
                                             rep(1, 2), rep(3, 2), rep(5, 2),
                                             rep(4, 2), rep(6, 2), rep(2, 2)),
                                           levels = 1:6))

#p223
amantadine <- tibble(Outcome = c(12.50, 14.00, 24.25, 22.50, 17.25, 16.25, 28.25, 29.75,
                                 20.00, 19.51, 10.50, 10.00, 19.50, 20.75, 22.50, 23.50,
                                 8.75, 8.75, 10.50, 9.75, 15.00, 18.50, 21.00, 21.50,
                                 22.00, 18.00, 15.00, 13.00, 14.00, 13.75, 22.75, 21.50, 17.75, 16.75),
                     Period = factor(rep(1:2, 17), levels = 1:2),
                     Treatment = factor(c(rep(c("A", "A"), 4),
                                          rep(c("B", "B"), 4),
                                          rep(c("A", "B"), 4),
                                          rep(c("B", "A"), 5)),
                                        levels = c("A", "B")),
                      Subject = factor(rep(1:17, each = 2),
                                       levels = 1:17),
                      Sequence = factor(c(rep("AA", 8), rep("BB", 8), rep("AB", 8),
                                          rep("BA", 10)),
                                        levels = c("AA", "AB", "BA", "BB")),
                      `Sequence Index` = factor(c(rep(1, 8), rep(4, 8), rep(2, 8),
                                                  rep(3, 10)),
                                                levels = 1:4),
                     Baseline = c(rep(14, 2), rep(27, 2), rep(19, 2),
                                  rep(30, 2), rep(21, 2), rep(11, 2),
                                  rep(20, 2), rep(25, 2), rep(9, 2),
                                  rep(12, 2), rep(17, 2), rep(21, 2),
                                  rep(23, 2), rep(15, 2), rep(13, 2),
                                  rep(24, 2), rep(18, 2)))

#p229
blood.pressure <- tibble(Outcome = c(159, 140, 137, 153, 172, 155,
                                     160, 156, 140, 160, 200, 132,
                                     170, 170, 160, 174, 132, 130,
                                     175, 155, 155, 154, 138, 150,
                                     160, 170, 168, 160, 160, 170,
                                     145, 140, 140, 148, 154, 138,
                                     170, 170, 150, 125, 130, 130,
                                     140, 112, 95, 125, 140, 125,
                                     150, 150, 145, 136, 130, 140,
                                     150, 140, 160, 150, 140, 150,
                                     202, 181, 170, 190, 150, 170,
                                     165, 154, 173, 160, 165, 140,
                                     140, 150, 180, 140, 125, 130,
                                     158, 160, 180, 180, 165, 160,
                                     170, 160, 160, 140, 158, 148,
                                     126, 170, 200, 130, 125, 150,
                                     144, 140, 120, 140, 160, 140,
                                     120, 145, 120, 145, 150, 150,
                                     155, 130, 140, 168, 168, 168,
                                     150, 160, 180, 120, 120, 140,
                                     150, 150, 160, 150, 140, 130,
                                     175, 180, 160, 140, 170, 150,
                                     150, 160, 130, 150, 130, 125,
                                     140, 150, 160, 140, 140, 130,
                                     126, 140, 138, 154, 145, 150,
                                     160, 140, 140, 210, 190, 190,
                                     110, 112, 130, 130, 140, 130,
                                     180, 190, 160, 155, 120, 160,
                                     170, 164, 158, 170, 140, 180,
                                     155, 130, 135, 115, 110, 120,
                                     180, 136, 150, 130, 120, 126,
                                     135, 140, 155, 148, 148, 162,
                                     180, 180, 190, 190, 155, 160,
                                     178, 152, 174, 172, 178, 180,
                                     164, 150, 160, 170, 140, 140,
                                     168, 176, 148, 130, 120, 130,
                                     160, 145, 112, 156, 152, 140,
                                     195, 195, 180, 130, 126, 122,
                                     130, 136, 130, 140, 140, 150,
                                     160, 160, 160, 140, 180, 165,
                                     140, 135, 125, 100, 129, 120,
                                     148, 164, 148, 150, 170, 134,
                                     205, 240, 150, 140, 140, 140,
                                     154, 180, 156, 150, 130, 160,
                                     140, 130, 130),
                     Period = factor(rep(1:3, 89), levels = 1:3),
                     Treatment = factor(c(rep(c("A", "B", "B"), 22),
                                          rep(c("B", "A", "A"), 27),
                                          rep(c("A", "B", "A"), 23),
                                          rep(c("B", "A", "B"), 17)),
                                        levels = c("A", "B")),
                     Subject = factor(rep(1:89, each = 3),
                                      levels = 1:89),
                     Sequence = factor(c(rep("ABB", 22*3), rep("BAA", 27*3), rep("ABA", 23*3),
                                         rep("BAB", 17*3)),
                                       levels = c("ABA", "ABB", "BAA", "BAB")),
                     `Sequence Index` = factor(c(rep(2, 22*3), rep(3, 27*3), rep(1, 23*3),
                                                 rep(4, 17*3)),
                                               levels = 1:4),
                     Baseline = c(rep(173, 3), rep(168, 3), rep(200, 3), rep(180, 3),
                                  rep(190, 3), rep(170, 3), rep(185, 3), rep(180, 3),
                                  rep(160, 3), rep(170, 3), rep(165, 3), rep(168, 3),
                                  rep(190, 3), rep(160, 3), rep(190, 3), rep(170, 3),
                                  rep(170, 3), rep(158, 3), rep(210, 3), rep(175, 3),
                                  rep(186, 3), rep(190, 3), rep(168, 3), rep(200, 3),
                                  rep(130, 3), rep(170, 3), rep(190, 3), rep(180, 3),
                                  rep(200, 3), rep(166, 3), rep(188, 3), rep(175, 3),
                                  rep(186, 3), rep(160, 3), rep(135, 3), rep(175, 3),
                                  rep(150, 3), rep(178, 3), rep(170, 3), rep(160, 3),
                                  rep(190, 3), rep(160, 3), rep(200, 3), rep(160, 3),
                                  rep(180, 3), rep(170, 3), rep(165, 3), rep(200, 3),
                                  rep(142, 3), rep(184, 3), rep(210, 3), rep(250, 3),
                                  rep(180, 3), rep(165, 3), rep(210, 3), rep(175, 3),
                                  rep(186, 3), rep(178, 3), rep(150, 3), rep(130, 3),
                                  rep(155, 3), rep(140, 3), rep(180, 3), rep(162, 3),
                                  rep(185, 3), rep(220, 3), rep(170, 3), rep(220, 3),
                                  rep(172, 3), rep(200, 3), rep(154, 3), rep(150, 3),
                                  rep(140, 3), rep(156, 3), rep(215, 3), rep(150, 3),
                                  rep(170, 3), rep(170, 3), rep(198, 3), rep(210, 3),
                                  rep(170, 3), rep(160, 3), rep(168, 3), rep(200, 3),
                                  rep(240, 3), rep(155, 3), rep(180, 3), rep(160, 3),
                                  rep(150, 3)))


#p234
blood.sugar <- tibble(Outcome = c(77, 52, 35, 56, 64, 90, 47, 52, 68, 90, 85, 52, 35, 39, 60, 94, 60, 60, 77, 94,
                                  77, 56, 43, 56, 64, 85, 52, 60, 81, 94, 103, 26, 68, 30, 30, 107, 60, 68, 90, 94,
                                  77, 43, 64, 39, 64, 90, 30, 30, 47, 60, 99, 39, 39, 73, 90, 103, 60, 60, 90, 99,
                                  81, 56, 22, 35, 39, 107, 47, 47, 47, 73, 77, 26, 56, 64, 64, 116, 52, 26, 68, 120,
                                  90, 73, 52, 60, 64, 94, 60, 60, 77, 90, 90, 60, 18, 35, 64, 94, 56, 47, 81, 99,
                                  103, 47, 22, 60, 99, 90, 47, 30, 43, 68, 85, 56, 56, 77, 94, 111, 73, 85, 103, 111,
                                  99, 47, 52, 47, 81, 99, 26, 64, 52, 90, 90, 43, 18, 30, 39, 111, 56, 43, 90, 111,
                                  90, 35, 22, 26, 22, 94, 8, 30, 26, 43, 103, 39, 22, 39, 94, 90, 18, 26, 18, 22,
                                  90, 64, 47, 47, 60, 81, 39, 47, 77, 94, 77, 56, 26, 64, 94, 94, 52, 47, 81, 90,
                                  90, 35, 12, 26, 68, 81, 26, 35, 30, 85, 81, 56, 60, 81, 103, 90, 22, 52, 85, 90,
                                  85, 68, 56, 68, 73, 94, 56, 68, 73, 90, 105, 73, 47, 77, 90, 97, 73, 52, 81, 94,
                                  103, 26, 22, 39, 68, 90, 35, 30, 39, 94, 103, 56, 26, 56, 103, 111, 68, 52, 77, 103,
                                  85, 68, 56, 64, 81, 94, 52, 43, 47, 56, 85, 43, 22, 68, 103, 111, 52, 99, 103, 120,
                                  85, 35, 8, 64, 99, 90, 35, 30, 43, 99, 73, 22, 12, 56, 103, 94, 52, 47, 73, 101,
                                  85, 35, 47, 77, 68, 85, 39, 52, 52, 64, 90, 12, 43, 85, 99, 92, 52, 73, 99, 101,
                                  85, 35, 39, 85, 77, 85, 47, 56, 77, 81, 90, 43, 35, 64, 81, 85, 52, 52, 77, 99,
                                  103, 56, 47, 64, 56, 103, 35, 39, 68, 99, 90, 35, 18, 64, 94, 107, 47, 43, 85, 101,
                                  90, 52, 35, 35, 39, 90, 43, 30, 47, 77, 77, 56, 64, 81, 99, 111, 60, 68, 107, 111,
                                  81, 22, 12, 22, 77, 81, 12, 2, 30, 77, 68, 12, 8, 8, 39, 85, 26, 12, 18, 39,
                                  85, 30, 22, 52, 94, 90, 30, 26, 52, 90, 85, 39, 22, 56, 90, 105, 68, 64, 81, 81,
                                  85, 47, 33, 77, 107, 85, 39, 39, 43, 60, 85, 35, 26, 43, 73, 90, 43, 35, 81, 90,
                                  94, 39, 35, 52, 85, 94, 26, 30, 39, 81, 90, 22, 26, 73, 99, 103, 47, 35, 64, 94),
                      Period = factor(rep(rep(1:4, each = 5), 22), levels = 1:4),
                      Treatment = factor(c(rep(rep(c("A", "B"), each = 5), 22),
                                           rep(rep(c("B", "A"), each = 5), 22)),
                                         levels = c("A", "B")),
                      Subject = factor(rep(1:22, each = 20),
                                          levels = 1:22),
                      Sequence = factor(c(rep("ABAB", 220), rep("BABA", 220)),
                                        levels = c("ABAB", "BABA")),
                      `Sequence Index` = factor(c(rep(1, 220), rep(2, 220)),
                                                levels = 1:2),
                      Hours = factor(rep(c(0, 1.5, 3.0, 4.5, 6.0), 88),
                                         levels = c(0, 1.5, 3.0, 4.5, 6.0)))



#p242
blood.press <- tibble(`Systolic Blood Pressure` = c(112, 113, 86, 86, 93, 90, 106, 91, 84, 102,
                                                    108, 103, 100, 100, 97, 103, 92, 106, 106, 96,
                                                    107, 108, 102, 111, 100, 105, 113, 109, 84, 99,
                                                    101, 100, 99, 81, 106, 100, 100, 111, 110, 96,
                                                    96, 101, 101, 100, 99, 101, 98, 99, 102, 101,
                                                    111, 99, 90, 93, 81, 91, 89, 95, 99, 97,
                                                    105, 113, 109, 104, 102, 102, 111, 106, 104, 101,
                                                    104, 96, 84, 84, 100, 91, 109, 94, 108, 108,
                                                    96, 92, 88, 89, 91, 121, 122, 135, 121, 107,
                                                    112, 109, 108, 92, 102, 101, 101, 98, 102, 106,
                                                    105, 116, 105, 108, 141, 103, 110, 109, 113, 112,
                                                    1120, 112, 111, 102, 99, 104, 108, 102, 112, 101,
                                                    96, 93, 94, 96, 83, 95, 88, 93, 88, 93,
                                                    103, 98, 97, 108, 93, 101, 108, 105, 104, 101,
                                                    114, 97, 96, 109, 102, 99, 110, 105, 104, 106,
                                                    115, 117, 91, 102, 126, 122, 128, 125, 119, 117,
                                                    104, 84, 88, 95, 97, 115, 93, 102, 90, 101,
                                                    103, 97, 84, 97, 102, 115, 108, 114, 113, 107,
                                                    82, 88, 85, 87, 81, 91, 87, 78, 85, 81,
                                                    133, 89, 93, 98, 92, 94, 90, 90, 94, 100,
                                                    87, 83, 78, 92, 80, 83, 88, 88, 93, 98,
                                                    124, 131, 115, 119, 115, 110, 108, 103, 116, 109,
                                                    116, 113, 109, 93, 112, 89, 108, 111, 107, 111,
                                                    121, 120, 87, 93, 94, 100, 95, 100, 114, 117,
                                                    118, 107, 105, 111, 105, 115, 137, 128, 115, 114,
                                                    111, 104, 112, 109, 108, 114, 116, 127, 117, 117,
                                                    113, 107, 115, 117, 105, 117, 104, 110, 105, 117,
                                                    111, 112, 102, 111, 107, 113, 105, 111, 113, 101,
                                                    115, 91, 111, 114, 104, 105, 112, 112, 102, 105,
                                                    112, 110, 109, 112, 110, 103, 116, 106, 110, 110,
                                                    120, 117, 110, 110, 110, 116, 115, 118, 125, 125,
                                                    104, 123, 119, 115, 120, 120, 127, 122, 125, 128,
                                                    117, 113, 117, 120, 125, 130, 123, 128, 126, 123,
                                                    134, 134, 123, 118, 111, 124, 125, 120, 119, 115,
                                                    125, 117, 129, 125, 124, 126, 132, 129, 125, 121,
                                                    118, 110, 119, 108, 105, 110, 113, 117, 123, 113),
                      Period = factor(rep(rep(1:3, each = 10), 12), levels = 1:3),
                      Treatment = factor(c(rep(c("C", "B", "A"), each = 10),
                                           rep(c("B", "A", "C"), each = 10),
                                           rep(c("A", "C", "B"), each = 10),
                                           rep(c("A", "B", "C"), each = 10),
                                           rep(c("C", "A", "B"), each = 10),
                                           rep(c("B", "C", "A"), each = 10),
                                           rep(c("C", "B", "A"), each = 10),
                                           rep(c("A", "C", "B"), each = 10),
                                           rep(c("B", "A", "C"), each = 10),
                                           rep(c("B", "C", "A"), each = 10),
                                           rep(c("C", "A", "B"), each = 10),
                                           rep(c("A", "B", "C"), each = 10)),
                                         levels = c("A", "B", "C")),
                      Subject = factor(rep(1:12, each = 30),
                                       levels = 1:12),
                      Sequence = factor(c(rep("CBA", 30),
                                          rep("BAC", 30),
                                          rep("ACB", 30),
                                          rep("ABC", 30),
                                          rep("CAB", 30),
                                          rep("BCA", 30),
                                          rep("CBA", 30),
                                          rep("ACB", 30),
                                          rep("BAC", 30),
                                          rep("BCA", 30),
                                          rep("CAB", 30),
                                          rep("ABC", 30)),
                                        levels = c("ABC", "ACB", "BAC", "BCA", "CAB", "CBA")),
                      `Sequence Index` = factor(c(rep(6, 30),
                                                  rep(3, 30),
                                                  rep(2, 30),
                                                  rep(1, 30),
                                                  rep(5, 30),
                                                  rep(4, 30),
                                                  rep(6, 30),
                                                  rep(2, 30),
                                                  rep(3, 30),
                                                  rep(4, 30),
                                                  rep(5, 30),
                                                  rep(1, 30)),
                                                levels = 1:6),
                      Hours = factor(rep(c(-30, -15, 15, 30, 45, 60, 75, 90, 120, 140), 36),
                                     levels = c(-30, -15, 15, 30, 45, 60, 75, 90, 120, 140)))

#p254
claudication <- tibble(LVET = c(590, 440, 500, 443, 490, 290, 250, 260,
                                507, 385, 320, 380, 323, 300, 440, 340,
                                250, 330, 300, 290, 400, 260, 310, 380,
                                460, 365, 350, 300, 317, 315, 307, 370,
                                430, 330, 300, 370, 410, 320, 380, 290,
                                390, 393, 280, 280, 430, 323, 375, 310,
                                365, 333, 340, 350, 355, 310, 295, 330),
                       Period = factor(rep(1:4, 14), levels = 1:4),
                       Treatment = factor(c("P", "B", "C", "A", "A", "C", "P", "B",
                                            "C", "A", "B", "P", "B", "P", "A", "C",
                                            "P", "A", "B", "C", "A", "B", "C", "P",
                                            "C", "P", "A", "B", "B", "C", "P", "A",
                                            "P", "B", "C", "A", "C", "B", "A", "P",
                                            "C", "A", "P", "B", "A", "C", "B", "P",
                                            "P", "B", "A", "C", "A", "P", "B", "C"),
                                          levels = c("P", "A", "B", "C")),
                       Subject = factor(rep(1:14, each = 4),
                                        levels = 1:14),
                       Sequence = factor(c(rep("PBCA", 4), rep("ACPB", 4), rep("CABP", 4),
                                           rep("BPAC", 4), rep("PABC", 4), rep("ABCP", 4),
                                           rep("CPAB", 4), rep("BCPA", 4), rep("PBCA", 4),
                                           rep("CBAP", 4), rep("CAPB", 4), rep("ACBP", 4),
                                           rep("PBAC", 4), rep("APBC", 4)),
                                         levels = c("ABCP", "ACBP", "ACPB", "APBC",
                                                    "BCPA", "BPAC", "CABP", "CAPB",
                                                    "CBAP", "CPAB", "PABC", "PBAC",
                                                    "PBCA")),
                       `Sequence Index` = factor(c(rep(13, 4), rep(3, 4), rep(7, 4),
                                                   rep(6, 4), rep(11, 4), rep(1, 4),
                                                   rep(10, 4), rep(5, 4), rep(13, 4),
                                                   rep(9, 4), rep(8, 4), rep(2, 4),
                                                   rep(12, 4), rep(4, 4)),
                                                 levels = 1:13))

# table 5.30
mcnulty <- tibble(Speed = c(2.356, 7.056, 8.021, 2.258, 2.869, 6.385, 7.007, 2.954,
                            4.138, 7.581, 9.681, 1.782, 2.710, 5.628, 5.518, 2.429,
                            1.990, 0.463, 0.573, 8.741, 15.712, 5.323, 3.687, 9.840,
                            6.409, 0.537, 2.795, 3.845, 9.535, 5.359, 4.602, 5.774,
                            0.097, 5.200, 6.275, 4.407, 2.405, 7.508, 2.099, 4.187,
                            1.001, 3.003, 6.910, 6.995, 5.909, 5.127, 4.956, 2.490,
                            1.562, 0.769, 1.538, 7.850, 13.136, 2.453, 2.222, 8.668,
                            2.881, 3.259, 1.990, 7.459, 8.143, 2.453, 5.115, 9.498,
                            1.428, 3.442, 0.671, 0.744, 0.219, 3.467, 3.076, 0.952,
                            0.598, 2.112, 1.111, 0.512, 0.695, 1.990, 4.224, 0.976,
                            3.796, 1.208, 2.124, 4.150, 3.601, 3.919, 2.869, 7.557,
                            7.227, 3.711, 4.859, 6.861, 6.360, 5.860, 3.919, 13.417,
                            2.356, 5.115, 7.215, 2.661, 5.323, 6.763, 3.577, 1.184,
                            5.738, 6.604, 8.106, 5.188, 6.763, 8.497, 7.447, 1.941,
                            3.577, 4.315, 6.006, 12.269, 10.670, 2.661, 5.909, 3.857,
                            13.283, 3.674, 2.624, 8.607, 9.168, 1.513, 4.663, 3.821,
                            2.063, 4.456, 3.845, 1.831, 1.306, 1.477, 3.687, 1.501,
                            2.099, 5.384, 5.371, 1.294, 2.246, 2.551, 5.347, 2.234,
                            4.749, 1.416, 2.954, 2.099, 3.149, 2.429, 1.526, 3.418,
                            3.247, 2.014, 1.916, 2.075, 2.576, 1.867, 1.880, 2.966,
                            0.634, 5.786, 11.549, 4.761, 4.431, 9.706, 7.288, 3.540,
                            0.988, 3.284, 6.714, 4.651, 3.845, 12.001, 6.580, 3.967,
                            3.442, 5.493, 3.369, 7.911, 9.425, 4.248, 4.822, 6.983,
                            3.540, 5.005, 3.210, 8.863, 5.225, 2.466, 4.785, 6.031,
                            1.220, 2.209, 1.416, 1.013, 1.269, 2.356, 5.432, 1.660,
                            1.538, 2.283, 3.564, 1.867, 1.892, 3.809, 4.798, 1.953,
                            1.404, 2.637, 2.392, 2.661, 1.367, 3.540, 2.356, 5.640,
                            5.384, 2.466, 2.917, 5.872, 5.213, 3.723, 2.515, 7.007,
                            0.817, 4.053, 4.004, 2.722, 1.452, 5.909, 3.039, 1.550,
                            2.673, 4.212, 6.543, 2.246, 1.867, 5.628, 4.016, 2.368,
                            3.577, 2.527, 1.526, 6.397, 6.397, 2.160, 1.831, 1.867,
                            3.857, 2.038, 2.759, 7.667, 3.284, 1.758, 2.551, 2.844),
                  Period = factor(rep(1:16, 16), levels = 1:16),
                  Treatment = factor(rep(c("1", "2", "8", "3", "7", "4", "6", "5",
                                           "1", "2", "8", "3", "7", "4", "6", "5",
                                           "2", "3", "1", "4", "8", "5", "7", "6",
                                           "2", "3", "1", "4", "8", "5", "7", "6",
                                           "3", "4", "2", "5", "1", "6", "8", "7",
                                           "3", "4", "2", "5", "1", "6", "8", "7",
                                           "4", "5", "3", "6", "2", "7", "1", "8",
                                           "4", "5", "3", "6", "2", "7", "1", "8",
                                           "5", "6", "4", "7", "3", "8", "2", "1",
                                           "5", "6", "4", "7", "3", "8", "2", "1",
                                           "6", "7", "5", "8", "4", "1", "3", "2",
                                           "6", "7", "5", "8", "4", "1", "3", "2",
                                           "7", "8", "6", "1", "5", "2", "4", "3",
                                           "7", "8", "6", "1", "5", "2", "4", "3",
                                           "8", "1", "7", "2", "6", "3", "5", "4",
                                           "8", "1", "7", "2", "6", "3", "5", "4"), 2),
                                          levels = as.character(1:8)),
                       Subject = factor(rep(1:16, each = 16),
                                        levels = 1:16),
                       Sequence = factor(c(rep("1283746512837465", 16),
                                           rep("2314857623148576", 16),
                                           rep("3425168734251687", 16),
                                           rep("4536271845362718", 16),
                                           rep("5647382156473821", 16),
                                           rep("6758413267584132", 16),
                                           rep("7861524378615243", 16),
                                           rep("8172635481726354", 16),
                                           rep("1283746512837465", 16),
                                           rep("2314857623148576", 16),
                                           rep("3425168734251687", 16),
                                           rep("4536271845362718", 16),
                                           rep("5647382156473821", 16),
                                           rep("6758413267584132", 16),
                                           rep("7861524378615243", 16),
                                           rep("8172635481726354", 16)),
                                         levels = c("1283746512837465", "2314857623148576",
                                                    "3425168734251687", "4536271845362718",
                                                    "5647382156473821", "6758413267584132",
                                                    "7861524378615243", "8172635481726354")),
                       `Sequence Index` = factor(rep(rep(1:8, each = 16), 2),
                                                 levels = 1:8))


#p308
claudication <- tibble(Outcome = c(rep(c(1, 1, 2), 1), rep(c(1, 1, 3), 1),
                                   rep(c(1, 2, 1), 2), rep(c(1, 2, 2), 3),
                                   rep(c(1, 2, 3), 4), rep(c(1, 3, 3), 2),
                                   rep(c(2, 2, 1), 1), rep(c(2, 2, 3), 1),
                                   rep(c(1, 1, 1), 2), rep(c(1, 2, 3), 3),
                                   rep(c(1, 3, 2), 2), rep(c(1, 3, 3), 4),
                                   rep(c(2, 1, 1), 1), rep(c(2, 2, 2), 2),
                                   rep(c(2, 3, 3), 2), rep(c(1, 1, 3), 1),
                                   rep(c(1, 2, 2), 1), rep(c(1, 2, 3), 1),
                                   rep(c(1, 3, 1), 1), rep(c(1, 3, 3), 1),
                                   rep(c(2, 1, 1), 1), rep(c(2, 1, 2), 2),
                                   rep(c(2, 1, 3), 1), rep(c(2, 2, 2), 1),
                                   rep(c(3, 1, 2), 2), rep(c(3, 1, 3), 3),
                                   rep(c(1, 1, 2), 1), rep(c(1, 3, 1), 1),
                                   rep(c(2, 2, 1), 6), rep(c(2, 3, 1), 1),
                                   rep(c(3, 1, 1), 1), rep(c(3, 2, 1), 1),
                                   rep(c(3, 2, 2), 1), rep(c(1, 1, 1), 3),
                                   rep(c(1, 2, 3), 2), rep(c(2, 1, 2), 1),
                                   rep(c(2, 2, 1), 1), rep(c(2, 3, 3), 1),
                                   rep(c(3, 1, 2), 2), rep(c(3, 1, 3), 4),
                                   rep(c(1, 1, 1), 1), rep(c(1, 3, 3), 1),
                                   rep(c(2, 1, 1), 3), rep(c(2, 1, 2), 1),
                                   rep(c(2, 2, 1), 1), rep(c(2, 3, 1), 2),
                                   rep(c(3, 1, 1), 2), rep(c(3, 1, 2), 1),
                                   rep(c(3, 1, 3), 1), rep(c(3, 3, 1), 1)),
                       Period = factor(rep(1:3, 86), levels = 1:3),
                       Treatment = factor(c(rep(c("A", "B", "C"), 15),
                                            rep(c("A", "C", "B"), 16),
                                            rep(c("B", "A", "C"), 15),
                                            rep(c("B", "C", "A"), 12),
                                            rep(c("C", "A", "B"), 14),
                                            rep(c("C", "B", "A"), 14)),
                                          levels = c("A", "B", "C")),
                       Subject = factor(rep(1:86, each = 3),
                                        levels = 1:86),
                       Sequence = factor(c(rep("ABC", 45), rep("ACB", 48), rep("BAC", 45),
                                           rep("BCA", 36), rep("CAB", 42), rep("CBA", 42)),
                                         levels = c("ABC", "ACB", "BAC", "BCA", "CAB", "CBA")),
                       `Sequence Index` = factor(c(rep(1, 45), rep(2, 48), rep(3, 45),
                                                   rep(4, 36), rep(5, 42), rep(6, 42)),
                                                 levels = 1:6))

#p323
table.7.1 <- tibble(AUC = c(58.16, 79.34, 69.68, 85.59, 121.84, NA, 208.33, 377.15, 17.22, 14.23,
                            1407.9, 750.79, 20.81, 21.27, NA, 8.67, 203.22, 269.40, 386.93, 412.42,
                            47.96, 33.89, 22.70, 32.59, 44.02, 72.36, 285.78, 423.05, 40.60, 20.33,
                            19.43, 17.75, 1048.60, 1160.53, 107.66, 82.70, 469.73, 928.05,
                            14.95, 20.09, 28.57, 28.47, 379.90, 411.72, 126.09, 46.88, 75.43, 106.43,
                            150.12, 142.29, 36.95, 5.00, 24.53, 26.05, 22.11, 34.64, 703.83, 476.56,
                            217.06, 176.02, 40.75, 152.40, 52.76, 51.57, 101.52, 23.49, 37.14, 30.54,
                            143.45, 42.69, 29.80, 29.55, 63.03, 92.94, NA, NA, 56.70, 21.03, 61.18,
                            66.41, 1376.02, 1200.28, 115.33, 135.55, 17.34, 40.35, 62.23, 64.92,
                            48.99, 61.74, 53.18, 17.51, NA, NA, 98.03, 236.17, 1070.98, 1016.52),
                    Cmax = c(2.589, 2.827, 2.480, 4.407, 5.319, NA, 9.634, 11.808, 1.855, 1.121,
                             13.615, 6.877, 1.210, 1.055, 0.995, 1.084, 7.496, 9.618, 16.106, 12.536,
                             2.679, 2.129, 1.727, 1.853, 3.156, 4.546, 8.422, 11.167, 1.900, 1.247,
                             1.185, 0.910, 18.976, 17.374, 5.031, 6.024, 6.962, 14.829, 0.987, 2.278,
                             1.105, 1.773, 12.615, 13.810, 6.977, 2.339, 4.925, 4.771, 5.145, 3.216,
                             2.442, 0.498, 1.442, 2.728, 2.007, 3.309, 15.133, 11.155, 9.433, 8.446,
                             1.787, 6.231, 3.570, 2.445, 4.476, 1.255, 2.169, 2.613, 5.182, 3.031,
                             1.714, 1.804, 3.201, 5.645, 0.891, 0.531, 2.203, 1.514, 3.617, 2.130,
                             27.312, 22.068, 4.688, 7.358, 1.072, 2.150, 3.025, 3.041, 2.706, 2.808,
                             3.240, 1.702, 1.680, NA, 3.434, 7.378, 21.517, 20.116),
                    Period = factor(rep(1:2, 49), levels = 1:2),
                    Treatment = factor(c(rep(c("R", "T"), 24),
                                         rep(c("T", "R"), 25)),
                                       levels = c("R", "T")),
                    Subject = factor(c(rep(c(1,3,5,8,10,11,13,15,18,20,21,24,26,27,31,32,36,37,39,43,44,45,47,50), each = 2),
                                       rep(c(2,4,6,7,9,12,14,16,17,19,22,23,25,28,29,30,33,34,38,40,41,42,46,48,49), each = 2)),
                                     levels = (1:50)[-35]),
                    Sequence = factor(c(rep("RT", 48), rep("TR", 50)),
                                      levels = c("RT", "TR")),
                    `Sequence Index` = factor(c(rep(1, 48), rep(2, 50)),
                                              levels = 1:2))



# Senn's repeated measurements one and the one from Biostatistics website

test <- read_csv("GS20.csv")
test <- gather(test,`Time Period`, PEF, `PEF1 1`:`PEF2 16`)
test <- separate(test,`Time Period`, into = c("Period", "Measurement"), sep = " ")
test$Period <- recode(test$Period, PEF1 = 1, PEF2  = 2)
test$Sex <- recode(test$Sex, male = "Male", female  = "Female")
test <- rename(test, Subject = `Patient ID`)
test <- rename(test, PEF = `PEF`)
test$Time <- recode(test$Measurement, "1" = 0, "2" = 0, "3" = 20, "4" = 40, "5" = 60, "6" = 90,
                           "7" = 120, "8" = 180, "9" = 240, "10" = 300, "11" = 360, "12" = 420, "13" = 480,
                           "14" = 600, "15" = 660, "16" = 720)
test$Treatment <- factor(sapply(1:nrow(test),
                                function(i, data) strsplit(data$Sequence[i], NULL)[[1]][data$Period[i]], test),
                         levels = c("F", "S"))
test$Sequence <- as.factor(test$Sequence)
test$`Sequence Index` <- factor(match(test$Sequence, levels(test$Sequence)),
                                     levels = 1:2)
test <- test[c("PEF", "Period", "Treatment", "Subject", "Sequence",
             "Sequence Index", "Measurement", "Time", "Age", "Sex", "Height", "Weight",
             "Duration")]

write_excel_csv(test, "GS20.csv")



