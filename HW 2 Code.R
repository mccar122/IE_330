#QUESTION 2
#A manufacturer is interested in the output voltage of a power supply used in a PC.
#Output voltage is assumed to be normally distributed with standard deviation 0.25
#volt, and the manufacturer wishes to test 𝐻0: μ = 5 volts against 𝐻1: μ ≠ 5 volts,
#using 𝑛 = 8 units.
#a. The acceptance region is 4.85 ≤ x ≤ 5.15. Find the value of α.
library(ltm)
data1a <- as.data.frame((rnorm(8,5,0.25)))
cronbach.alpha(data1a)
#b. Find the power of the test for detecting a true mean output voltage of 5.1 volts.
oneb_m <- 5
oneb_s <- 0.25
oneb_n <-8
oneb_error <- qnorm(0.975)*oneb_s/sqrt(oneb_n)
oneb_left <- oneb_m + oneb_error
oneb_right <- oneb_m + oneb_error
oneb_assumed <- oneb_m + 0.1
oneb_Zleft <- (oneb_left-oneb_assumed)/(oneb_s/sqrt(oneb_n))
oneb_Zright <-(oneb_right-oneb_assumed)/(oneb_s/sqrt(oneb_n))
oneb_p <- pnorm(oneb_Zright)-pnorm(oneb_Zleft)
oneb_beta <- 1 - oneb_p

#Problem 3
#The life in hours of a battery is known to be approximately normally distributed with
#standard deviation σ = 1.25 hours. A random sample of 10 batteries has a mean life
#of 𝑥̅ = 40.5 hours.
#a. Is there evidence to support the claim that battery life exceeds 40 hours? Use α = 0.5.
threea_z <- (40.5 - 40) / (1.25 / sqrt(10))
threea_evidence <- threea_z > qnorm(0.95)
#b. What is the P-value for the test in part (a)?
threeb_p <- 2*pnorm(-abs(threea_z))
#c. What is the β-error for the test in part (a) if the true mean life is 42 hours?
threec_b <- pnorm(1.65 - (2 * sqrt(10))/1.25)

#Problem 4
#The bacterial strain Acinetobacter has been tested for its adhesion properties. A
#sample of five measurements gave readings of 2.69, 5.76, 2.67, 1.62 and 4.12 dyne-
#  cm2. Assume that the standard deviation is known to be 0.66 dyne-cm2 and that the
#scientists are interested in high adhesion (at least 2.5 dyne-cm2).
#a. Should the alternative hypothesis be one-sided or two-sided?
#b. Test the hypothesis that the mean adhesion is 2.5 dyne-cm2.
foura_z <- (3.372 - 2.5) / (0.66 / sqrt(5))
foura_evidence <- threea_z > qnorm(0.95)
#c. What is the P-value of the test statistic?
fourb_p <- 2*pnorm(-abs(foura_z))

#Problem 5
#An article in the ASCE Journal of Energy Engineering (1999, Vol. 125, pp. 59–75)
#describes a study of the thermal inertia properties of autoclaved aerated concrete
#used as a building material. Five samples of the material were tested in a structure,
#and the average interior temperatures (∘C) reported were as follows: 23.01, 22.22,
#22.04, 22.62, and 22.59.
#a. Test the hypotheses 𝐻0: μ = 22.5 versus 𝐻1: μ ≠ 22.5, using α = 0.05. Find the P-
#value.
fivea_n <- 5
fivea_s <-0.38
fivea_mean<- 22.496
fivea_t <- ((22.496 - 22.5) / (0.38 / sqrt(5)))
fivea_p <- 2*pt(-abs(fivea_t),df=fivea_n-1)
#b. Explain how the question in part (a) could be answered by constructing a two-
#sided confidence interval on the mean interior temperature.
fiveb_MoE <- 2.776 * fivea_s / sqrt(fivea_n)
fiveb_CI <- c(fivea_mean - fiveb_MoE, fivea_mean + fiveb_MoE)

#Problem 6
#Cloud seeding has been studied for many decades as a weather modification
#procedure (for an interesting study of this subject, see the article in Technometrics,
#“A Bayesian Analysis of a Multiplicative Treatment Effect in Weather
#Modification,” 1975, Vol. 17, pp. 161–166). The rainfall in acre-feet from 20 clouds
#that were selected at random and seeded with silver nitrate follows: 18.0, 30.7, 19.8,
#27.1, 22.3, 18.8, 31.8, 23.4, 21.2, 27.9, 31.9, 27.1, 25.0, 24.7, 26.9, 21.8, 29.2, 34.8,
#26.7, and 31.6.
#a. Can you support a claim that mean rainfall from seeded clouds exceeds 25 acre-
#feet? Use α = 0.01. Find the P-value.
sixa_t <- ((26.03 - 25) / (4.78 / sqrt(20)))
sixa_p <- 2*pt(-abs(sixa_t),df=19)
#b. Explain how the question in part (a) could be answered by constructing a one-
#sided confidence bound on the mean diameter.
sixb_MoE <- 2.776 * 4.78 / sqrt(20)

#Problem 7
#An article in Medicine and Science in Sports and Exercise [“Electrostimulation
#Training Effects on the Physical Performance of Ice Hockey Players” (2005, Vol.
#37, pp. 455–460)] considered the use of electromyostimulation (EMS) as a method
#to train healthy skeletal muscle. EMS sessions consisted of 30 contractions (4-
#second duration, 85 Hz) and were carried out three times per week for 3 weeks on
#17 ice hockey players. The 10-meter skating performance test showed a standard
#deviation of 0.09 seconds.
#a. Is there strong evidence to conclude that the standard deviation of performance
#time exceeds the historical value of 0.75 seconds? Use α = 0.05. Find the P-value for
#this test.
seven_n <- 17
seven_ss<- 0.09
seven_s <- 0.75
seven_alpha <- 0.05
seven_CV <- c(qchisq(0.025, 16, lower.tail=TRUE), qchisq(0.975, 16, lower.tail=TRUE))
seven_test<- (seven_n - 1)*seven_ss^2 / seven_s
#b. Discuss how part (a) could be answered by constructing a 95% one-sided
#confidence interval for σ.
sevenb_CI <-sqrt(((seven_n - 1)*seven_ss^2) / seven_CV)

#Problem 8
#The percentage of titanium in an alloy used in aerospace castings is measured in 51
#randomly selected parts. The sample standard deviation is s = 0.37.
#a. Test the hypothesis 𝐻0: σ = 0.25 versus 𝐻1: σ ≠ 0.25 using α = 0.05. State any
#necessary assumptions about the underlying distribution of the data. Find the P-
#  value.
eighta_n <- 51
eighta_s <- 0.37
eight_Test <- ((eighta_n - 1) * eighta_s^2) / 0.25^2
eight_Chi <- qchisq(eight_Test, eighta_n, lower.tail=TRUE)
#b. Explain how you could answer the question in part (a) by constructing a 95% two-
#  sided confidence interval for σ.

#Problem 9
#Ten samples were taken from a plating bath used in an electronics manufacturing
#process, and the pH of the bath was determined. The sample pH values are 7.91,
#7.85, 6.82, 8.01, 7.46, 6.95, 7.05, 7.35, 7.25, and 7.42. Manufacturing engineering
#believes that pH has a median value of 7.0.
#Do the sample data indicate that this statement is correct? Use the sign test with α =
#  0.05 to investigate this hypothesis. Find the P-value for this test

nine_p <- (8:20, sum(choose(10,8)*0.5))*2
nine_eval <- nine_p > 0.05
#Problem 10
#The impurity level (in ppm) is routinely measured in an intermediate chemical
#product. The following data were observed in a recent test: 2.4, 2.5, 1.7, 1.6, 1.9,
#2.6, 1.3, 1.9, 2.0, 2.5, 2.6, 2.3, 2.0, 1.8, 1.3, 1.7, 2.0, 1.9, 2.3, 1.9, 2.4, 1.6.
#Can you claim that the median impurity level is less than 2.5 ppm? State and test the
#appropriate hypothesis using the sign test with α = 0.05. What is the P-value for this
#test?
ten_p <- (1/20)^20 * (1:18, sum(choose(20,i)))



