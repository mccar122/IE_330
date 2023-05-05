#Problem 1
#Two machines are used for filling plastic bottles with a net volume of 16.0 ounces.
#The fill volume can be assumed to be normal with standard deviation 𝜎1 = 0.0302
#and 𝜎2 = 0.0254 ounces. A member of the quality engineering staff suspects that
#both machines fill to the same mean net volume, whether or not this volume is 16.0
#ounces. A random sample of 10 bottles is taken from the output of each machine.
#Machine 1 Machine 2
#16.03 16.01 16.02 16.03
#16.04 15.96 15.97 16.04
#16.05 15.98 15.96 16.02
#16.05 16.02 16.01 16.01
#16.02 15.99 15.99 16.00
#a. Do you think the engineer is correct? Use α = 0.05.
#b. What is the P-value for this test?
#c. Calculate a 95% confidence interval on the difference in means. Provide a
#practical interpretation of this interval.
M1<-c(16.03,16.04,16.05,16.05,16.02,16.01,15.96,15.98,16.02,15.99)
M2<-c(16.02,15.97,15.96,16.01,15.99,16.03,16.04,16.02,16.01,16.00)
M0<- 0
S1<- 0.0302
S2<- 0.0254
N1<- length(M1)
N2<- length(M2)
z.test(M1,M2,alternative = "two.sided", mu=0,S1,S2,conf.level=0.95)

#Problem 2
#A polymer is manufactured in a batch chemical process. Viscosity measurements are
#normally made on each batch, and long experience with the process has indicated
#that the variability in the process is fairly stable with σ = 19.128. Fifteen batch
#viscosity measurements are given as follows:
#  724, 718, 776, 760, 745, 759, 795, 756, 742, 740, 761, 749, 739, 747, 742
#A process change that involves switching the type of catalyst used in the process is
#made. Following the process change, eight batch viscosity measurements are taken:
#  735, 775, 729, 755, 783, 760, 738, 780
#Assume that process variability increased due to the catalyst change to σ = 21.2833.
#If the difference in mean batch viscosity is 10 or less, the manufacturer would like
#to detect it with a high probability.
#a. Formulate and test an appropriate hypothesis using α = 0.10. What are your
#conclusions?
#  b. Find the P-value.
#c. Find a 90% confidence interval on the difference in mean batch viscosity resulting
#from the process change.
#d. Compare the results of parts (a) and (c) and discuss your findings.
m1<- 1/15 * sum(724, 718, 776, 760, 745, 759, 795, 756, 742, 740, 761, 749, 739, 747, 742)
m2<- 1/8 * sum(735, 775, 729, 755, 783, 760, 738, 780)
delta_u = 10
H1 <- delta_u
z <- z.test(M1,M2,alternative = "two.sided", mu=0,19.125,21.2833,conf.level=0.9)
LC <- (m1 - m2) - qnorm(z)* sqrt(((19.125^2/15)+(21.2833^2/8)))
UC<- (m1 - m2) + qnorm(z)* sqrt(((19.125^2/15)+(21.2833^2/8)))

#Problem 3
#Two catalysts may be used in a batch chemical process. Twelve batches were
#prepared using catalyst 1, resulting in an average yield of 86 and a sample standard
#deviation of 3. Fifteen batches were prepared using catalyst 2, and they resulted in
#an average yield of 89 with a standard deviation of 2. Assume that yield
#measurements are approximately normally distributed with the same standard
#deviation.
#a. Is there evidence to support a claim that catalyst 2 produces a higher mean yield
#than catalyst 1? Use α = 0.01.
#b. Find a 99% confidence interval on the difference in mean yields that can be used
#to test the claim in part (a).
s1 <- 3/sqrt(15)
s2 <- 2/sqrt(15)
xbar <- 89-86
p_val <- 2*pnorm(-abs((xbar)/(s1/sqrt(15))))
t<- qt(0.01, (12+13), lower.tail=TRUE)
CI <- xbar + t*sqrt((1/12)+(1/15))

#Problem 4
#A computer scientist is investigating the usefulness of two different design
#languages in improving programming tasks. Twelve expert programmers who are
#familiar with both languages are asked to code a standard function in both languages
#and the time (in minutes) is recorded. The data follow:
#  Time
#Programmer Design Language 1 Design Language 2
#1 17 18
#2 16 14
#3 21 19
#4 14 11
#5 18 23
#6 24 21
#7 16 10
#8 14 13
#9 21 19
#10 23 24
#11 13 15
#12 18 20
#Find a 95% confidence interval on the difference in mean coding times. Is there any
#indication that one design language is preferable
DL1<- c(17, 16, 21, 14, 18, 24, 16, 14, 21, 23, 13, 18)
DL2<- c(18, 14, 19, 11, 23, 21, 10, 13, 19, 24, 15, 20)
n = 12
M1<- avg(DL1)
M2<- avg(DL2)
t<- qt(0.0975, (11), lower.tail=TRUE)
LC <- (M1-M2) - t*sqrt(2.96^2 / n)
UC<- (M1-M2) + t*sqrt(2.96^2 / n)

#Problem 5
#Two chemical companies can supply a raw material. The concentration of a
#particular element in this material is important. The mean concentration for both
#suppliers is the same, but you suspect that the variability in concentration may differ
#for the two companies. The standard deviation of concentration in a random sample 4
#of 𝑛1 = 10 batches produced by company 1 is 𝑠1 = 4.7 grams per liter, and for
#company 2, a random sample of 𝑛2 = 16 batches yields 𝑠2 = 5.8 grams per liter. Is
#there sufficient evidence to conclude that the two population variances differ? Use
#α = 0.05.
n1 = 10
s1 = 4.7
n2 = 16
s2 = 5.8
a = 0.05
F1 = s1^2 / s2^2
F2<- f(0.975, 9, 15)
Rslt <- F1 > F2

#Problem 6
#A research study quantified the absorption of electromagnetic energy and the
#resulting thermal effect from cellular phones. The experimental results were
#obtained from in vivo experiments conducted on rats. The arterial blood pressure
#values (mmHg) for the control group (8 rats) during the experiment are 𝑥̅1 = 90, 𝑠1
#= 5 and for the test group (9 rats) are 𝑥̅2 = 115, 𝑠2 = 10.
#a. Is there evidence to support the claim that the test group has higher mean blood
#pressure? Use α = 0.05, and assume that both populations are normally distributed
#but the variances are not equal.
#b. What is the P-value for this test?
#c. Calculate a confidence interval to answer the question in part (a).
#d. Do the data support the claim that the mean blood pressure from the test group is
#at least 15 mmHg higher than the control group? Make the same assumptions as in
#part (a).
#e. Explain how the question in part (d) could be answered with a confidence interval.
x1 <- 90
s1 <- 5
n1 <- 8
x2 <- 115
s2 <- 10
n2 <- 9
ta<- (x1-x2) / sqrt((s1^2 / n1) + (s2^2 / n2))
p_val <- pnorm(ta)
CI_LL <- (x1-x2) - ta * sqrt((s1^2 / n1) + (s2^2 / n2))
CI_UL <- (x1-x2) + ta * sqrt((s1^2 / n1) + (s2^2 / n2))
H0 <- (x1-x2)-15
td <- H0 / sqrt((s1^2 / n1) + (s2^2 / n2))
