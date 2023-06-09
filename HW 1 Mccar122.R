#Q1) s = 0.001, n = 15, xbar = 74.036, a = 0.01
##a)
1a1 <- xbar + Z[a/2] * (s / sqrt(n))
1a2 <- xbar - Z[a/2] * (s / sqrt(n))
##b)
1b_LC <- xbar - Z[a] * (x / sqrt(n))

#Q2) s = 20, a = 0.05, CL = 40, 
##a)
##CL = Xbar +- Z[] * s / sqrt(N)
2a_N <- (4 *s^2 * Z[a]^2) / (CL)^2
##b)
2b_N <- (4 *s^2 * Z[a/2]^2) / (CL)^2

#Q3) X = c(2.69, 5.76, 2.67, 1.62, 4.12), s = 0.66, n = 5
##a)
3a_xbar <- sum(X) / n
3a_xi <- x - xbar
3a_s <- sqrt(xi^2 / (n-1))
3a_LC <- xbar - Z[a/2] * sqrt(s/sqrt(n))
3a_UC <- xbar + Z[a/2] * sqrt(s/sqrt(n))
##b)
3b_E <- 0.55 / 2
3b_N <- (Z[a/2]^2 * s^2) / E^2

#Q4)n = 16, xbar = 60,139.7, s = 3645.94, 95% CI, a = 0.05
##a)
4_ME <- t[a]*sqrt(s^2 / n)
4_CL <- P((s - ME) < u < (s + ME))

#Q5) x = c(2216, 2237, 2249, 2204, 2225, 2301, 2281, 2263, 2318, 2255, 2275, 2295)
##a) a = 0.05 n =12
5a_xbar <- sum(x) / n
5a_s <-sd(X)
5a_LC <- xbar - Z[a/2] * sqrt(s/sqrt(n))
5a_UC <- xbar + Z[a/2] * sqrt(s/sqrt(n))
##b)
5b <- xbar - t[a] * s / sqrt(n)

#Q6) s = 0.37, n = 51, a = 0.05
##a)
6_LC <- sqrt(((n-1) * s^2) / (a * t[]))
6_UC <- sqrt(((n-1) * s^2) / ((1-a) * t[]))

#Q7) a = 0.05, n =41
#x = c(101, 104, 104, 77, 89, 88, 104, 96, 82, 70, 89, 91, 39, 103, 93, 85, 104,
#######104, 81, 67,104, 104, 104, 87, 104, 89, 78, 104, 86, 76, 103, 102, 80, 
#######45, 94, 104, 104, 76, 80,72, 73)
7_xbar <- sum(x) / n
7_s <- sd(x)
7_LC <- xbar - Z[a/2] * sqrt(s/sqrt(n))
7_UC <- xbar + Z[a/2] * sqrt(s/sqrt(n))

#Q8) xbar = 50, v = 5, n = c(16, 30, 71)
8a_LP <- P(X^2 >= ((n-1) * 7.44)/v)
8a_UP <- P(X^2 <= ((n-1) * 2.56)/v)

#Q9)  n = 40
# x<- c(0.6248 0.6237 0.6118 0.6159 0.6298 0.6192
####...0.6520 0.6368 0.6220 0.6151 0.6121 0.6548
####...0.6226 0.6280 0.6096 0.6300 0.6107 0.6392
####...0.6230 0.6131 0.6223 0.6297 0.6435 0.5978
####...0.6351 0.6275 0.6261 0.6262 0.6262 0.6314
####...0.6128 0.6403 0.6521 0.6049 0.6170
####...0.6134 0.6310 0.6065 0.6214 0.6141
9_xbar <- sum(x) / n
9_s <- sd(x)
9_LC <- xbar - (t[a] * s) / sqrt(n)
9_UC <- xbar + (t[a] * s) / sqrt(n)


  
