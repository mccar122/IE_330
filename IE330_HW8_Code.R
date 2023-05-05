### Problem 3 ###
q1_xbar = 140.03
q1_r = 13.63
q1_s = 5.1
m = 30
n = 5
q1_xdbar = q1_xdbar / m
q1_rbar = q1_r / m
q1_sbar = q1_s / m

## X and R Charts Limits ##
# X Chart Limits #
q1_xr_x_CL = q1_xdbar
q1_xr_x_UCL = q1_xr_x_CL + 0.577 * q1_sbar
q1_xr_x_LCL = q1_xr_x_CL - 0.577 * q1_sbar

# R Chart Limits #
q1_xr_r_CL = q1_rbar
q1_xr_r_UCL = 2.11 * q1_rbar
q1_xr_r_LCL = 0 * q1_rbar

## X and S Charts Limits ##
# X Chart Limits #
q1_xs_x_CL = q1_xdbar
q1_xs_x_UCL = q1_xs_x_CL + (3 * q1_sbar) / (0.94 * sqrt(n))
q1_xs_x_LCL = q1_xs_x_CL - (3 * q1_sbar) / (0.94 * sqrt(n))

# S Chart Limits #
q1_xs_s_CL = q1_sbar
q1_xs_s_UCL = q1_sbar + (3 * q1_sbar * sqrt(1-0.94^2))/0.94
q1_xs_s_LCL = q1_sbar - (3 * q1_sbar * sqrt(1-0.94^2))/0.94
q1_xs_s_LCL = 0 #the previous row will give you a negative


### Problem 2 ###
library(qcc)
data <- matrix(data = c(4.960,	4.946, 4.950,	4.956, 4.958,
               4.958,	4.927,	4.935,	4.940,	4.920,
               4.971,	4.929,	4.965,	4.952,	4.938,
               4.940,	4.982,	4.970,	4.953,	4.960,
               4.964,	4.951,	4.953,	4.962,	4.956,
               4.969,	4.951,	4.955,	4.966,	4.954,
               4.960,	4.944,	4.957,	4.948,	4.951,
               4.969,	4.949,	4.963,	4.952,	4.962,
               4.984,	4.928,	4.960,	4.943,	4.955,
               4.970,	4.934,	4.961,	4.940,	4.965,
               4.975,	4.959,	4.962,	4.971,	4.968,
               4.945,	4.977,	4.950,	4.969,	4.954,
               4.976,	4.964,	4.970,	4.968,	4.972,
               4.970,	4.954,	4.964,	4.959,	4.968,
               4.982,	4.962,	4.968,	4.975,	4.963,
               4.961,	4.943,	4.950,	4.949,	4.957,
               4.980,	4.970,	4.975,	4.978,	4.977,
               4.975, 4.968,	4.971,	4.969,	4.972,
               4.977,	4.966,	4.969,	4.973,	4.970,
               4.975,	4.967,	4.969,	4.972,	4.972), nrow = 20, ncol = 5)
XbarChart = qcc(data, type = "xbar", nsigmas = 5)
RChart = qcc(data, type = "R", nsigmas = 5)
SChart = qcc(data, type = "S", nsigmas = 5)

