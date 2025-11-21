data <- read.csv("E:/研究生阶段作业/应用回归分析数据/4.9.csv")
y = data$Y
x = data$X
# (1)用普通最小二乘法建立y与x的回归方程，并画出残差散点图
model = lm(y ~ x, data = data)
cat("y =", round(coef(model)[1],4), "+", round(coef(model)[2],4), "x\n")
e = residuals(model)
plot(x, e, main = "残差图", 
     xlab = "x", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
summary_model = summary(model)
print(summary_model)
# (2)诊断该问题是否存在异方差性
e_abs = abs(e)
hete_data = data.frame(x = x, e_abs = e_abs)
hete_data$rank_x = rank(hete_data$x)
hete_data$rank_e_abs = rank(hete_data$e_abs)
rank_x = hete_data$rank_x
rank_e_abs = hete_data$rank_e_abs
d = rank_e_abs - rank_x
n = length(x)
r = 1 - 6 / (n*(n^2 - 1)) * sum(d^2)
t_value = sqrt(n - 2) * r / sqrt(1 - r^2)
t = qt(0.975, df = n-2)
abs(t_value) > t
# (3)如果存在异方差性，用幂指数型的权函数建立加权最小二乘回归方程。
results = data.frame(Power = numeric(0), LogLikelihood = numeric(0))
possible_k = c(-2.0, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2.0)
for (k in possible_k) {
  w_try = 1 / x^k
  wls_model = lm(y ~ x, data = data, weights = w_try)
  e_try =  residuals(wls_model)
  n = length(residuals)
  sigma_hat_try = sum(w_try * e_try^2) / n
  log_likelihood = -n/2 * log(2 * pi * sigma_hat_try) - 1/(2 * sigma_hat_try) * sum(w_try * e_try^2) - 1/2 * sum(log(w_try))
  new_row = data.frame(Power = k, LogLikelihood = log_likelihood)
  results = rbind(results, new_row)
}
best_k = results$Power[which.max(results$LogLikelihood)]
cat("\n最优幂指数 k =", best_k, "\n")

w = 1 / x^best_k
new_model = lm(y ~ x, data = data, weights = w)
cat("y* =", round(coef(new_model)[1],4), "+", round(coef(new_model)[2],4), "x*\n")
# (4)用方差稳定变换y’=√y消除异方差性
data$y_sqrt = sqrt(data$Y)
stable_model = lm(y_sqrt ~ x, data = data)
cat("√y =", round(coef(stable_model)[1],4), "+", round(coef(stable_model)[2],4), "x*\n")
e_stable = residuals(stable_model)
plot(x, e_stable, main = "残差图", 
     xlab = "x", ylab = "e'",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
e_stable_abs = abs(e_stable)
stable_hete_data = data.frame(x = x, e_stable_abs = e_stable_abs)
stable_hete_data$rank_x = rank(stable_hete_data$x)
stable_hete_data$rank_e_abs = rank(stable_hete_data$e_stable_abs)
stable_d = stable_hete_data$rank_e_abs - stable_hete_data$rank_x
n = length(x)
stable_r = 1 - 6 / (n*(n^2 - 1)) * sum(stable_d^2)
stable_t_value = sqrt(n - 2) * r / sqrt(1 - stable_r^2)
t = qt(0.975, df = n-2)
abs(stable_t_value) > t

