# 周次	销售额y	周演出场次x1	周点击率x2
data <- read.csv("E:/研究生阶段作业/应用回归分析数据/4.13.csv")
# (1)用普通最小二乘法建立y与x1和x2的回归方程，用残差图及 DW 检验诊断序列的自相关性。
model = lm(data$y ~ data$x1 + data$x2, data = data)
e = residuals(model)
plot(data$y, e, main = "残差图", 
     xlab = "y", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
plot(data$week, e, main = "按时间顺序的残差图", 
     xlab = "t", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
library(lmtest)
DW = dwtest(model)
print(DW)
# (2)用选代法处理序列相关，并建立回归方程。
n = length(e)
data2 = data.frame(e_t = e[2:n], e_t_1 = e[1:(n-1)],
                   x1_t = data$x1[2:n], x1_t_1 = data$x1[1:(n-1)], 
                   x2_t = data$x2[2:n], x2_t_1 = data$x2[1:(n-1)], 
                   y_t = data$y[2:n], y_t_1 = data$y[1:(n-1)],
                   week_2 = data$week[2:n])
rou_hat = 1 - DW$statistic/2
y_2 = data2$y_t - rou_hat * data2$y_t_1
x1_2 = data2$x1_t - rou_hat * data2$x1_t_1
x2_2 = data2$x2_t - rou_hat * data2$x2_t_1
model2 = lm(y_2 ~ x1_2 + x2_2, data = data2)
cat("y' =", round(coef(model2)[1],4), "+", round(coef(model2)[2],4), "x1'", "+", round(coef(model2)[3],4), "x2'\n")
# (3)用一阶差分法处理数据，并建立回归方程。
data3 = data.frame(y_delta = data2$y_t - data2$y_t_1,
                   x1_delta = data2$x1_t - data2$x1_t_1,
                   x2_delta = data2$x2_t - data2$x2_t_1,
                   week_3 = data$week[2:n])
model3 = lm(y_delta ~ x1_delta + x2_delta, data = data3)
cat("y_delta =", round(coef(model3)[1],4), "+", round(coef(model3)[2],4), "x1_delta", "+", round(coef(model3)[3],4), "x2_delta\n")
# (4)比较以上各方法所建回归方程的优良性。
e2 = residuals(model2)
plot(data2$y_t, e2, main = "迭代法残差图", 
     xlab = "y", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
plot(data2$week_2, e2, main = "按时间顺序的迭代法残差图", 
     xlab = "t", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
DW2 = dwtest(model2)
print(DW2)

e3 = residuals(model3)
plot(data3$y_delta, e3, main = "差分法残差图", 
     xlab = "y_delta", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
plot(data3$week_3, e3, main = "按时间顺序的差分法残差图", 
     xlab = "t", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
DW3 = dwtest(model3)
print(DW3)

summary(model2)
summary(model3)
