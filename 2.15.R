# 输入数据
week <- 1:10
x <- c(825, 215, 1070, 550, 480, 920, 1350, 325, 670, 1215)  # 新保单数目
y <- c(3.5, 1.0, 4.0, 2.0, 1.0, 3.0, 4.5, 1.5, 3.0, 5.0)    # 加班时间
# 构建数据框
insurance_data = data.frame(week = week, x = x, y = y)
# (1)画散点图
plot(x, y, main = "保险公司加班时间与新保单数量关系", 
     xlab = "新保单数目(x)", ylab = "加班时间(y)(小时)",
     pch = 16, col = "black", cex = 1.2)
# (3)用最小二乘估计求出回归方程
model = lm(y ~ x, data = insurance_data)
summary_model = summary(model)
cat("y =", round(coef(model)[1],4), "+", round(coef(model)[2],4), "x\n")
print(summary_model)
# (4)求回归标准误差σ ̂
sigma_hat = summary_model$sigma
cat("sigma_hat =", round(sigma_hat, 4),"\n")
# (5)求β0-hat和β1-hat的置信区间
conf_int = confint(model, level = 0.95)
print(conf_int)
cat("β0-hat的置信区间", "[",round(conf_int[1],4),",",round(conf_int[3],4),"]","\n")
cat("β1-hat的置信区间", "[",round(conf_int[2],4),",",round(conf_int[4],4),"]","\n")
# (6)计算x与y的决定系数
r_squared = summary_model$r.squared
cat("决定系数为",round(r_squared,4),"\n")
# (7)对回归方程做方差分析
anova_result = anova(model)
print(anova_result)
# (8)β1-hat显著性检验
cat("β1-hat显著性检验的p-value为",round(summary_model$coefficients[2,4],4),"\n")
# (9)做相关系数的显著性检验
cor_test = cor.test(x, y)
print(cor_test)
cat("相关系数的p-value为",round(cor_test$p.value,4))
# (10)对回归方程作残差图并做相应的分析
y_hat <- predict(model, data = your_data)
e <- y - y_hat
plot(x, e, main = "残差图", 
     xlab = "x", ylab = "e",
     pch = 16, col = "black", cex = 1.2, abline(h=0))
# (11)该公司预计下一周签发新保单x_0=1000张，需要的加班时间是多少
x0 = 1000
y0_hat = predict(model, newdata = data.frame(x = x0))
cat("该公司预计下一周签发新保单x_0=1000张，需要的加班时间为",round(y0_hat,4),"小时\n")
# (12)给出y_0的置信度为95%的精确预测区间和近似预测区间
# y0的95%精确预测区间
n = length(y)
L_xx = sum((x - mean(x))^2)
h_00 = 1 / n + (x0 - mean(x))^2 / L_xx
t_value = qt(0.975, df = n-2)
margin_error = t_value * sqrt(1 + h_00) * sigma_hat
precise_lower = y0_hat - margin_error
precise_upper = y0_hat + margin_error
cat("y0的精确预测区间为[", round(precise_lower,4), ",",round(precise_upper,4), "]\n")
# 近似预测区间
approx_lower = y0_hat - 2 * sigma_hat
approx_upper = y0_hat + 2 * sigma_hat
cat("y0的近似预测区间为[", round(approx_lower,4), ",",round(approx_upper,4), "]\n")
# (13)E(y0)的95%的区间估计
conf_interval = predict(model, newdata = data.frame(x = x0), interval = "confidence", level = 0.95)
margin_error2 = t_value * sqrt(h_00) * sigma_hat
conf_lower = y0_hat - margin_error2
conf_upper = y0_hat + margin_error2
cat("E(y0)的95%的区间估计为[", round(conf_lower,4), ",",round(conf_upper,4), "]\n")