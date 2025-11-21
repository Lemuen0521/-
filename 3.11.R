# 准备工作
y = c(160,260,210,265,240,220,275,160,275,250)
x1 = c(70,75,65,74,72,68,78,66,70,65)
x2 = c(35,40,40,42,38,45,42,36,44,42)
x3 = c(1.0,2.4,2.0,3.0,1.2,1.5,4.0,2.0,3.2,3.0)
freight_data = data.frame(id = id, x1 = x1,x2 = x2, x3 = x3, y = y)
# (1) 计算出 y，x1，x2，x3的相关系数矩阵。
cor_matrix = cor(freight_data[, c("y", "x1", "x2", "x3")])
print(round(cor_matrix, 4))
# (2)求y关于x1，x2，x3的三元线性回归方程。
model = lm(y ~ x1 + x2 + x3, data = freight_data)
summary_model = summary(model)
cat("y =", round(coef(model)[1],4), "+", round(coef(model)[2],4), "x1", "+", round(coef(model)[3],4), "x2","+", round(coef(model)[4],4), "x3")
# (3)对所求得的方程做拟合优度检验。
cat("决定系数 R² =", round(summary_model$r.squared,4))
cat("调整R² =", round(summary_model$adj.r.squared,4))
# (4)对回归方程做显著性检验。
print(summary_model)
# (5)对每一个回归系数做显著性检验。
coef_summary = summary_model$coefficients
print(coef_summary)
# (6)如果有的回归系数没通过显著性检验，将其剔除，重新建立回归方程，再做回归方程的显著性检验和回归系数的显著性检验。
new_model = lm(y ~ x1 + x2, data = freight_data)
summary_new_model = summary(new_model)
cat("y=", round(coef(new_model)[1],4), "+", round(coef(new_model)[2],4), "x1", "+", round(coef(new_model)[3],4), "x2\n")
print(summary_new_model)
# (7)求出每一个回归系数的置信水平为95%的置信区间。
conf_int = confint(new_model, level = 0.95)
cat("常数项系数95%的置信区间为","[",round(conf_int[1],4),",",round(conf_int[4],4),"]\n")
cat("x1系数95%的置信区间为","[",round(conf_int[2],4),",",round(conf_int[5],4),"]\n")
cat("x2系数95%的置信区间为","[",round(conf_int[3],4),",",round(conf_int[6],4),"]\n")
# (8)求标准化回归方程。
L_11 = sum((x1 - mean(x1))^2)
x1_stand = (x1 - mean(x1)) / sqrt(L_11)
L_22 = sum((x2 - mean(x2))^2)
x2_stand = (x2 - mean(x2)) / sqrt(L_22)
L_yy = sum((y - mean(y))^2)
y_stand = (y - mean(y)) / sqrt(L_yy)
model_stand = lm(y_stand ~ x1_stand + x2_stand , data = freight_data)
summary_stand_model = summary(model_stand)
cat("y =", round(coef(model_stand)[1],4), "+", round(coef(model_stand)[2],4), "x1", "+", round(coef(model_stand)[3],4), "x2\n")
# (9)求当x01=75，x02=42，x03=3.1时的y0-hat，给定置信水平为 95%，用SPSS 软件计算精确置信区间，用手工计算近似预测区间。
x01 = 75
x02 = 42
x0 = data.frame(x1 = x01, x2 = x02)
y0_hat = predict(new_model, newdata = x0)
cat("当x01=75，x02=42，x03=3.1时的y0为",y0_hat)
pred_interval = predict(new_model, newdata = x0, interval = "prediction", level = 0.95)
print(pred_interval)
cat("y0_hat的精确置信区间为[",round(pred_interval[2],4),",",round(pred_interval[3],4),"]\n")
sigma_hat = summary_new_model$sigma
cat(sigma_hat)
approx_lower = y0_hat - 2 * sigma_hat
approx_upper = y0_hat + 2 * sigma_hat
cat("y0_hat的近似置信区间为[", round(approx_lower,4), ",",round(approx_upper,4), "]\n")
# (10)结合回归方程对问题做一些基本分析。

# 4.16
# 异常值检验
