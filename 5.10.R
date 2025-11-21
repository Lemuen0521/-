# x1 年份
# x2 国民生产总值(单位:10亿美元)
# X3 新房动工数(单位:1000栋)
# x4 失业率(%)
# x5 滞后6个月的最惠利率(%)
# x6 用户用线增量(%)
# y 年电话线销售量(百万尺双线)
data = read.csv("E:/研究生阶段作业/应用回归分析数据/5.10.csv")
# (1)建立y对x2~x6的线性回归方程
full_model = lm(y ~ x2 + x3 + x4 + x5 + x6, data = data)
summary(full_model)
# (2)用后退法选择自变量
n = nrow(data)
all_vars = c("x2", "x3", "x4", "x5", "x6")
p_full = length(all_vars)
SSE_full = sum(full_model$residuals^2)
MSE_full = SSE_full / (n - p_full - 1)
for (var in all_vars) {
  reduced_vars = setdiff(all_vars, var)
  reduced_formula = paste("y ~", paste(reduced_vars, collapse = " + "))
  reduced_model = lm(as.formula(reduced_formula), data = data)
  SSE_reduced = sum(reduced_model$residuals^2)
  SSR_partial = SSE_reduced - SSE_full
  F_value = (SSR_partial / 1) / MSE_full
  p_value = pf(F_value, 1, n - p_full - 1, lower.tail = FALSE)
  cat("检验变量", var, "的F统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4), "\n")
}

reduced5_model = lm(y ~ x2 + x3 + x4 + x6, data = data)
reduced5_vars = c("x2", "x3", "x4", "x6")
p_reduced5 = length(reduced5_vars)
for (var in reduced5_vars) {
  reduced_vars = setdiff(reduced5_vars, var)
  reduced_formula = paste("y ~", paste(reduced_vars, collapse = " + "))
  reduced_model = lm(as.formula(reduced_formula), data = data)
  SSE_reduced = sum(reduced_model$residuals^2)
  SSR_partial = SSE_reduced - SSE_full
  F_value = (SSR_partial / 1) / MSE_full
  p_value = pf(F_value, 1, n - p_full - 1, lower.tail = FALSE)
  cat("检验变量", var, "的F统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4), "\n")
}
backward_model = lm(y ~ x2 + x3 + x4 + x6, data = data)
cat("y =", 
    round(coef(backward_model)[1],4), "+",
    round(coef(backward_model)[2],4), "x2 +", 
    round(coef(backward_model)[3],4), "x3 +",
    round(coef(backward_model)[4],4), "x4 +",
    round(coef(backward_model)[5],4), "x6 \n")
# 机算
backward_model <- step(full_model, direction = "backward", trace = 0)
summary(backward_model)
# (3)用逐步回归法选择自变量
cat("设定引入变量的显著性水平为0.1，剔除变量的显著性水平为0.15")
# ①初始无变量
current_vars = character(0)
remaining_vars = c("x2", "x3", "x4", "x5", "x6")
for (var in remaining_vars) {
  simple_model = lm(as.formula(paste("y ~", var)), data = data)
  anova_result = anova(simple_model)
  F_value = anova_result$`F value`[1]
  p_value = anova_result$`Pr(>F)`[1]
  cat("变量", var, "的单独F检验值为", round(F_value, 4), "对应p值为", round(p_value, 4), "\n")
}
# ②在引入x3的情况下
current_vars = c("x3")
remaining_vars = setdiff(remaining_vars, "x3")
for (var in remaining_vars) {
  candidate_vars = c(current_vars, var)
  candidate_model = lm(as.formula(paste("y ~", paste(candidate_vars, collapse = " + "))), data = data)
  drop1_result = drop1(candidate_model, test = "F")
  var_row = which(rownames(drop1_result) == var)
  F_value = drop1_result$`F value`[var_row]
  p_value = drop1_result$`Pr(>F)`[var_row]
  cat("引入变量", var, "的单独F检验统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4),"\n")
}
# 引入x5，检验3/5都在的时候显著性如何
model_3_5 = lm(y ~ x3 + x5, data = data)
drop1(model_3_5, test = "F")
# ③在3/5都存在的情况下
current_vars = c("x3", "x5")
remaining_vars = setdiff(remaining_vars, current_vars)
for (var in remaining_vars) {
  candidate_vars = c(current_vars, var)
  candidate_model = lm(as.formula(paste("y ~", paste(candidate_vars, collapse = " + "))), data = data)
  drop1_result = drop1(candidate_model, test = "F")
  var_row = which(rownames(drop1_result) == var)
  F_value = drop1_result$`F value`[var_row]
  p_value = drop1_result$`Pr(>F)`[var_row]
  cat("引入变量", var, "的单独F检验统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4),"\n")
}
# 引入x4，检验3/4/5都在的时候显著性如何
model_3_4_5 = lm(y ~ x3 + x4 + x5, data = data)
drop1(model_3_4_5, test = "F")
# ④在3/4/5都存在的情况下
current_vars = c("x3", "x4", "x5")
remaining_vars = setdiff(remaining_vars, current_vars)
for (var in remaining_vars) {
  candidate_vars = c(current_vars, var)
  candidate_model = lm(as.formula(paste("y ~", paste(candidate_vars, collapse = " + "))), data = data)
  drop1_result = drop1(candidate_model, test = "F")
  var_row = which(rownames(drop1_result) == var)
  F_value = drop1_result$`F value`[var_row]
  p_value = drop1_result$`Pr(>F)`[var_row]
  cat("引入变量", var, "的单独F检验统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4),"\n")
}
# 引入x6，检验3/4/5/6都在的时候显著性如何
model_3_4_5_6 = lm(y ~ x3 + x4 + x5 + x6, data = data)
drop1(model_3_4_5_6, test = "F")
# ⑤在3/4/5/6都存在的情况下
current_vars = c("x3", "x4", "x5", "x6")
remaining_vars = setdiff(remaining_vars, current_vars)
for (var in remaining_vars) {
  candidate_vars = c(current_vars, var)
  candidate_model = lm(as.formula(paste("y ~", paste(candidate_vars, collapse = " + "))), data = data)
  drop1_result = drop1(candidate_model, test = "F")
  var_row = which(rownames(drop1_result) == var)
  F_value = drop1_result$`F value`[var_row]
  p_value = drop1_result$`Pr(>F)`[var_row]
  cat("引入变量", var, "的单独F检验统计量值为", round(F_value, 4), "对应p值为", round(p_value, 4),"\n")
}
# 引入x2，检验2/3/4/5/6都在的时候显著性如何
model_2_3_4_5_6 = lm(y ~ x2 + x3 + x4 + x5 + x6, data = data)
drop1(model_2_3_4_5_6, test = "F")
# ⑥最终结果
stepwise_model = lm(y ~ x2 + x3 + x4 + x6, data = data)
cat("y =", 
    round(coef(stepwise_model)[1],4), "+",
    round(coef(stepwise_model)[2],4), "x2 +", 
    round(coef(stepwise_model)[3],4), "x3 +",
    round(coef(stepwise_model)[4],4), "x4 +",
    round(coef(stepwise_model)[5],4), "x6 \n")
# 机算
stepwise_model <- step(full_model, direction = "both", trace = 0)
summary(stepwise_model)
# (4)根据以上计算结果分析后退法与逐步回归法的差异
cat("\n=== 模型比较 ===\n")
cat("完整模型 AIC:", AIC(full_model), "\n")
cat("后退法 AIC:", AIC(backward_model), "\n")
cat("逐步回归法 AIC:", AIC(stepwise_model), "\n")
