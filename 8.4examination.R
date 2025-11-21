# 加载所需包
library(caret)
library(pls)
library(ggplot2)
library(reshape2)

# 读取数据
data <- read.csv("E:/研究生阶段作业/应用回归分析数据/8.3.csv")

# 标准化数据
datas <- data.frame(scale(data))

# 1. 普通最小二乘法（OLS）
ols <- lm(y ~ x1 + x2 + x3 + x4, data = data)

# 2. 逐步回归
stepwise <- step(ols, direction = "both", trace = 0)

# 3. 主成分回归（PCR）
pcr_model <- pcr(y ~ ., data = datas, validation = "LOO")

# 4. 偏最小二乘回归（PLS）
pls_model <- plsr(y ~ ., data = datas, validation = "LOO")

# 计算各模型的拟合指标
models <- list(
  OLS = ols,
  Stepwise = stepwise,
  PCR = pcr_model,
  PLS = pls_model
)

# 初始化结果数据框
results <- data.frame(
  Model = names(models),
  R2 = numeric(4),
  Adj_R2 = numeric(4),
  RMSE = numeric(4),
  AIC = numeric(4),
  BIC = numeric(4),
  Num_Vars = numeric(4)
)

# 计算每个模型的指标
for (i in 1:length(models)) {
  model <- models[[i]]
  model_name <- names(models)[i]
  
  if (model_name %in% c("OLS", "Stepwise")) {
    # 线性模型
    y_pred <- predict(model, data)
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    rmse <- sqrt(mean((data$y - y_pred)^2))
    aic_val <- AIC(model)
    bic_val <- BIC(model)
    num_vars <- length(coef(model)) - 1  # 不包括截距
  } else if (model_name == "PCR") {
    # PCR模型 - 使用2个主成分
    y_pred <- predict(model, datas, ncomp = 2)
    r2 <- R2(model, ncomp = 2, estimate = "train")$val
    adj_r2 <- NA  # PCR没有调整R²
    rmse <- RMSEP(model, ncomp = 2, estimate = "train")$val
    aic_val <- NA
    bic_val <- NA
    num_vars <- 2  # 使用的主成分数
  } else if (model_name == "PLS") {
    # PLS模型 - 使用2个成分
    y_pred <- predict(model, datas, ncomp = 2)
    r2 <- R2(model, ncomp = 2, estimate = "train")$val
    adj_r2 <- NA  # PLS没有调整R²
    rmse <- RMSEP(model, ncomp = 2, estimate = "train")$val
    aic_val <- NA
    bic_val <- NA
    num_vars <- 2  # 使用的成分数
  }
  
  results[i, "R2"] <- r2
  results[i, "Adj_R2"] <- adj_r2
  results[i, "RMSE"] <- rmse
  results[i, "AIC"] <- aic_val
  results[i, "BIC"] <- bic_val
  results[i, "Num_Vars"] <- num_vars
}

# 显示结果
print("四种回归方法的比较结果：")
print(results)

# 图1: RMSE比较
p1 <- ggplot(results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  labs(title = "模型RMSE比较", y = "RMSE", x = "模型") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
print(p1)

# 图2: 变量个数比较
p2 <- ggplot(results, aes(x = Model, y = Num_Vars, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = Num_Vars), vjust = -0.5, size = 4) +
  labs(title = "模型变量数比较", y = "变量个数", x = "模型") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
print(p2)

# 交叉验证预测误差比较
cv_results <- data.frame(
  Model = c("OLS", "Stepwise", "PCR", "PLS"),
  RMSEP = c(
    sqrt(mean((resid(ols))^2)), 
    sqrt(mean((resid(stepwise))^2)), 
    results$RMSE[results$Model == "PCR"],  
    results$RMSE[results$Model == "PLS"]))
print("交叉验证预测误差比较：")
print(cv_results)

# 图3: 预测误差比较图
p3 <- ggplot(cv_results, aes(x = Model, y = RMSEP, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSEP, 3)), vjust = -0.5, size = 4) +
  labs(title = "模型预测误差(RMSEP)比较", y = "RMSEP", x = "模型") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
print(p3)

# 图5: 调整R²比较（仅适用于OLS和逐步回归）
p5 <- ggplot(results[!is.na(results$Adj_R2), ], aes(x = Model, y = Adj_R2, fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(Adj_R2, 3)), vjust = -0.5, size = 4) +
  labs(title = "模型调整R²比较", y = "调整R²", x = "模型", 
       subtitle = "注：PCR和PLS无调整R²") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
print(p5)

# 系数比较
coef_comparison <- data.frame(
  Variable = c("Intercept", "x1", "x2", "x3", "x4"),
  OLS = coef(ols),
  Stepwise = c(coef(stepwise), rep(NA, 5 - length(coef(stepwise))))
)

print("系数比较：")
print(coef_comparison)

# 图6: 模型系数比较（仅OLS和逐步回归）
coef_melt <- melt(coef_comparison, id.vars = "Variable", 
                  variable.name = "Model", value.name = "Coefficient")
coef_melt <- coef_melt[!is.na(coef_melt$Coefficient), ]

p6 <- ggplot(coef_melt, aes(x = Variable, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "模型系数比较", y = "系数值", x = "变量") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(p6)

# 模型诊断：残差分析
cat("\n=== 模型诊断图 ===\n")
cat("显示OLS和逐步回归的残差图...\n")

# 图7: OLS残差图
par(mfrow = c(1, 1))
plot(ols, which = 1, main = "OLS 残差图")

# 等待用户按键继续
cat("按回车键查看下一个残差图...")
readline()

# 图8: 逐步回归残差图
plot(stepwise, which = 1, main = "Stepwise 残差图")

# 综合评估
cat("\n=== 综合评估 ===\n")
cat("1. 拟合优度最佳模型:", results$Model[which.max(results$R2)], "\n")
cat("2. 预测误差最小模型:", results$Model[which.min(results$RMSE)], "\n")
cat("3. 最简洁模型:", results$Model[which.min(results$Num_Vars)], "\n")

# 推荐模型
if (results$Num_Vars[which.min(results$RMSE)] <= 2) {
  cat("4. 推荐模型:", results$Model[which.min(results$RMSE)], "(平衡了精度和简洁性)\n")
} else {
  cat("4. 推荐模型:", results$Model[which.min(results$Num_Vars)], "(更注重模型简洁性)\n")
}
