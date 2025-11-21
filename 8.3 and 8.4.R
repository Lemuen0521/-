# 本例为回归分析中经典的Hald水泥问题。
# y 某种水泥在凝固时放出的热量y(卡/克，cal/g) 与水泥中的四种化学成分的含量(%)有关
# x1 铝酸三钙
# x2 硅酸三钙
# x3 铁铝酸四钙
# x4 硅酸二钙
data = read.csv("E:/研究生阶段作业/应用回归分析数据/8.3.csv")
# 标准化数据
datas = data.frame(scale(data))
library(car)
library(caret)
library(ggplot2)
library(reshape2)
library(pls)
library(GGally)
library(gridExtra)
library(ggpattern)
# 法1：普通最小二乘法
ols = lm(y ~ x1 + x2 + x3 + x4 , data = data)
summary(ols)

# 法2：本例用逐步回归法做变量选择，希望从中选出主要的变量，建立，关于四种成分的线性回归方程
stepwise = step(ols, direction = "both", trace = 0)
summary(stepwise)

# 法3：主成分回归方法
# 计算累计贡献率
pca = prcomp(datas[, c("x1", "x2", "x3", "x4")])
summary(pca)
# 主成分回归PCR
scores = pca$x
pcr1 = lm(data$y ~ scores[,1:2])
summary(pcr1)

# 法4：偏最小二乘回归
pls1 = plsr(y~., data = datas, validation = "LOO")
summary(pls1, what = "all")
# 还原回归系数
pls2 = plsr(y~.,data = datas, ncomp = 3, validation = "LOO", jackknife = TRUE)
coef(pls2)

# 结果比较
# 1. 首先绘制四个x变量的配对图
pairs_plot = ggpairs(data[, c("x1", "x2", "x3", "x4")]) + theme_minimal()
print(pairs_plot)

# 2. 计算四种方法的LOOCV RMSE
# 准备存储结果
results <- data.frame(
  Method = character(),
  RMSE_LOOCV = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)
# 法1: 普通最小二乘法
ols_loocv <- NULL
for(i in 1:nrow(data)) {
  train_data <- data[-i, ]
  test_data <- data[i, ]
  ols_model <- lm(y ~ x1 + x2 + x3 + x4, data = train_data)
  pred <- predict(ols_model, newdata = test_data)
  ols_loocv <- c(ols_loocv, pred)
}
rmse_ols <- sqrt(mean((data$y - ols_loocv)^2))
# 法2: 逐步回归法
stepwise_loocv <- NULL
for(i in 1:nrow(data)) {
  train_data <- data[-i, ]
  test_data <- data[i, ]
  full_model <- lm(y ~ x1 + x2 + x3 + x4, data = train_data)
  step_model <- step(full_model, direction = "both", trace = 0)
  pred <- predict(step_model, newdata = test_data)
  stepwise_loocv <- c(stepwise_loocv, pred)
}
rmse_stepwise <- sqrt(mean((data$y - stepwise_loocv)^2))
# 法3: 主成分回归 LOOCV RMSE
pcr_loocv <- NULL
for(i in 1:nrow(data)) {
  train_data <- data[-i, ]
  test_data <- data[i, ]
  # 对训练集做PCA
  train_pca <- prcomp(train_data[, c("x1", "x2", "x3", "x4")], scale. = TRUE)
  train_scores <- train_pca$x[, 1:2]
  # 训练PCR模型
  pcr_model <- lm(train_data$y ~ train_scores)
  # 对测试集进行相同的PCA变换
  test_scaled <- scale(test_data[, c("x1", "x2", "x3", "x4")], 
                       center = train_pca$center, 
                       scale = train_pca$scale)
  test_scores <- test_scaled %*% train_pca$rotation[, 1:2]
  pred <- predict(pcr_model, newdata = data.frame(train_scores = test_scores))
  pcr_loocv <- c(pcr_loocv, pred)
}
rmse_pcr <- sqrt(mean((data$y - pcr_loocv)^2))
# 法4: 偏最小二乘 LOOCV RMSE (使用pls包内置的交叉验证)
pls_model <- plsr(y ~ x1 + x2 + x3 + x4, data = data, validation = "LOO", ncomp = 3)
rmse_pls <- RMSEP(pls_model)$val[1,,][4]  # 取3个成分的RMSE
# 存储结果
results <- rbind(results,
                 data.frame(Method = "OLS", RMSE_LOOCV = rmse_ols, R_squared = summary(ols)$r.squared),
                 data.frame(Method = "Stepwise", RMSE_LOOCV = rmse_stepwise, R_squared = summary(stepwise)$r.squared),
                 data.frame(Method = "PCR", RMSE_LOOCV = rmse_pcr, R_squared = summary(pcr1)$r.squared),
                 data.frame(Method = "PLS", RMSE_LOOCV = rmse_pls, R_squared = pls_model$Xvar[3]/pls_model$Xtotvar))
# 输出结果
print(results)

# 3. 主成分回归的特有分析
# 碎石图 - 主成分的标准差
scree_data <- data.frame(  PC = 1:length(pca$sdev),Standard_Deviation = pca$sdev)
scree_plot <- ggplot(scree_data, aes(x = PC, y = Standard_Deviation)) + 
  geom_line() + geom_point() +
  labs(title = "主成分回归 - 碎石图", 
       x = "主成分", y = "标准差") +
  theme_minimal()
print(scree_plot)
# 主成分载荷图
loadings_data <- as.data.frame(pca$rotation[, 1:3])
loadings_data$Variable <- rownames(loadings_data)
loadings_long <- reshape2::melt(loadings_data, id.vars = "Variable")
loadings_plot <- ggplot(loadings_long, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.3) +
  scale_fill_manual(values = c("white", "gray70", "gray40")) +
  labs(title = "主成分回归 - 前三个主成分的载荷", 
       x = "原始变量", y = "载荷值") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))
print(loadings_plot)
# 为四种方法都绘制(y-hat, y)散点图
# 1. OLS的预测值
ols_pred <- predict(ols)
# 2. Stepwise的预测值
stepwise_pred <- predict(stepwise)
# 3. PCR的预测值
pcr_pred <- predict(pcr1)
# 4. PLS的预测值
pls_pred <- predict(pls_model, ncomp = 3)
# 创建数据框
scatter_data <- data.frame(
  Actual = rep(data$y, 4),
  Predicted = c(ols_pred, stepwise_pred, pcr_pred, as.numeric(pls_pred)),
  Method = rep(c("OLS", "Stepwise", "PCR", "PLS"), each = nrow(data))
)
# 绘制四个方法的散点图
scatter_plots <- ggplot(scatter_data, aes(x = Actual, y = Predicted)) +
  geom_point(aes(shape = Method), size = 3, alpha = 0.8, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ Method, ncol = 2) +
  labs(title = "四种回归方法的预测值 vs 实际值",
       x = "实际值 y", y = "预测值 y-hat") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(16, 17, 15, 18)) 
print(scatter_plots)

# 5. 综合比较图
# RMSE比较图
rmse_plot <- ggplot(results, aes(x = Method, y = RMSE_LOOCV, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "四种方法的LOOCV RMSE比较", y = "RMSE") +
  theme_minimal()
print(rmse_plot)
# R²比较图
r2_plot <- ggplot(results, aes(x = Method, y = R_squared, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "四种方法的R²比较", y = "R-squared") +
  theme_minimal()
print(r2_plot)

# 输出详细比较结果
cat("\n=== 主成分回归重点比较 ===\n")
cat("PCR使用前两个主成分，累计方差解释率:", summary(pca)$importance[3,2], "\n")
cat("PCR模型R²:", summary(pcr1)$r.squared, "\n")
cat("PCR LOOCV RMSE:", rmse_pcr, "\n")

cat("\n=== 偏最小二乘重点比较 ===\n")
cat("PLS使用3个成分，X方差解释率:", pls_model$Xvar[3]/pls_model$Xtotvar, "\n")
cat("PLS Y方差解释率:", pls_model$Yvar[3]/pls_model$Ytotvar, "\n")
cat("PLS LOOCV RMSE:", rmse_pls, "\n")