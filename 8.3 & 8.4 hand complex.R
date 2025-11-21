# 本例为回归分析中经典的Hald水泥问题。
# y 某种水泥在凝固时放出的热量y(卡/克，cal/g) 与水泥中的四种化学成分的含量(%)有关
# x1 铝酸三钙
# x2 硅酸三钙
# x3 铁铝酸四钙
# x4 硅酸二钙
data = read.csv("E:/研究生阶段作业/应用回归分析数据/8.3.csv")

# 法1：普通最小二乘法
ols = lm(y ~ x1 + x2 + x3 + x4 , data = data)
summary(ols)

# 法2：本例用逐步回归法做变量选择，希望从中选出主要的变量，建立，关于四种成分的线性回归方程
stepwise = step(ols, direction = "both", trace = 0)
summary(stepwise)

# 法3：主成分回归方法
# p个x，n个样本
# 标准化，消除量纲的影响
X_scaled = scale(data[, c("x1", "x2", "x3", "x4")])
mu_x = attr(X_scaled, "scaled:center")
sigma_x = attr(X_scaled, "scaled:scale")
# 协方差矩阵
cov_matrix = cov(X_scaled)
# 提取特征值和特征向量
eigen_result = eigen(cov_matrix)
eigenvalues = eigen_result$values
eigenvectors = eigen_result$vectors
# 计算主成分得分
principal_scores = X_scaled %*% eigenvectors
colnames(principal_scores) = paste0("PC", 1:ncol(principal_scores))
# 计算方差贡献率 和 累计方差贡献率
total_var = sum(eigenvalues)
cumulative_var = 0
for (i in 1:length(eigenvalues)) {
  var_percentage = (eigenvalues[i] / total_var) * 100
  cumulative_var =cumulative_var + var_percentage
  cat("第",i,"个主成分的方差贡献率为",var_percentage,"%\n")
  cat("前",i,"个主成分的累计贡献率为",cumulative_var,"%\n")
}
# 计算因子载荷
eigenvectors = eigen_result$vectors
factor_loadings = eigenvectors %*% diag(sqrt(eigenvalues))
rownames(factor_loadings) = colnames(X_scaled)
colnames(factor_loadings) = paste0("factor", 1:ncol(factor_loadings))
print(round(factor_loadings, 4))
# 前两个主成分的累计贡献率大于85%，所以选前两个做主成分因子载荷分析
# 将前两个主成分加入数据
data$PC1 = principal_scores[, 1]
data$PC2 = principal_scores[, 2]
# 主成分回归
pcr_model = lm(y ~ PC1 + PC2, data = data)
summary(pcr_model)

# 比较：多重共线性诊断
library(car)
# 全模型VIF
vif_full = vif(full_model)
print(vif_full)
# 逐步回归模型VIF
vif_step = vif(stepwise_model)
print(vif_step)

# 使用交叉验证计算RMSE（更可靠）
library(caret)
# 全模型交叉验证RMSE
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
cv_full <- train(y ~ x1 + x2 + x3 + x4, data = data, 
                 method = "lm", trControl = train_control)
cv_rmse_full <- cv_full$results$RMSE
# 逐步回归交叉验证RMSE
cv_step <- train(y ~ x1 + x2 + x4, data = data,  # 根据你的逐步回归结果
                 method = "lm", trControl = train_control)
cv_rmse_step <- cv_step$results$RMSE
# 主成分回归交叉验证RMSE
cv_pcr <- train(y ~ PC1 + PC2, data = data,
                method = "lm", trControl = train_control)
cv_rmse_pcr <- cv_pcr$results$RMSE
cat("\n=== 交叉验证RMSE比较 ===\n")
cat("全模型CV-RMSE:", round(cv_rmse_full, 4), "\n")
cat("逐步回归CV-RMSE:", round(cv_rmse_step, 4), "\n")
cat("主成分回归CV-RMSE:", round(cv_rmse_pcr, 4), "\n")

# 系数压缩效果图比较
# 将主成分系数转回原始变量空间
cat("\n=== 主成分回归系数转换 ===\n")
pc_coef <- coef(pcr_model)  # 截距, PC1, PC2系数
# PC1和PC2在标准化变量上的系数
beta_star <- eigenvectors[, 1] * pc_coef["PC1"] + eigenvectors[, 2] * pc_coef["PC2"]
# 逆标准化：转回原始尺度
beta_original <- beta_star / sigma_x
intercept_original <- pc_coef["(Intercept)"] - sum(beta_original * mu_x)
# 主成分回归的原始变量系数
pcr_coef_original <- c(intercept_original, beta_original)
names(pcr_coef_original) <- c("(Intercept)", "x1", "x2", "x3", "x4")
cat("主成分回归的原始变量系数:\n")
print(round(pcr_coef_original, 4))
# 系数稳定性比较图
library(ggplot2)
library(reshape2)
# 收集各模型的系数
coef_comparison <- data.frame(
  Variable = c("x1", "x2", "x3", "x4"),
  Full_Model = coef(full_model)[-1],  # 去掉截距
  Stepwise = c(coef(stepwise_model)["x1"], coef(stepwise_model)["x2"], 
               coef(stepwise_model)["x3"], coef(stepwise_model)["x4"]),
  PCR = beta_original
)
# 处理NA值，逐步回归可能删除了x3
coef_comparison[is.na(coef_comparison)] = 0
# 绘制系数压缩效果图
coef_melted <- melt(coef_comparison, id.vars = "Variable", 
                    variable.name = "Model", value.name = "Coefficient")

# 系数压缩效果图 - 纯黑白线图
ggplot(coef_melted, aes(x = Variable, y = Coefficient, linetype = Model, group = Model)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  labs(title = "回归系数压缩效果比较",
       subtitle = "主成分回归的系数压缩与稳定性",
       x = "变量", 
       y = "系数值",
       linetype = "模型类型") +
  scale_linetype_manual(
    values = c("Full_Model" = "solid", "Stepwise" = "dashed", "PCR" = "dotdash"),
    labels = c("Full_Model" = "全模型", "Stepwise" = "逐步回归", "PCR" = "主成分回归")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "black"),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.line = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(expand = expansion(mult = 0.1))

# 法4：偏最小二乘回归
library(pls)
# 全数据标准化
datas = data.frame(scale(data))
pls1 = plsr(y~., data = datas, validation = "LOO")
summary(pls1, what = "all")
# 还原回归系数
pls2 = plsr(y~.,data = datas, ncomp = 3, validation = "LOO", jackknife = TRUE)
coef(pls2)