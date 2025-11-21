# 准备工作
y = c(160,260,210,265,240,220,275,160,275,250)
x1 = c(70,75,65,74,72,68,78,66,70,65)
x2 = c(35,40,40,42,38,45,42,36,44,42)
x3 = c(1.0,2.4,2.0,3.0,1.2,1.5,4.0,2.0,3.2,3.0)
freight_data = data.frame(x1 = x1,x2 = x2, x3 = x3, y = y)
model = lm(y ~ x1 + x2 + x3, data = freight_data)
summary_model = summary(model)
# 手动算
X <- cbind(1, x1, x2, x3) 
H <- X %*% solve(t(X) %*% X) %*% t(X)
h_ii <- diag(H)
sigma_hat = summary_model$sigma
e <- residuals(model)  
SRE = e / sigma_hat / sqrt(1 - h_ii)
n = length(y)
SRE_d = SRE * sqrt((n - 3 - 2) / (n - 3 - 1 - SRE^2))
print(SRE_d)
which(abs(SRE_d)>3)

# 机算
# 计算删除学生化残差SRE(i)的值
SRE = rstudent(model)
which(abs(SRE)>3)
# 可以知道第六个数据是异常值
h = hatvalues(lm.sol)#计算杠杆值

# 计算库克值
# 手算
D = e^2 * h_ii / ((3 + 1) * sigma_hat^2 * (1 - h_ii)^2)
which(D > 1)
# 机算
D = cooks.distance(model)