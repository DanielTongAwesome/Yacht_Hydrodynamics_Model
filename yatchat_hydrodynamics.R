# ====== library  =========================================
library(leaps)
# NOTE: change the address here when u using the code
yacht<- read.table("~/Library/Mobile Documents/com~apple~CloudDocs/Study/STAT306/Student_Result_Prediction/yacht_hydrodynamics.data", quote="\"", comment.char="")
s <- regsubsets(V7 ~ ., data=yacht, method= "forward")
ss <- summary(s)
names(ss)
ss$which
ss$rsq

# plot v7 against v6 
plot(V7 ~ V6, data=yacht)

# ====== train & test dataset ==============================

smp_size <- floor(0.7 * nrow(yacht))
set.seed(123)
train_ind <- sample(seq_len(nrow(yacht)), size = smp_size)
train <- yacht[ train_ind, ]
test  <- yacht[-train_ind, ]


# ====== regression part ==============================

# test model 1
# without log just v7 - v6+v2
reg_1 <- lm(V7 ~ V6 + V2  , data=yacht)

# test model  2
yacht$V7 = log(yacht$V7)
reg_2 <-lm(V7 ~ V6,           data=yacht)

# test model  3
reg_3 <-lm(V7 ~ V6 + I(V6^2), data=yacht)

# test model  4
reg_4 <-lm(V7 ~ V6 + I(V6^2) + I(V6^3) + I(V6^4) , data=yacht)


summary(reg_1)
summary(reg_2)
summary(reg_3)
summary(reg_4)


qqnorm(reg_1$residuals)
qqline(reg_1$residuals)

qqnorm(reg_2$residuals)
qqline(reg_2$residuals)

qqnorm(reg_3$residuals)
qqline(reg_3$residuals)

qqnorm(reg_4$residuals)
qqline(reg_4$residuals)

plot(reg_1$fitted.values, reg_1$residuals)
plot(reg_2$fitted.values, reg_2$residuals)
plot(reg_3$fitted.values, reg_3$residuals)
plot(reg_4$fitted.values, reg_4$residuals)


# ====== test part ===============================
error_raw_1 <-sum( (log(test$V7) - predict(reg_1, test) )^2)
error_raw_2 <-sum( (log(test$V7) - predict(reg_2, test) )^2)
error_raw_3 <-sum( (log(test$V7) - predict(reg_3, test) )^2)
error_raw_4 <-sum( (log(test$V7) - predict(reg_4, test) )^2)

(error_1 <- (error_raw_1)/dim(test)[1])
(error_2 <- (error_raw_2)/dim(test)[1])
(error_3 <- (error_raw_3)/dim(test)[1])
(error_4 <- (error_raw_4)/dim(test)[1])


AIC_1<-AIC(reg_1, k=2)
AIC_2<-AIC(reg_2, k=2)
AIC_3<-AIC(reg_3, k=3)
AIC_4<-AIC(reg_4, k=5)




