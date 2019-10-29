library(ggplot2)
library(plyr)
install.packages("pROC")
library(pROC)
install.packages("SDMTools")
library(SDMTools)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
## 设置工作目录
setwd("/Users/liyuanjie/Downloads/rtb_exercise")
##导入数据
rtb_data <- read.csv("rtb_exercise.csv", fileEncoding = 'utf-8') 
str(rtb_data)  #查看每列数据的数据类型

rtb_data$atype<- as.factor(rtb_data$atype)
rtb_data$instl<- as.factor(rtb_data$instl)
rtb_data$isp<- as.factor(rtb_data$isp)
rtb_data$nt<- as.factor(rtb_data$nt)

head(rtb_data,2) #查看前2行数据
mydata <- rtb_data
mydata$dc <- factor(mydata$dc, levels = c(1,0) )

## 描述统计分析
## 探索广告交易平台与点击率之间的关系
mydata$dc <- as.factor(mydata$dc) #将是否点击转化为因子型数据
mydata$atype <- factor(mydata$atype, levels = c(8,3,7,13),labels = c( "Inmobi","Baidu","Zplay","Iflytek" )) #将广告交易平台转化为因子型数据
str(mydata)
atype_dc <- table(mydata$atype,mydata$dc)
p<-spineplot(atype_dc,main = "交易平台" ,col=c( "lightblue", "grey" ),xaxlables=c( "Inmobi","Baidu","Zplay","Iflytek" ),yaxlabels=c( "点击","未点击" ))
p+theme (text = element_text(family = "STKaiti", size = 14))

##探索竞价低价与点击率之间的关系
hist(mydata$bidf, breaks = 100, xlab = "竞价低价", ylab = "频数", col = "lightblue")#直方图看一下广告竞价底价的分布规律
mydata$bidf <- replace(mydata$bidf, mydata$bidf>0,"大于0") #将竞价底价大于0的归为一类
mydata$bidf <- factor(mydata$bidf, levels = c("0","大于0"))
bidf_dc<- table(mydata$bidf, mydata$dc)
p<-spineplot(bidf_dc,main = "竞价底价" ,col=c( "lightblue", "grey" ),yaxlabels=c( "点击","未点击" ))
p+theme (text = element_text(family = "STKaiti", size = 14))

## 探索手机型号与点击率之间的关系
phone <- table(mydata$mfr)
phone = phone[order(phone, decreasing =T)]
str(phone)
barplot(phone, names.arg = names(phone), col = "lightblue", xlab = "手机型号", ylab="频数")#用户使用手机型号的分布图

phone_dc <- table(mydata$dc, mydata$mfr)
phone_dc[1,] = phone_dc[2, ]/(phone_dc[2, ] + phone_dc[1, ])#计算每个手机型号的广告点击率
phone_dc = phone_dc[-2,]
phone_dc = phone_dc[order(phone_dc,decreasing = F)]
barplot(phone_dc, col="lightblue", horiz = T,xlab = "点击率", ylab = "手机型号" )

count(mydata$dc ==0)
abline(count(mydata$dc==1)/count(mydata$dc = 0))

## 探索手机运营商与点击率之间的关系
mydata$isp <- factor(mydata$isp, levels = c(0, 3, 2, 1))
isp_dc <- table(mydata$isp, mydata$dc)
p<-spineplot(isp_dc, main = "手机运营商",col=c( "lightblue", "grey" ),xaxlabel= c("未知","中国电信","中国联通","中国移动" ), yaxlabels=c( "点击","未点击" ))
p+theme (text = element_text(family = "STKaiti", size = 14))

##探索网络状况与点击率之间的关系

str(mydata)
mydata$nt<- replace(x<-mydata$nt, x>2&x<6, 2) #将2G，3G，4G合并为一类
mydata$nt <- factor(mydata$nt, levels = c(1,0,2))
nt_dc <- table(mydata$nt, mydata$dc)
p<-spineplot(nt_dc, main = "手机网络" ,col=c( "lightblue", "grey" ),xaxlabel= c("WIFI","未知", "2G/3G/4G"), yaxlabels=c("点击","未点击" ))
p+theme (text = element_text(family = "STKaiti", size = 14))

##探索广告尺寸与点击率之间的关系
mydata$instl <- factor(mydata$instl, levels = c(0,1))
instl_dc <- table(mydata$instl, mydata$dc)
p<-spineplot(instl_dc, main = "广告尺寸" ,col=c( "lightblue", "grey" ),xaxlabel= c("非全插屏广告","全插屏广告"), yaxlabels=c( "点击","未点击" ))
p+theme (text = element_text(family = "STKaiti", size = 14))

##探索广告投放时间与点击率之间的关系
mydata$period <- factor(mydata$period, levels = c("上午","下午","晚上"))
period_dc <- table(mydata$period, mydata$dc)
p<-spineplot(period_dc, main = "广告投放时间" ,col=c( "lightblue", "grey" ),xaxlabel= c("上午","下午","晚上"), yaxlabels=c( "点击","未点击" ))

glm_all<-glm(rtb_data$dc~.,data = rtb_data,family = binomial())
glm_AIC <- step(glm_all, k=2)
summary(glm_AIC)
##分割训练集和测试集
mydata<-rtb_data
str(mydata)
set.seed(1234)
train_sub = createDataPartition(mydata$dc, p = .8, list = F, times = 1)
##随机法分割数据集 train_sub <- sample(nrow(mydata),4/5*nrow(mydata))
train_data = mydata[train_sub,]
test_data = mydata[-train_sub,]
str(train_data)

##进行逻辑回归
glm_full <- glm(dc ~ ., family = binomial(), data = train_data)
glm_AIC <- step(glm_full, k=2) #AIC准则
summary(glm_AIC)
##进行模型评估
predict_dc <- predict(glm_AIC, test_data, type = 'response')
ypre2 =1*(predict_dc > mean(test_data$dc))#测试样本中点击率作为阈值
confusionMatrix(factor(ypre2),factor(test_data$dc))#利用混淆矩阵评估模型
#画ROC曲线
plot(roc(test_data$dc,predict_dc),print.auc = T, print.thres = T, xlab ='特异度', ylab = '灵敏度')
