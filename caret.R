library(caret)
library(tidyr)

library(mice)
library(VIM)
library(missForest)

# 补全缺省值 NA ----
newdata1 <- missForest(input)

newdata2 <- newdata1$ximp
#删除方差为0的变量
zerovar=nearZeroVar(newdata2)
newdata3=newdata2[,-zerovar]

#首先删除强相关的变量


# descrCorr = cor(newdata1)
# 
# highCorr = findCorrelation(descrCorr, 0.90)
# 
# newdata2 = newdata1[, -highCorr]

# #随后解决多重共线性，本例中不存在多重共线性问题
# comboInfo = findLinearCombos(newdata2)
# newdata2=newdata2[, -comboInfo$remove]

Process = preProcess(newdata2)
newdata3 = predict(Process, newdata2)


ctrl= rfeControl(functions = rfFuncs, method = "repeatedcv",verbose = FALSE, returnResamp = "final")
#functions是确定用什么样的模型进行自变量排序，本例选择的模型是随机森林即rfFuncs，可以选择的
#还有lmFuncs（线性回归），nbFuncs（朴素贝叶斯），treebagFuncs（装袋决策树），caretFuncs
#(自定义的训练模型）。method是确定用什么样的抽样方法，本例使用cv即交叉检验, 还有提升boot以及
#留一交叉检验LOOCV

Profile = rfe(newdata3, output,  rfeControl = ctrl)
print(Profile)
plot(Profile)


featurePlot(x = newdata3,
            y = output, plot = "pairs",
            auto.key = list(columns = 2))

featurePlot(x = newdata3, 
            y = output,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(2, 5), 
            auto.key = list(columns = 2))

featurePlot(x = newdata3, 
            y = output, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(2,5 ), 
            auto.key = list(columns = 2))
# 建模 ----
#首先按照比例划分训练集与测试集
newdata4=newdata3[,Profile$optVariables]
inTrain = createDataPartition(output, p = 3/4, list = FALSE)
trainx = newdata4[inTrain,]
testx = newdata4[-inTrain,]
trainy = output[inTrain]
testy = output[-inTrain]

#作图查看前6个变量的分布情况
featurePlot(trainx[,1:4],trainy,plot='box')

# model training
fitControl = trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 3,
  returnResamp = "all")

gbmFit = train(
  trainx,trainy,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE)


# 预测模型准确性 ----
predict(gbmFit, newdata = testx)


accu <- data.frame(
  predict = predict(gbmFit, newdata = testx),
  testy = testy
) %>%
  mutate(
    compare = ifelse(predict == testy, 1, 0)
  )
nrow(subset(accu, compare ==1)) / nrow(accu)

# 特征重要性评估 ----
library(gbm)
gbmImp <- varImp(gbmFit, scale = FALSE)
plot(gbmImp, top = 4)



