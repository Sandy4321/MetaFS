

library(sva)
library(bladderbatch)
library(pamr)
library(limma)
data("bladderdata")
pheno = pData(bladderEset)
edata = exprs(bladderEset)

set.seed(12354)
trainIndicator = sample(1:57,size=30,replace=F)
testIndicator = (1:57)[-trainIndicator]
trainData = edata[,trainIndicator]
testData = edata[,testIndicator]
trainPheno = pheno[trainIndicator,]
testPheno = pheno[testIndicator,]

mydata = list(x=trainData,y=trainPheno$cancer)
mytrain = pamr.train(mydata)
table(pamr.predict(mytrain,testData,threshold=2),testPheno$cancer)

trainMod = model.matrix(~cancer,data=trainPheno)

trainMod0 = model.matrix(~1,data=trainPheno)
trainSv = sva(trainData,trainMod,trainMod0)



batch <- pheno$batch

modcombat <- model.matrix(~1, data=pheno)
combat_edata <- ComBat(dat = edata, batch = batch, mod = modcombat, 
                       par.prior = TRUE, prior.plots = FALSE)
boxplot(combat_edata)



