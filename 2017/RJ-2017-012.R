yeast = getTaskData(yeast.task)
labels = colnames(yeast)[1:14]
yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
lrn = makeLearner("classif.rpart", predict.type = "prob")
multilabel.lrn1 = makeMultilabelBinaryRelevanceWrapper(lrn)
multilabel.lrn2 = makeMultilabelNestedStackingWrapper(lrn)
multilabel.lrn3 = makeLearner("multilabel.rFerns")
multilabel.lrn4 = makeLearner("multilabel.randomForestSRC")
mod = train(multilabel.lrn1, yeast.task, subset = 1:1500)
pred = predict(mod, task = yeast.task, subset = 1501:1600)
performance(pred, measures = list(multilabel.hamloss, timepredict))
# multilabel.hamloss timepredict
# 0.230 0.174
listMeasures("multilabel")
# [1] "multilabel.ppv" "timepredict" "multilabel.hamloss" "multilabel.f1"
# [5] "featperc" "multilabel.subset01" "timeboth" "timetrain"
# [9] "multilabel.tpr" "multilabel.acc"
rdesc = makeResampleDesc(method = "CV", stratify = FALSE, iters = 3)
r = resample(learner = multilabel.lrn1, task = yeast.task, resampling = rdesc,
measures = list(multilabel.hamloss), show.info = FALSE)
r
# Resample Result
# Task: multi
# Learner: multilabel.classif.rpart
# multilabel.hamloss.aggr: 0.23
# multilabel.hamloss.mean: 0.23
# multilabel.hamloss.sd: 0.00
# Runtime: 6.36688
head(getMultilabelBinaryPerformances(r$pred, measures = list(acc, mmce, auc)))
# acc.test.mean mmce.test.mean auc.test.mean
# label1 0.7389326 0.2610674 0.6801810
# label2 0.5908151 0.4091849 0.5935160
# label3 0.6512205 0.3487795 0.6631469
# label4 0.6921804 0.3078196 0.6965552
# label5 0.7517584 0.2482416 0.6748458
# label6 0.7343815 0.2656185 0.6054968
library(parallelMap)
parallelStartSocket(2)
lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
mod = train(lrn, yeast.task)
pred = predict(mod, yeast.task)
# http://mulan.sourceforge.net/datasets-mlc.html
# http://languagelog.ldc.upenn.edu/nll/
# http://bailando.sims.berkeley.edu/enron_email.html
# http://www.cs.cmu.edu/~enron/
# http://lamda.nju.edu.cn/data_MIMLtext.ashx
# http://lamda.nju.edu.cn/data_MIMLimage.ashx
# http://slashdot.org
STA(rf)
STA(rf)
STA(ad)
BR(ad)
DBR(rf)
BR(ad)
CC(ad)
BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)
STA(rf)
STA(ad)</div>
slashdot*
yeast*
scene
image
reuters
enron*
langLog*
genbase*
emotions
birds*
0.00 0.25 0.50
Hamming loss</div>
DBR(ad)
CC(ad)
DBR(ad)
DBR(ad)
DBR(ad)
DBR(ad)
CC(ad)
BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)BR(ad), CC(ad), STA(ad), NST(ad)
DBR(rf)
STA(ad)</div>
slashdot*
yeast*
scene
image
reuters
enron*
langLog*
genbase*
emotions
birds*
0.00 0.25 0.50 0.75 1.00
1
∏
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
>
>
>
>
>
0
0
0
0
0
0
0
0
0







1
1
1
