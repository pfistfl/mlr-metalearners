library(devtools)
library(parallelMap)
load_all("../mlr")

#multilabel
yeast = getTaskData(yeast.task)

#multiv. regression
enb = farff::readARFF("data/enb.arff")

#mixed output
wq = farff::readARFF("data/wq.arff")

wq$`37880` = as.factor(ifelse(wq$`37880` >= 3, "high", "low"))
names(wq)[17:30] = paste0("T", names(wq)[17:30])

#tasks:
yeast.task2 = makeMultilabelTask(data = yeast, target = paste0("label", 1:14))
enb.task = makeMultiRegrTask(data = enb, target = c("Y1", "Y2"))
wq.task = makeMixedOutputTask(data = wq, target = names(wq)[17:30])



#meta learners: chains
tn = getTaskTargetNames(wq.task)

object = list(tasks = sapply(tn, function(x) NULL), 
  learners = sapply(tn, function(x) NULL),
  models = sapply(tn, function(x) NULL), 
  yhat = sapply(tn, function(x) NULL))
for (i in 1:(length(tn) - 1)) object$learners[[i]] = makeLearner("regr.lm")
object$learners[[length(tn)]] = makeLearner("classif.logreg")

library(magrittr)
MOUT = wq.task

for (i in 1:length(tn)) {
  ta = tn[i]
  if (i == 1) object$tasks[[ta]] = MOUT %>% convertTaskForTarget(target = ta)
  if (i >= 2) {
    task2 = MOUT %>% convertTaskForTarget(target = ta)
    k = 1
    while (i - k >= 1) {
      task2 = task2 %>% addFeature(object$yhat[i - k], name = paste0(names(object$yhat[i - k]), "true"))
      k = k + 1
    }
    object$tasks[[ta]] = task2
  }

  object$models[[ta]] = object$tasks[[ta]] %>% train(learner = object$learners[[ta]], task = .)
  object$yhat[[ta]] = object$tasks[[ta]] %>% makeOobPred(learner = object$learners[[ta]])
}

