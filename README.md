# mlr-metalearners

** No working code exists yet, this is a WiP!** 

Extending mlr using several meta-learning techniques such as stacking, multioutput and multilabel ensembles

The general idea is to extend mlr to allow for easy incorporation of chaining, stacking and blending 
via a mlrCPO-esque syntax.


## Possible Functions

```
# Create a multiOutTask
makeMultiOutputTask(data, targets = c("regr" = "y1", "classif" = "y2"", "regr" = "y3"))


# Drop irrelevant targets and convert to appropriate task.
# MOUT > dropOtherTargetCols() %>% makeClassif|RegrTask()
convertTaskForTarget(task, target)

# MOUT > Add a new feature to the task
addFeature(new.feature)

# task > predict(task, model) | getOOB(resample(task, lrn, rdesc)) | copyTarget(task)
getPrediction(task, model, truevspreds, rdesc)




maskFeatures()
unmaskFeatures()
addFeature()
dropFeature()
trainPredict()

# Chaining:
y1 <- X
y2 <- X, y1
y3 <- X, y2, y2

# TRAIN:
train = function() {
  
  tasks[[1]] = MOUT %>% convertTaskForTarget("y1")
  models[[1]] = tasks[[1]] %>% train(lrns[["y1"]])
  y_hats[[1]] = getPrediction(tasks[[1]], models[[1]], ...)
  
  tasks[[2]] = MOUT %>% convertTaskForTarget("y2") %>% addFeature(y_hats[[1]])
  models[[2]] = tasks[[2]] %>% train(lrns[["y2"]])
  y_hats[[2]] = getPrediction(tasks[[2]], models[[2]], ...)
  
  tasks[[3]] = MOUT %>% convertTaskForTarget("y3") %>% addFeature(y_hats[[1:2]])
  models[[3]] = tasks[[3]] %>% train(lrns[["y3"]])
  return(models)
}
# TEST: 
test = function() {
  preds[[1]] = MOUT_test %>% convertTaskForTarget("y1") %>% predict(models[[1]])
  preds[[2]] = MOUT_test %>% convertTaskForTarget("y2") %>% addFeature(preds[[1]]) %>% predict(models[[2]])
  preds[[3]] = MOUT_test %>% convertTaskForTarget("y3") %>% addFeature(preds[[1:2]]) %>% predict(models[[3]])
  return(preds)
}

Conditioning = FULL
y1 <- X, y2, y3
y2 <- X, y1, y3
y3 <- X, y1, y2

train = function() {

  tasks[[1]] = MOUT %>% convertTaskForTarget("y1")
  models.oob[[1]] = tasks[[1]] %>% train(lrns[["y1"]])
  tasks[[2]] = MOUT %>% convertTaskForTarget("y2")
  models.oob[[2]] = tasks[[2]] %>% train(lrns[["y2"]])
  tasks[[3]] = MOUT %>% convertTaskForTarget("y3")
  models.oob[[3]] = tasks[[3]] %>% train(lrns[["y3"]])
  
  y_hats[[1]] = getPrediction(tasks[[1]], models.oob[[1]], ...)
  y_hats[[2]] = getPrediction(tasks[[2]], models.oob[[2]], ...)
  y_hats[[3]] = getPrediction(tasks[[3]], models.oob[[3]], ...)
  
  models[[1]] = MOUT %>% convertTaskForTarget("y1") %>% addFeature(y_hats[[2, 3]]) %>% train(lrns[["y1"]])
  models[[2]] = MOUT %>% convertTaskForTarget("y2") %>% addFeature(y_hats[[1, 3]]) %>% train(lrns[["y2"]])
  models[[3]] = MOUT %>% convertTaskForTarget("y3") %>% addFeature(y_hats[[1, 2]]) %>% train(lrns[["y3"]])

  return(list(models, models.oob))
}
# TEST: 
test = function() {
  preds.oob[[1]] = MOUT_test %>% convertTaskForTarget("y1") %>% predict(models.oob[[1]])
  preds.oob[[2]] = MOUT_test %>% convertTaskForTarget("y2") %>% predict(models.oob[[2]])
  preds.oob[[2]] = MOUT_test %>% convertTaskForTarget("y3") %>% predict(models.oob[[3]])
  
  preds[[1]] = MOUT_test %>% convertTaskForTarget("y1") %>% addFeature(y_hats[[2, 3]]) %>% predict(models[[1]])
  preds[[2]] = MOUT_test %>% convertTaskForTarget("y2") %>% addFeature(y_hats[[2, 3]]) %>% predict(models[[2]])
  preds[[3]] = MOUT_test %>% convertTaskForTarget("y3") %>% addFeature(y_hats[[2, 3]]) %>% predict(models[[3]])
  
  return(preds)
}

y1 <- X
y2 <- X
y3 <- X, y1, y2
 
lrn1 = metaLearner(condition = "zero", targets = c("y1", "y2")) %>5
lrn2 = metaLearner(condition = "zer0", targets = "y3")

```


## Simple usecase MultiOutput

A simple usecase would be first predicting the `age` variable in 
mlr's `pid.task` using a `regr.lm` learner and then predicting the *original* target `diabetes` using a 
`classif.rpart` learner.

```r
# Get Task Data
pid.data = getTaskData(pid.task)
# Create a MultiOutput Task
motsk = makeMultiOutputTask(pid.data, targets = c("age", "diabetes"))
lrn.regr = makeLearner("regr.lm")
lrn.clf = makeLearner("classif.rpart")

# Define the learner pipeline:
# FIXME: This is super-bad syntax, but I think it kind of showcases the problem.
#        We have to come up with a decent syntax for this.
motsk %>>% selectTarget("age") %>% train(lrn.regr) %>% selectTarget("diabetes") %>% train(lrn.clf)

```
## Simple usecase Stacking

A simple usecase for stacking would be predicting `diabetes` in mlr's `pid.task` using a `regr.lm` using
several learners, such as `classif.rpart`,`classif.svm`, `classif.xgboost`, ... etc. and then use majority voting
to determine the class.
```r
lrn.1 = makeLearner("classif.rpart") # lrn.2 = ...
# FIXME: This is super-bad syntax, but I think it kind of showcases the problem.
#        We have to come up with a decent syntax for this.
pid.tsk %>>% addPreds(trainPredictLrns(list(lrn.1, lrn.2, lrn.3))) %>>% dropFeatures() %>>% majorityVote()
```