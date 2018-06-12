# mlr-metalearners

** No working code exists yet, this is a WiP!** 

Extending mlr using several meta-learning techniques such as stacking, multioutput and multilabel ensembles

The general idea is to extend mlr to allow for easy incorporation of chaining, stacking and blending 
via a mlrCPO-esque syntax.


## Possible Operators

```
maskFeatures()
unmaskFeatures()
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