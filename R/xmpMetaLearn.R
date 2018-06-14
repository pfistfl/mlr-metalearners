# Example from Martin Binder from the mlrCPO Vignette
# This Cpo uses a learner that predicts the target variable and then creates 
# a new task target based on whether the learner predicted the target 
# correctly or not. 

xmpMetaLearn = makeCPOTargetOp("xmp.meta",
  pSS(lrn: untyped),
  dataformat = "task",
  properties.target = c("classif", "twoclass"),
  predict.type.map = c(response = "response", prob = "prob"),
  
  cpo.train = function(data, target, lrn) {
   cat("*** cpo.train ***\n")
   lrn = setPredictType(lrn, "prob")
   train(lrn, data)
  },
  
  cpo.retrafo = function(data, target, control, lrn) {
   cat("*** cpo.retrafo ***\n")
   prediction = predict(control, target)
   tname = getTaskTargetNames(target)
   tdata = getTaskData(target)
   tdata[[tname]] = factor(prediction$data$response == prediction$data$truth)
   makeClassifTask(getTaskId(target), tdata, tname, positive = "TRUE",
                   fixup.data = "no", check.data = FALSE)
  },
  
  cpo.train.invert = function(data, control, lrn) {
   cat("*** cpo.train.invert ***\n")
   predict(control, newdata = data)$data
  },
  
  cpo.invert = function(target, control.invert, predict.type, lrn) {
   cat("*** cpo.invert ***\n")
   if (predict.type == "prob") {
     outmat = as.matrix(control.invert[grep("^prob\\.", names(control.invert))])
     revmat = outmat[, c(2, 1)]
     outmat * target[, "prob.TRUE", drop = TRUE] +
       revmat * target[, "prob.FALSE", drop = TRUE]
   } else {
     stopifnot(levels(target) == c("FALSE", "TRUE"))
     numeric.prediction = as.numeric(control.invert$response)
     numeric.res = ifelse(target == "TRUE",
                          numeric.prediction,
                          3 - numeric.prediction)
     factor(levels(control.invert$response)[numeric.res],
            levels(control.invert$response))
   }
})

# We can chain the xmpMetaLearns and then normally pipe into resample
xmpMetaLearn(makeLearner("classif.svm"), id = "1") %>>%
  makeLearner("classif.rpart") %>%
  resample(pid.task, cv2)

# Alternatively, apply it to a task.
pid.task %>>%  xmpMetaLearn(makeLearner("classif.logreg")) %>>%
  train(., learner = makeLearner("classif.rpart"))
