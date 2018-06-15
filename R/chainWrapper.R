#' @title Fuse learner with SMOTE oversampling for imbalancy correction in binary classification.
#'
#' @description
#' Creates a learner object, which can be
#' used like any other learner object.
#' Internally uses [smote] before every model fit.
#'
#' Note that observation weights do not influence the sampling and are simply passed
#' down to the next learner.
#'
#' @template arg_learner
#' @template ret_learner
#' @family wrapper
#' @export
makeChainLearnerWrapper = function(first.learner, second.learner, second.lrn.id = NULL, oob.strat = cv3, ...) {
  
  # Create a ModelMultiplexer
  mm = makeModelMultiplexer(list(first.learner, second.learner))
  assertString(second.lrn.id, null.ok = TRUE)
  assert(checkClass(oob.strat, "ResampleDesc", null.ok = TRUE), checkClass(oob.strat, "ResampleInstance"))
  
  # Add the ModelMultiplexer to a BaseWrapper
  id = stringi::stri_paste(mm$base.learners[[1]]$id, "_", ifelse(is.null(second.lrn.id), 
    mm$base.learners[[2]]$id, second.lrn.id), sep = "")
  lrn = makeBaseWrapper(id, "classif", mm,
    par.set = makeParamSet(makeUntypedLearnerParam(id = "oob.strat", default = mlr::cv3)),
    package = c(mm$base.learners[[1]]$package, mm$base.learners[[2]]$package),
    learner.subclass = "chainWrapper",
    model.subclass = "chainModel")
  
  return(lrn)
}

#' @export
trainLearner.chainWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, oob.strat = cv3, ...) {
  
  .task = subsetTask(.task, .subset)
  
  # Train using first learner
  first.model = train(.learner$next.learner, .task, weights = .weights)
  
  # FIXME: Incorporate different methods, here we do stupid blending
  if (is.null(oob.strat)) {
    model.preds = predict(first.model, .task)$data$response
  } else {
    model.preds = makeOobPred(.task, .learner$next.learner, rdesc = oob.strat)
  }
  
  # Create a new task with the additional predictions
  nextdata = getTaskData(.task, target.extra = FALSE)
  # Avoid name clashes in feature names
  new.feat.name = setdiff(paste(.learner$next.learner$base.learners[[1]]$id,
    "prd", seq_len(sum(.task$task.desc$n.feat) + 2),  sep = "."), colnames(nextdata))[1]
  nextdata[[new.feat.name]] =  model.preds
  .task = makeClassifTask(id = getTaskId(.task), target = getTaskTargetNames(.task),
    data = nextdata)
  
  m = train(setHyperPars(.learner$next.learner, selected.learner = .learner$next.learner$base.learners[[2]]$id),
    .task, weights = .weights) 
  cm = makeChainModel(next.model = m, cl = "ChainModel")
  cm$first.model = first.model
  return(cm)
}

predictLearner.chainWrapper = function(.learner, .model, .newdata, ...) {
  # Predict from first model
  prd.first.model = predict(.model$learner.model$first.model, newdata = .newdata)$data$response
  # Add correctly named column with predictions from first learner
  .newdata[[setdiff(.model$learner.model$next.model$features, colnames(.newdata))]] = prd.first.model
  # Return predictions
  prd = predict(.model$learner.model$next.model, newdata = .newdata)
  return(prd$data$response)
}


if (FALSE) {
  cw = makeChainLearnerWrapper("classif.rpart", "classif.svm")
  cm = train(cw, pid.task)
  cp = predict(cm, pid.task)
  
  resample(cw, pid.task, cv3)
  
  lrn = "classif.rpart" %>%
    makeChainLearnerWrapper("classif.svm") %>%
    makeChainLearnerWrapper("classif.ranger") %>%
    makeChainLearnerWrapper("classif.svm")
  resample(lrn, pid.task, cv3)
  
  m1 = makeModelMultiplexer(c("classif.rpart", "classif.ranger"))
  m2 = makeModelMultiplexer(list(m1, makeLearner("classif.svm")))
  m3 = makeModelMultiplexer(list(m2, makeLearner("classif.ksvm")))
}