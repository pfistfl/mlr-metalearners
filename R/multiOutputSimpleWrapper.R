#' @title ADD TITLE #FIXME
#'
#' @description
#' ADD DESCRIPTION #FIXME
#'
#' Models can easily be accessed via [getLearnerModel].
#'
#' @template arg_learner
#' @template arg_multilabel_cvfolds
#' @template ret_learner
#' @references
#' ADD REFRENCES #FIXME
#' @family wrapper
#' @family multilabel
#' @family multiregr
#' @family mixedoutput
#' @export
#' @example # add example FIXME#inst/examples/MultilabelWrapper.R
makeMultiOutputSimpleWrapper = function(output.type = "mixedoutput", regr, classif, individual = FALSE) {
  if (!output.type %in% c("mixedoutput", "multilabel", "multiregr")) stop("type must be 'mixedoutput', 'multilabel', or 'multiregr'")
  regr = checkLearner(regr)
  classif = checkLearner(classif)
  
  #regr
  id = stri_paste(output.type, "simplewrapper", getLearnerId(regr), sep = ".")
  packs = getLearnerPackages(regr)
  type = getLearnerType(regr)
  x.regr = makeHomogeneousEnsemble(id, type, regr, packs, learner.subclass = "MultioutputSimpleWrapper",
    model.subclass = "MultioutputSimpleModel")
  
  #classif
  id = stri_paste(output.type, "simplewrapper", getLearnerId(classif), sep = ".")
  packs = getLearnerPackages(classif)
  type = getLearnerType(classif)
  x.classif = makeHomogeneousEnsemble(id, type, classif, packs, learner.subclass = "MultioutputSimpleWrapper",
    model.subclass = "MultioutputSimpleModel")
  
  type = output.type
  id = stri_paste(output.type, "simplewrapper", sep = ".")
  x = makeMultiOutputLearnerBaseConstructor(id, type, univ.learners = list(regr = x.regr, classif = x.classif),
    learner.subclass = "MultioutputSimpleWrapper", model.subclass = "MultioutputSimpleModel")
  
  return(x)
}

#' @export
trainLearner.MultioutputSimpleWrapper = function(.learner, .task, .subset = NULL, .weights = NULL, ...) {
  .task = subsetTask(.task, subset = .subset)
  
  tt = .task$target.type
  classif.targets = names(tt)[tt == "factor"]
  regr.targets = names(tt)[tt == "numeric"]
  
  #train classif
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.classif = parallelMap(
    doMixedOutputSimpleClassifTrainIteration, tn = classif.targets,
    more.args = list(weights = .weights, learner = .learner$univ.learners$classif$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.classif) = classif.targets
  
  #train regr
  parallelLibrary("mlr", master = FALSE, level = "mlr.ensemble", show.info = FALSE)
  exportMlrOptions(level = "mlr.ensemble")
  models.regr = parallelMap(
    doMixedOutputSimpleRegrTrainIteration, tn = regr.targets,
    more.args = list(weights = .weights, learner = .learner$univ.learners$regr$next.learner, task = .task),
    level = "mlr.ensemble")
  names(models.regr) = regr.targets
  
  makeHomChainModel(.learner, models.regr)
}

doMixedOutputSimpleClassifTrainIteration = function(tn, learner, task, weights) {
  setSlaveOptions()
  data = getTaskData(task)
  task = makeClassifTask(id = tn, data = dropNamed(data, setdiff(getTaskTargetNames(task), tn)), target = tn)
  train(learner, task, weights = weights)
}

doMixedOutputSimpleRegrTrainIteration = function(tn, learner, task, weights) {
  setSlaveOptions()
  data = getTaskData(task)
  task = makeRegrTask(id = tn, data = dropNamed(data, setdiff(getTaskTargetNames(task), tn)), target = tn)
  train(learner, task, weights = weights)
}

#' @export
predictLearner.MultioutputSimpleWrapper = function(.learner, .model, .newdata, .subset = NULL, ...) {
  models = getLearnerModel(.model, more.unwrap = FALSE)
  f = function(m) getPredictionResponse(predict(m, newdata = .newdata, subset = .subset, ...))
  data.frame(lapply(models$next.model, f))
}


makeMultiOutputLearnerBaseConstructor = function(id, type, univ.learners, learner.subclass, model.subclass) {
  # if (length(par.vals) == 0L)
  #   names(par.vals) = character(0L)
  properties = unique(unlist(lapply(univ.learners, "getLearnerProperties")))
  learner = makeS3Obj(classes = c(learner.subclass, "Learner"),
    id = id,
    type = type,
    package = "mlr",
    properties = properties,
    univ.learners = univ.learners
    # par.set = par.set,
    # par.vals = par.vals,
    # predict.type = predict.type
  )
  learner$fix.factors.prediction = FALSE
  learner$model.subclass = model.subclass
  return(learner)
}


if (FALSE) {
  tn = getTaskTargetNames(wq.task)

  wq = farff::readARFF("data/wq.arff")
  wq$`37880` = as.factor(ifelse(wq$`37880` >= 3, "high", "low"))
  names(wq)[17:30] = paste0("T", names(wq)[17:30])
  wq.task = makeMixedOutputTask(data = wq, target = names(wq)[17:30])
  
  lrn = makeMultiOutputSimpleWrapper(output.type = "mixedoutput", regr = "regr.lm", classif = "classif.logreg")
  mod = train(lrn, wq.task)
  predict(mod, wq.task)
  
  .learner = lrn
  .task = wq
}