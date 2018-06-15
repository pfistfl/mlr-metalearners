addFeature = function(task, feature, name = NULL) {
  assertVector(feature)
  checkTask(task)
  if (is.null(name)) stop("please provide name for feature")
  fname = getTaskFeatureNames(task)
  if (name %in% fname) stop("feature name is already in the dataset, please provide unique feature name")
  
  tdata = getTaskData(task)
  eval(parse(text = paste0("tdata = data.frame(tdata, ", name,  " = feature)")))
  
  type = getTaskType(task)
  id = paste(getTaskId(task), paste0("add.", name), sep = ".")
  task2 = constructTask(data = tdata, target = getTaskTargetNames(task), type = type, id = id)
  task2$add.feature = name
  class(task2) = c("addFeatureTask", class(task2))
  task2
}


constructTask = function(data, target, type, id) {
  constructor = switch(type,
    classif = makeClassifTask,
    multilabel = makeMultilabelTask,
    multiregr = makeMultiRegrTask,
    mixedoutput = makeMixedOutputTask,
    regr = makeRegrTask,
    surv = makeSurvTask)
  constructor(id = id, data = data, target = target, fixup.data = "no", check.data = FALSE)
}

print.addFeatureTask = function(x, ...) {
  add.feature = x$add.feature
  print.SupervisedTask(x)
  cat(paste0("additional Features: ", add.feature))
}
