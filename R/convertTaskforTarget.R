#converts a mlr multioutput task to a univariate task fitting the target
convertTaskForTarget = function(task, target, type = NULL) {
  if (!target %in% getTaskTargetNames(task)) stop(paste0("target must be one of ", paste(getTaskTargetNames(task), collapse = ", ")))
  if (is.null(type)) type = switch(task$target.type[[target]], "numeric" = "regr", "factor" = "classif")
  if (!type %in% c("classif", "regr")) stop("type can only be 'classif' or 'regr'")
  fname = getTaskFeatureNames(task)
  tdata = getTaskData(task)[, c(fname, target)]
  id = getTaskId(task)
  
  if (type == "classif") return(makeClassifTask(id = paste(id, target, sep = "."), data = tdata, target = target))
  if (type == "regr") return(makeRegrTask(id = paste(id, target, sep = "."), data = tdata, target = target))
}
