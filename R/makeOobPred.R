makeOobPred = function(task, learner, oob.strat = NULL, ...) {
  if (is.null(oob.strat)) {
    warning("no oob.strat provided, will return true target values")
    return(getTaskTargets(task))
  } else {
    if (!oob.strat %in% c("CV", "RepCV", "LOO")) stop("oob.strat must be element of 'CV', 'RepCV', 'LOO'")
  rdesc = makeResampleDesc(oob.strat, ...)
  res = resample(learner, task, resampling = rdesc)
  if (oob.strat %in% c("CV", "LOO")) return(res$pred$data[order(res$pred$data$id), ]$response)
  if (oob.strat == "RepCV") return(res$pred$data[order(res$pred$data$id), ] %>% group_by(id) %>% 
    summarize(mresponse = mean(response)) %>% pull(mresponse))
  }
}
