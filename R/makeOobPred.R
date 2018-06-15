#' Make Out of Bag predictions
#' @param rdesc [`ResampleDesc`] | [`ResampleInstance`]\cr
#'   Out of Bag prediction strategy. One of "CV", "RepCV", "LOO". 
makeOobPred = function(task, learner, rdesc) {
  if (!(inherits(rdesc, "ResampleDesc") | inherits(rdesc, "ResampleInstance"))) {
    warning("no oob.strat provided, will return true target values")
    return(getTaskTargets(task))
  } else {
    # Get the rdesc
    if (inherits(rdesc, "ResampleInstance")) {
      rd = rdesc$desc
    } else {
      rd = rdesc
    }
    if (!inherits(rd, c("CVDesc", "RepCVDesc", "LOODesc")))
      stop("rdesc must be either 'CV', 'RepCV' or 'LOO'")
    res = resample(learner, task, resampling = rdesc)
    if (inherits(rd, c("CVDesc", "LOODesc")))
      return(res$pred$data[order(res$pred$data$id), ]$response)
    if (inherits(rd, c("RepCVDesc"))) 
      return(res$pred$data[order(res$pred$data$id), ] %>%
        group_by(id) %>% 
        summarize(mresponse = mean(response)) %>%
        pull(mresponse)
      )
  }
}
