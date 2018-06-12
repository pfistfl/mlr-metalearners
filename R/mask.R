# we create two very similar CPOs, so write out the arguments here and do.call them later.
cpoMask.callargs = alist(
  cpo.name = "select",
  par.set = makeParamSet(makeUntypedLearnerParam("keep", default = character(0)),
                         makeUntypedLearnerParam("mask", default = character(0))),
  dataformat = "df.features",
  export = FALSE,
  cpo.train = {
    assertCharacter(names, any.missing = FALSE, unique = TRUE)
      badnames = names[!names %in% names(data)]
      if (length(badnames)) {
        stopf("Column%s not found: %s", ifelse(length(badnames) > 1, "s", ""), collapse(badnames, sep = ", "))
      }
      if (length(mask) != 0) {
        index = setdiff(!match(mask, names(data)), index)
      } else {
        index = setdiff(match(keep, names(data)), index)
      }
      if (invert) {
        index = setdiff(seq_along(data), index)
      }
  },
  cpo.retrafo = {
    data[control]
  })

#' @title Mask All Columns Except Certain Selected Ones from Data
#'
#' @template cpo_doc_intro
#'
#' @description
#' Mask columns by name.
#'
#' \code{cpoMaskFreeProperties} behaves just as \code{cpoSelect}, with the additional function
#' that it is treated like a \code{\link{CPO}} that removes all data properties from the data.
#' This disables the internal property check and can be useful when trying to compose \code{\link{CPO}}s
#' that do not have compatible properties.
#'
#' @param keep [\code{character}]\cr
#'   Names of columns to to keep. Matching columns will be kept in order of their names occurring, but after
#'   the columns indicated in \dQuote{index}.
#' @param mask [\code{character}]\cr
#'   Names of columns to to mask. Matching columns will be dropped in order of their names occurring, but after
#'   the columns indicated in \dQuote{index}.
#'
#' @template cpo_doc_outro
#' @export
cpoMask = do.call(makeCPO, cpoMask.callargs)
registerCPO(cpoMask, "data", "feature selection ", "Mask features from a data set by name.")

cpoMask.callargs$cpo.name = "mask"
cpoMask.callargs$properties.adding = paste0(cpo.dataproperties, ".sometimes")
cpoMask.callargs$properties.needed = paste0(cpo.dataproperties, ".sometimes")
#' @rdname cpoSelect
#' @export
cpoMaskFreeProperties = do.call(makeCPO, cpoSelect.callargs)  # nolint
registerCPO(cpoMaskFreeProperties, "data", "feature selection ", "Mask features from a data set, also reset data properties.")