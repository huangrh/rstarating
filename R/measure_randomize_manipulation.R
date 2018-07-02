#' Measure Manipulation In The Input Data Frame For CMS Star Ratings.
#'
#' randomize the measure or set the denominator to one.
#'
#' @param dat A input data frame for CMS star ratings.
#' @param method A string to tell the manipulation method.
#' @param measures A list of measures. This is used by randomize method only.
#' @return A data frame for CMS Star Ratings.
#' @seealso \code{\link{measure_randomize}} for measure randomization,
#'   \code{\link{measure_den_one}} for the manipulation to set the denominator
#'   to one.
#'
#' @export
#'
measure_manipulate <- function(dat, method = c("randomize","den_one"),measures=NULL) {
  switch(
    method[1],
    randomize = measure_randomize(measures, dat),
    den_one   = measure_den_one(dat))
}

#' Measure Randomization
#'
#' Randomize one or more measures. This is for test only.
#'
#' @param x A list of measures.
#' @param dat A input data frame for cms star ratings.
#' @return A data frame for cms star ratings.
#' @examples
#' \dontrun{
#' # To randomize the non-null values in the specified measures in the input data frame.
#' measure_randomize(c("hai_1","hai_2"),dat=cms_star_rating_input_2017dec)
#' }
#'
#' @export
measure_randomize <- function(x,dat=cms_star_rating_input_2017dec) {
  #
  measures <- x[x %in% names(dat)]
  if (!identical(x, measures)) {
    message_1 <- paste0("The input measures, ",
                        toString(x[!(x %in% measures)]),
                        " is not in the input data frame.")
    warning(message_1)
  }

  # randomization
  for (measure in measures) {
    measure_den <- paste0(measure,"_den")
    na_idx <- is.na(dat[,measure])
    dat_idx <- seq_along(na_idx)

    # permute
    dat_idx[!na_idx]  <- sample(dat_idx[!na_idx])
    dat[,measure]     <- dat[dat_idx,measure]
    dat[,measure_den] <- dat[dat_idx,measure_den]
  }

  #output
  dat
}


#' Set The Demoninator To One.
#'
#' This function is for testing purpose for the latent variable model. Set the
#' demoninator to 1one is a way to remove the weigting from the latent variable
#' model.
#'
#' @param dat A input data frame for cms star ratings.
#' @return A data frame in which the denominators are set to 1.
#'
measure_den_one <- function(dat = cms_star_rating_input_2017dec) {
  mtl <- rstarating::create_measure_tbl(dat)
  for (idx in mtl[mtl$type=="denominator","name"]) {
    dat[idx] <- ifelse(is.na(dat[idx]),NA,1)
  }

  #
  dat
}
