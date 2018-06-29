# Copyright (C) 2016-2018 Ren-Huai Huang <huangrenhuai@gmail.com>
#
# This file is part of rstarating.
#
# rstarating is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rstarating is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rstarating.  If not, see <http://www.gnu.org/licenses/>.


#' Calculation Of Summary Score From Group Score
#'
#' A function to calculate the summary score from group score. The weigt of a
#' group score is redistributed into other group if the group is missed.
#'
#' @param x A data frame containing seven group scores.
#' @param wts A vector containing the arbitrary weighting corresponding to input
#'    data frame.
#' @return Summary scores corresponding to each row from the input data frame
#'
#'
#' @export
sum_score <- function(x,wts=NULL) {
  # initialize the weight.
  if (is.null(wts)) { wts <- c(outcome_mort  = 0.22,
                               outcome_safty = 0.22,
                               outcome_readm = 0.22,
                               patient_exp   = 0.22,
                               care_effe     = 0.04,
                               process_time  = 0.04,
                               effi_image    = 0.04)}
  ccnid <- x[,1]
  x <- as.data.frame(x)

  # Test if the name of wts matches the column name.
  if (all(names(wts) %in% colnames(x))) {
    # Subset the group score only.
    x <- x[names(wts)]

    # calculate summary score for each row.
    sum_score <- apply(x,1,function(row) {
      # redistribute the weight
      wt_row <- wts * as.numeric(!is.na(row))
      wt_row <- wt_row/sum(wt_row)
      ifelse(all(is.na(row)),NA,sum(row * wt_row,na.rm=TRUE))
    })

    # Winsorize the summary score.
    sum_score_win <- winsorize(sum_score,
                               min=max(sum_score[sum_score<quantile(sum_score,0.005)]),
                               max=min(sum_score[sum_score>quantile(sum_score,0.995)]))
  } else {
    sum_score     <- NA
    sum_score_win <- NA
  }

  # out
  cbind.data.frame(ccnid,sum_score,sum_score_win)
}



#' Redistribute weights when there is missing group
#'
#' A function to recalculate weights. the corresponding weigt is redistributed
#' into other group if the group is completely missed.
#'
#' @param grp The data to be weighted. For example, the group score in the cms
#'   SAS Pack star rating.
#' @param wt A vector of weights. Refer to line 95-102 in Sas pack #2 k-mean
#'   clustering.
#'
#' @return The weight redistributed. the weight set to zero when the
#'   corresponding term is missing and the corresponding weigt is redistributed
#'   to other term.
#'
#' @examples
#' reweight(grp=c(1,1,1,1,1,1,NA))
#'
#' @export
reweight <- function(grp=rep(1,7), wt=NULL){
  # initialize the weight
  if (is.null(wt)) {
    wt <- c(outcome_mort  = 0.22,
            outcome_safty = 0.22,
            outcome_readm = 0.22,
            patient_exp   = 0.22,
            care_effe     = 0.04,
            process_time  = 0.04,
            effi_image    = 0.04)
  }

  # make sure the length is equal
  if (length(grp) != length(wt))
    stop("Make sure the length of wt is same as the length of data")

  # redistribute the weight
  wt <- wt * as.numeric(!is.na(grp))
  (out <- wt/sum(wt))
}
