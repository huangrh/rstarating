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


#' Measure Table Clean Up.
#'
#' Clean
#'
#' @param x A data frame containing measure scores and their correponding
#'   denominators. Each column is a measure or denominator.
#' @param cutoff The cutoff.  The measure is removed if the number of hospital
#'   reported in the measure is less than the cutoff.
#' @return A list containing four data frames: \itemize{ \item msdtbl: measure
#'   score and denominator table. \item mstbl_std: winsorized and standized measure score
#'   table. \item  wtbl: weight table. \item report indicator: 1 means the star
#'   will be report for the corresponding hospital and 0 means no star is
#'   reported for the corresponding hospital. }
#'
#' @export
mstbl <- function (x,cutoff = 100) {

  # Clean up column names and the provide ID column
  names(x) <- tolower(names(x))
  if (!is.null(x$x)) x$x=NULL

  colnames(x) <- gsub("provider_id","ccnid",colnames(x))
  if (is.null(x$ccnid)) {
    stop("provider_id or ccnid is missing")
  } else {
    x$ccnid <- as.integer(gsub("^b'|'$", "", x$ccnid))
    }

  # ---------------------------------------------------------------------------
  # Combine the two columns OP_27 and IMM_3 because those two measures are same.
  # IMM-3/OP-27: Healthcare Personnel Influenza Vaccination
  # COMBINE IMM-3 and OP-27 AS ONE VARIABLE: IMM_3_OP_27
  x$imm_3_op_27 = ifelse(is.na(x$imm_3), x$op_27, x$imm_3)
  x[c("op_27","imm_3")] = NULL
  # Combine the corespongding denominator
  x$imm_3_op_27_den <- ifelse(is.na(x$imm_3_den),x$op_27_den,x$imm_3_den)
  x[c("op_27_den","imm_3_den")] = NULL

  # Remove measure which have less than 100 hospital reported
  mtbl <- create_measure_tbl(x)
  x[mtbl[mtbl$count <= cutoff,"name"]]=NULL

  # REMOVE HOSPITALS WHICH DO NOT HAVE ANY FINAL INCLUDED MEASURES
  mtbl <- create_measure_tbl(x)
  keep_hsp <- apply(x[mtbl$name], 1, function(x) !all(is.na(x)))
  x <- x[keep_hsp,]

  #--------------------------------------------------------------
  # calculate the denominator for the PtExp group
  mtbl <- create_measure_tbl(x)
  ptexp_den_names <- mtbl[mtbl$type %in% "denominator" &
                          mtbl$group %in% "patient_exp", "name"]

  ptexp_den <- x[,ptexp_den_names[1]] * x[,ptexp_den_names[2]] /100
  x[ptexp_den_names] <- NULL  #drop the column:"h_resp_rate_p" & "h_numb_comp"

  # create new column for the den in the ptExp group
  for (den in paste0(mtbl[mtbl$type  %in% "score" &
                          mtbl$group %in% "patient_exp","name"],"_den")) {
    x[,den] <- ptexp_den
  }

  #
  # assign NA to the measure score if the corresponding den is missing
  mtbl <- create_measure_tbl(x)

  for (col in mtbl[mtbl$type %in% "score","name"]) {
    den <- paste0(col,"_den")
    x[,col] <- ifelse(is.na(x[,den]),NA, x[,col])
  }

  # ------------------------------------------------------------------
  # Standardize the measure scores: Scale the column to mean = 0, std = 1
  #
  std_x  <- as.data.frame(apply(x[mtbl[mtbl$type %in% "score","name"]], 2, scale, center=TRUE,scale=TRUE))


  # ------------------------------------------------------------------
  # Re-direct measures: Flip measures which have negative directions
  # so all measures are in the same direction and a higher score means better
  # Ref SAS Pack #0 Lines 360-405
  #
  mtbl2 <- create_measure_tbl(std_x)
  flip_measures <- mtbl2[mtbl2$direction == -1, "name"]
  flip_measures <- flip_measures[flip_measures %in% measure(std_x)]

  std_x[flip_measures] <- -1 * std_x[flip_measures]

  # ------------------------------------------------------------------
  # Winsorization standardized measure scores at Z= -3 & Z=3
  std_x[] <- lapply(std_x, winsorize, min=-3, max =3)
  std_x   <- cbind.data.frame(x["ccnid"],std_x)

  # -----------------------------------------------------------------
  wts_x  <- create_weight(x)


  # ------------------------------------------------------------------
  # Reporting criteria set by CMS:
  # 1. Group criterion: at least THREE measues per group.
  # 2. Hospital report criterion:   at least THREE valid groups per hospital,
  #    with at least one Outcome group:
  #    (that is, Mortality, Safty of Care, Readmission).
  # indicator 1 means the hospital passed the report criteria
  report_indicator <- valid_report(x)
  report_indicator <- cbind.data.frame(x["ccnid"],report_indicator)

  # ------------------------------------------------------------------
  # output
  structure(list(msdtbl    = x,
                 mstbl_std = std_x,
                 wtbl      = wts_x,
                 report_indicator = report_indicator), class="mstbl")
}



#' Winsorization Function
#'
#' Winsorize a vector according to the cutoff parameters specified by min and
#' max.
#'
#' @param x A numeric vector to be winsorized. It should be scaled and centered
#'   to a normal distribution.
#' @param min The left side cutoff
#' @param max The right side cutoff
#' @return A winsorized vector
#' @examples
#' out <- winsorize(x = rnorm(1000), min=qnorm(0.05), max = qnorm(0.95))
#' hist(out,breaks=20)
#'
#' @export
#'
winsorize <- function (x,
                       min = qnorm(0.05), # 5% percentile
                       max = qnorm(0.95)) # 95% percentile
{
  x[ x < min ] <- min
  x[ x > max ] <- max
  x
}



#' Calculate the weights from the denominator
#'
#' Calculate the weights from the denominator
#'
#' The formula:
#' \deqn{w_{khd} = \frac{n_{khd}}{\sum_{i=1}^{N_{kd}}(n_{khd})} * N_{kd}}{w_khd = (n_khd)/(N_kd)}
#'  Where:
#'  \itemize{
#'  \item \eqn{w_{khd}}{w_khd}: Weight for measure \code{k}, hospital \code{h}, measure group \code{d}
#'  \item \eqn{n_{khd}}{n_khd}: Denominator for hospital \code{h}, measure \code{k} in measrue group \code{d}
#'  \item \eqn{N_{kd}}{N_kd}:  The total number of hospitals for measure \code{k} in measure group \code{d}
#'  }
#'
#'  \deqn{R^2 = 1 - \frac{estimated variance}{observed variance}}{R-
#' squared = 1 - (estimated variance)/(observed variance)}
#'
#' @param x A data frame containing the measure denominator of the hospital compare star rating.
#' @return A data frame containing the weighting factor which will used to perform lvm.
#' @examples:
#'
#'
#' @export
create_weight <- function(x = r_x) {
  # Create the denominator table
  mtbl    <- rstarating::create_measure_tbl(x)
  den_tbl <- x[names(x) %in% (mtbl[mtbl$type %in% "denominator", "name"])]

  # The function to calculate the weight from a column of dat
  weighting <- function(measure) {
    sum = sum(measure, na.rm=TRUE)
    len = length(na.omit(measure))
    (out <- measure / sum * len)
  }

  # calculate the weight
  den_tbl[] <- sapply(den_tbl, weighting, simplify=FALSE)

  # update the column name by appending with  "_wt"
  names(den_tbl) <- sub("_den$","_wt",names(den_tbl))
  # To exclude "h_resp_rate_p" and "h_numb_comp"
  den_tbl <- den_tbl[grepl("_wt$",names(den_tbl))]

  # add the provider id back
  (den_tbl <- cbind(x["ccnid"],den_tbl))
}
