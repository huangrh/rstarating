# Copyright (C) 2016-2017 Ren-Huai Huang <huangrenhuai@gmail.com>
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


#' Prepare the subset of the data in a measure group for CMS star rating
#'
#' Prepare the subset of the data table and weight table in each measure group
#' in CMS Hospital Compare star rating. In addition, the hospital is removed if
#' the hospital didn't report any data in the group.
#'
#' @param object A mstbl object,which contains the measure score table and
#'   weight table.
#' @param groups A vector of group names. The default is null and it will
#'   automatically use all of the groups in the object.
#' @return A list containing a sublist for each measure group. Each has a
#'   stadardized measure score table and a weight table.
#'
#' @export
subgroup <- function(object,groups=NULL) {
  alldf  <- merge(x=object$mstbl_std,y=object$wtbl,all=TRUE)

  # all
  mtbl   <- create_measure_tbl(alldf)
  all_groups <- unique(mtbl$group)

  if (is.null(groups)) {
    groups <- all_groups
  } else if (any(groups %in% all_groups)) {
    groups <- groups[groups %in% all_groups]
  } else stop("The group name do not match.")


  alldata<- sapply(groups, grouping, df=alldf, simplify = FALSE)
  alldata
}


#
grouping <- function(group,df=alldf) {

  # Measure table
  mtbl <- create_measure_tbl(df)

  # data table
  measure_name<- mtbl[(mtbl$group %in% group) & (mtbl$type %in% "score"),"name"]
  dat_tbl     <- df[, names(df) %in% c("provider_id",measure_name)]


  # weight table
  wt_name <- sapply(measure_name, function(x) paste0(x,"_wt"))
  wt_tbl  <- df[, names(df) %in% c("provider_id",wt_name)]

  # remove the na rows
  na_row_idx<- apply(is.na(dat_tbl[-1]),1,all)
  dat_tbl   <- dat_tbl[!(na_row_idx),];
  wt_tbl    <- wt_tbl[!(na_row_idx),];


  (out <- list(mstbl = dat_tbl, wtbl = wt_tbl))
}


