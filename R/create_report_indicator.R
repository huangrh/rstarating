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


#' Report Indicator Generator
#'
#' Create a report indicator for each hospital according to the reporting
#' criteria setted by CMS.
#'
#' Reporting criteria setted by CMS: 1. minimum measure threshold: 3 measues per
#' group 2. minimum group threshold:   3 group per hospital, with at least on
#' Outcome group (those are, Mortality, Safty of Care, Readmission)
#'
#' @param x A data frame with each measure in a column and each hospital in a
#'   row.
#' @return A vector of report indicators: 0 as not being reported, or 1 as
#'   getting reported.
#'
#' @export
valid_report <- function(x) {

  # Apply criteria #1
  mtbl <- create_measure_tbl(x)
  report_tbl <- as.data.frame(sapply(unique(mtbl$group), function(grp_name) {
    # take all the measure names in a measure group
    m_names <- mtbl[mtbl$group %in% grp_name & mtbl$type %in% "score","name"]

    # summarize the row and apply 1st criteria: 3 measures per group
    indicator <- rowSums(as.data.frame(!is.na(x[,m_names])),na.rm = TRUE) >=3
  }))

  # Apply criteria #2
  report_indicator <- apply(report_tbl, 1, function(row) {
    sum(row) >=3 & (row["outcome_safty"]  ||
                      row["outcome_mort"] ||
                      row["outcome_readm"])})
  (report_indicator <- as.integer(report_indicator))
}
