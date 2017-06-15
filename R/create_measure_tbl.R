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


#' Measure Name Filter
#'
#' Output a vector of measure name from a measure score table according to the
#' type and the measure group.
#'
#' @param x A measure score table.
#' @param type "id","score", "denominator", or "wt".
#' @param groups A vector of measure group.
#' @param cutoff A integer. The measure is removed if there is less than the
#'   cutoff number of hospital reported.
#' @return A vector of measure names.
#'
#' @aliases measure
#'
#' @export cols measure
cols <- measure <- function(x=sasdata, type=c("score","denominator","wt","id"),groups=NULL,cutoff = 1L) {
  mtbl <- create_measure_tbl(x)
  if (is.null(groups)) groups <- unique(mtbl$group)
  (out <- mtbl[mtbl$type %in% type & mtbl$group %in% groups & mtbl$count >=cutoff,"name"])
}


#' Measure Table Creator
#'
#' Create a measure table to hold the measure information for the column of the
#' sas input data data
#'
#' @param data_tbl The sas data input file.
#'
#' @return A measure table with name of the measure, type (weight, denominator or
#'   score), the assigned group, and hospital counts, etc.
#'
#' @export
#'
create_measure_tbl <- function(data_tbl = sas_data) {

  # hospital counts for each measure in the data_tbl
  hsp_counts <- sapply(data_tbl, function(col){
    len <- length(na.omit(col))
  })

  #  The type of measure: score, denominator, hospital_id
  #  2 denominators for measuers in patient experience measure group
  #  reference to sas pack program 0 @ line 44--45
  col_den <- grepl("_den|-den|h_numb_comp|h_resp_rate_p", tolower(names(data_tbl)))
  col_wt  <- grepl("_wt|-wt",  tolower(names(data_tbl)))

  # create the measure table with hospital report count
  measure_tbl <- data.frame(name = names(hsp_counts),
                            type = ifelse(col_den, "denominator",
                                          ifelse(col_wt,"wt","score")),
                            count = hsp_counts, row.names = NULL,
                            stringsAsFactors = FALSE)

  # Assign each measure into a group.
  measure_tbl  <- rstarating:::assign_group(measure_tbl)
  (measure_tbl <-na.omit(measure_tbl))
}


#' Measure Assignment Into Measure Group
#'
#' Assign the measure into the seven measure group according to SAS Pack
#'
#' @param measure_tbl A measure
#' @return The measure_tbl with assignment of a group
#'
assign_group <- function(measure_tbl = measure_tbl){
  # init the measure group with NA
  measure_tbl$group <-  NA

  #######################################################
  # 1 outcome-mortality  measure group (outcome_mort)
  # ref: SAS Pack Program #0, line 148
  # ALL 7 Mortality measures in 2016 JULY
  grp <- c('MORT_30_AMI',
           'MORT_30_CABG',
           'MORT_30_COPD',
           'MORT_30_HF',
           'MORT_30_PN',
           'MORT_30_STK',
           'PSI_4_SURG_COMP')

  # Also include the denominator in the group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "outcome_mort",
                              measure_tbl$group)

  #########################################################
  # 2 outcome-safty measure group           (outcome_safty)
  # ref: SAS Pack #0, line 177
  # all 8 safety measures in 2016 July
  grp <- c('COMP_HIP_KNEE',
           'HAI_1',
           'HAI_2',
           'HAI_3',
           'HAI_4',
           'HAI_5',
           'HAI_6',
           'PSI_90_SAFETY')

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "outcome_safty",
                              measure_tbl$group)

  #########################################################
  # 3 outcome-readmission measure group     (outcome_readm)
  # ref: SAS Pack #0 line 204
  # ALL 8 READMISSION measures in 2016 JULY
  grp <- c('READM_30_AMI',
           'READM_30_CABG',
           'READM_30_COPD',
           'READM_30_HF',
           'READM_30_HIP_KNEE',
           'READM_30_HOSP_WIDE',
           'READM_30_PN',
           'READM_30_STK')

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "outcome_readm",
                              measure_tbl$group)

  #######################################################
  # 4 patient experience measure group      (patient_exp)
  # ref: SAS Pack #0, line 232
  # ALL 11 PATIENT EXPERIENCE measures in 2016 JULY
  # denominator for this group is special
  # ref to SAS Pack #0, line 45
  grp <- tolower(c("H_NUMB_COMP", "H_RESP_RATE_P"))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "patient_exp",
                              measure_tbl$group)
  #
  grp <- c('H_COMP_1_LINEAR',
           'H_COMP_2_LINEAR',
           'H_COMP_3_LINEAR',
           'H_COMP_4_LINEAR',
           'H_COMP_5_LINEAR',
           'H_COMP_6_LINEAR',
           'H_COMP_7_LINEAR',
           'H_CLEAN_HSP_LINEAR',
           'H_QUIET_HSP_LINEAR',
           'H_HSP_RATING_LINEAR',
           'H_RECMND_LINEAR')
  grp <- c(grp,gsub("_LINEAR","",grp))

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "patient_exp",
                              measure_tbl$group)

  ################################################
  # 5 efficiency group              (effi_image)
  # ref line 255
  # ALL 5 EFFICIENCY measures in 2016 JULY
  # July 2016 Efficient Use of Medical Imagery Measures
  grp <- c('OP_8',
           'OP_10',
           'OP_11',
           'OP_13',
           'OP_14')

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))


  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "effi_image",
                              measure_tbl$group)

  ########################################################
  # 6 process timeliness measures           (process_time)
  # ref: SAS Pack #9 Line 285
  # ALL 10 (actually 9)  PROCESS TIMELINESS measures in 2016 JULY
  grp <- c('ED_1B', 'ED_2B', 'OP_1', 'OP_2', 'OP_3B',
           'OP_5', 'OP_18B', 'OP_20', 'OP_21')

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))


  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "process_time",
                              measure_tbl$group)

  ############################################
  # 7 Precess effectiveness measures group  (care_effe)
  # ref: SAS Pack #0, line 315
  # ALL 19 Process Effectiveness measures in 2016 JULY
  grp <- c('AMI_7A', 'CAC_3', 'IMM_2', 'IMM_3_OP_27', 'IMM_3','OP_27','OP_4',
            'OP_22', 'OP_23',  'OP_29', 'OP_30','PC_01',
            'STK_1', 'STK_4', 'STK_6', 'STK_8', 'VTE_1',
            'VTE_2', 'VTE_3', 'VTE_5', 'VTE_6')

  # Also assign the denominator group
  grp <- tolower(c(grp,paste0("std_",grp),paste0(grp,"_den"),paste0(grp,"_wt")))

  measure_tbl$group <- ifelse(gsub("-|/|:","_",tolower(measure_tbl$name)) %in% grp,
                              "care_effe",
                              measure_tbl$group)
  (measure_tbl)
}



