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
                            count = hsp_counts,
							group     = NA,
							direction = NA,
							row.names = NULL,
                            stringsAsFactors = FALSE)

  # =============================================================================
  # Add measure direction
  add_measure_direction <- function(direction, measures, x=measure_tbl){
    measures <- c(paste0("^",measures,"$"),gsub("$","_",measures))
    for (measure in measures) {
      x$direction <- ifelse(grepl(measure, gsub("-|/| ","_",x$name),ignore.case = TRUE),
                            direction,x$direction)
    }

    # output
    x
  }
  # Add negative measure direction
  measure_tbl <- add_measure_direction(
    direction = -1,
    measures = c(
      # 1----- outcome_safty (all)---------------
      "hai_1","hai_2","hai_3", "hai_4","hai_5","hai_6","psi_90_safety","comp_hip_knee",
      # 2----- outcome_readm (all)---------------
      "readm_30_ami" , "readm_30_cabg", "readm_30_copd", "readm_30_hf", "readm_30_hip_knee",
      "readm_30_hosp_wide", "readm_30_pn", "readm_30_stk","edac_30_ami", "edac_30_hf",  "op_32",
      # 3----- outcome_mort (all)----------------
      "mort_30_ami", "mort_30_cabg", "mort_30_copd","mort_30_hf", "mort_30_pn",
      "mort_30_stk","psi_4_surg_comp",
      # 4----- process_time ----------------
      "ed_1b", "ed_2b","op_3b", "op_5","op_18b", "op_20","op_21",
      # 5----- effi_image ------------------
      "op_8",   "op_10", "op_11", "op_13", "op_14",
      # 6----- care_effe -------------------
      "pc_01", "vte_6","op_22"))

  # add positive measure direction
  measure_tbl <- add_measure_direction(
    direction = 1,
    measures = c("cac_3", "stk_1", "stk_4", "stk_6", "stk_8", "vte_1", "vte_2",
                 "vte_3", "vte_5",
                 'op_1', 'op_2', 'op_4', 'op_23', 'op_29', 'op_30', 'imm_2', 'op_33',
                 'imm_3', 'op_27', 'h_comp_1', 'h_comp_2', 'h_comp_3',
                 'h_comp_4', 'h_comp_5', 'h_comp_6', 'h_comp_7',
                 'h_clean_hsp', 'h_quiet_hsp', 'h_hsp_rating', 'h_recmnd',
                 'h_resp_rate_p','h_numb_comp'))

  # Assign each measure into a group.
  measure_tbl  <- rstarating:::assign_group(measure_tbl)
  measure_tbl  <- measure_tbl[!is.na(measure_tbl$group),]
  rownames(measure_tbl) <- NULL
  #
  measure_tbl
}


#' Measure Group Assignment
#'
#' Assign the measure into the seven measure group according to SAS Pack
#'
#' @param measure_tbl A measure
#' @return The measure_tbl with assignment of a group
#'
assign_group <- function(measure_tbl = measure_tbl){
  # init the measure group with NA
  measure_tbl$group <-  NA

  #
  assign_group_1 <- function(group_name,group_measures=group_measures,x=measure_tbl) {
    group_measures <- c(gsub("$","$",group_measures),gsub("$","_",group_measures))
    for (measure in group_measures) {
      x$group <- ifelse(grepl(measure, gsub("-|/| ","_",x$name),ignore.case = TRUE),
                        group_name,x$group)
    }
    x
  }

  #######################################################
  # 1 outcome-mortality  measure group (outcome_mort)
  # ref: SAS Pack Program #0, line 148
  # ALL 7 Mortality measures in 2016 JULY
  measure_tbl <- assign_group_1(group_name="outcome_mort",
                                group_measures = c('MORT_30_AMI',
                                                   'MORT_30_CABG',
                                                   'MORT_30_COPD',
                                                   'MORT_30_HF',
                                                   'MORT_30_PN',
                                                   'MORT_30_STK',
                                                   'PSI_4_SURG_COMP'))

  #########################################################
  # 2 outcome-safty measure group           (outcome_safty)
  # ref: SAS Pack #0, line 177
  # all 8 safety measures in 2016 July
  measure_tbl <- assign_group_1(group_name="outcome_safty",
                                group_measures <- c('COMP_HIP_KNEE',
                                                    'HAI_1',
                                                    'HAI_2',
                                                    'HAI_3',
                                                    'HAI_4',
                                                    'HAI_5',
                                                    'HAI_6',
                                                    'PSI_90_SAFETY'))

  #########################################################
  # 3 outcome-readmission measure group     (outcome_readm)
  # ref: SAS Pack #0 line 204
  # ALL 8 READMISSION measures in 2016 JULY
  measure_tbl <- assign_group_1(
    group_name="outcome_readm",
    group_measures = c('READM_30_AMI',
                       'READM_30_CABG',
                       'READM_30_COPD',
                       'READM_30_HF',
                       'READM_30_HIP_KNEE',
                       'READM_30_HOSP_WIDE',
                       'READM_30_PN',
                       'READM_30_STK',
                       # Following 3 measures were added in Dec. 2017.
                       'EDAC_30_AMI','EDAC_30_HF','OP_32'))

  #######################################################
  # 4 patient experience measure group      (patient_exp)
  # ref: SAS Pack #0, line 232
  # ALL 11 PATIENT EXPERIENCE measures in 2016 JULY
  measure_tbl <- assign_group_1(
    group_name = "patient_exp",
    group_measures =
      c('H_COMP_1',
        'H_COMP_2',
        'H_COMP_3',
        'H_COMP_4',
        'H_COMP_5',
        'H_COMP_6',
        'H_COMP_7',
        'H_CLEAN_HSP',
        'H_QUIET_HSP',
        'H_HSP_RATING',
        'H_RECMND',
        #Following 2 measures are the ptExp denominator,ref to SAS Pack #0, line 45
        "H_NUMB_COMP",
        "H_RESP_RATE_P"))



  ################################################
  # 5 efficiency group              (effi_image)
  # ref line 255
  # ALL 5 EFFICIENCY measures in 2016 JULY
  # July 2016 Efficient Use of Medical Imagery Measures
  measure_tbl <- assign_group_1(
    group_name="effi_image",
    group_measures = c('OP_8',
                       'OP_10',
                       'OP_11',
                       'OP_13',
                       'OP_14'))

  ########################################################
  # 6 process timeliness measures           (process_time)
  # ref: SAS Pack #9 Line 285
  # ALL 10 (actually 9)  PROCESS TIMELINESS measures in 2016 JULY
  measure_tbl <- assign_group_1(
    group_name="process_time",
    group_measures = c('ED_1B',
                       'ED_2B',
                       'OP_1',
                       'OP_2',
                       'OP_3B',
                       'OP_5',
                       'OP_18B', 'OP_20', 'OP_21'))

  ############################################
  # 7 Precess effectiveness measures group  (care_effe)
  # ref: SAS Pack #0, line 315
  # ALL 19 Process Effectiveness measures in 2016 JULY
  measure_tbl <- assign_group_1(
    group_name = "care_effe",
    group_measures=c('AMI_7A', 'CAC_3', 'IMM_2', 'IMM_3_OP_27', 'IMM_3','OP_27', 'OP_4',
                     'OP_22', 'OP_23',  'OP_29', 'OP_30','PC_01',
                     'STK_1', 'STK_4', 'STK_6', 'STK_8', 'VTE_1',
                     'VTE_2', 'VTE_3', 'VTE_5', 'VTE_6','OP_33'))

  # Output
  measure_tbl
}



