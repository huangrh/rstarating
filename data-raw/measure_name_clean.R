# measure copied from Sas pack program 0
measure_all <-
  'AMI_7A CAC_3 COMP_HIP_KNEE ED_1B ED_2B HAI_1 HAI_2 HAI_3 HAI_4 HAI_5
HAI_6 H_CLEAN_HSP_LINEAR H_COMP_1_LINEAR H_COMP_2_LINEAR H_COMP_3_LINEAR
H_COMP_4_LINEAR H_COMP_5_LINEAR H_COMP_6_LINEAR H_COMP_7_LINEAR     H_HSP_RATING_LINEAR
H_QUIET_HSP_LINEAR H_RECMND_LINEAR IMM_2 IMM_3_OP_27 MORT_30_AMI
MORT_30_CABG MORT_30_COPD MORT_30_HF MORT_30_PN MORT_30_STK
OP_1 OP_2 OP_3B OP_4 OP_5 OP_8 OP_10 OP_11 OP_13 OP_14
OP_18B OP_20 OP_21 OP_22 OP_23 OP_29 OP_30 PC_01 PSI_4_SURG_COMP PSI_90_SAFETY   READM_30_AMI READM_30_CABG
READM_30_COPD READM_30_HF READM_30_HIP_KNEE READM_30_HOSP_WIDE READM_30_PN
READM_30_STK STK_1 STK_4 STK_6 STK_8
VTE_1 VTE_2 VTE_3 VTE_5 VTE_6'

# clean up
# This produces 67 measure names
strings <- strsplit(measure_all, split=" ")[[1]]
table(index <- !sapply(strings, function(string) string == ""))
strings <- strings[index]
strings <- sapply(strings, function(x) gsub("\n", "", x))
measure_name_67 <- tolower(paste(strings, sep=" "))
