# #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
#
#                        Copyright (c) 2020
#            Marcel Ribeiro-Dantas <marcel.ribeiro-dantas@curie.fr>
#
# This script is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

suppressMessages(library(readr))
suppressMessages(library(dplyr))

# Reading SEER dataset ----------------------------------------------------

# First half, default character
# which(colnames(dataset) == 'Year of diagnosis')
# 4
# which(colnames(dataset) == 'Survival months')
# 149
# which(colnames(dataset) == 'Age at diagnosis')
# 168
# which(colnames(dataset) == 'Year of birth')
# 172
# Second half, default integer
# which(colnames(dataset) == 'SEER registry')
# 5
# which(colnames(dataset) == 'Sequence number')
# 154
# which(colnames(dataset) == 'Patient ID')
# 205
# which(colnames(dataset) == 'Rural-Urban Continuum Code 2013')
# 384
# which(colnames(dataset) == 'Health Service Area (NCI Modified)')
# 424
# which(colnames(dataset) == 'Health Service Area')
# 425
# which(colnames(dataset) == 'CHSDA 2012')
# 426
# which(colnames(dataset) == 'CHSDA 2006')
# 427
# which(colnames(dataset) == 'Appalachia Region (ARC 2008 Revision)')
# 429
# which(colnames(dataset) == 'Appalachia (ARC 2007)')
# 430
# which(colnames(dataset) == 'Age Standard for Survival (15-44,45-54,55-64,65-74,75+)')
# 431

print('Reading SEER dataset...')
dataset <- readr::read_delim(file = 'data/raw/SEER_BC_single_file.tsv',
                             delim = '\t',
                             col_types = paste(c(rep('c', 3), 'i',
                                                 rep('c', 144), 'i',
                                                 rep('c', 18), 'i',
                                                 rep('c', 3), 'i',
                                                 rep('c', 36),
                                                 # Here second half starts
                                                 rep('i', 175), 'c',
                                                 rep('i', 39), rep('c', 4),
                                                 'i', rep('c', 3)),
                                               collapse = '')
                             )

# Post-reading NA replacement ####
# Some values mean NA for some variables, but do not for others. Therefore, they
# can only be replaced as NA after reading the file. Otherwise, it would replace
# values in some variables that are not NA for such variables.
dataset <- dataset %>%
  # Insurance
  mutate(`Insurance Recode (2007+)` =
           ifelse(`Insurance Recode (2007+)` == 'Insurance status unknown',
                  NA,
                  `Insurance Recode (2007+)`)) %>%
  # Radiation recode
  # The reason the same is not done to Chemotherapy is that if we did so,
  # there would have only one value apart from NA and there would be no
  # information (entropy = 0)
  mutate(`Radiation recode` =
           ifelse(`Radiation recode` == 'None/Unknown',
                  NA,
                  `Radiation recode`)) %>%
  # Regional nodes positive (1988+)
  mutate(`Regional nodes positive (1988+)` =
           ifelse(`Regional nodes positive (1988+)` == 99,
                  NA,
                  `Regional nodes positive (1988+)`)) %>%
  # Regional nodes examined  (1988+)
  mutate(`Regional nodes examined (1988+)` =
           ifelse(`Regional nodes examined (1988+)` == 99,
                  NA,
                  `Regional nodes examined (1988+)`)) %>%
  # CS tumor size (2004-2015)
  mutate(`CS tumor size (2004-2015)` =
           ifelse(`CS tumor size (2004-2015)` %in% 999,
                  NA,
                  `CS tumor size (2004-2015)`)) %>%
  # CS extension (2004-2015)
  mutate(`CS extension (2004-2015)` =
           ifelse(`CS extension (2004-2015)` == 999,
                  NA,
                  `CS extension (2004-2015)`)) %>%
  # CS lymph nodes (2004-2015)
  mutate(`CS lymph nodes (2004-2015)` =
           ifelse(`CS lymph nodes (2004-2015)` == 999,
                  NA,
                  `CS lymph nodes (2004-2015)`)) %>%
  # CS site-specific factor 1 (2004+ varying by schema)
  mutate(`CS site-specific factor 1 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 1 (2004+ varying by schema)` == 999,
                  NA,
                  `CS site-specific factor 1 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 2 (2004+ varying by schema)
  mutate(`CS site-specific factor 2 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 2 (2004+ varying by schema)` == 999,
                  NA,
                  `CS site-specific factor 2 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 3 (2004+ varying by schema)
  mutate(`CS site-specific factor 3 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 3 (2004+ varying by schema)` %in%
                    c('099', '988'),
                  NA,
                  `CS site-specific factor 3 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 4 (2004+ varying by schema)
  mutate(`CS site-specific factor 4 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 4 (2004+ varying by schema)` == 987,
                  NA,
                  `CS site-specific factor 4 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 5 (2004+ varying by schema)
  mutate(`CS site-specific factor 5 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 5 (2004+ varying by schema)` == 987,
                  NA,
                  `CS site-specific factor 5 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 6 (2004+ varying by schema)
  mutate(`CS site-specific factor 6 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 6 (2004+ varying by schema)` == 987,
                  NA,
                  `CS site-specific factor 6 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 7 (2004+ varying by schema)
  mutate(`CS site-specific factor 7 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 7 (2004+ varying by schema)` == 999,
                  NA,
                  `CS site-specific factor 7 (2004+ varying by schema)`)) %>%
  # CS site-specific factor 15 (2004+ varying by schema)
  mutate(`CS site-specific factor 15 (2004+ varying by schema)` =
           ifelse(`CS site-specific factor 15 (2004+ varying by schema)` == 999,
                  NA,
                  `CS site-specific factor 15 (2004+ varying by schema)`)) %>%
  # RX Summ--Surg Prim Site (1998+)
  mutate(`RX Summ--Surg Prim Site (1998+)` =
           ifelse(`RX Summ--Surg Prim Site (1998+)` == 99,
                  NA,
                  `RX Summ--Surg Prim Site (1998+)`)) %>%
  mutate(`CS Tumor Size/Ext Eval (2004-2015)` =
           ifelse(`CS Tumor Size/Ext Eval (2004-2015)` == 9,
                  NA,
                  `CS Tumor Size/Ext Eval (2004-2015)`)) %>%
  mutate(`CS Reg Node Eval (2004-2015)` =
           ifelse(`CS Reg Node Eval (2004-2015)` == 9,
                  NA,
                  `CS Reg Node Eval (2004-2015)`)) %>%
  mutate(`CS mets at dx (2004-2015)` =
           ifelse(`CS mets at dx (2004-2015)` == 99,
                  NA,
                  `CS mets at dx (2004-2015)`)) %>%
  mutate(`Derived SEER Cmb Stg Grp (2016+)` =
           ifelse(`Derived SEER Cmb Stg Grp (2016+)` == 99,
                  NA,
                  `Derived SEER Cmb Stg Grp (2016+)`)) %>%
  mutate(`Breast - Adjusted AJCC 6th M (1988-2015)` =
           ifelse(`Breast - Adjusted AJCC 6th M (1988-2015)` == 'MX',
                  NA,
                  `Breast - Adjusted AJCC 6th M (1988-2015)`)) %>%
  mutate(`Breast - Adjusted AJCC 6th N (1988-2015)` =
           ifelse(`Breast - Adjusted AJCC 6th N (1988-2015)` == 'NX Adjusted',
                  NA,
                  `Breast - Adjusted AJCC 6th N (1988-2015)`)) %>%
  mutate(`Breast - Adjusted AJCC 6th T (1988-2015)` =
           ifelse(`Breast - Adjusted AJCC 6th T (1988-2015)` == 'TX Adjusted',
                  NA,
                  `Breast - Adjusted AJCC 6th T (1988-2015)`)) %>%
  mutate(`Derived AJCC T, 7th ed (2010-2015)` =
           ifelse(`Derived AJCC T, 7th ed (2010-2015)` == 'TX',
                  NA,
                  `Derived AJCC T, 7th ed (2010-2015)`)) %>%
  mutate(`Derived AJCC N, 7th ed (2010-2015)` =
           ifelse(`Derived AJCC N, 7th ed (2010-2015)` == 'NX',
                  NA,
                  `Derived AJCC N, 7th ed (2010-2015)`)) %>%
  mutate(`Derived AJCC T, 6th ed (2004-2015)` =
           ifelse(`Derived AJCC T, 6th ed (2004-2015)` == 'TX',
                  NA,
                  `Derived AJCC T, 6th ed (2004-2015)`)) %>%
  mutate(`Derived AJCC N, 6th ed (2004-2015)` =
           ifelse(`Derived AJCC N, 6th ed (2004-2015)` == 'NX',
                  NA,
                  `Derived AJCC N, 6th ed (2004-2015)`)) %>%
  mutate(`Derived AJCC M, 6th ed (2004-2015)` =
           ifelse(`Derived AJCC M, 6th ed (2004-2015)` == 'MX',
                  NA,
                  `Derived AJCC M, 6th ed (2004-2015)`)) %>%
  mutate(`Race/ethnicity` =
           ifelse(`Race/ethnicity` == '99',
                  NA,
                  `Race/ethnicity`)) %>%
  mutate(`Months since last birthday` =
           ifelse(`Months since last birthday` == 'Unknown number of months',
                  NA,
                  `Months since last birthday`)) %>%
  mutate(`SEER cause-specific death classification` =
           ifelse(`SEER cause-specific death classification` == 'N/A not first tumor',
                  NA,
                  `SEER cause-specific death classification`)) %>%
  mutate(`SEER other cause of death classification` =
           ifelse(`SEER other cause of death classification` == 'N/A not first tumor',
                  NA,
                  `SEER other cause of death classification`)) %>%
  mutate(`CS Mets Eval (2004-2015)` =
           ifelse(`CS Mets Eval (2004-2015)` == 9,
                  NA,
                  `CS Mets Eval (2004-2015)`))

# Removing variables with a single value ####
# This can happen because of variables that only make sense for other types of
# cancer, for example.
dataset <- dataset %>%
  select_if(~ length(unique(na.omit(.))) > 1)

# Adding variable to identify patients with more than 1 primary ####
dataset %>%
  group_by(`Patient ID`, `SEER registry`) %>%
  mutate('More Than One Primary' = ifelse(n()>1,
                                                                   TRUE,
                                                                   FALSE)) %>%
  ungroup() -> dataset
# Keep the oldest (first) tumor in SEER (not always the 1st of that person, but
# the 1st case of breast cancer in SEER.
# dataset %>%
 # group_by(`Patient ID`, `SEER registry`) %>%
 # filter(row_number() == 1) %>%
 # ungroup() -> dataset

# Removing Patient ID and Registry ID ####
dataset$`Patient ID` <- NULL
dataset$`SEER registry` <- NULL

# Check for variables with same information
# dataset %>%
#   summarise_all(entropy) -> asd
# asd <- t(asd)
# asd <- data.frame(column_name = row.names(asd), asd)
# asd <- asd[asd$asd %in% asd$asd[duplicated(asd$asd)],]
# View(asd)

vars_to_remove <- c()
# Year of Birth is enough
vars_to_remove <- c(vars_to_remove, 'Age recode with <1 year olds',
                    'Age Standard for Survival (15-44,45-54,55-64,65-74,75+)',
                    # Primary Site - Labeled is more informative
                    'Primary Site',
                    # ICD-O-3 Hist/behav has strings and same information
                    'ICD-O-3 Hist/behav, malignant', 'Histologic Type ICD-O-3',
                    colnames(dataset[, grepl('^SS', colnames(dataset))]),
                    # Instead try to create an index
                    colnames(dataset[,
                                     grepl('1997-1999|2004-2007|2000-2003',
                                           colnames(dataset))]),
                    colnames(dataset[,
                                     grepl('2013-2017',
                                           colnames(dataset))]),
                    'Month of diagnosis', # recode var is better
                    'Months since last birthday', # Doesn't connect to anything
                    'Behavior code ICD-O-2', # 1 sample in one of the 2 classes
                    'Site recode B ICD-O-3/WHO 2008',
                    'Appalachia (ARC 2007)',
                    'Appalachia Region (ARC 2008 Revision)'
                    )
dataset %>%
  select(-all_of(vars_to_remove)) -> dataset
rm(vars_to_remove)

# Filter out most variables related to the county, otherwise it will be too many
vars_to_remove <- c()
# dataset %>%
  # select(contains('male' )) %>%
  # colnames -> vars_to_remove
dataset %>%
  select(starts_with('%')) %>%
  colnames -> vars_to_remove
dataset %>%
  select(-all_of(vars_to_remove)) -> dataset
rm(vars_to_remove)

# Summarize median income
dataset %>%
  rowwise() %>%
  mutate('Median family income' = mean(c(`Median family income (in tens) ACS 2007-11`,
                                         `Median family income (in tens) ACS 2008-12`,
                                         `Median family income (in tens) ACS 2009-13`,
                                         `Median family income (in tens) ACS 2010-14`,
                                         `Median family income (in tens) ACS 2011-15`,
                                         `Median family income (in tens) ACS 2012-2016`),
                                         na.rm = TRUE)) %>%
  mutate('Median household income' = mean(c(`Median household income (in tens) ACS 2007-11`,
                                            `Median household income (in tens) ACS 2008-12`,
                                            `Median household income (in tens) ACS 2009-13`,
                                            `Median household income (in tens) ACS 2010-14`,
                                            `Median household income (in tens) ACS 2011-15`,
                                            `Median household income (in tens) ACS 2012-2016`),
                                          na.rm = TRUE)) %>%
  select(-c(contains('ACS'))) %>%
  # Pick race
  select(-c('Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)',
            'Race recode (W, B, AI, API)', 'Race recode (White, Black, Other)',
            'Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)')) -> dataset



# Merge Tumor Size Variables ----------------------------------------------
dataset <- dataset %>%
  mutate(`Tumor Size Summary (2016+)` = coalesce(`CS tumor size (2004-2015)`)) %>%
  select(-c(`CS tumor size (2004-2015)`))


# Renaming variables ------------------------------------------------------
dataset <- dataset %>%
  rename('YearOfDiagnosis' = 'Year of diagnosis',
         'TumorSize' = 'Tumor Size Summary (2016+)',
         'StateCounty' = 'State-county',
         'PRCDA16' = 'PRCDA 2016',
         'PRCDARegion' = 'PRCDA Region',
         'SiteAYARec' = 'AYA site recode/WHO 2008',
         'Quadrant' = 'Primary Site - labeled',
         'DiagConf' = 'Diagnostic Confirmation',
         'Histology' = 'ICD-O-3 Hist/behav',
         'HistGrouping' = 'Histology recode - broad groupings',
         'ICCCSiteRecExt' = 'ICCC site rec extended ICD-O-3/WHO 2008',
         'ICCCSiteRec' = 'ICCC site recode ICD-O-3/WHO 2008',
         'SimpleStage' = 'Summary stage 2000 (1998+)',
         'Stage' = 'SEER Combined Summary Stage 2000 (2004+)',
         'SimpleStage2' = 'SEER historic stage A (1973-2015)',
         'SimpleStage3' = 'Derived SS1977 (2004-2015)',
         'AJCCStgGrp7th' = 'Derived AJCC Stage Group, 7th ed (2010-2015)',
         'AJCC7thT' = 'Derived AJCC T, 7th ed (2010-2015)',
         'AJCC7thN' = 'Derived AJCC N, 7th ed (2010-2015)',
         'AJCC7thM' = 'Derived AJCC M, 7th ed (2010-2015)',
         'CmbStgGrp' = 'Derived SEER Cmb Stg Grp (2016+)',
         'CombT' = 'Derived SEER Combined T (2016+)',
         'CombN' = 'Derived SEER Combined N (2016+)',
         'CombM' = 'Derived SEER Combined M (2016+)',
         'CombTSrc' = 'Derived SEER Combined T Src (2016+)',
         'CombNSrc' = 'Derived SEER Combined N Src (2016+)',
         'CombMSrc' = 'Derived SEER Combined M Src (2016+)',
         'AJCCStgGrp6th' = 'Derived AJCC Stage Group, 6th ed (2004-2015)',
         'BreastAJCC6thStg' = 'Breast - Adjusted AJCC 6th Stage (1988-2015)',
         'AJCC6thT' = 'Derived AJCC T, 6th ed (2004-2015)',
         'AJCC6thN' = 'Derived AJCC N, 6th ed (2004-2015)',
         'AJCC6thM' = 'Derived AJCC M, 6th ed (2004-2015)',
         'BCAdjAJCC6thT' = 'Breast - Adjusted AJCC 6th T (1988-2015)',
         'BCAdjAJCC6thN' = 'Breast - Adjusted AJCC 6th N (1988-2015)',
         'BCAdjAJCC6thM' = 'Breast - Adjusted AJCC 6th M (1988-2015)',
         'CombTNMEd' = 'TNM Edition Number (2016+)',
         'TypeSurgeryPrimitiveSite' = 'RX Summ--Surg Prim Site (1998+)',
         'OtherSurgery' = 'RX Summ--Surg Oth Reg/Dis (2003+)',
         'TimingOfRadiotherapy' = 'Radiation sequence with surgery',
         'Surgery' = 'Reason no cancer-directed surgery',
         'Radiation' = 'Radiation recode',
         'ChemoTherapy' = 'Chemotherapy recode (yes, no/unk)',
         'DelayDiagnosisFirstTreatment' = 'Months from diagnosis to treatment',
         'NumberOfNodeRemoved' = 'Regional nodes examined (1988+)',
         'NumberOfPositiveNode' = 'Regional nodes positive (1988+)',
         'BoneMetastasisAtDiagnosis' = 'SEER Combined Mets at DX-bone (2010+)',
         'BrainMetastasisAtDiagnosis' = 'SEER Combined Mets at DX-brain (2010+)',
         'LiverMetastasisAtDiagnosis' = 'SEER Combined Mets at DX-liver (2010+)',
         'LungMetastasisAtDiagnosis' = 'SEER Combined Mets at DX-lung (2010+)',
         'DistantNodeMetastasisAtDiagnosis' = 'Mets at DX-Distant LN (2016+)',
         'OtherMetastasisAtDiagnosis' = 'Mets at DX-Other (2016+)',
         'BreastSubtype' = 'Breast Subtype (2010+)',
         'ER' = 'ER Status Recode Breast Cancer (1990+)',
         'PR' = 'PR Status Recode Breast Cancer (1990+)',
         'HER2' = 'Derived HER2 Recode (2010+)',
         'TumorExtension' = 'CS extension (2004-2015)',
         'LNInvolvement' = 'CS lymph nodes (2004-2015)',
         'MetastasisAtDiagnosis' = 'CS mets at dx (2004-2015)',
         'Method2EvalTumSizeExt' = 'CS Tumor Size/Ext Eval (2004-2015)',
         'Method2EvalRegNode' = 'CS Reg Node Eval (2004-2015)',
         'Method2EvalMet' = 'CS Mets Eval (2004-2015)',
         'ERAssay' = 'CS site-specific factor 1 (2004+ varying by schema)',
         'PRAssay' = 'CS site-specific factor 2 (2004+ varying by schema)',
         'nPositiveAxillaryLN' = 'CS site-specific factor 3 (2004+ varying by schema)',
         'IHCRegionalLN' = 'CS site-specific factor 4 (2004+ varying by schema)',
         'MOLRegionalLN' = 'CS site-specific factor 5 (2004+ varying by schema)',
         'InvasiveComponent' = 'CS site-specific factor 6 (2004+ varying by schema)',
         'BRScore' = 'CS site-specific factor 7 (2004+ varying by schema)',
         'HER2Summary' = 'CS site-specific factor 15 (2004+ varying by schema)',
         'CSversionCurr' = 'CS version input current (2004-2015)',
         'CSversionOrig' = 'CS version input original (2004-2015)',
         'COD2SiteRecode' = 'COD to site recode',
         'COD2SiteRecode2' = 'COD to site rec KM',
         'DeathSpecificOfBreastCancer' = 'SEER cause-specific death classification',
         'DeathDueToOtherCause' = 'SEER other cause of death classification',
         'SurvivalDelayInMonths' = 'Survival months',
         'SurvMonthsFlag' = 'Survival months flag',
         'VitalStatus' = 'Vital status recode (study cutoff used)',
         'TumorSequence' = 'Sequence number',
         'FirstMalignantPrimary' = 'First malignant primary indicator',
         'PrimByIRules' = 'Primary by international rules',
         'SequenceRecode' = 'Record number recode',
         'NInSituMaligTumor' = 'Total number of in situ/malignant tumors for patient',
         'NBenignBorderTumor' = 'Total number of benign/borderline tumors for patient',
         'HistologyICDO2' = 'Histology ICD-O-2',
         'SiteRecICDO2_9' = 'Recode ICD-O-2 to 9',
         'SiteRecICDO2_10' = 'Recode ICD-O-2 to 10',
         'AgeRecode' = 'Age recode with single ages and 85+',
         'Hispanic' = 'Origin recode NHIA (Hispanic, Non-Hisp)',
         'AgeAtDiagnosis' = 'Age at diagnosis',
         'Ethnicity' = 'Race/ethnicity',
         'IHSLink' = 'IHS Link',
         'YearOfBirth' = 'Year of birth',
         'MonthOfDiagnosis' = 'Month of diagnosis recode',
         'TypeReportSource' = 'Type of Reporting Source',
         'Insurance' = 'Insurance Recode (2007+)',
         'MaritalStatus' = 'Marital status at diagnosis',
         'RuralUrban' = 'Rural-Urban Continuum Code 2013',
         'HealthServiceAreaNCI' = 'Health Service Area (NCI Modified)',
         'HealthServiceArea' = 'Health Service Area',
         'CHSDA12' = 'CHSDA 2012',
         'CHSDA06' = 'CHSDA 2006',
         'CostLivingIndex' = 'Normalized cost-of-living index 2004',
         'MedianFamIncome' = 'Median family income',
         'MedianHouseholdIncome' = 'Median household income'
         )


# Post-run ----------------------------------------------------------------

# After running miic once and analyzing the network, some ideas came up
# Remove SiteRecICDO2_9 and SiteRecICDO2_10, because most info is already on
# PrimSite
dataset <- dataset %>%
  select(-c(SiteRecICDO2_9, SiteRecICDO2_10))

# Change TypeSurgeryPrimitiveSite
dataset <- dataset %>%
  # Set TypeSurgeryPrimitiveSite to NA if No Surgery (00)
  mutate(TypeSurgeryPrimitiveSite,
         TypeSurgeryPrimitiveSite = ifelse(TypeSurgeryPrimitiveSite == '00',
                                           NA,
                                           TypeSurgeryPrimitiveSite))

# Remove AgeRecode. The diff is 85+ instead of ages >85
dataset <- dataset %>%
  select(-c('AgeRecode')) %>%
# Remove CS Version variables
  select(-c('CSversionCurr', 'CSversionOrig'))

# Merge categories --------------------------------------------------------
# Some variables have categories with very few values such as Ethnicity. MIIC
# Won't try to optmize the categories the way it does with continuous vars, so
# it's up to us. TODO: Ethnicity
dataset <- dataset %>%
  mutate(Ethnicity = ifelse(Ethnicity %in% c('Korean (1988+)',
                                             'Chinese',
                                             'Vietnamese (1988+)',
                                             'Japanese',
                                             'Other Asian (1991+)',
                                             'Hmong (1988+)',
                                             'Thai (1994+)',
                                             'Asian Indian or Pakistani, NOS (1988+)',
                                             'Kampuchean (1988+)',
                                             'Pakistani (2010+)',
                                             'Asian Indian (2010+)',
                                             'Laotian (1988+)',
                                             'Filipino'
                                             ),
                            'Asian', Ethnicity)) %>%
  mutate(Ethnicity = ifelse(Ethnicity %in% c('Pacific Islander, NOS (1991+)',
                                             'Fiji Islander (1991+)',
                                             'Samoan (1991+)',
                                             'Hawaiian',
                                             'Guamanian, NOS (1991+)',
                                             'Micronesian, NOS (1991+)',
                                             'Melanesian, NOS (1991+)',
                                             'Polynesian, NOS (1991+)',
                                             'Chamorran (1991+)',
                                             'New Guinean (1991+)',
                                             'Tahitian (1991+)',
                                             'Tongan (1991+)'
                                             ),
                            'Pacific Islanders',
                            Ethnicity))


# Remove some non-continuous values from some variables -------------------
dataset <- dataset %>%
  # Insurance
  mutate(NumberOfNodeRemoved =
           ifelse(NumberOfNodeRemoved > 90,
                  NA,
                  NumberOfNodeRemoved)) %>%
  mutate(NumberOfPositiveNode =
           ifelse(NumberOfPositiveNode > 90,
                  NA,
                  NumberOfPositiveNode)) %>%
  mutate(TumorSize = as.numeric(TumorSize)) %>%
  mutate(TumorSize =
           ifelse(TumorSize %in% c(990, 996, 997, 998, 999, 888),
                  NA,
                  TumorSize)) %>%
  # For 991-995 it's “less than 1cm, less than 2cm.. for now I'll guess size/2”
  # Confirm this with Anne-Sophie
  mutate(TumorSize =
           ifelse(TumorSize %in% c(991, 992, 993, 994, 995),
                  ((as.integer(TumorSize)-990)/2)*10,
                  TumorSize))


# Remove redundant variables according to Enora
# also Hispanic. Ethnicity is enough.
dataset <- dataset %>%
  select(-c(StateCounty, PRCDA16, PRCDARegion, SiteAYARec, ICCCSiteRec,
            DiagConf, HistGrouping, ICCCSiteRecExt, SimpleStage, Stage,
            SimpleStage2, SimpleStage3, AJCCStgGrp7th, CmbStgGrp, CombT, CombN,
            CombM, CombTSrc, CombNSrc, CombMSrc, AJCCStgGrp6th,
            BreastAJCC6thStg, AJCC6thT, AJCC6thM, AJCC6thN, CombTNMEd,
            TumorExtension, LNInvolvement, ERAssay, PRAssay,
            nPositiveAxillaryLN, MOLRegionalLN, IHCRegionalLN, BRScore,
            HER2Summary, SurvMonthsFlag, COD2SiteRecode2, COD2SiteRecode,
            TumorSequence, PrimByIRules, SequenceRecode, NInSituMaligTumor,
            NBenignBorderTumor, HistologyICDO2, IHSLink, TypeReportSource,
            HealthServiceArea, HealthServiceAreaNCI, CHSDA12, CHSDA06,
            Hispanic))


# Searching for problematic discrete variables ----------------------------
dataset %>%
  select_if(~ length(unique(na.omit(.))) > 20) %>%
  colnames

# Remove county
dataset <- dataset %>%
  select(-c('County'))

# Update some levels ------------------------------------------------------
library(stringr)

# Quadrant
dataset <- dataset %>%
  mutate(Quadrant = str_sub(Quadrant, 7)) %>%
# TypeSurgeryPrimitiveSite
# https://seer.cancer.gov/manuals/2018/AppendixC/Surgery_Codes_Breast_2018.pdf
  mutate(
    TypeSurgeryPrimitiveSite = case_when(
      TypeSurgeryPrimitiveSite == "19" ~ "Local tumor destruction, NOS",
      TypeSurgeryPrimitiveSite == "20" ~ "Lumpectomy",
      TypeSurgeryPrimitiveSite == "21" ~ "Lumpectomy",
      TypeSurgeryPrimitiveSite == "22" ~ "Lumpectomy",
      TypeSurgeryPrimitiveSite == "23" ~ "Lumpectomy",
      TypeSurgeryPrimitiveSite == "24" ~ "Lumpectomy",
      TypeSurgeryPrimitiveSite == "30" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "40" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "41" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "42" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "43" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "44" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "45" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "46" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "47" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "48" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "49" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "50" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "51" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "52" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "53" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "54" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "55" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "56" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "57" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "58" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "59" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "60" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "61" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "62" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "63" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "64" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "65" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "66" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "67" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "68" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "69" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "71" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "72" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "73" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "74" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "75" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "76" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "80" ~ "Mastectomy",
      TypeSurgeryPrimitiveSite == "90" ~ "Surgery, NOS")
  ) %>%
# InvasiveComponent
# (https://training.seer.cancer.gov/schema/breast/factor6.html)
  mutate(
    InvasiveComponent = case_when(
      InvasiveComponent == "000" ~ "Entire tumor reported as invasive (No in situ component reported)",
      InvasiveComponent == "010" ~ "Entire tumor reported as in situ (No invasive component reported)",
      InvasiveComponent == "020" ~ "Invasive and in situ components present",
      InvasiveComponent == "030" ~ "Invasive and in situ components present",
      InvasiveComponent == "040" ~ "Invasive and in situ components present",
      InvasiveComponent == "050" ~ "Invasive and in situ components present",
      InvasiveComponent == "060" ~ "Invasive and in situ components present")
  ) %>%
# Method2EvalMet
# https://staging.seer.cancer.gov/cs/input/02.05.50/breast/mets_eval/?breadcrumbs=(~schema_list~),(~view_schema~,~breast~)
  mutate(Method2EvalMet = case_when(
    Method2EvalMet == "0" ~ "Evaluation of distant metastasis based on physical examination, imaging examination, and/or other non-invasive clinical evidence.  No pathologic examination of metastatic tissue performed or pathologic examination was negative.",
    Method2EvalMet == "1" ~ "Evaluation of distant metastasis based on endoscopic examination or other invasive technique, including surgical observation without biopsy.  No pathologic examination of metastatic tissue performed or pathologic examination was negative.",
    Method2EvalMet == "2" ~ "No microscopic examination of metastatic specimen done prior to death, but positive metastatic evidence derived from autopsy (tumor was suspected or diagnosed prior to autopsy).",
    Method2EvalMet == "3" ~ "Specimen from metastatic site microscopically positive WITHOUT pre-surgical systemic treatment or radiation OR specimen from metastatic site microscopically positive, unknown if pre-surgical systemic treatment or radiation performed  OR specimen from metastatic site microscopically positive prior to neoadjuvant treatment.",
    Method2EvalMet == "5" ~ "Specimen from metastatic site microscopically positive WITH pre-surgical systemic treatment or radiation, BUT metastasis based on clinical evidence.",
    Method2EvalMet == "6" ~ "Specimen from metastatic site microscopically positive WITH pre-surgical systemic treatment or radiation, BUT metastasis based on pathologic evidence.")
  ) %>%
# Method2EvalTumSizeExt
# https://staging.seer.cancer.gov/cs/input/02.05.50/breast/extension_eval/?breadcrumbs=(~schema_list~),(~view_schema~,~breast~)
  mutate(Method2EvalTumSizeExt = case_when(
    Method2EvalTumSizeExt == "0" ~ "No surgical resection done.  Evaluation based on physical examination, imaging examination, or other non-invasive clinical evidence.  No autopsy evidence used.",
    Method2EvalTumSizeExt == "1" ~ "No surgical resection done.  Evaluation based on endoscopic examination, diagnostic biopsy, including fine needle aspiration biopsy, or other invasive techniques, including surgical observation without biopsy.  No autopsy evidence used.",
    Method2EvalTumSizeExt == "2" ~ "No surgical resection done, but evidence derived from autopsy (tumor was suspected or diagnosed prior to autopsy).",
    Method2EvalTumSizeExt == "3" ~ "Surgical resection performed WITHOUT pre-surgical systemic treatment or radiation OR surgical resection performed, unknown if pre-surgical systemic treatment or radiation performed AND Evaluation based on evidence acquired before treatment, supplemented or modified by the additional evidence acquired during and from surgery, particularly from pathologic examination of the resected specimen. No surgical resection done.  Evaluation based on positive biopsy of highest T classification.",
    Method2EvalTumSizeExt == "5" ~ "Surgical resection performed AFTER neoadjuvant therapy and tumor size/extension based on clinical evidence, unless the pathologic evidence at surgery (AFTER neoadjuvant) is more extensive (see code 6).",
    Method2EvalTumSizeExt == "6" ~ "Surgical resection performed AFTER neoadjuvant therapy AND tumor size/extension based on pathologic evidence,  because pathologic evidence at surgery is more extensive than clinical evidence before treatment.")
  ) %>%

# Method2EvalRegNode
# https://staging.seer.cancer.gov/cs/input/02.05.50/breast/nodes_eval/?breadcrumbs=(~schema_list~),(~view_schema~,~breast~)
  mutate(Method2EvalRegNode = case_when(
    Method2EvalRegNode == "0" ~ "No regional lymph nodes removed for examination",
    Method2EvalRegNode == "1" ~ "No regional lymph nodes removed for examination",
    Method2EvalRegNode == "2" ~ "No regional lymph nodes removed for examination",
    Method2EvalRegNode == "3" ~ "Any microscopic assessment of regional nodes (including FNA, incisional core needle bx, excisional bx, sentinel node bx or node resection), WITH removal of the primary site adequate for pathologic T classification (treatment) or biopsy assessment of the highest T category. OR Any microscopic assessment of a regional node in the highest N category, regardless of the T category information.",
    Method2EvalRegNode == "5" ~ "Regional lymph nodes removed for examination AFTER neoadjuvant therapy",
    Method2EvalRegNode == "6" ~ "Regional lymph nodes removed for examination AFTER neoadjuvant therapy")
  ) %>%
# MetastasisAtDiagnosis
# https://staging.seer.cancer.gov/cs/input/02.05.50/breast/mets/?breadcrumbs=(~schema_list~),(~view_schema~,~breast~)
  mutate(MetastasisAtDiagnosis = ifelse(MetastasisAtDiagnosis %in% c('00', '05', '07'),
                                        'No',
                                        MetastasisAtDiagnosis)) %>%
  mutate(MetastasisAtDiagnosis = ifelse(MetastasisAtDiagnosis %in% c('10', '40', '42', '44', '50', '60'),
                                        'Yes',
                                        MetastasisAtDiagnosis)) %>%
  mutate(AJCC7thT = case_when(
    grepl('T0', AJCC7thT) ~ 'T0',
    grepl('T1', AJCC7thT) ~ 'T1',
    grepl('T2', AJCC7thT) ~ 'T2',
    grepl('T3', AJCC7thT) ~ 'T3',
    grepl('T4', AJCC7thT) ~ 'T4',
    grepl('Tis', AJCC7thT) ~ 'Tis',
    is.na(AJCC7thT) ~ NA_character_,
    grepl('999999999', AJCC7thT) ~ '999999999',
    TRUE ~ 'ERROR')
  ) %>%
  mutate(AJCC7thM = case_when(
    grepl('M0', AJCC7thM) ~ 'M0',
    grepl('M1', AJCC7thM) ~ 'M1',
    is.na(AJCC7thM) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(AJCC7thN = case_when(
    grepl('N0', AJCC7thN) ~ 'N0',
    grepl('N1', AJCC7thN) ~ 'N1',
    grepl('N2', AJCC7thN) ~ 'N2',
    grepl('N3', AJCC7thN) ~ 'N3',
    is.na(AJCC7thN) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(DeathDueToOtherCause = case_when(
    DeathDueToOtherCause == 'Dead (attributable to causes other than this cancer dx)' ~ 'Yes',
    DeathDueToOtherCause == 'Alive or dead due to cancer' ~ 'No',
    DeathDueToOtherCause == 'Dead (missing/unknown COD)' ~ NA_character_,
    is.na(DeathDueToOtherCause) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(DeathSpecificOfBreastCancer = case_when(
    DeathSpecificOfBreastCancer == 'Dead (attributable to this cancer dx)' ~ 'Yes',
    DeathSpecificOfBreastCancer == 'Alive or dead of other cause' ~ 'No',
    DeathSpecificOfBreastCancer == 'Dead (missing/unknown COD)' ~ NA_character_,
    is.na(DeathSpecificOfBreastCancer) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(Surgery = case_when(
    Surgery == 'Surgery performed' ~ 'Performed',
    Surgery == 'Not performed, patient died prior to recommended surgery' ~ 'Recommended',
    Surgery == 'Recommended but not performed, unknown reason' ~ 'Recommended',
    Surgery == 'Recommended but not performed, patient refused' ~ 'Recommended',
    Surgery == 'Not recommended, contraindicated due to other cond; autopsy only (1973-2002)' ~ 'Not Recommended',
    Surgery == 'Not recommended' ~ 'Not Recommended',
    Surgery == 'Recommended, unknown if performed' ~ NA_character_,
    is.na(Surgery) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(PR = ifelse(PR == 'Borderline', NA, PR)) %>%
  mutate(ER = ifelse(ER == 'Borderline', NA, ER)) %>%
  mutate(Ethnicity = case_when(
    Ethnicity == 'American Indian/Alaska Native' ~ 'Other',
    Ethnicity == 'Pacific Islanders' ~ 'Other',
    is.na(Ethnicity) ~ NA_character_,
    TRUE ~ Ethnicity)
  ) %>%
  mutate(OtherSurgery = case_when(
    OtherSurgery == 'None; diagnosed at autopsy' ~ 'No',
    OtherSurgery == 'Non-primary surgical procedure performed' ~ 'Yes',
    OtherSurgery == 'Non-primary surgical procedure to distant lymph node(s)' ~ 'Yes',
    OtherSurgery == 'Non-primary surgical procedure to other regional sites' ~ 'Yes',
    OtherSurgery == 'Non-primary surgical procedure to distant site' ~ 'Yes',
    OtherSurgery == 'Any combo of sur proc to oth rg, dis lym nd, and/or dis site' ~ 'Yes',
    is.na(OtherSurgery) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(MaritalStatus = case_when(
    MaritalStatus == 'Married (including common law)' ~ 'Married (including common law/domestic partner)',
    MaritalStatus == 'Single (never married)' ~ 'Single',
    MaritalStatus == 'Unmarried or Domestic Partner' ~ 'Married (including common law/domestic partner)',
    is.na(MaritalStatus) ~ NA_character_,
    TRUE ~ MaritalStatus)
  ) %>%
  mutate(TimingOfRadiotherapy = case_when(
    TimingOfRadiotherapy == 'Radiation after surgery' ~ 'After surgery',
    TimingOfRadiotherapy == 'No radiation and/or cancer-directed surgery' ~ 'Not performed',
    TimingOfRadiotherapy == 'Intraoperative rad with other rad before/after surgery' ~ 'Before/After/During surgery',
    TimingOfRadiotherapy == 'Intraoperative radiation' ~ 'During surgery',
    TimingOfRadiotherapy == 'Radiation before and after surgery' ~ 'Before/After surgery',
    TimingOfRadiotherapy == 'Radiation prior to surgery' ~ 'Before surgery',
    TimingOfRadiotherapy == 'Surgery both before and after radiation' ~ 'Before/After surgery',
    TimingOfRadiotherapy == 'Sequence unknown, but both were given' ~ NA_character_,
    is.na(TimingOfRadiotherapy) ~ NA_character_,
    TRUE ~ 'ERROR')
  ) %>%
  mutate(Radiation = case_when(
    Radiation == 'Beam radiation' ~ 'Yes',
    Radiation == 'Radioactive implants (includes brachytherapy) (1988+)' ~ 'Yes',
    Radiation == 'Radiation, NOS  method or source not specified' ~ 'Yes',
    Radiation == 'Combination of beam with implants or isotopes' ~ 'Yes',
    Radiation == 'Radioisotopes (1988+)' ~ 'Yes',
    Radiation == 'Recommended, unknown if administered' ~ NA_character_,
    Radiation == 'Refused (1988+)' ~ 'No',
    is.na(Radiation) ~ NA_character_,
    TRUE ~ 'ERROR')
  )

# Remove Breast Cancer Adjusted AJCC variables
# (1) There are more NAs in the Breast Cancer Adjusted AJCC variables.
# (2) It's AJCC 6th, instead of 7th
dataset <- dataset %>%
  select(-c('BCAdjAJCC6thT', 'BCAdjAJCC6thM', 'BCAdjAJCC6thN'))


# Merge DistantNodeMetastasis, OtherMetastasis, MetastasisAtDiagno --------
# They're all about metastasis at diagnosis but the last two are for 2016+
# which is about 60k patients.
dataset <- dataset %>%
  mutate(MetastasisAtDiagnosis = case_when(
    OtherMetastasisAtDiagnosis == 'None; no other metastases' ~ 'No',
    DistantNodeMetastasisAtDiagnosis == 'None; no lymph node metastases' ~ 'No',
    DistantNodeMetastasisAtDiagnosis == 'Yes; distant lymph node metastases' ~ 'Yes',
    OtherMetastasisAtDiagnosis == 'Yes; distant mets in known site(s) other than bone brain liver lung dist LN' ~ 'Yes',
    is.na(OtherMetastasisAtDiagnosis) ~ MetastasisAtDiagnosis,
    is.na(DistantNodeMetastasisAtDiagnosis) ~ MetastasisAtDiagnosis,
  TRUE ~ 'ERROR')
) %>% # Herve requested to put these two variables black
  # select(-c(DistantNodeMetastasisAtDiagnosis, OtherMetastasisAtDiagnosis)) %>%
  # Merge Insurance like in http://doi.wiley.com/10.1002/cncr.29120
  mutate(Insurance = case_when(
    Insurance == 'Insured' ~ 'Non-Medicaid',
    Insurance == 'Insured/No specifics' ~ 'Non-Medicaid',
    Insurance == 'Any Medicaid' ~ 'Medicaid',
    is.na(Insurance) ~ NA_character_,
    TRUE ~ Insurance)
  ) %>%
  # Merge Histology levels
  mutate(Histology =
           ifelse(Histology %in% c('8500/3: Infiltrating duct carcinoma, NOS',
                                   '8520/3: Lobular carcinoma, NOS',
                                   '8522/3: Infiltrating duct and lobular carcinoma',
                                   '8523/3: Infiltrating duct mixed with other types of carcinoma'),
                  as.character(Histology),
                  'Other')
  )


# Rename levels according to RT2Lab dictionary ----------------------------
# Separate again, tehre is no resection done in the WITHOUT
dataset <- dataset %>%
  mutate(Method2EvalTumSizeExt = case_when(
  grepl('No surgical resection done', Method2EvalTumSizeExt) ~ 'No resection',
  grepl('WITHOUT', Method2EvalTumSizeExt) ~ 'Resection without pre-surgery treatment',
  grepl('AFTER', Method2EvalTumSizeExt) ~ 'Resection after neoadjuvent chemo',
  is.na(Method2EvalTumSizeExt) ~ NA_character_,
  grepl('999999999', Method2EvalTumSizeExt) ~ '999999999',
  TRUE ~ 'ERROR')
)

# Replace all occurrences of , in the dataset
dataset <- data.frame(lapply(dataset, function(x) {gsub(",", "", x)}))
# Replace double space to one space
dataset <- data.frame(lapply(dataset, function(x) {gsub("  ", " ", x)}))

# Merge raw levels --------------------------------------------------------

library(forcats)
# Is there any cell with the value “999999999” (Other)?
if (is.na(Reduce("|", dataset=="999999999"))) {
  dataset <- dataset %>%
    # Change to “999999999” all levels that are less common than 0.1%
    # of the dataset in discrete variables
    mutate_at(vars(-TumorSize, -MedianFamIncome, -DelayDiagnosisFirstTreatment,
                   -NumberOfNodeRemoved, -NumberOfPositiveNode,
                   -SurvivalDelayInMonths, -AgeAtDiagnosis, -YearOfBirth,
                   -CostLivingIndex, -MedianHouseholdIncome),
              ~fct_lump_min(., min=sum(!is.na(.))/1000,
                            other_level="999999999")
    )
} else {
  warning('It wasn\'t possible to merge rare values for the \
           new value name already exists in the dataset.')
}

# Only patients not diagnosed with metastasis -----------------------------
# dataset %>%
  # filter(MetastasisAtDiagnosis == 'No') -> dataset

# Remove metastasis variables ---------------------------------------------

# According to https://healthcaredelivery.cancer.gov/seermedicare/considerations/measures.html
# metastasis is only recorded at diagnosis. Besides, according to AS, the
# treatment and follow-up of patients with metastasis is very different and we
# should focus on patietns that did not have metastasis at diagnosis. She said
# she's even more inclined to do it, because the interesting variables ins/ms
# are not associated to metastasis variables.

dataset <- dataset %>%
  # Hervé requested to put MetastasisAtDiagnosis back
  # select(-c(MetastasisAtDiagnosis, Method2EvalMet))
  select(-c(Method2EvalMet))

# Engineering radiation variable
dataset <- dataset %>%
  # Remove old Radiation variable (too much NA)
  select(-c(Radiation)) %>%
  # Create new one
  mutate(
    Radiation = case_when(
      TimingOfRadiotherapy == 'After surgery' ~ "Yes",
      TimingOfRadiotherapy == 'Before surgery' ~ "Yes",
      TimingOfRadiotherapy == 'Before/After surgery' ~ "Yes",
      TimingOfRadiotherapy == 'Before/After/During surgery' ~ "Yes",
      TimingOfRadiotherapy == 'During surgery' ~ "Yes",
      TimingOfRadiotherapy == 'Not performed' ~ "No",
      is.na(TimingOfRadiotherapy) ~ NA_character_,
    )
  ) %>%
  # Remove TimingOfRadiotherapy
  select(-c(TimingOfRadiotherapy))

# Save intermediate object file so I don't have to re-run everything
save(dataset, file = 'data/processed/BC_full_49var.RData')
load('data/processed/BC_full_49var.RData')

# Saving Breast Cancer raw dataset ####
write.table(dataset,
            file='data/processed/BC_2010-16_SEER18.tsv',
            quote=FALSE, sep='\t', row.names = FALSE)

# Saving Breast Cancer raw dataset subsampled ####
set.seed(2020)
write.table(dataset[sample(nrow(dataset), 200000), ],
            file='data/processed/BC_2010-16_SEER18_200K_A.tsv',
            quote=FALSE, sep='\t', row.names = FALSE)
set.seed(2018)
write.table(dataset[sample(nrow(dataset), 200000), ],
            file='data/processed/BC_2010-16_SEER18_200K_B.tsv',
            quote=FALSE, sep='\t', row.names = FALSE)
set.seed(2015)
write.table(dataset[sample(nrow(dataset), 200000), ],
            file='data/processed/BC_2010-16_SEER18_200K_C.tsv',
            quote=FALSE, sep='\t', row.names = FALSE)

# How different are the 4 samplings?
# set.seed(2020)
# A <- dataset[sample(nrow(dataset), 50000), ]
# set.seed(2018)
# B <- dataset[sample(nrow(dataset), 50000), ]
# set.seed(2015)
# C <- dataset[sample(nrow(dataset), 50000), ]
# set.seed(2012)
# D <- dataset[sample(nrow(dataset), 50000), ]
#
# nrow(setdiff(A,B))
# nrow(setdiff(A,C))
# nrow(setdiff(A,D))


# Survival analysis for report --------------------------------------------

# library(survival)
# set.seed(2020)
# A <- dataset[sample(nrow(dataset), 50000), ]
# km_data <- A %>%
#   select(DeathDue2BC, HadSurgery, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(DeathDue2BC,
#          DeathDue2BC = ifelse(DeathDue2BC == 'Dead (attributable to this cancer dx)',
#                               1,
#                               0))
# km.by.surgery <- survfit(Surv(SurvMonths, DeathDue2BC) ~ HadSurgery, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.surgery, km_data,
#            legend.title = "Cause-specific",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("#E7B800", "#2E9FDF", "blue", "purple"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(46, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(24, "bold")
# )
#
# # HadSurgery overall survival
# km_data <- A %>%
#   select(VitalStatus, HadSurgery, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(VitalStatus,
#          VitalStatus = ifelse(VitalStatus == 'Dead',
#                               1,
#                               0))
# km.by.surgery <- survfit(Surv(SurvMonths, VitalStatus) ~ HadSurgery, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.surgery, km_data,
#            legend.title = "Overall",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("#E7B800", "#2E9FDF", "blue", "purple"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(46, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(24, "bold")
# )
#
# # MaritalStatus overall
#
# km_data <- A %>%
#   select(VitalStatus, MaritalStatus, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(VitalStatus,
#          VitalStatus = ifelse(VitalStatus == 'Dead',
#                               1,
#                               0))
# km.by.ms <- survfit(Surv(SurvMonths, VitalStatus) ~ MaritalStatus, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.ms, km_data,
#            legend.title = "Overall Survival",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("blue", "green", "red", "purple", "yellow", "black"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(36, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(12, "bold")
# )
#
# # MaritalStatus cause specific
#
# km_data <- A %>%
#   select(DeathDue2BC, MaritalStatus, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(DeathDue2BC,
#          DeathDue2BC = ifelse(DeathDue2BC == 'Dead (attributable to this cancer dx)',
#                               1,
#                               0))
# km.by.ms <- survfit(Surv(SurvMonths, DeathDue2BC) ~ MaritalStatus, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.ms, km_data,
#            legend.title = "Cause-specific Survival",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("blue", "green", "red", "purple", "yellow", "black"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(36, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(12, "bold")
# )
#
#
# # Insurance overall
#
# km_data <- A %>%
#   select(VitalStatus, Insurance, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(VitalStatus,
#          VitalStatus = ifelse(VitalStatus == 'Dead',
#                               1,
#                               0))
# km.by.ms <- survfit(Surv(SurvMonths, VitalStatus) ~ Insurance, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.ms, km_data,
#            legend.title = "Overall Survival",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("blue", "green", "red", "purple", "yellow", "black"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(36, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(14, "bold")
# )
#
# # Insurance cause specific
#
# km_data <- A %>%
#   select(DeathDue2BC, Insurance, SurvMonths) %>%
#   mutate(SurvMonths = as.numeric(SurvMonths)) %>%
#   mutate(DeathDue2BC,
#          DeathDue2BC = ifelse(DeathDue2BC == 'Dead (attributable to this cancer dx)',
#                               1,
#                               0))
# km.by.ms <- survfit(Surv(SurvMonths, DeathDue2BC) ~ Insurance, type="kaplan-meier", conf.type="log", data=km_data)
#
# library(survminer)
# ggsurvplot(km.by.ms, km_data,
#            legend.title = "Cause-specific Survival",
#            pval = TRUE,
#            conf.int = TRUE,
#            tables.height = 0.2,
#            tables.theme = theme_cleantable(),
#            # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
#            # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
#            palette = c("blue", "green", "red", "purple", "yellow", "black"),
#            ggtheme = theme_bw(), # Change ggplot2 theme
#            font.main = c(36, "bold"),
#            font.x = c(14, "bold"),
#            font.y = c(14, "bold"),
#            font.tickslab = c(14, "bold"),
#            font.legend = c(14, "bold")
# )
#
