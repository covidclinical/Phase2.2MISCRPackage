
### compare table 1
# table 1 categorical

load('/4ceData/outputMISC_washout_replaceEarlier14/BCH_table1Categorical.RData')
early <- table1_categorical
load('/4ceData/outputMISC_washout_replaceLater14/BCH_table1Categorical.RData')
late <- table1_categorical
load('/4ceData/outputMISC_washout14/BCH_table1Categorical.RData')
replace <- table1_categorical
load('/4ceData/outputMISC_basic/BCH_table1Categorical.RData')
original <- table1_categorical
rm(table1_categorical)

compare_t1_cat <- left_join(early, late, by = c('categories', 'variant_misc'), suffix = c('.early', '.late')) %>%
  left_join(replace, by = c('categories', 'variant_misc')) %>%
  left_join(original, by = c('categories', 'variant_misc'), suffix = c('.replace', '.original')) %>%
  mutate(differ_from_original = (n.original != n.early) | (n.original != n.late) | (n.original != n.replace),
         ndiff_early = n.early-n.original,
         ndiff_late = n.late-n.original,
         ndiff_replace = n.replace-n.original) %>%
  filter(differ_from_original)


# table 1 continuous

load('/4ceData/outputMISC_washout_replaceEarlier14/BCH_table1Continuous.RData')
early <- table1_continuous
early$type <- 'early'
load('/4ceData/outputMISC_washout_replaceLater14/BCH_table1Continuous.RData')
late <- table1_continuous
late$type <- 'late'
load('/4ceData/outputMISC_washout14/BCH_table1Continuous.RData')
replace <- table1_continuous
replace$type <- 'replace'
load('/4ceData/outputMISC_basic/BCH_table1Continuous.RData')
original <- table1_continuous
original$type <- 'original'
rm(table1_continuous)

compare_t1_cont <- rbind(early, late, original, replace)


### compare table 2
# table 2 at admission

load('/4ceData/outputMISC_washout_replaceEarlier14/BCH_table2AtAdmission.RData')
early <- table2_admission
early$type <- 'early'
load('/4ceData/outputMISC_washout_replaceLater14/BCH_table2AtAdmission.RData')
late <- table2_admission
late$type <- 'late'
load('/4ceData/outputMISC_washout14/BCH_table2AtAdmission.RData')
replace <- table2_admission
replace$type <- 'replace'
load('/4ceData/outputMISC_basic/BCH_table2AtAdmission.RData')
original <- table2_admission
original$type <- 'original'
rm(table2_admission)

compare_t2_admission <- rbind(early, late, original, replace)


# table 2 during admission

load('/4ceData/outputMISC_washout_replaceEarlier14/BCH_table2DuringAdmission.RData')
early <- table2_during
early$type <- 'early'
load('/4ceData/outputMISC_washout_replaceLater14/BCH_table2DuringAdmission.RData')
late <- table2_during
late$type <- 'late'
load('/4ceData/outputMISC_washout14/BCH_table2DuringAdmission.RData')
replace <- table2_during
replace$type <- 'replace'
load('/4ceData/outputMISC_basic/BCH_table2DuringAdmission.RData')
original <- table2_during
original$type <- 'original'
rm(table2_during)

compare_t2_during <- rbind(early, late, original, replace)


### compare table 3

load('/4ceData/outputMISC_washout_replaceEarlier14/BCH_table3.RData')
early <- table3
early$type <- 'early'
load('/4ceData/outputMISC_washout_replaceLater14/BCH_table3.RData')
late <- table3
late$type <- 'late'
load('/4ceData/outputMISC_washout14/BCH_table3.RData')
replace <- table3
replace$type <- 'replace'
load('/4ceData/outputMISC_basic/BCH_table3.RData')
original <- table3
original$type <- 'original'
rm(table3)

compare_t3 <- left_join(early, late, by = c('categories', 'variant_misc'), suffix = c('.early', '.late')) %>%
  left_join(replace, by = c('categories', 'variant_misc')) %>%
  left_join(original, by = c('categories', 'variant_misc'), suffix = c('.replace', '.original')) %>%
  mutate(differ_from_original = (n.original != n.early) | (n.original != n.late) | (n.original != n.replace),
         ndiff_early = n.early-n.original,
         ndiff_late = n.late-n.original,
         ndiff_replace = n.replace-n.original) %>%
  filter(differ_from_original)



