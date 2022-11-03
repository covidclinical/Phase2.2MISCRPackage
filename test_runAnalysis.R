dir.input = "/4ceData/Input/4CE_MISC_15SEP2022_V2"
dir.output = "/4ceData/outputMISC"
obfuscation = 3
raceAvailable= TRUE
country = "US"
verbose = TRUE
dateFormat = "%Y-%m-%d"
data_update_date = "2022-06-01"
cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# # comment to compile
FourCePhase2.2MISC::runAnalysis( dir.input = "/4ceData/Input/4CE_MISC_15SEP2022_V2",
                                 dir.output = "/4ceData/outputMISC",
                                 obfuscation = 3,
                                 raceAvailable= TRUE,
                                 country = "US",
                                 verbose = TRUE,
                                 dateFormat = "%Y-%m-%d",
                                 data_update_date = "2022-06-01")

###


# code to test edge cases

head(misc_complete)
misc_complete_original <- misc_complete

# test if a site does not have 3 variants
misc_complete <- misc_complete_original
misc_complete <- misc_complete %>% filter(variant_misc == "Alpha")


# test if a site does not have 3 variants
# if there is at least 2, run the statistical test, implement this test case
misc_complete_original <- misc_complete
misc_complete <- misc_complete %>% filter(variant_misc %in% c("Alpha", "Delta"))


# test if there are labs with no values for certain variants
# example: 1751-7 (albumin)
misc_complete <- misc_complete_original
misc_complete[misc_complete$concept_code == '1751-7' & misc_complete$variant_misc == 'Delta', "value"] <- 99999
misc_complete <- misc_complete %>% filter(value != 99999)
