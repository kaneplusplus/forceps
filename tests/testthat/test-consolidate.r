
data(lc_adsl)
data(lc_biomarkers)
data(lc_demography)
data_list <- list(demography = lc_demography, 
                  biomarkers = lc_biomarkers, 
                  adverse_events = lc_adverse_events, 
                  adsl = lc_adsl)

consolidate(data_list, on = "usubjid")
