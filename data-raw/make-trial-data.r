library(haven)
library(dplyr)
library(here)
library(usethis)
library(tidyr)

# Adverse Events
ae <- read_sas(here::here("data-raw/adam-test-raw/ae.sas7bdat"))

ae <- ae %>%
  rename(usubjid = PtID, ae = AE, ae_type = AEType, grade = Grade, 
         ae_duration = AEDuration, ae_day = AEDay, ae_treat = AETreat, 
         os_days = osdays) %>%
  select(usubjid, ae, ae_type, grade, ae_day, ae_duration, ae_treat) %>%
  mutate(ae_duration = as.numeric(ae_duration),
         ae_treat = as.logical(as.numeric(ae_treat)),
         grade = as.integer(grade))

ae <- left_join(ae, ae %>% group_by(usubjid) %>% summarize(ae_count = n()))

attributes(ae$ae_duration)$label <- "Adverse Event Duration"
attributes(ae$ae_treat)$label <- "Was the Adverse Event Treated?"
attributes(ae$grade)$label <- "Adverse Event Grade?"
attributes(ae$ae_count)$label <- "Total Patient Adverse Events"

attributes(ae$ae_duration)$format.sas <- "numeric"
attributes(ae$ae_treat)$format.sas <- "logical"
attributes(ae$grade)$format.sas <- "numeric"
attributes(ae$ae_count)$format.sas <- "integer"
attributes(ae$ae)$format.sas <- "character"
attributes(ae$ae_type)$format.sas <- "character"

unlist(lapply(ae, function(x) attributes(x)$label))

lc_adverse_events <- ae

use_data(lc_adverse_events, overwrite = TRUE)

# Biomarkers

bio <- read_sas(here::here("data-raw/adam-test-raw/biomarker.sas7bdat"))

bio <- bio %>%
  rename(usubjid = PtID, egfr_mutation = TP53, smoking = Smoking,
         ecog = ECOG, prior_resp = PriorResp) %>%
  select(usubjid, egfr_mutation, smoking, ecog, prior_resp) 

bio$egfr_mutation[bio$egfr_mutation == ""] <- NA
bio$egfr_mutation[bio$egfr_mutation == "1"] <- "unknown"
bio$egfr_mutation[bio$egfr_mutation == "2"] <- "negative"
bio$egfr_mutation[bio$egfr_mutation == "3"] <- "positive"

bio$smoking[bio$smoking == "0"] <- "never smoked"
bio$smoking[bio$smoking == "1"] <- "former smoker"
bio$smoking[bio$smoking == "2"] <- "current smoker"
bio$smoking[bio$smoking == "3"] <- "unknown"

bio$ecog[bio$ecog == 0] <- "fully active"
bio$ecog[bio$ecog == "1"] <- "ambulatory"

bio$prior_resp[bio$prior_resp == ".U"] <- NA
bio$prior_resp[bio$prior_resp == "1"] <- "treatment naive"
bio$prior_resp[bio$prior_resp == "2"] <- "complete response"
bio$prior_resp[bio$prior_resp == "4"] <- "partial response"
bio$prior_resp[bio$prior_resp == "5"] <- "stable disease"
bio$prior_resp[bio$prior_resp == "6"] <- "progressive disease"

attributes(bio$egfr_mutation)$format.sas <- "character"
attributes(bio$smoking)$format.sas <- "character"
attributes(bio$ecog)$format.sas <- "character"
attributes(bio$prior_resp)$format.sas <- "character"

lc_biomarkers <- bio

unlist(lapply(bio, function(x) attributes(x)$label))

use_data(lc_biomarkers, overwrite = TRUE)

# Demography

demo <- read_sas(here::here("data-raw/adam-test-raw/demo.sas7bdat"))

demo$site_id <- sample( 1:10, nrow(demo), replace = TRUE)
demo <- demo %>%
  rename(usubjid = PtID, sex = Sex, race = Race, refractory = Refractory, 
         chemo_stop = PriorTrmt, age = Age) %>%
  select(usubjid, site_id, sex, refractory, chemo_stop, age)

demo$sex[demo$sex == 1] <- "female"
demo$sex[demo$sex == 2] <- "male"

demo$refractory <- as.logical(demo$refractory)

demo$chemo_stop[demo$chemo_stop == "99"] <- NA
demo$chemo_stop[demo$chemo_stop == "2"] <- "treatment ineffective"
demo$chemo_stop[demo$chemo_stop == "1"] <- "adverse events"
demo$chemo_stop[demo$chemo_stop == "3"] <- "patient discontinued"
demo$chemo_stop[demo$chemo_stop == "4"] <- "unspecified"
demo$chemo_stop[demo$chemo_stop == ".U"] <- "unknown"

attributes(demo$chemo_stop)$format.sas <- "character"
unlist(lapply(demo, function(x) attributes(x)$label))

lc_demography <- demo
use_data(lc_demography, overwrite = TRUE)

# Response

demo <- read_sas(here::here("data-raw/adam-test-raw/demo.sas7bdat"))

lc_adsl <- demo %>%
  rename(usubjid = PtID, best_response = BestResp, pfs_days = pfsdays,
         os_days = osdays) %>%
  select(usubjid, best_response, pfs_days, pfs_censor, os_days, os_censor)

lc_adsl$chemo_stop <- sample(lc_demography$chemo_stop, nrow(lc_adsl))

lc_adsl$best_response <- 
  sample(c("Progressive Disease", "Stable Disease", "Partial Response",
           "Complete Response"), nrow(lc_adsl), replace = TRUE)

attributes(lc_adsl$best_response)$label <- "Best Recist Response"

lc_adsl$arm <- sample(c("treatment", "standard of care"), nrow(lc_adsl),
                      replace = TRUE)

attributes(lc_adsl)$label <- "Treatment Group"

unlist(lapply(lc_adsl, function(x) attributes(x)$label))

use_data(lc_adsl, overwrite = TRUE)

ae <- nest(lc_adverse_events, adverse_events = -c(usubjid, ae_count))

lc_trial <- full_join(lc_demography, ae, by = "usubjid") %>%
  full_join(lc_biomarkers, by = "usubjid") %>%
  full_join( lc_adsl %>% select(-chemo_stop), by = "usubjid")
