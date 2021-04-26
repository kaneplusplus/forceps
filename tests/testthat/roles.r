#library(survminer)
#library(survival)
#library(breastCancerNKI)
#library(dplyr)

data(nki)

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2656909/

nki <- pData(nki) %>%
  rename(
    os_time = t.os, 
    os_event = e.os, 
    mets_free_time = t.dmfs, 
    mets_free_event = e.dmfs,
    estrogen_receptor = er,
    brca_mutation = brca.mutation,
    lymph_node = node) %>%
  select(size, age, estrogen_receptor, grade, brca_mutation, mets_free_time,
         mets_free_event, os_time, os_event, treatment) %>%
  as_tibble()

ggsurvplot(
  survfit(Surv(mets_free_time, mets_free_event) ~ treatment, data = nki),
  data = nki)

nki$response <- nki$mets_free_time >= 2546 & nki$os_time >= 4000

nki <- na.omit(nki)

responses <- c(
  "Surv(mets_free_time, mets_free_event)",
  "Surv(os_time, os_event)",
  "response")

baseline_demo <- c("age", "estrogen_receptor", "brca_mutate")

baseline_tumor <- c("size", "grade")

roles <- list(
  treatment = "treatment",
  demography = baseline_demo,
  baseline_tumor = baseline_tumor,
  responses = responses)

nkia <- add_roles(nki, roles)
