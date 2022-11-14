#' Description
#'
linearSurvivalAge = function(donor_data){

  inter = dplyr::filter(donor_data, donor_data$donor_vital_status == "deceased")
  survival_days = inter$donor_survival_time
  age_at_diagnosis = inter$donor_age_at_diagnosis
  inter = lm(survival_days ~ age_at_diagnosis)
  plot(age_at_diagnosis, survival_days, xlab = "Donor's Age at Diagnosis", ylab = "Donor's survival time(days)") + abline(inter)


  return(result)
}
