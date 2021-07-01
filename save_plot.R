age_pyramid=age.pyramid.plot(age.pyramid.input)

patient_by_country=patient.by.country.plot(patient.by.country.input)

symptom_prevalence=symptom.prevalence.plot(symptom.prevalence.input)
symptom_upset=upset.plot(symptom.upset.input, which.plot = "symptom")
symptom_heatmap=heatmap_plot(data_plot_heatmap)

abdominal_pain=plot.prop.by.age_symptoms_abdominal_pain(data_plot_symptoms_abdominal_pain, FALSE)
altered_consciousness_confusion=plot.prop.by.age_symptoms_altered_consciousness_confusion(data_plot_symptoms_altered_consciousness_confusion, FALSE)
constitutional=plot.prop.by.age_symptoms_constitutional(data_plot_symptoms_constitutional, FALSE)
cough=plot.prop.by.age_symptoms_cough(data_plot_symptoms_cough, FALSE)
cough_fever=plot.prop.by.age_symptoms_cough_fever(data_plot_symptoms_cough_fever, FALSE)
cought_fever_shortness_of_breath=plot.prop.by.age_symptoms_cought_fever_shortness_of_breath(data_plot_symptoms_cought_fever_shortness_of_breath, FALSE)
history_of_fever=plot.prop.by.age_symptoms_history_of_fever(data_plot_symptoms_history_of_fever, FALSE)
shortness_of_breath=plot.prop.by.age_symptoms_shortness_of_breath(data_plot_symptoms_shortness_of_breath, FALSE)
upper_respiratory_tract_symptoms=plot.prop.by.age_symptoms_upper_respiratory_tract_symptoms(data_plot_symptoms_upper_respiratory_tract_symptoms, FALSE)
diarrhoea=plot.prop.by.age_symptoms_diarrhoea(data_plot_symptoms_diarrhoea, FALSE)
vomiting_nausea=plot.prop.by.age_symptoms_vomiting_nausea(data_plot_symptoms_vomiting_nausea, FALSE)

comorbidity_prevalence=comorbidity.prevalence.plot(comorbidity.prevalence.input) + ggtitle("(a)")
comorbidity_upset=upset.plot(comorbidity.upset.input, which.plot = "comorbidity") + ggtitle("(b)")

asthma=plot.prop.by.age_comorbid_asthma(data_plot_comorbid_asthma, FALSE)
dementia=plot.prop.by.age_comorbid_dementia(data_plot_comorbid_dementia, FALSE)
diabetes=plot.prop.by.age_comorbid_diabetes(data_plot_comorbid_diabetes, FALSE)
hypertension=plot.prop.by.age_comorbid_hypertension(data_plot_comorbid_hypertension, FALSE)
neoplasm=plot.prop.by.age_comorbid_malignant_neoplasm(data_plot_comorbid_malignant_neoplasm, FALSE)
obesity=plot.prop.by.age_comorbid_obesity(data_plot_comorbid_obesity, FALSE)
smoking=plot.prop.by.age_comorbid_smoking(data_plot_comorbid_smoking, FALSE)

resp=p_resp(data_plot_vs_resp)
hr=p_hr(data_plot_vs_hr)
temp=p_temp(data_plot_vs_temp)
sysbp=p_sysbp(data_plot_vs_sysbp)
oxysat=p_oxysat(data_plot_vs_oxysat)
alt=p_lab_alt(data_plot_lab_alt)
ast=p_lab_ast(data_plot_lab_ast)
bili=p_lab_bili(data_plot_lab_bili)
pt=p_lab_pt(data_plot_lab_pt)
urean=p_lab_urean(data_plot_lab_urean)
wbc=p_lab_wbc(data_plot_lab_wbc)
neut=p_lab_neut(data_plot_lab_neut)
lym=p_lab_lym(data_plot_lab_lym)
crp=p_lab_crp(data_plot_lab_crp)

treatment_prevalence=treatment.prevalence.plot(treatment.use.proportion.input, icu = FALSE) + ggtitle("(a)")
treatment_upset=upset.plot(treatment.upset.input, which.plot = "treatment") + ggtitle("(b)")
treatment_prevalence_icu=treatment.prevalence.plot(icu.treatment.use.proportion.input, icu = TRUE) + ggtitle("(a)")
treatment_upset_icu=upset.plot(icu.treatment.upset.input, which.plot = "icu.treatment") + ggtitle("(b)")

length_of_stay_icu=length.of.stay.icu.plot(length.of.stay.icu.input)
admission_to_icu=admission.to.icu.plot(admission.to.icu.input)
length_of_stay_sex=length.of.stay.sex.plot(length.of.stay.sex.input) 
length_of_stay_age=length.of.stay.age.plot(length.of.stay.age.input)
status_by_time=status.by.time.after.admission.plot(status.by.time.after.admission.input)
outcomes_by_admission=outcomes.by.admission.date.plot(outcome_admission_date_input)
case_def=plot_case_def(case.def.input)

save(age_pyramid,patient_by_country,symptom_prevalence,symptom_upset,symptom_heatmap,
     abdominal_pain,altered_consciousness_confusion,constitutional,cough,cough_fever,cought_fever_shortness_of_breath,
     history_of_fever,shortness_of_breath,upper_respiratory_tract_symptoms,diarrhoea,vomiting_nausea,
     comorbidity_prevalence,comorbidity_upset,asthma,dementia,diabetes,hypertension,neoplasm,obesity,smoking,
     resp,hr,temp,sysbp,oxysat,alt,ast,bili,pt,urean,wbc,neut,lym,crp,
     treatment_prevalence,treatment_upset,treatment_prevalence_icu,treatment_upset_icu,length_of_stay_icu,
     admission_to_icu,length_of_stay_sex,length_of_stay_age,status_by_time,case_def,
     file="plots.Rdata")
