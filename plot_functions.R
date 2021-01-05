flowchart <- function(){
  
  summary_input<-summary_input%>%
    mutate(slider_icu_ever=replace(slider_icu_ever,is.na(slider_icu_ever),FALSE))
  
  grViz("digraph flowchart {
      graph [layout=dot, fontsize=12, fontname=Arial]
      
      # node definitions with substituted label text
      node [shape = rectangle, style = filled, fontname=Arial]        
      tab1 [label = '@@1', fillcolor=Navy, fontcolor=white]
      
      node [shape = plaintext, fillcolor=white] 
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      
      node [shape = diamond, fillcolor=white]
      tab4 [label = '@@4', height=2, width=2]
      tab5 [label = '@@5', height=2, width=2]
      tab6 [label = '@@6', height=2, width=2]
      
      node [shape = rectangle, fillcolor= lightCoral] 
      tab7 [label = '@@7',height=1, width=2]
      tab8 [label = '@@8',height=0.8, width=1.5]
      tab9 [label = '@@9',height=0.8, width=1.5]
      tab10 [label = '@@10',height=0.8, width=1.5]
      tab11 [label = '@@11',height=0.8, width=1.5]

      node [shape = rectangle, fillcolor= lightSalmon] 
      tab12 [label = '@@12',height=1, width=2]
      tab13 [label = '@@13',height=0.8, width=1.5]
      tab14 [label = '@@14',height=0.8, width=1.5]
      tab15 [label = '@@15',height=0.8, width=1.5]
      tab16 [label = '@@16',height=0.8, width=1.5]
      
      # edge definitions with the node IDs
      tab1 -> tab2 [tailport=e, headport=w, constraint=false]
      tab1 -> tab3 [tailport=s]
      tab2 -> tab4 [headport=n]
      tab2 -> tab5 [headport=n]
      tab3 -> tab6
      tab6 -> tab7 [label = '@@19']
      tab6 -> tab12 [label = '@@20']
      tab7 -> tab8 [label = '@@21']
      tab7 -> tab9 [label = '@@22']
      tab7 -> tab10 [label = '@@23']
      tab7 -> tab11 [label = '@@24']
      tab12 -> tab13 [label = '@@25']
      tab12 -> tab14 [label = '@@26']
      tab12 -> tab15 [label = '@@27']
      tab12 -> tab16 [label = '@@28']
      }

      [1]: paste0('All patients in ISARIC database \\n (N=', nrow(summary_input), ')')
      [2]: paste0('EXCLUDED 0%')
      [3]: paste0('ANALYSED 100%')
      [4]: paste0('<14-days follow-up\\n (N=0)')
      [5]: paste0('>14-days\\n follow-up and\\n negative or not\\n confirmed\\n (N=0)')
      [6]: paste0('>14-days\\n follow-up and\\n  laboratory-confirmed or\\n clinically-diagnosed \\n SARS-COV-2 infection \\n(N=', nrow(summary_input), ')')
      [7]: paste0('ICU/HDU \\n (N=', nrow(summary_input %>% filter(slider_icu_ever==TRUE)), ')')
      [8]: paste0('In hospital\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==TRUE & slider_outcome=='Ongoing care')),')')
      [9]: paste0('Deceased\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==TRUE & slider_outcome=='Death')), ')')
      [10]: paste0('Discharged\\n alive (N=', nrow(summary_input %>% filter(slider_icu_ever==TRUE & slider_outcome=='Discharge')), ')')
      [11]: paste0('Lost to follow-up\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==TRUE & slider_outcome=='LTFU')), ')')
      [12]: paste0('No ICU/HDU or ICU/HDU\\nstatus unknown\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==FALSE)),')')
      [13]: paste0('In hospital\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==FALSE & slider_outcome=='Ongoing care')),')')
      [14]: paste0('Deceased\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==FALSE & slider_outcome=='Death')), ')')
      [15]: paste0('Discharged\\n alive (N=', nrow(summary_input %>% filter(slider_icu_ever==FALSE  & slider_outcome=='Discharge')), ')')
      [16]: paste0('Lost to follow-up\\n (N=', nrow(summary_input %>% filter(slider_icu_ever==FALSE & slider_outcome=='LTFU')), ')')
      [17]: '0%'
      [18]: '0%'
      [19]: paste0(round(prop.table(table(summary_input$slider_icu_ever))[2]*100,0), '%')
      [20]: paste0(round(prop.table(table(summary_input$slider_icu_ever))[1]*100,0), '%')
      [21]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==TRUE) %$% table(slider_outcome))[4]*100,0), '%')
      [22]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==TRUE) %$% table(slider_outcome))[1]*100,0), '%')
      [23]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==TRUE) %$% table(slider_outcome))[2]*100,0), '%')
      [24]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==TRUE) %$% table(slider_outcome))[3]*100,0), '%')
      [25]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==FALSE) %$% table(slider_outcome))[4]*100,0), '%')
      [26]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==FALSE) %$% table(slider_outcome))[1]*100,0), '%')
      [27]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==FALSE) %$% table(slider_outcome))[2]*100,0), '%')
      [28]: paste0(round(prop.table(summary_input %>% filter(slider_icu_ever==FALSE) %$% table(slider_outcome))[3]*100,0), '%')

      "
  )
}


age.pyramid.plot <- function(aggregated.tbl, ...){
  # print(nrow(aggregated.tbl))

  # @todo get rid of this at the aggregation stage
  aggregated.tbl <- aggregated.tbl %>% filter(!is.na(slider_agegp10))
  
  
  aggregated.tbl <- aggregated.tbl %>%
    group_by(slider_agegp10, slider_sex, slider_outcome) %>%
    summarise(count = sum(count)) 
  
  max.count = aggregated.tbl %>% group_by(slider_agegp10, slider_sex) %>% summarise(sac = sum(abs(count))) %>% pull(sac) %>% max()
  
  order.of.magnitude <- ceiling(log10(max.count))
  
  if(as.numeric(substr(as.character(max.count), 1, 1)) > 5){
    tick.increment <- 10^(order.of.magnitude-1)
  } else {
    tick.increment <- 10^(order.of.magnitude-1)/2
  }
  
  plot.breaks <- seq(-(ceiling(max.count/tick.increment)*tick.increment), ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)
  plot.labels <- as.character(c(rev(seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by = tick.increment)),
                                0,
                                seq(tick.increment, ceiling(max.count/tick.increment)*tick.increment, by= tick.increment)))
  
  plt <- ggplot() +
    geom_bar(data = (aggregated.tbl %>% filter(slider_sex == "Male")), aes(x=slider_agegp10, y=-count, fill = slider_outcome), stat = "identity", col = NA) +
    geom_bar(data = aggregated.tbl %>% filter(slider_sex == "Female"), aes(x=slider_agegp10, y=count, fill = slider_outcome),  stat = "identity", col = NA) +
    coord_flip(clip = 'off') +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F") +
    xlab("Age group") +
    ylab("Number of patients") +
    scale_x_discrete(drop = "F") +
    scale_y_continuous(
      # currently in hard-coded increments of 5. @todo make this better
      breaks = plot.breaks,
      labels = plot.labels,
      limits = c(-1.1*max.count, 1.1*max.count)) +
    annotation_custom(
      grob = textGrob(label = "Males", hjust = 0.5, gp = gpar(cex = 1.5)),
      ymin = -max.count/2,
      ymax = -max.count/2,
      xmin = length(levels(aggregated.tbl$slider_agegp10))+1.5 ,
      xmax = length(levels(aggregated.tbl$slider_agegp10))+1.5) +
    annotation_custom(
      grob = textGrob(label = "Females", hjust = 0.4, gp = gpar(cex = 1.5)),
      ymin = max.count/2,
      ymax = max.count/2,
      xmin = length(levels(aggregated.tbl$slider_agegp10))+1.5,
      xmax = length(levels(aggregated.tbl$slider_agegp10))+1.5) +
    theme(plot.margin=unit(c(30,5,5,5.5,5.5),"pt"),
          axis.text.x=element_text(angle = 90, vjust = 0.5))
  
  plt
  
}


outcomes.by.admission.date.plot <- function(aggregated.tbl, embargo.limit, ...){
  
  aggregated.tbl <- aggregated.tbl %>%
    group_by(year.epiweek.admit) %>%
    filter(calendar.year.admit == max(calendar.year.admit)) %>%
    filter(calendar.month.admit == max(calendar.month.admit)) %>%
    ungroup() %>%
    as_tibble() %>%
    complete(
      slider_sex,
      nesting(slider_agegp10, lower.age.bound, upper.age.bound),
      slider_country,
      nesting(
        calendar.year.admit,
        calendar.month.admit,
        year.epiweek.admit
      ),
      slider_outcome,
      slider_icu_ever,
      fill = list(count = 0)
    ) %>%
    arrange(year.epiweek.admit) %>%
    as.data.table() %>%
    lazy_dt(immutable = FALSE) %>%
    group_by(
      slider_sex,
      slider_outcome,
      slider_country,
      slider_agegp10,
      lower.age.bound,
      upper.age.bound,
      slider_icu_ever
    ) %>%
    mutate(cum.count = cumsum(count)) %>%
    ungroup() %>%
    filter(cum.count > 0) %>%
    mutate(temp.cum.count = cum.count) %>%
    select(-cum.count) %>%
    group_by(year.epiweek.admit, slider_outcome) %>%
    summarise(cum.count = sum(temp.cum.count)) %>%
    as_tibble()
  
  
  
  peak.cases <- aggregated.tbl %>% group_by(year.epiweek.admit) %>% summarise(count = sum(cum.count)) %>% pull(count) %>% max()
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = year.epiweek.admit, y=cum.count, fill = slider_outcome), width = 0.95) +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F") +
    xlab("Epidemiological week of admission/symptom onset") +
    ylab("Cumulative patient records") +
    scale_x_discrete(drop = F) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  return(plt)
}


prevalence.plot.preparation <- function(aggregated.tbl, variable.name){
  
  aggregated.tbl %>%
    group_by(!!!syms(variable.name)) %>%
    summarise(
      times.present = sum(times.present),
      times.recorded = sum(times.recorded)
    ) %>%
    mutate(p.present = times.present / times.recorded) %>%
    mutate(p.absent = 1 - p.present) %>%
    as.data.table() %>%
    dt_pivot_longer(c(p.present, p.absent),
                    names_to = "affected",
                    values_to = "proportion") %>%
    lazy_dt(immutable = FALSE) %>%
    mutate(affected = affected == "p.present") %>%
    filter(!is.nan(proportion)) %>%
    as_tibble() %>%
    mutate(label = glue("{times.present} / {times.recorded}"))
  
}

symptom.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    prevalence.plot.preparation(variable.name = "nice.symptom")
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = nice.symptom, y = proportion, fill = affected)) +
    geom_text(data = aggregated.tbl %>% filter(affected), aes(x=nice.symptom, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Symptom") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    aes(x = fct_reorder(nice.symptom, proportion, .desc = TRUE)) +
    scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), name = "Symptom\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}

comorbidity.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    prevalence.plot.preparation(variable.name = "nice.comorbidity")
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = nice.comorbidity, y = proportion, fill = affected)) +
    geom_text(data = aggregated.tbl %>% filter(affected), aes(x=nice.comorbidity, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Comorbidity") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("indianred1", "indianred4"), name = "Condition\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}


treatment.prevalence.plot <- function(aggregated.tbl, icu = FALSE, ...){
  aggregated.tbl <- aggregated.tbl %>%
    prevalence.plot.preparation(variable.name = "nice.treatment")
  
  if(icu){
    colours <- c("darkorchid2", "darkorchid4")
  } else {
    colours <- c("chartreuse2", "chartreuse4")
  }
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = nice.treatment, y = proportion, fill = affected)) +
    geom_text(data = aggregated.tbl %>% filter(affected), aes(x=nice.treatment, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Treatment") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = colours, name = "Treatment\nreceived", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}

upset.plot <- function(aggregated.tbl, which.plot = "comorbidity", ...){
  
  colour = case_when(which.plot == "comorbidity" ~ "indianred3",
                     which.plot == "symptom" ~ "deepskyblue3",
                     which.plot == "treatment" ~ "chartreuse3",
                     which.plot == "icu.treatment" ~ "darkorchid4")
  
  unspun.table <- aggregated.tbl %>% nest(data = c(slider_sex,
                                                   slider_country,
                                                   slider_icu_ever,
                                                   slider_outcome,
                                                   slider_monthyear,
                                                   slider_agegp10,
                                                   which.present,
                                                   lower.age.bound,
                                                   upper.age.bound)) %>%
    mutate(repdata = map2(count, data, function(c,d){
      d %>% slice(rep(1:nrow(d), c))
    })) %>%
    select(-data) %>%
    unnest(repdata) %>%
    select(-count)
  
  
  ggplot(unspun.table, aes(x = which.present)) +
    geom_bar(aes(y=..count../sum(..count..)), fill = colour) +
    theme_bw() +
    xlab("Conditions present at admission") +
    ylab("Proportion of patients") +
    scale_x_upset()
}
######Plots for the vs data############


p_resp <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10)) +xlab("Age groups") + ylab("Respiratory rate (min)") + 
    theme_bw() + labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_hr <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))+xlab("Age groups") + ylab("Heart rate (min)") + 
    theme_bw() + labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_temp <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10)) +xlab("Age groups") + ylab("Temperature (Celsius)") + 
    theme_bw() + labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F)
}

p_sysbp <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))+xlab("Age groups") + ylab("Systolic blood pressure (mmHg)") + 
    theme_bw() + labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_oxysat <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("Oxygen saturation in room air (%)") +
    geom_text(aes(label=..count..), y=0, stat='count', colour="red", size=4)+
    theme_bw() + labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

######Plots for the lab data############


p_lab_crp <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10)) +xlab("Age groups") + ylab("CRP (mg/L)") + 
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_lym <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))+xlab("Age groups") + ylab("Lymphocytes (10^9/L)") + 
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_neut <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10)) +xlab("Age groups") + ylab("Neutrophils (10^9/L)") + 
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_wbc <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=slider_agegp10, y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))+xlab("Age groups") + ylab("WCC (10^9/L)") + 
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_urean <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("Urea (mmol/L)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_pt <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("Protrombin time (s)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_alt <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("ALT (units/L)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_aptt <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("APTT (s)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_bili <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("Bilirubin (mmol/L)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}

p_lab_ast <- function(aggregated.tbl){
  N = nrow(aggregated.tbl)
  
  ggplot(data = aggregated.tbl, aes(x=factor(slider_agegp10), y=value)) + 
    geom_boxplot(aes(fill=slider_agegp10))  + xlab("Age groups") + ylab("AST (units/L)") +
    theme_bw() +labs(title = N) + 
    scale_fill_brewer(palette = "Set3", guide = F) 
}


###########################################################
length.of.stay.sex.plot <- function(aggregated.tbl, ...){
  plt <- ggplot(aggregated.tbl, aes(x = sex, y = length.of.stay, fill=sex)) +
    geom_violin(trim=F)+
    geom_boxplot(width=0.1, fill="white", outlier.shape = NA)  +
    scale_fill_viridis(drop = F, discrete = "true", option = "magma", begin = 0.25, end = 0.75) +
    labs(title=" ", x="Sex", y = "Length of hospital stay", fill="Sex") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +  ylim(c(0,max(aggregated.tbl$length.of.stay))) + xlim(c("Male","Female"))+
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  plt
}


length.of.stay.age.plot <- function(aggregated.tbl, ...){
  plt <- ggplot(aggregated.tbl, aes(x = agegp10, y = length.of.stay, fill=agegp10)) +
    geom_violin(trim=F) +
    geom_boxplot(width=0.05, fill="white", outlier.shape = NA)  +
    labs(title="  ", x="Age group", y = "Length of hospital stay", fill="Age") +
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) + #ylim(0, length(0, max(aggregated.tbl$length.of.stay))+5) +
    scale_fill_viridis(option = "magma", discrete = T, drop = F, begin = 0.4, end = 1) +
    scale_x_discrete(drop = F) +
    ylim(c(0,max(aggregated.tbl$length.of.stay))) +xlim(unique(length.of.stay.age.input$agegp10) %>% sort())+
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"), panel.background = element_rect(fill = 'white', colour = 'white'), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),  axis.line = element_line(colour = "black"), panel.border = element_rect(colour = 'black', fill = NA, size=1) )
  
  plt
}

admission.to.icu.plot <- function(aggregated.tbl,...){
  plt <-  ggplot(aggregated.tbl, aes(x = admission.to.icu)) +
    geom_histogram(aes(y=..density..), fill="lightblue", binwidth = 1)  +
    labs(title="  ", x="Time (in days) from admission to ICU", y = "Density") +
    theme_bw() + 
    theme(
      plot.title = element_text( size=14, face="bold", hjust = 0.5),
      axis.title.x = element_text( size=12),
      axis.title.y = element_text( size=12)
    ) +
    scale_x_continuous(limits = c(-1,20), breaks =  seq(0,20,1)) 
  
  plt
}

status.by.time.after.admission.plot <- function(aggregated.tbl, ...){
  
  plt <-  ggplot(aggregated.tbl)+ 
    geom_bar(aes(x = day, fill = status), position = "fill") +
    scale_fill_brewer(palette = "Dark2", name  = "Status", drop = F, labels = c("Discharged", "Transferred","Unknown", "Ongoing care", "Ward", "ICU", "Death")) +
    theme_bw() +
    xlab("Days relative to admission") +
    annotate(geom = "segment", x = 14.5, xend = 14.5, y = 0, yend = 1) +
    ylab("Proportion")
  
  plt
}

length.of.stay.icu.plot <- function(aggregated.tbl,...){
  plt <- ggplot(data = aggregated.tbl, aes(x = type, y = dur, fill = type)) +
    geom_violin(trim = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = c("darkorchid2", "darkorchid4")) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA)  +
    labs(title = " ", x = "Location", y = "Length of stay (days)") +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                      colour = "grey"),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                      colour = "grey"),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
  plt
}

patient.by.country.plot <- function(aggregated.tbl,...){
  
  aggregated.tbl <- aggregated.tbl %>%   mutate(Country = slider_country) %>%
    group_by(Country) %>%
    summarise(count = n()) 
  
  plt <- ggplot(data=aggregated.tbl) +
    geom_col(aes(x = Country, y=count), fill="turquoise4") +
    geom_text(aes(x=Country, y= 0.95*(count), label=count), size=3, angle = 90, hjust = 1, col ="white") +
    theme_bw() +
    scale_x_discrete(expand = c(0,1.5))+
    ylab("Patient records (pseudo log scale)") +
    theme(axis.text.x = element_text(angle = 45, hjust=1, size = 10), axis.title.x=element_blank()) +
    scale_y_continuous( trans = pseudo_log_trans(), expand = c(0,0.1), breaks = c(0,1, 10, 100, 1000, 10000, 100000), minor_breaks = NULL)
  plt
}

############################################
#' @export plot.prop.by.age
#' @keywords internal
############################################
plot.prop.by.age <- function(data, var, name, ymax = 1, sz = 750, condition.in.label = TRUE, ...) {
  data2 <- data
  summ <- data2 %>%
    add_column(All = 1)%>%
    add_column(a = var) %>%
    filter(!is.na(a)) %>%
    group_by(slider_agegp10) %>%
    dplyr::summarise(
      All = sum(All, na.rm = TRUE),
      v = sum(a, na.rm = TRUE)
    )
  d <- binom.confint(summ$v, summ$All, conf.level = .95, method = "exact")
  d$X <- summ$slider_agegp10
  d$lbl <- paste(d$x, d$n, sep = "/\n", collapse = NULL)
  censored.lbl <- paste("-", d$n, sep = "/\n", collapse = NULL)
  d$lbl[d$x <= 5] <- censored.lbl[d$x <= 5]
  d$size <- d$n / sz
  #xlabs <- c(
  #  "<10",
  # "10-",
  #  "20-",
  # "30-",
  #  "40-",
  #  "50-",
  #  "60-",
  #  "70-",
  #  "80-",
  #  expression(phantom(x) >= 90)
  #)
  N <- paste("N = ", sum(summ$All), sep = "", collapse = NULL)
  pts <- geom_point(
    data = d,
    aes(x = d$X, y = mean),
    shape = "square",
    size = (d$size)/5,
    colour = "navy"
  )
  lines <- geom_linerange(
    data = d,
    aes(x = X, ymin = lower, ymax = upper),
    colour = "#000000",
    show.legend = FALSE
  )
  xa <- scale_x_discrete(
    name = "Age group (years)"#,
    #labels = xlabs
  )
  ya <- scale_y_continuous(
    name = name,
    limits = c(0, ymax)
  )
  #  lbls <- geom_text(
  #    data = d,
  #    aes(x = X, y = ymax, label = lbl),
  #    size = 2
  #  )
  p <- ggplot() +
    pts +
    lines +
    #    lbls +
    xa + ya +
    theme_bw() + theme(axis.text = element_text(size = 13, angle = 90, hjust = 1)) + 
    labs(title = N)
  
  return(p)
  
}
#data_plot_comorbid_asthma
plot.prop.by.age_comorbid_asthma <- function(data_plot_comorbid_asthma, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_asthma, data_plot_comorbid_asthma$value,
                   ifelse(full.label, "Proportion with asthma", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_malignant_neoplasm
plot.prop.by.age_comorbid_malignant_neoplasm <- function(data_plot_comorbid_malignant_neoplasm, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_malignant_neoplasm, data_plot_comorbid_malignant_neoplasm$value,
                   ifelse(full.label, "Proportion with malignancy", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_obesity
plot.prop.by.age_comorbid_obesity <- function(data_plot_comorbid_obesity, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_obesity, data_plot_comorbid_obesity$value,
                   ifelse(full.label, "Proportion with obesity", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_diabetes
plot.prop.by.age_comorbid_diabetes <- function(data_plot_comorbid_diabetes, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_diabetes, data_plot_comorbid_diabetes$value,
                   ifelse(full.label, "Proportion with diabetes mellitus", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_dementia
plot.prop.by.age_comorbid_dementia <- function(data_plot_comorbid_dementia, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_dementia, data_plot_comorbid_dementia$value,
                   ifelse(full.label, "Proportion with dementia", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_smoking
plot.prop.by.age_comorbid_smoking <- function(data_plot_comorbid_smoking, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_smoking, data_plot_comorbid_smoking$value,
                   ifelse(full.label, "Proportion with currently smoke", "Proportion" ), ymax = 0.7)
}
#data_plot_comorbid_hypertension
plot.prop.by.age_comorbid_hypertension <- function(data_plot_comorbid_hypertension, full.label = TRUE){
  plot.prop.by.age(data_plot_comorbid_hypertension, data_plot_comorbid_hypertension$value,
                   ifelse(full.label, "Proportion with hypertension", "Proportion" ), ymax = 0.7)
}



#data_plot_symptoms_history_of_fever
plot.prop.by.age_symptoms_history_of_fever <-  function(data_plot_symptoms_history_of_fever, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_history_of_fever, data_plot_symptoms_history_of_fever$value,
                   ifelse(full.label, "Proportion with fever", "Proportion" ))
}
#data_plot_symptoms_cough
plot.prop.by.age_symptoms_cough <-  function(data_plot_symptoms_cough, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_cough, data_plot_symptoms_cough$value,
                   ifelse(full.label, "Proportion with cough", "Proportion" ))
}
#data_plot_symptoms_cough_fever
plot.prop.by.age_symptoms_cough_fever <-  function(data_plot_symptoms_cough_fever, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_cough_fever, data_plot_symptoms_cough_fever$value,
                   ifelse(full.label, "Proportion with fever or cough", "Proportion" ))
}
#symptoms_symptoms_shortness_of_breath
plot.prop.by.age_symptoms_shortness_of_breath <-  function(data_plot_symptoms_shortness_of_breath, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_shortness_of_breath, data_plot_symptoms_shortness_of_breath$value,
                   ifelse(full.label, "Proportion with shortness of breath", "Proportion" ))
}
#data_plot_symptoms_cought_fever_shortness_of_breath
plot.prop.by.age_symptoms_cought_fever_shortness_of_breath <-  function(data_plot_symptoms_cought_fever_shortness_of_breath, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_cought_fever_shortness_of_breath, 
                   data_plot_symptoms_cought_fever_shortness_of_breath$value,
                   ifelse(full.label, "Proportion with cough, fever, or shortness of breath", "Proportion" ))
}
#data_plot_symptoms_upper_respiratory_tract_symptoms
plot.prop.by.age_symptoms_upper_respiratory_tract_symptoms <-  function(data_plot_symptoms_upper_respiratory_tract_symptoms, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_upper_respiratory_tract_symptoms, data_plot_symptoms_upper_respiratory_tract_symptoms$value,
                   ifelse(full.label, "Proportion with upper respiratory tract symptoms", "Proportion" ))
}
#data_plot_symptoms_altered_consciousness_confusion
plot.prop.by.age_symptoms_altered_consciousness_confusion <-  function(data_plot_symptoms_altered_consciousness_confusion, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_altered_consciousness_confusion, data_plot_symptoms_altered_consciousness_confusion$value,
                   ifelse(full.label, "Proportion with confusion", "Proportion" ))
}
#data_plot_symptoms_constitutional
plot.prop.by.age_symptoms_constitutional <-  function(data_plot_symptoms_constitutional, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_constitutional, data_plot_symptoms_constitutional$value,
                   ifelse(full.label, "Proportion with constitutional symptoms", "Proportion" ))
}
#data_plot_symptoms_vomiting_nausea
plot.prop.by.age_symptoms_vomiting_nausea <-  function(data_plot_symptoms_vomiting_nausea, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_vomiting_nausea, data_plot_symptoms_vomiting_nausea$value,
                   ifelse(full.label, "Proportion with nausea or vomiting", "Proportion" ))
}
#data_plot_symptoms_diarrhoea
plot.prop.by.age_symptoms_diarrhoea <-  function(data_plot_symptoms_diarrhoea, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_diarrhoea, data_plot_symptoms_diarrhoea$value,
                   ifelse(full.label, "Proportion with diarrhoea", "Proportion" ))
}
#data_plot_symptoms_abdominal_pain
plot.prop.by.age_symptoms_abdominal_pain <-  function(data_plot_symptoms_abdominal_pain, full.label = TRUE){
  plot.prop.by.age(data_plot_symptoms_abdominal_pain, data_plot_symptoms_abdominal_pain$value,
                   ifelse(full.label, "Proportion with abdominal pain", "Proportion" ))
}
######################
#######Heatmap graph
######################
heatmap_plot <- function(data_plot_heatmap){
  ggplot(data_plot_heatmap) +
    geom_tile(aes(x=label.x, y=label.y, fill=phi.correlation)) +
    scale_fill_gradient2(low = "deepskyblue3", mid = "white", high = "indianred3",
                         name = "phi coefficient", limits = c(-1,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          text = element_text(size=9),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_fixed()
}




tables_supplementary <- function(table_sup, title_table_1 = NA, title_table_2 = NA){
  table_example <- flextable(table_sup)
  table_example <- fontsize(table_example, i = NULL, j = NULL, size = 8, part = "header")
  table_example <- fontsize(table_example, i = NULL, j = NULL, size = 8, part = "body")
  if(!is.na(title_table_2)){
    table_example <- add_header_lines(table_example, values = title_table_2, top = TRUE)
  }
  if(!is.na(title_table_1)){
    table_example <- add_header_lines(table_example, values = title_table_1, top = TRUE)
  }
  table_example <- autofit(table_example, add_w = 0.1, add_h = 0.1, part = c("body", "header"))

  table_example
}
