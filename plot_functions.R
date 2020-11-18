age.pyramid.plot <- function(aggregated.tbl, ...){
  
  # print(nrow(aggregated.tbl))
  
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
    geom_bar(data = (aggregated.tbl %>% filter(slider_sex == "Male")), aes(x=slider_agegp10, y=-count, fill = slider_outcome), stat = "identity") +
    geom_bar(data = aggregated.tbl %>% filter(slider_sex == "Female"), aes(x=slider_agegp10, y=count, fill = slider_outcome),  stat = "identity") +
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
          axis.text.x=element_text(angle = -90, vjust = 0.5))
  
  plt
  
}


outcomes.by.admission.date.plot <- function(aggregated.tbl, embargo.limit, ...){
  
  peak.cases <- aggregated.tbl %>% group_by(year.epiweek.admit) %>% summarise(count = sum(cum.count)) %>% pull(count) %>% max()
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = year.epiweek.admit, y=cum.count, fill = slider_outcome), width = 0.95) +
    theme_bw() +
    scale_fill_brewer(palette = 'Set2', name = "Outcome", drop="F") +
    xlab("Epidemiological week of admission/symptom onset") +
    ylab("Cumulative patient records") +
    ylim(c(0, 1.05*peak.cases)) +
    scale_x_discrete(drop = F) +
    annotate(geom = "text", label = "*", x = length(levels(aggregated.tbl$year.epiweek.admit)),
             y = peak.cases, size =15) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  return(plt)
}


symptom.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    mutate(label = glue("{times.present}/{times.recorded}")) 
  
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = nice.symptom, y = proportion, fill = affected)) +
    geom_text(data = aggregated.tbl %>% filter(affected), aes(x=nice.symptom, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Symptom") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), name = "Symptom\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}

comorbidity.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    mutate(label = glue("{times.present}/{times.recorded}")) 
  
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
    mutate(label = glue("{times.present}/{times.recorded}")) 
  
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

###################

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

