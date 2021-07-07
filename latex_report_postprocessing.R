library(rmarkdown)
library(tidyverse)
library(glue)
library(tinytex)
install_tinytex()


# run the old version
render("Static_report.Rmd", "pdf_document")


#read the TeX file
tex.lines <- read_lines("Static_report.tex")


# Replace figure 5
fig5.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of symptoms seen at admission amongst COVID-19 patients"))

old.fig5 <- tex.lines[fig5.line.no]

fig5.caption.and.label <- str_match(old.fig5, ".*(\\\\caption.*)")[,2]

new.fig5 <- str_replace(old.fig5, fixed(fig5.caption.and.label), "")

interfigure.5.a <- paste0(fig5.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")
interfigure.5.b <- paste0("\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig5 <- str_replace(new.fig5, fixed("\\newline"), interfigure.5.a)
new.fig5 <- str_replace(new.fig5, fixed("\\newline"), interfigure.5.b)

tex.lines[fig5.line.no] <- new.fig5
tex.lines <- append(tex.lines, "\\setcounter{figure}{5}", after = fig5.line.no )



# Replace figure 6
fig8.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of comorbidities or other concomitant conditions"))

old.fig8 <- tex.lines[fig8.line.no]

fig8.caption.and.label <- str_match(old.fig8, ".*(\\\\caption.*)")[,2]

new.fig8 <- str_replace(old.fig8, fixed(fig8.caption.and.label), "")

interfigure.8 <- paste0(fig8.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig8 <- str_replace(new.fig8, fixed("\\newline"), interfigure.8)

tex.lines[fig8.line.no] <- new.fig8

tex.lines <- append(tex.lines, "\\setcounter{figure}{8}", after = fig8.line.no )


# Replace figure 8
fig11.line.no <- which(startsWith(tex.lines, "\\subfloat[Alanine transaminase (ALT"))

old.fig11 <- tex.lines[fig11.line.no]

fig11.caption.and.label <- str_match(old.fig11, ".*(\\\\caption.*)")[,2]

new.fig11 <- str_replace(old.fig11, fixed(fig11.caption.and.label), "")

interfigure.11 <- paste0(fig11.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat\\subfloat[Neutrophils")

new.fig11 <- str_replace(new.fig11, fixed("\\newline\\subfloat[Neutrophils"), interfigure.11)

tex.lines[fig11.line.no] <- new.fig11

tex.lines <- append(tex.lines, "\\setcounter{figure}{11}", after = fig11.line.no )



# Replace figure 9
fig12.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[1]

old.fig12 <- tex.lines[fig12.line.no]

fig12.caption.and.label <- str_match(old.fig12, ".*(\\\\caption.*)")[,2]

new.fig12 <- str_replace(old.fig12, fixed(fig12.caption.and.label), "")

interfigure.12 <- paste0(fig12.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig12 <- str_replace(new.fig12, fixed("\\newline"), interfigure.12)

tex.lines[fig12.line.no] <- new.fig12

tex.lines <- append(tex.lines, "\\setcounter{figure}{12}", after = fig12.line.no )


# Replace figure 13
fig13.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[2]

old.fig13 <- tex.lines[fig13.line.no]

fig13.caption.and.label <- str_match(old.fig13, ".*(\\\\caption.*)")[,2]

new.fig13 <- str_replace(old.fig13, fixed(fig13.caption.and.label), "")

interfigure.13 <- paste0(fig13.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig13 <- str_replace(new.fig13, fixed("\\newline"), interfigure.13)

tex.lines[fig13.line.no] <- new.fig13

tex.lines <- append(tex.lines, "\\setcounter{figure}{13}", after = fig13.line.no )



write_lines(tex.lines, "Static_report_final.tex")
xelatex('Static_report_final.tex')
