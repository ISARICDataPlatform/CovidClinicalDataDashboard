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



# Replace figure 6
fig6.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of comorbidities or other concomitant conditions"))

old.fig6 <- tex.lines[fig6.line.no]

fig6.caption.and.label <- str_match(old.fig6, ".*(\\\\caption.*)")[,2]

new.fig6 <- str_replace(old.fig6, fixed(fig6.caption.and.label), "")

interfigure.6 <- paste0(fig6.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig6 <- str_replace(new.fig6, fixed("\\newline"), interfigure.6)

tex.lines[fig6.line.no] <- new.fig6



# Replace figure 8
fig8.line.no <- which(startsWith(tex.lines, "\\subfloat[Alanine transaminase (ALT"))

old.fig8 <- tex.lines[fig8.line.no]

fig8.caption.and.label <- str_match(old.fig8, ".*(\\\\caption.*)")[,2]

new.fig8 <- str_replace(old.fig8, fixed(fig8.caption.and.label), "")

interfigure.8 <- paste0(fig8.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat\\subfloat[Neutrophils")

new.fig8 <- str_replace(new.fig8, fixed("\\newline\\subfloat[Neutrophils"), interfigure.8)

tex.lines[fig8.line.no] <- new.fig8



# Replace figure 9
fig9.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[1]

old.fig9 <- tex.lines[fig9.line.no]

fig9.caption.and.label <- str_match(old.fig9, ".*(\\\\caption.*)")[,2]

new.fig9 <- str_replace(old.fig9, fixed(fig9.caption.and.label), "")

interfigure.9 <- paste0(fig9.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig9 <- str_replace(new.fig9, fixed("\\newline"), interfigure.9)

tex.lines[fig9.line.no] <- new.fig9




# Replace figure 10
fig10.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[2]

old.fig10 <- tex.lines[fig10.line.no]

fig10.caption.and.label <- str_match(old.fig10, ".*(\\\\caption.*)")[,2]

new.fig10 <- str_replace(old.fig10, fixed(fig10.caption.and.label), "")

interfigure.10 <- paste0(fig10.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig10 <- str_replace(new.fig10, fixed("\\newline"), interfigure.10)

tex.lines[fig10.line.no] <- new.fig10


write_lines(tex.lines, "Static_report_final.tex")
xelatex('Static_report_final.tex')
