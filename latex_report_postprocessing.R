library(rmarkdown)
library(tidyverse)
library(glue)
library(tinytex)
#install_tinytex()


# run the old version
render("Static_report.Rmd", "pdf_document")


#read the TeX file
tex.lines <- read_lines("Static_report.tex")


# Replace figure 6
fig6.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of symptoms seen at admission amongst COVID-19 patients"))

old.fig6 <- tex.lines[fig6.line.no]

fig6.caption.and.label <- str_match(old.fig6, ".*(\\\\caption.*)")[,2]

new.fig6 <- str_replace(old.fig6, fixed(fig6.caption.and.label), "")

interfigure.6.a <- paste0(fig6.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")
interfigure.6.b <- paste0("\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig6 <- str_replace(new.fig6, fixed("\\newline"), interfigure.6.a)
new.fig6 <- str_replace(new.fig6, fixed("\\newline"), interfigure.6.b)

tex.lines[fig6.line.no] <- new.fig6
tex.lines <- append(tex.lines, "\\setcounter{figure}{6}", after = fig6.line.no )



# Replace figure 10
fig10.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of comorbidities or other concomitant conditions"))

old.fig10 <- tex.lines[fig10.line.no]

fig10.caption.and.label <- str_match(old.fig10, ".*(\\\\caption.*)")[,2]

new.fig10 <- str_replace(old.fig10, fixed(fig10.caption.and.label), "")

interfigure.10 <- paste0(fig10.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig10 <- str_replace(new.fig10, fixed("\\newline"), interfigure.10)

tex.lines[fig10.line.no] <- new.fig10

tex.lines <- append(tex.lines, "\\setcounter{figure}{10}", after = fig10.line.no )


# Replace figure 14
fig14.line.no <- which(startsWith(tex.lines, "\\subfloat[Alanine transaminase (ALT"))

old.fig14 <- tex.lines[fig14.line.no]

fig14.caption.and.label <- str_match(old.fig14, ".*(\\\\caption.*)")[,2]

new.fig14 <- str_replace(old.fig14, fixed(fig14.caption.and.label), "")

interfigure.14 <- paste0(fig14.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat\\subfloat[Neutrophils")

new.fig14 <- str_replace(new.fig14, fixed("\\newline\\subfloat[Neutrophils"), interfigure.14)

tex.lines[fig14.line.no] <- new.fig14

tex.lines <- append(tex.lines, "\\setcounter{figure}{14}", after = fig14.line.no )



# Replace figure 15
fig15.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[1]

old.fig15 <- tex.lines[fig15.line.no]

fig15.caption.and.label <- str_match(old.fig15, ".*(\\\\caption.*)")[,2]

new.fig15 <- str_replace(old.fig15, fixed(fig15.caption.and.label), "")

interfigure.15 <- paste0(fig15.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig15 <- str_replace(new.fig15, fixed("\\newline"), interfigure.15)

tex.lines[fig15.line.no] <- new.fig15

tex.lines <- append(tex.lines, "\\setcounter{figure}{15}", after = fig15.line.no )


# Replace figure 16
fig16.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[2]

old.fig16 <- tex.lines[fig16.line.no]

fig16.caption.and.label <- str_match(old.fig16, ".*(\\\\caption.*)")[,2]

new.fig16 <- str_replace(old.fig16, fixed(fig16.caption.and.label), "")

interfigure.16 <- paste0(fig16.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig16 <- str_replace(new.fig16, fixed("\\newline"), interfigure.16)

tex.lines[fig16.line.no] <- new.fig16

tex.lines <- append(tex.lines, "\\setcounter{figure}{16}", after = fig16.line.no )



write_lines(tex.lines, "Static_report_final.tex")
xelatex('Static_report_final.tex')
