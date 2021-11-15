library(rmarkdown)
library(tidyverse)
library(glue)
library(tinytex)
#install_tinytex()


# run the old version
render("Static_report.Rmd", "pdf_document")


#read the TeX file
tex.lines <- read_lines("Static_report.tex")


# Replace figure 7
fig7.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of symptoms seen at admission amongst COVID-19 patients"))

old.fig7 <- tex.lines[fig7.line.no]

fig7.caption.and.label <- str_match(old.fig7, ".*(\\\\caption.*)")[,2]

new.fig7 <- str_replace(old.fig7, fixed(fig7.caption.and.label), "")

interfigure.7.a <- paste0(fig7.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")
interfigure.7.b <- paste0("\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig7 <- str_replace(new.fig7, fixed("\\newline"), interfigure.7.a)
new.fig7 <- str_replace(new.fig7, fixed("\\newline"), interfigure.7.b)

tex.lines[fig7.line.no] <- new.fig7
tex.lines <- append(tex.lines, "\\setcounter{figure}{7}", after = fig7.line.no )



# Replace figure 11
fig11.line.no <- which(startsWith(tex.lines, "\\subfloat[Frequency of comorbidities or other concomitant conditions"))

old.fig11 <- tex.lines[fig11.line.no]

fig11.caption.and.label <- str_match(old.fig11, ".*(\\\\caption.*)")[,2]

new.fig11 <- str_replace(old.fig11, fixed(fig11.caption.and.label), "")

interfigure.11 <- paste0(fig11.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig11 <- str_replace(new.fig11, fixed("\\newline"), interfigure.11)

tex.lines[fig11.line.no] <- new.fig11

tex.lines <- append(tex.lines, "\\setcounter{figure}{11}", after = fig11.line.no )


# Replace figure 15
fig15.line.no <- which(startsWith(tex.lines, "\\subfloat[Alanine transaminase (ALT"))

old.fig15 <- tex.lines[fig15.line.no]

fig15.caption.and.label <- str_match(old.fig15, ".*(\\\\caption.*)")[,2]

new.fig15 <- str_replace(old.fig15, fixed(fig15.caption.and.label), "")

interfigure.15 <- paste0(fig15.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat\\subfloat[Neutrophils")

new.fig15 <- str_replace(new.fig15, fixed("\\newline\\subfloat[Neutrophils"), interfigure.15)

tex.lines[fig15.line.no] <- new.fig15

tex.lines <- append(tex.lines, "\\setcounter{figure}{15}", after = fig15.line.no )



# Replace figure 16
fig16.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[1]

old.fig16 <- tex.lines[fig16.line.no]

fig16.caption.and.label <- str_match(old.fig16, ".*(\\\\caption.*)")[,2]

new.fig16 <- str_replace(old.fig16, fixed(fig16.caption.and.label), "")

interfigure.16 <- paste0(fig16.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig16 <- str_replace(new.fig16, fixed("\\newline"), interfigure.16)

tex.lines[fig16.line.no] <- new.fig16

tex.lines <- append(tex.lines, "\\setcounter{figure}{16}", after = fig16.line.no )


# Replace figure 17
fig17.line.no <- which(startsWith(tex.lines, "\\subfloat[Proportions of patients receiving each treatment"))[2]

old.fig17 <- tex.lines[fig17.line.no]

fig17.caption.and.label <- str_match(old.fig17, ".*(\\\\caption.*)")[,2]

new.fig17 <- str_replace(old.fig17, fixed(fig17.caption.and.label), "")

interfigure.17 <- paste0(fig17.caption.and.label, "\\end{figure}", "\\begin{figure}\\ContinuedFloat")

new.fig17 <- str_replace(new.fig17, fixed("\\newline"), interfigure.17)

tex.lines[fig17.line.no] <- new.fig17

tex.lines <- append(tex.lines, "\\setcounter{figure}{17}", after = fig17.line.no )



write_lines(tex.lines, "Static_report_final.tex")
xelatex('Static_report_final.tex')
