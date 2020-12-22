library(extrafont)
library(tidyverse)
library(ISLR)
library(here)
library(reactable)
library(htmltools)
library(patchwork)

knitr::opts_chunk$set(echo=T, warning=F, message=F, fig.align='center')

keyword_colour <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "#1188ce", x)
  } else if (knitr::is_html_output()) {
    sprintf("<strong><span style='font-family:monospace; color: %s;'>%s</span></strong>", "#1188ce", 
            x)
  } else x
}

source("visualization_functions.R")
