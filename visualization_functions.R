options(
  reactable.theme = reactableTheme(
    color = "#333",
    headerStyle = list(
      borderBottom = "2px solid black"
    ),
    tableStyle = list(
      borderTop = "2px solid black",
      borderBottom = "2px solid black"
    )
  )
)


theme_islr = function(){ 
  
  theme_bw(base_size = 14, base_family = "Roboto") %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.caption = element_text(           #caption
        hjust = 1),               #right align
      
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

rt_caption = function(text, tab_num){
  
  p(class = "caption",
      span(class = "tab-num",
           str_c("TABLE ", tab_num, ".")
           ),
      em(
        HTML(text)
      )
      )
}

prep_reg_table = function(reg_model){
  summary(reg_model)$coefficients %>% 
    as_tibble(rownames = "term") %>% 
    rename(Coefficient = Estimate, 
           `t-statistic` = `t value`, 
           pval = `Pr(>|t|)`) %>% 
    mutate(term = str_replace_all(term, c("\\:" = " X ",
                                          "I(?=\\()|[\\(\\)]" = "")),
           `p-value` = case_when(pval < .0001 ~ "<0.0001",
                                 T ~ str_c(round(pval, 4)))) %>%
    select(-pval)
}

data_colour <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "#B44C1C", x)
  } else if (knitr::is_html_output()) {
    sprintf("<strong><span style='font-family:monospace; color: %s;'>%s</span></strong>", "#B44C1C", 
            x)
  } else 
    strong(
      span(class = "data-colour",
           x)
    )
}

keyword_colour <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "#1188ce", x)
  } else if (knitr::is_html_output()) {
    sprintf("<strong><span style='font-family:monospace; color: %s;'>%s</span></strong>", "#1188ce", 
            x)
  } else x
}
