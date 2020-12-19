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

reg_table = function(reg_df){
   reg_df %>% 
    gt() %>% 
    cols_label(term ="") %>% 
    cols_align(columns = vars(`p-value`), align = "right") %>% 
    fmt_markdown(columns = vars(term)) %>% 
    fmt_number(columns = vars(Coefficient),decimals = 4) %>% 
    fmt_number(columns = vars(`Std. Error`), decimals = 3) %>% 
    fmt_number(columns = vars(`t-statistic`), decimals = 2) %>% 
    tab_style(
      style = cell_borders(
        sides = "right",
        style = "solid",
        weight = px(2)
      ),
      locations = list(
        cells_body(
          columns = vars(term)
        ),
        cells_column_labels(
          columns = vars(term)
        )
      )
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      ),
      locations = cells_body(
        columns = everything()
      )
    ) %>% 
    tab_options(
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.top.color = "black",
      column_labels.border.top.style = "solid",
      table_body.border.bottom.color = "black",
      table_body.border.bottom.style = "solid",table.font.color = "black",
      row.striping.include_table_body = F)
}