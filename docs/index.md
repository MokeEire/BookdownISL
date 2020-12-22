--- 
title: "An Introduction to Statistical Learning: with Applications in R"
author: "Gareth James, Daniela Witten, Trevor Hastie, & Robert Tibshirani"
date: "2020-12-22"
description: This is a minimal example of using the bookdown package to write a book.
  The output format for this example is bookdown::gitbook.
knit: bookdown::render_book
site: bookdown::bookdown_site
documentclass: book
link-citations: yes
bibliography:
- book.bib
biblio-style: apalike
cover-image: "images/isl_cover.jpg"
url: "http://faculty.marshall.usc.edu/gareth-james/ISL/"
github-repo: "MokeEire/BookdownISL"
css: style.css
---


# Preface {-}
***
<div style="text-align:center;">
![](https://images-na.ssl-images-amazon.com/images/I/41pP5+SAv-L._SX330_BO1,204,203,200_.jpg)
</div>

I (not an author) am compiling this book for myself as a learning exercise for both the contents of ISLR and the use of the [**bookdown**](https://bookdown.org/) package. 

From the [book's official website](https://statlearning.com/):

>This book provides an introduction to statistical learning methods. It is aimed for upper level undergraduate students, masters students and Ph.D. students in the non-mathematical sciences. The book also contains a number of R labs with detailed explanations on how to implement the various methods in real life settings, and should be a valuable resource for a practicing data scientist.

## Resources used in making this book {-}

- The first place you should go to learn about bookdown.
  - [bookdown: Authoring Books and Technical Documents with R Markdown](https://bookdown.org/yihui/bookdown/)
- Getting code folding to work using a variety of StackOverflow posts
  - [Code folding in bookdown](https://stackoverflow.com/questions/45360998/code-folding-in-bookdown)
  - [How to add code folding to output chunks in rmarkdown html documents](https://stackoverflow.com/questions/37755037/how-to-add-code-folding-to-output-chunks-in-rmarkdown-html-documents)
  - [Enable code folding in bookdown and blogdown](https://statnmap.com/2017-11-13-enable-code-folding-in-bookdown-and-blogdown/)
  
## Custom functions used throughout the book {-}

In order to make some of the tables and plots, I wrote some custom functions.
They are contained in the file [`visualization_functions.R`](https://github.com/MokeEire/BookdownISL/blob/master/visualization_functions.R), but here is a description of each function and why it is used.

### `theme_islr()` {-}

This is a plot theming function to mimic the figure style found in the book.


```r
theme_islr = function(){ 
  
  theme_bw(base_size = 14, base_family = "Roboto") %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      
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
```

### `prep_reg_table()` {-}

This function takes the output from an `lm()` object and formats it for regression tables.


```r
prep_reg_table = function(reg_model){
  # Select the coefficients object from the summary of the regression model
  summary(reg_model)$coefficients %>% 
    # Convert it to a tibble, and make the rownames a column
    as_tibble(rownames = "term") %>% 
    # Rename the columns to match ISLR's tables
    rename(Coefficient = Estimate, 
           `t-statistic` = `t value`, 
           pval = `Pr(>|t|)`) %>% 
    
    mutate(
      # Replace the shorthand formulas given to lm() with regression publishing style
      term = str_replace_all(term, c("\\:" = " X ", # Interaction terms
                                     "I(?=\\()|[\\(\\)]" = "")), # Identity function
      # Create a p-value column which shows a less than sign in cases of very small p-values
      `p-value` = case_when(pval < .0001 ~ "<0.0001",
                            T ~ str_c(round(pval, 4)))) %>%
    select(-pval)
}
```

