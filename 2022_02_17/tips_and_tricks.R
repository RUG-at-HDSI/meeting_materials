#' ---
#' title: "Tips and Tricks That Make Me Love R"
#' author: "Christian Testa"
#' date: "February 17, 2022"
#' output: 
#'   html_document:
#'       toc: true
#'       theme: united
#' ---
#' 
#' # Intro
#' 
#' These are some tips and tricks that I love using in R — please enjoy! 
#' 
#' This document is uploaded online here: 
#'
#'    * as source code: https://github.com/RUG-at-HDSI/meeting_materials/tree/main/2022_02_17/tips_and_tricks.R
#'    * as a rendered web-page: https://rpubs.com/ctesta000/tips_and_tricks 
#'    
#' 
#' # Tip \#1 The `@examples` Tag
#' 
#' The first tip is to make use of Roxygen2 style tags, especially the
#' @examples tag. The @examples tag allows you to write commented out examples
#' that make use of your declared functions or variables without 
#' being part of the main script. Moreover, they're easy to run in 
#' RStudio because you can evaluate them with the Source button or 
#' Cmd+Enter or Ctrl+Enter. 
#' 
#' Read more here:
#' @source https://roxygen2.r-lib.org/ 
#' 
#' @examples 
#'     
#'     # code is indented so it displays as code in Rmarkdown
#'     my_summary_function(mtcars, 'cyl')
#'     my_summary_function(iris, 'Sepal.Length')
#'     my_summary_function(data.frame(x = 1), 'x')
#' 
my_summary_function <- function(df, var) {
  paste0(
    'mean: ', 
    round(mean(df[[var]]), 1),
    ', sd: ',
    round(sd(df[[var]]), 1)
  )
}


#' # Tip \#2:  Using Rmarkdown even with .R Files
#' My second tip is that you can use rmarkdown even with regular .R files!
#'
#' Check out the output of `rmarkdown::render("tips_and_tricks.R")`
#' 
#' The output is uploaded online here: https://rpubs.com/ctesta000/tips_and_tricks 
#' 
#' @source https://bookdown.org/yihui/rmarkdown-cookbook/spin.html 
#' 
#' Sometimes this can be helpful when you want to share the output of 
#' a script you have lying around, but you don't have a lot of time to 
#' go through it and break it into little ```{r} ``` chunks to follow the 
#' Rmarkdown syntax. 
#' 
#' 
#' 
#' # Tip \#3: `DT::datatable`
#' 
#' Have you ever had a table that you wanted to quickly search through in R?
#' 
#' Use `datatable` from the `DT` package! 
#' 
#' If you don't have it, install it with `install.packages("DT")`
library(DT)
datatable(mtcars)


#' # Tip \#4: Simulate Color Blindness with `colorblindr`
#' 
#' Have you worried about if the plots you make are readable for 
#' people with colorblindness? 
#' 
#' Read more here: 
#' @source https://www.nature.com/articles/nmeth.1618 
#' @source https://github.com/clauswilke/colorblindr 
#'
#' @examples 
#' 
#'     # install the package with
#'     remotes::install_github("wilkelab/cowplot") # or 
#'     install.packages("colorspace", repos = "http://R-Forge.R-project.org")
#'     
library(ggplot2)
fig <- ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
fig

library(colorblindr)
cvd_grid(fig)

#' Here we're going to specify a knitr option so this code doesn't run in the
#' Rmarkdown because it launches an interactive app.
#+ eval=FALSE
view_cvd(fig) # starts the interactive app

#' # Tip \#5: `browseVignettes` 
#' 
#' Use `browseVignettes` to access all the great explanations package 
#' authors have written to accompany their code! 
#' 
#' Almost every time I run `browseVignettes` I learn new things that I wanted
#' to know about the packages I'm using.
#' 
#+ eval=FALSE
browseVignettes('dplyr')
browseVignettes('viridis')
browseVignettes('colorblindr')
browseVignettes('rmarkdown')
browseVignettes('tidyverse')

#' # Tip \#6: `ggplotly`
#' 
#' If you don't have plotly, you can install it with `install.packages('plotly')`
#' 
#' Once you have it, you can convert many `ggplot2` visualizations into
#' interactive visualizations (with hover-text!) easily just using the 
#' `ggplotly` command.
#' 
#' @source https://plotly-r.com/improving-ggplotly.html 
#' 
#' @source https://www.rdocumentation.org/packages/plotly/versions/4.10.0/topics/ggplotly
#' 
library(plotly)

fig <- ggplot(mtcars, 
              aes(x = hp, 
                  y = mpg, 
                  label = rownames(mtcars))) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("mtcars Horsepower and MPG by # of Cylinders")

print(fig)
ggplotly(fig)



#' # Tip \#7: Use Keyboard Shortcuts in RStudio 
#' 
#' Learn about all the best ones here: 
#' 
#'   * https://datacornering.com/my-favorite-rstudio-tips-and-tricks/
#'   * https://www.rstudio.com/blog/r-markdown-tips-tricks-1-rstudio-ide/
#'   
#'   
#'   
#' # Tip \#8: Use `renv` to make more reproducible R projects 
#' 
#' Check out the details here: 
#' 
#' @source https://rstudio.github.io/renv/articles/renv.html
#' 
#+ eval=FALSE
usethis::create_package("newproject") # create a new project
renv::init()
# write your code 
renv::snapshot()


#' # Tip \#9: Just for fun, check out `txtplot`
#' 
#' Install it with `install.package("txtplot")`
library(txtplot)

## can include axis labels when desired
txtplot(cars[,1], cars[,2], xlab = "speed", ylab = "distance")

## text based density plot
txtdensity(rnorm(500))

## text based boxplots
rand1 <- rnorm(100, 1, 2)
rand2 <- rnorm(50, 2, 2)
rand3 <- rnorm(50, 2, 5)
txtboxplot(rand1, rand2, rand3)

## try this if your terminal supports shade/block characters
txtimage(datasets::volcano, alphabet = " \u2591\u2592\u2593\u2588")
