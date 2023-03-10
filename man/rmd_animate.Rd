% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown.R
\name{rmd_animate}
\alias{rmd_animate}
\title{In-line rendering of an animated plot in an R Markdown document}
\usage{
rmd_animate(device, ...)
}
\arguments{
\item{device}{The \link{animate} object.}

\item{...}{Optional parameters to pass to \link{insert_animate}.}
}
\description{
In-line rendering of an animated plot in an R Markdown document
}
\note{
This function should only be used in a code chunk of an R Markdown document.
}
\examples{
\donttest{
input <- tempfile(fileext = ".Rmd")
output <- tempfile(fileext = ".html")
writeLines('
```{r, echo = FALSE, message = FALSE}
# Run / include the following in a code chunk of an R Markdown document
library(animate)
device <- animate$new(500, 500, virtual = TRUE)  # set `virtual = TRUE` for R Markdown document
attach(device)

# Data
id <- new_id(1:10)
s <- 1:10 * 2 * pi / 10
s2 <- sample(s)

# Plot
par(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
plot(2*sin(s), 2*cos(s), id = id)
points(sin(s2), cos(s2), id = id, transition = list(duration = 2000))

# Render in-line in an R Markdown document
rmd_animate(device, click_to_play(start = 3))  # begin the plot at the third frame
```
```{r, echo = FALSE, message = FALSE}
par(xlim = NULL, ylim = NULL)  # Reset `xlim` and `ylim` in `par`
# Do some other plots
off()
detach(device)
```
', input)
knitr::knit(input, output)
# browseURL(output)
}

}
