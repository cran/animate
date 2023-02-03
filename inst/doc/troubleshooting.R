## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = F, eval = T------------------------------------------------------
`%+%` <- paste0
random_color <- \() {
  # colors <- c("lightblue", "orange", "lightpink")
  colors <- c("lightblue")
  "background:" %+% sample(colors, 1)
}
tag_style <- \() "padding:3px 6px; border-radius:4px;" %+% random_color()
div_style <- "margin: 10px 0px 20px 0px;"


tags <- \(...) {
  dots <- match.call(expand.dots = FALSE)$...
  dots |>
    lapply(\(x) htmltools::tags$span("#" %+% x, style = tag_style())) |>
    htmltools::tags$div(style = div_style)
}

## ---- eval = F----------------------------------------------------------------
#  device <- animate$new(width, height, attr = list(style = MY_STYLE))

## ---- eval = F----------------------------------------------------------------
#  animate_it <- function(..., width = 600, height = 600, options = click_to_play()) {
#    # Setup
#    require(animate)     # 'require' is designed for use inside functions
#    device <- animate$new(width, height, virtual = TRUE,
#                          attr = list(style = "border:1px solid lightgray"))
#    attach(device)       # Make methods of the device available in the namespace
#    pryr::f(...)()       # Main code
#    rmd_animate(device, options = options)  # Embed animated plot in R Markdown Document
#  }
#  
#  # Usage
#  animate_it({
#    id <- new_id(1:10)
#    plot(1:10, runif(10, 0, 1), id = id)
#    plot(1:10, runif(10, 0, 1), id = id, transition = TRUE)
#  })

## ---- eval = F----------------------------------------------------------------
#  setup <- function(width = 600, height = 600) {
#    require(animate)
#    device <- animate$new(width, height, attr = list(style = "border:1px solid lightgray"))
#    attach(device)
#  }
#  
#  cleanup <- function() {
#    clear()
#    off()
#    detach(device)
#  }
#  
#  
#  # Usage
#  setup()
#  id <- new_id(1:10)
#  plot(1:10, runif(10, 0, 1), id = id)
#  plot(1:10, runif(10, 0, 1), id = id, transition = TRUE)
#  cleanup()

