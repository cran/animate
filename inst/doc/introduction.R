## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  remotes::install_github("kcf-jackson/animate")

## -----------------------------------------------------------------------------
#  library(animate)
#  device <- animate$new(width = 600, height = 400)  # takes ~0.5s
#  
#  device$plot(1:10, 1:10)
#  device$points(1:10, 10 * runif(10), bg = "red")
#  device$lines(1:100, sin(1:100 / 10 * pi / 2))
#  device$clear()
#  
#  device$off()  # switch off the device when you are done

## -----------------------------------------------------------------------------
#  library(animate)
#  device <- animate$new(600, 400)
#  attach(device)  # overrides the 'base' primitives
#  
#  plot(1:10, 1:10)
#  points(1:10, 10 * runif(10), bg = "red")
#  lines(1:100, sin(1:100 / 10 * pi / 2))
#  clear()
#  
#  off()
#  detach(device)  # restore the 'base' primitives

## -----------------------------------------------------------------------------
#  device <- animate$new(600, 400)
#  attach(device)

## -----------------------------------------------------------------------------
#  x <- 1:10
#  y <- 1:10
#  id <- new_id(x)   # Give each point an ID: c("ID-1", "ID-2", ..., "ID-10")
#  plot(x, y, id = id)
#  
#  new_y <- 10:1
#  plot(x, new_y, id = id, transition = TRUE)  # Use transition

## ---- eval = T, echo = F------------------------------------------------------
animate::insert_animate("introduction/basic_plot.json")

## -----------------------------------------------------------------------------
#  clear()  # Clear the canvas
#  x <- 1:10
#  y <- 10 * runif(10)
#  id <- new_id(y, prefix = "points")   # Give each point an ID
#  plot(x, y, id = id, bg = "red")
#  
#  new_y <- 10 * runif(10)
#  points(x, new_y, id = id, bg = "lightgreen", cex = 1:10 * 30, transition = list(duration = 2000))

## ---- eval = T, echo = F------------------------------------------------------
animate::insert_animate("introduction/basic_points.json")

## -----------------------------------------------------------------------------
#  clear()  # Clear the canvas
#  x <- 1:100
#  y <- sin(x / 5 * pi / 2)
#  id <- "line-1"   # a line needs only 1 ID (as the entire line is considered as one unit)
#  plot(x, y, id = id, type = 'l')
#  
#  for (n in 101:200) {
#    new_x <- 1:n
#    new_y <- sin(new_x / 5 * pi / 2)
#    plot(new_x, new_y, id = id, type = 'l')
#    Sys.sleep(0.02)   # about 50 frames per second
#  }

## ---- eval = T, echo = F------------------------------------------------------
animate::insert_animate("introduction/basic_lines.json.gz", animate::click_to_loop())

## -----------------------------------------------------------------------------
#  # Define the simulation system
#  Lorenz_sim <- function(sigma = 10, beta = 8/3, rho = 28, x = 1, y = 1, z = 1, dt = 0.015) {
#    # Auxiliary variables
#    dx <- dy <- dz <- 0
#    xs <- x
#    ys <- y
#    zs <- z
#    env <- environment() # a neat way to capture all the variables
#  
#    # Update the variables using the ODE within 'env'
#    step <- function(n = 1) {
#      for (i in 1:n) {
#        evalq(envir = env, {
#            dx <- sigma * (y - x) * dt
#            dy <- (x * (rho - z) - y) * dt
#            dz <- (x * y - beta * z) * dt
#            x <- x + dx
#            y <- y + dy
#            z <- z + dz
#            xs <- c(xs, x)
#            ys <- c(ys, y)
#            zs <- c(zs, z)
#        })
#      }
#    }
#  
#    env
#  }

## -----------------------------------------------------------------------------
#  # device <- animate$new(600, 400)
#  # attach(device)
#  world <- Lorenz_sim()
#  for (i in 1:2000) {
#    plot(world$x, world$y, id = "ID-1", xlim = c(-30, 30), ylim = c(-30, 40))
#    lines(world$xs, world$ys, id = "lines-1", xlim = c(-30, 30), ylim = c(-30, 40))
#    world$step()
#    Sys.sleep(0.025)
#  }
#  # Switch to xz-plane
#  plot(world$x, world$z, id = "ID-1", xlim = c(-30, 30), ylim = range(world$zs), transition = TRUE)
#  lines(world$xs, world$zs, id = "lines-1", xlim = c(-30, 30), ylim = range(world$zs), transition = TRUE)
#  
#  # off()
#  # detach(device)

## ---- echo = F, eval = T------------------------------------------------------
animate::insert_animate("introduction/Lorenz_system.json.gz", animate::click_to_loop())

## -----------------------------------------------------------------------------
#  particle_sim <- function(num_particles = 50) {
#    # Particles move within the unit box
#    x <- runif(num_particles)
#    y <- runif(num_particles)
#    vx <- rnorm(num_particles) * 0.01
#    vy <- rnorm(num_particles) * 0.01
#    id <- new_id(x)
#    color <- sample(c("black", "red"), num_particles, replace = TRUE, prob = c(0.5, 0.5))
#  
#    env <- environment()
#    step <- function(n = 1) {
#      for (i in 1:n) {
#        evalq(envir = env, {
#            # The particles turn around when they hit the boundary of the box
#            x_turn <- x + vx > 1 | x + vx < 0
#            vx[x_turn] <- vx[x_turn] * -1
#  
#            y_turn <- y + vy > 1 | y + vy < 0
#            vy[y_turn] <- vy[y_turn] * -1
#  
#            x <- x + vx
#            y <- y + vy
#        })
#      }
#    }
#  
#    env
#  }

## -----------------------------------------------------------------------------
#  # device <- animate$new(500, 500)
#  # attach(device)
#  world <- particle_sim(num_particles = 50)
#  for (i in 1:1000) {
#    points(world$x, world$y, id = world$id, bg = world$color, xlim = c(0, 1), ylim = c(0, 1))
#    world$step()
#    Sys.sleep(0.02)
#  }
#  # off()
#  # detach(device)

## ---- echo = F, eval = T------------------------------------------------------
animate::insert_animate("introduction/particle_system.json.gz", animate::click_to_loop(wait = 20))

## -----------------------------------------------------------------------------
#  random_walk_sim <- function(grid_size = 20, num_walkers = 10) {
#    .side <- seq(0, 1, length.out = grid_size)
#    grid <- expand.grid(.side, .side)
#    id <- paste("ID", 1:grid_size^2, sep = "-")
#  
#    .index_to_coord <- function(n) c(ceiling(n / grid_size), (n-1) %% grid_size + 1)
#    .coord_to_index <- function(x) (x[1] - 1) * grid_size + x[2]
#    .step <- function(coord) {
#      k <- sample(list(c(-1,0), c(1,0), c(0,-1), c(0,1)), 1)[[1]]
#      (coord + k - 1) %% grid_size + 1
#    }
#  
#    .walkers_index <- sample(grid_size^2, num_walkers)
#    .walkers_coord <- Map(.index_to_coord, .walkers_index)
#    .walkers_color <- sample(c("red", "green", "blue", "black", "orange"), num_walkers, replace = TRUE)
#    color <- rep("lightgrey", grid_size^2)
#    color[.walkers_index] <- .walkers_color
#  
#    env <- environment()
#    step <- function() {
#      evalq(envir = env, expr = {
#        # Update each walker's coordinate and change the color state
#        .walkers_coord <- Map(.step, .walkers_coord)
#        .walkers_index <- unlist(Map(.coord_to_index, .walkers_coord))
#        color <- rep("lightgrey", grid_size^2)
#        color[.walkers_index] <- .walkers_color
#      })
#    }
#  
#    env
#  }

## -----------------------------------------------------------------------------
#  # device <- animate$new(600, 600)
#  # attach(device)
#  set.seed(123)
#  world <- random_walk_sim(grid_size = 15, num_walkers = 8)
#  for (i in 1:100) {
#    coord <- world$grid
#    points(coord[,1], coord[,2], id = world$id, bg = world$color, pch = "square", cex = 950, col = "black")
#    world$step()
#    Sys.sleep(0.3)
#  }
#  # off()
#  # detach(device)

## ---- echo = F, eval = T------------------------------------------------------
animate::insert_animate("introduction/random_walk_2d.json.gz", animate::click_to_loop(wait = 300))

## ---- echo = T, eval = T, message = F-----------------------------------------
library(animate)
device <- animate$new(500, 500, virtual = TRUE)
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

## -----------------------------------------------------------------------------
#  library(shiny)
#  library(animate)
#  
#  ui <- fluidPage(
#    actionButton("buttonPlot", "Plot"),
#    actionButton("buttonPoints", "Points"),
#    actionButton("buttonLines", "Lines"),
#    animateOutput()
#  )
#  
#  server <- function(input, output, session) {
#    device <- animate$new(600, 400, session = session)
#    id <- new_id(1:10)
#  
#    observeEvent(input$buttonPlot, {     # Example 1
#      device$plot(1:10, 1:10, id = id)
#    })
#  
#    observeEvent(input$buttonPoints, {   # Example 2
#      device$points(1:10, runif(10, 1, 10), id = id, transition = TRUE)
#    })
#  
#    observeEvent(input$buttonLines, {    # Example 3
#      x <- seq(1, 10, 0.1)
#      y <- sin(x)
#      id <- "line_1"
#      device$lines(x, y, id = id)
#      for (n in 11:100) {
#        x <- seq(1, n, 0.1)
#        y <- sin(x)
#        device$lines(x, y, id = id)
#        Sys.sleep(0.05)
#      }
#    })
#  }
#  
#  shinyApp(ui = ui, server = server)

