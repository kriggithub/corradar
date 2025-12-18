# radar_axis_pos <- function(data, variables = NULL, max.iteration = 1000, minimum.degrees = 3) {
#
#   #packages
#   #library(tidyverse)
#   #library(digest)
#
#   #select numeric variables if they aren't specified
#   if (is.null(variables)) {
#     numeric_columns <- sapply(data, is.numeric)
#     variables <- names(data)[numeric_columns]
#   }
#
#   #ensure data only contains numeric columns
#   data <- data[, variables, drop = FALSE] %>% select_if(is.numeric)
#
#   #calculate correlation matrix for selected variables
#   corr.matrix <- cor(data, use='complete.obs')
#
#
#   #convert correlation matrix to target length differences
#   corr.to.length <- function(mat) {result <- (1-abs(mat))*pi}
#   ideal.length.matrix <- corr.to.length(corr.matrix)
#
#
#   #create matrix of current position from origin (initially equidistant)
#   num_var <- nrow(corr.matrix)
#   names_var <- rownames(corr.matrix)
#   current.placement <- data.frame(matrix(ncol=num_var, nrow=1))
#   colnames(current.placement) <- names_var
#   initial <- 2*pi / num_var
#   current.placement[1,] <- cumsum(rep(initial, num_var))
#   current.placement[current.placement == 2*pi] <- 0
#
#
#   #intializers for loop
#   anneal <- max.iteration
#   iteration.count <- 0
#   iteration.count <- iteration.count + 1
#   var.distance <- matrix(ncol=num_var, nrow=num_var)
#   stored.placements <- new.env(hash = TRUE, parent = emptyenv())
#   difference.matrix <- (var.distance - ideal.length.matrix)/2
#   real.difference <- (var.distance - ideal.length.matrix)/2
#   minimum.distance <- ((2*minimum.degrees)/360)*pi
#   max.length <- pi
#
#   #create list for all iteration details/store forces
#   iterations <- list()
#   iterations[[iteration.count]] <- list(
#     Iteration = iteration.count,
#     Difference_Matrix = difference.matrix,
#     Current_Placement = current.placement,
#     Real_Difference = real.difference
#   )
#   placement.str <- digest::digest(current.placement)
#   assign(placement.str, iteration.count, envir = stored.placements)
#   sum.diff.stored <- numeric()
#
#
#   #begin to create difference matrix to find either attractive or repulsive force
#   for (i in 1:num_var) {
#     for (j in 1:num_var) {
#       difference <- abs(current.placement[1, i] - current.placement[1, j])
#       if (difference > max.length) {
#         var.distance[i,j] <- (2*pi) - difference
#       } else {
#         var.distance[i,j] <- difference
#       }
#     }
#   }
#   colnames(var.distance) <- names_var
#   rownames(var.distance) <- names_var
#
#
#
#   #start loop
#   while (iteration.count < max.iteration ) {
#     iteration.count <- iteration.count + 1
#     #Compare target (2) vs. current (4) and apply to variables to make new current.position
#     #subtract tables (4-2) (difference matrix and apply annealing)
#     #positive values mean attract
#     #negative values means repel
#     difference.matrix <- (var.distance - ideal.length.matrix)/2
#     #real.difference not adjusted for annealing
#     real.difference <- (var.distance - ideal.length.matrix)/2
#     difference.matrix <- difference.matrix*(anneal/max.iteration)
#     #go through each value in difference matrix and apply force based on criteria:
#
#     #if attract:
#     #AND distance <= pi
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         if (difference.matrix[i, j] > 0) {
#           if (current.placement[1, i] > current.placement[1, j]
#               & current.placement[1, i] - current.placement[1, j] <= pi) {
#             current.placement[1, i] <- current.placement[1, i] - abs(difference.matrix[i, j])
#             current.placement[1, j] <- current.placement[1, j] + abs(difference.matrix[i, j])
#           }
#         }
#       }
#     }
#     #if attract:
#     #AND distance > pi
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         if (difference.matrix[i, j] > 0) {
#           if (current.placement[1, i] > current.placement[1, j]
#               & current.placement[1, i] - current.placement[1, j] > pi) {
#             current.placement[1, i] <- current.placement[1, i] + abs(difference.matrix[i, j])
#             current.placement[1, j] <- current.placement[1, j] - abs(difference.matrix[i, j])
#           }
#         }
#       }
#     }
#     #if repel:
#     #AND distance <= pi
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         if (difference.matrix[i, j] < 0) {
#           if (current.placement[1, i] > current.placement[1, j]
#               & current.placement[1, i] - current.placement[1, j] <= pi) {
#             current.placement[1, i] <- current.placement[1, i] + abs(difference.matrix[i, j])
#             current.placement[1, j] <- current.placement[1, j] - abs(difference.matrix[i, j])
#           }
#         }
#       }
#     }
#     #if repel:
#     #AND distance > pi
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         if (difference.matrix[i, j] < 0) {
#           if (current.placement[1, i] > current.placement[1, j]
#               & current.placement[1, i] - current.placement[1, j] > pi) {
#             current.placement[1, i] <- current.placement[1, i] - abs(difference.matrix[i, j])
#             current.placement[1, j] <- current.placement[1, j] + abs(difference.matrix[i, j])
#           }
#         }
#       }
#     }
#     #make sure position stays within the range of 0~2pi after applying difference.matrix
#     current.placement[current.placement == 2*pi] <- 0
#     while (any(current.placement < 0)) {
#       current.placement[current.placement < 0] <- current.placement[current.placement < 0] + (2 * pi)
#     }
#     while (any(current.placement > 2*pi)) {
#       current.placement[current.placement > 2*pi] <- current.placement[current.placement > 2*pi] - (2 * pi)
#     }
#
#     first.var.pos.c <- current.placement[[1]]
#     current.placement[1] <- 0
#     current.placement[-1] <- current.placement[-1] - first.var.pos.c
#
#
#
#     current.placement[current.placement == 2*pi] <- 0
#     while (any(current.placement < 0)) {
#       current.placement[current.placement < 0] <- current.placement[current.placement < 0] + (2 * pi)
#     }
#     while (any(current.placement > 2*pi)) {
#       current.placement[current.placement > 2*pi] <- current.placement[current.placement > 2*pi] - (2 * pi)
#     }
#
#     #recalculate difference matrix
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         difference <- abs(current.placement[1, i] - current.placement[1, j])
#         if (difference > max.length) {
#           var.distance[i,j] <- (2*pi) - difference
#         } else {
#           var.distance[i,j] <- difference
#         }
#       }
#     }
#
#     #store iteration information
#     iterations[[iteration.count]] <- list(
#       Iteration = iteration.count,
#       Difference_Matrix = difference.matrix,
#       Current_Placement = current.placement,
#       Real_Difference = real.difference
#     )
#
#     anneal <- (anneal-1)
#
#     sum.diff.matrix <- sum(abs(real.difference))/2
#     sum.diff.stored[iteration.count] <- sum.diff.matrix
#
#
#   }
#
#   #store final state
#   iterations[[iteration.count + 1]] <- list(
#     Iteration = iteration.count + 1,
#     Difference_Matrix = difference.matrix,
#     Current_Placement = current.placement,
#     Real_Difference = real.difference
#   )
#
#
#   min.iteration <- which.min(sum.diff.stored)
#   cat(sprintf("Minimum sum.diff.matrix found at iteration %d with value %.5f\n",
#               min.iteration, sum.diff.stored[min.iteration]))
#
#
#   print(iteration.count[min.iteration])
#   final.current.placement <- iterations[[min.iteration]]$Current_Placement
#   afinal.current.placement <- final.current.placement
#
#
#   #check final current placement for repulsive forces if too close
#   final.var.distance <- matrix(ncol=num_var, nrow=num_var)
#
#   for (i in 1:num_var) {
#     for (j in 1:num_var) {
#       final.difference <- abs(afinal.current.placement[1, i] - afinal.current.placement[1, j])
#       if (final.difference > max.length) {
#         final.var.distance[i,j] <- (2*pi) - final.difference
#       } else {
#         final.var.distance[i,j] <- final.difference
#       }
#     }
#   }
#   colnames(final.var.distance) <- names_var
#   rownames(final.var.distance) <- names_var
#
#
#
#
#   #check if charged particles needs to be applied
#   for (i in 1:num_var) {
#     for (j in 1:num_var) {
#       if (i != j) {  # exclude diagonals
#         if (final.var.distance[i, j] < minimum.distance) {
#           if (afinal.current.placement[1, i] > afinal.current.placement[1, j]) {
#             afinal.current.placement[1, i] <- (afinal.current.placement[1, i] + afinal.current.placement[1, j])/2
#             afinal.current.placement[1, j] <- (afinal.current.placement[1, i] + afinal.current.placement[1, j])/2
#             afinal.current.placement[1, i] <- afinal.current.placement[1, i] + (0.5*minimum.distance)
#             afinal.current.placement[1, j] <- afinal.current.placement[1, j] - (0.5*minimum.distance)
#           }
#         }
#       }
#     }
#   }
#
#
#
#   afinal.current.placement[afinal.current.placement == 2*pi] <- 0
#   while (any(afinal.current.placement < 0)) {
#     afinal.current.placement[afinal.current.placement < 0] <- afinal.current.placement[afinal.current.placement < 0] + (2 * pi)
#   }
#   while (any(afinal.current.placement > 2*pi)) {
#     afinal.current.placement[afinal.current.placement > 2*pi] <- afinal.current.placement[afinal.current.placement > 2*pi] - (2 * pi)
#   }
#
#
#
#
#   #rotate it so the first variable in afinal.current.placement is at origin
#   true.final.placement <- afinal.current.placement
#   first.var.pos <- afinal.current.placement[[1]]
#
#
#
#   true.final.placement[1] <- 0
#   true.final.placement[-1] <- afinal.current.placement[-1] - first.var.pos
#
#
#
#   #make sure positions are back in range
#
#   true.final.placement[true.final.placement == 2*pi] <- 0
#   while (any(true.final.placement < 0)) {
#     true.final.placement[true.final.placement < 0] <- true.final.placement[true.final.placement < 0] + (2 * pi)
#   }
#   while (any(true.final.placement > 2*pi)) {
#     true.final.placement[true.final.placement > 2*pi] <- true.final.placement[true.final.placement > 2*pi] - (2 * pi)
#   }
#
#
#   print("True Final Placement:")
#   print(true.final.placement)
#
#   return(true.final.placement = true.final.placement)
#
# }
#
#
#
#
#
#
#
#
# #sample use of the function
# #iris
# #iris.pos <- radar_plot_pos(iris, max.iteration = 2000, minimum.degrees = 5)
# #iris.pos
#
#
# #asthma
# #library(readxl)
# #asthma_data_set_2 <- read_excel("~/CoSIBS/Research_Project/Data/asthma-data-set-2.xlsx")
# #asthma_data_set_2[asthma_data_set_2 == 9999] <- NA
# #asthma_data_set_2[asthma_data_set_2 == 999] <- NA
# #asthma.pos <- radar_plot_pos(asthma_data_set_2, variables = c("Waist", "BMI", "Hip", "Neck", "HDL", "Age", "SBP", "DBP", "Glucose"), max.iteration = 1000)
# #as.numeric(asthma.pos)
#
#
#
# #gapminder (for fun)
# #library(gapminder)
# #gapminder.pos <- radar_plot_pos(gapminder)
# #gapminder.pos
