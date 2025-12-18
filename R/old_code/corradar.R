#
# corradar <- function(data, variables = NULL, cluster.by = NULL, plot.title = NULL, max.iteration = 1000, minimum.degrees = 3){
#
#
#   #Load position function
#   radar_plot_pos <- function(data, variables = NULL, max.iteration = 1000, minimum.degrees = 3) {
#
#     #packages
#     library(tidyverse)
#     library(digest)
#
#     #select numeric variables if they aren't specified
#     if (is.null(variables)) {
#       numeric_columns <- sapply(data, is.numeric)
#       variables <- names(data)[numeric_columns]
#     }
#
#     #ensure data only contains numeric columns
#     data <- data[, variables, drop = FALSE] %>% select_if(is.numeric)
#
#     #calculate correlation matrix for selected variables
#     corr.matrix <- cor(data, use='complete.obs')
#
#
#     #convert correlation matrix to target length differences
#     corr.to.length <- function(mat) {result <- (1-abs(mat))*pi}
#     ideal.length.matrix <- corr.to.length(corr.matrix)
#
#
#     #create matrix of current position from origin (initially equidistant)
#     num_var <- nrow(corr.matrix)
#     names_var <- rownames(corr.matrix)
#     current.placement <- data.frame(matrix(ncol=num_var, nrow=1))
#     colnames(current.placement) <- names_var
#     initial <- 2*pi / num_var
#     current.placement[1,] <- cumsum(rep(initial, num_var))
#     current.placement[current.placement == 2*pi] <- 0
#
#
#     #intializers for loop
#     anneal <- max.iteration
#     iteration.count <- 0
#     iteration.count <- iteration.count + 1
#     var.distance <- matrix(ncol=num_var, nrow=num_var)
#     stored.placements <- new.env(hash = TRUE, parent = emptyenv())
#     difference.matrix <- (var.distance - ideal.length.matrix)/2
#     real.difference <- (var.distance - ideal.length.matrix)/2
#     minimum.distance <- ((2*minimum.degrees)/360)*pi
#     max.length <- pi
#
#     #create list for all iteration details/store forces
#     iterations <- list()
#     iterations[[iteration.count]] <- list(
#       Iteration = iteration.count,
#       Difference_Matrix = difference.matrix,
#       Current_Placement = current.placement,
#       Real_Difference = real.difference
#     )
#     placement.str <- digest(current.placement)
#     assign(placement.str, iteration.count, envir = stored.placements)
#     sum.diff.stored <- numeric()
#
#
#     #begin to create difference matrix to find either attractive or repulsive force
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
#     colnames(var.distance) <- names_var
#     rownames(var.distance) <- names_var
#
#
#
#     #start loop
#     while (iteration.count < max.iteration ) {
#       iteration.count <- iteration.count + 1
#       #Compare target (2) vs. current (4) and apply to variables to make new current.position
#       #subtract tables (4-2) (difference matrix and apply annealing)
#       #positive values mean attract
#       #negative values means repel
#       difference.matrix <- (var.distance - ideal.length.matrix)/2
#       #real.difference not adjusted for annealing
#       real.difference <- (var.distance - ideal.length.matrix)/2
#       difference.matrix <- difference.matrix*(anneal/max.iteration)
#       #go through each value in difference matrix and apply force based on criteria:
#
#       #if attract:
#       #AND distance <= pi
#       for (i in 1:num_var) {
#         for (j in 1:num_var) {
#           if (difference.matrix[i, j] > 0) {
#             if (current.placement[1, i] > current.placement[1, j]
#                 & current.placement[1, i] - current.placement[1, j] <= pi) {
#               current.placement[1, i] <- current.placement[1, i] - abs(difference.matrix[i, j])
#               current.placement[1, j] <- current.placement[1, j] + abs(difference.matrix[i, j])
#             }
#           }
#         }
#       }
#       #if attract:
#       #AND distance > pi
#       for (i in 1:num_var) {
#         for (j in 1:num_var) {
#           if (difference.matrix[i, j] > 0) {
#             if (current.placement[1, i] > current.placement[1, j]
#                 & current.placement[1, i] - current.placement[1, j] > pi) {
#               current.placement[1, i] <- current.placement[1, i] + abs(difference.matrix[i, j])
#               current.placement[1, j] <- current.placement[1, j] - abs(difference.matrix[i, j])
#             }
#           }
#         }
#       }
#       #if repel:
#       #AND distance <= pi
#       for (i in 1:num_var) {
#         for (j in 1:num_var) {
#           if (difference.matrix[i, j] < 0) {
#             if (current.placement[1, i] > current.placement[1, j]
#                 & current.placement[1, i] - current.placement[1, j] <= pi) {
#               current.placement[1, i] <- current.placement[1, i] + abs(difference.matrix[i, j])
#               current.placement[1, j] <- current.placement[1, j] - abs(difference.matrix[i, j])
#             }
#           }
#         }
#       }
#       #if repel:
#       #AND distance > pi
#       for (i in 1:num_var) {
#         for (j in 1:num_var) {
#           if (difference.matrix[i, j] < 0) {
#             if (current.placement[1, i] > current.placement[1, j]
#                 & current.placement[1, i] - current.placement[1, j] > pi) {
#               current.placement[1, i] <- current.placement[1, i] - abs(difference.matrix[i, j])
#               current.placement[1, j] <- current.placement[1, j] + abs(difference.matrix[i, j])
#             }
#           }
#         }
#       }
#       #make sure position stays within the range of 0~2pi after applying difference.matrix
#       current.placement[current.placement == 2*pi] <- 0
#       while (any(current.placement < 0)) {
#         current.placement[current.placement < 0] <- current.placement[current.placement < 0] + (2 * pi)
#       }
#       while (any(current.placement > 2*pi)) {
#         current.placement[current.placement > 2*pi] <- current.placement[current.placement > 2*pi] - (2 * pi)
#       }
#
#       first.var.pos.c <- current.placement[[1]]
#       current.placement[1] <- 0
#       current.placement[-1] <- current.placement[-1] - first.var.pos.c
#
#
#
#       current.placement[current.placement == 2*pi] <- 0
#       while (any(current.placement < 0)) {
#         current.placement[current.placement < 0] <- current.placement[current.placement < 0] + (2 * pi)
#       }
#       while (any(current.placement > 2*pi)) {
#         current.placement[current.placement > 2*pi] <- current.placement[current.placement > 2*pi] - (2 * pi)
#       }
#
#       #recalculate difference matrix
#       for (i in 1:num_var) {
#         for (j in 1:num_var) {
#           difference <- abs(current.placement[1, i] - current.placement[1, j])
#           if (difference > max.length) {
#             var.distance[i,j] <- (2*pi) - difference
#           } else {
#             var.distance[i,j] <- difference
#           }
#         }
#       }
#
#       #store iteration information
#       iterations[[iteration.count]] <- list(
#         Iteration = iteration.count,
#         Difference_Matrix = difference.matrix,
#         Current_Placement = current.placement,
#         Real_Difference = real.difference
#       )
#
#       anneal <- (anneal-1)
#
#       sum.diff.matrix <- sum(abs(real.difference))/2
#       sum.diff.stored[iteration.count] <- sum.diff.matrix
#
#
#     }
#
#     #store final state
#     iterations[[iteration.count + 1]] <- list(
#       Iteration = iteration.count + 1,
#       Difference_Matrix = difference.matrix,
#       Current_Placement = current.placement,
#       Real_Difference = real.difference
#     )
#
#
#     min.iteration <- which.min(sum.diff.stored)
#     cat(sprintf("Minimum sum.diff.matrix found at iteration %d with value %.5f\n",
#                 min.iteration, sum.diff.stored[min.iteration]))
#
#
#     print(iteration.count[min.iteration])
#     final.current.placement <- iterations[[min.iteration]]$Current_Placement
#     afinal.current.placement <- final.current.placement
#
#
#     #check final current placement for repulsive forces if too close
#     final.var.distance <- matrix(ncol=num_var, nrow=num_var)
#
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         final.difference <- abs(afinal.current.placement[1, i] - afinal.current.placement[1, j])
#         if (final.difference > max.length) {
#           final.var.distance[i,j] <- (2*pi) - final.difference
#         } else {
#           final.var.distance[i,j] <- final.difference
#         }
#       }
#     }
#     colnames(final.var.distance) <- names_var
#     rownames(final.var.distance) <- names_var
#
#
#
#
#     #check if charged particles needs to be applied
#     for (i in 1:num_var) {
#       for (j in 1:num_var) {
#         if (i != j) {  # exclude diagonals
#           if (final.var.distance[i, j] < minimum.distance) {
#             if (afinal.current.placement[1, i] > afinal.current.placement[1, j]) {
#               afinal.current.placement[1, i] <- (afinal.current.placement[1, i] + afinal.current.placement[1, j])/2
#               afinal.current.placement[1, j] <- (afinal.current.placement[1, i] + afinal.current.placement[1, j])/2
#               afinal.current.placement[1, i] <- afinal.current.placement[1, i] + (0.5*minimum.distance)
#               afinal.current.placement[1, j] <- afinal.current.placement[1, j] - (0.5*minimum.distance)
#             }
#           }
#         }
#       }
#     }
#
#
#
#     afinal.current.placement[afinal.current.placement == 2*pi] <- 0
#     while (any(afinal.current.placement < 0)) {
#       afinal.current.placement[afinal.current.placement < 0] <- afinal.current.placement[afinal.current.placement < 0] + (2 * pi)
#     }
#     while (any(afinal.current.placement > 2*pi)) {
#       afinal.current.placement[afinal.current.placement > 2*pi] <- afinal.current.placement[afinal.current.placement > 2*pi] - (2 * pi)
#     }
#
#
#
#
#     #rotate it so the first variable in afinal.current.placement is at origin
#     true.final.placement <- afinal.current.placement
#     first.var.pos <- afinal.current.placement[[1]]
#
#
#
#     true.final.placement[1] <- 0
#     true.final.placement[-1] <- afinal.current.placement[-1] - first.var.pos
#
#
#
#     #make sure positions are back in range
#
#     true.final.placement[true.final.placement == 2*pi] <- 0
#     while (any(true.final.placement < 0)) {
#       true.final.placement[true.final.placement < 0] <- true.final.placement[true.final.placement < 0] + (2 * pi)
#     }
#     while (any(true.final.placement > 2*pi)) {
#       true.final.placement[true.final.placement > 2*pi] <- true.final.placement[true.final.placement > 2*pi] - (2 * pi)
#     }
#
#
#     print("True Final Placement:")
#     print(true.final.placement)
#
#     return(true.final.placement = true.final.placement)
#
#   }
#
#
#
#   ###############################
#   #plotting
#
#   #ensure variable data is numeric and no NAs
#   data <- data %>%
#     mutate(across(all_of(variables), ~ as.numeric(.)))
#   data <- na.omit(data)
#
#
#   #create dataframe to store mean and sd for each axis
#
#   means <- data %>%
#     group_by(.data[[cluster.by]]) %>%
#     summarize(across(all_of(variables), mean, na.rm = TRUE))
#
#
#   sds <- data %>%
#     group_by(.data[[cluster.by]]) %>%
#     summarize(across(all_of(variables), sd, na.rm = TRUE))
#
#
#
#
#
#
#   #find # of clusters
#   no_clusters <- n_distinct(sds[[cluster.by]])
#
#   #find max observed value for each variable
#   max.values <- apply(data[variables], 2, max, na.rm = TRUE)
#
#
#   #make data proportionate to axis data frame
#
#   means_df <- means %>%
#     mutate(across(-all_of(cluster.by), ~ .x / max.values[deparse(substitute(.x))]))
#   sds_df <- sds %>%
#     mutate(across(-all_of(cluster.by), ~ .x / max.values[deparse(substitute(.x))]))
#
#
#
#
#   #convert means and sd to long format
#
#   means_long <- pivot_longer(
#     as.data.frame(means_df),
#     cols = -all_of(cluster.by),
#     names_to = "variable",
#     values_to = "mean"
#   )
#
#   sds_long <- pivot_longer(
#     as.data.frame(sds_df),
#     cols = -all_of(cluster.by),
#     names_to = "variable",
#     values_to = "sd"
#   )
#
#
#   plot_df <- merge(means_long, sds_long, by = c("variable", cluster.by))
#
#
#   # Get positions using the radar_plot_pos function
#   pos <- radar_plot_pos(data, variables, max.iteration, minimum.degrees)
#
#   # Convert pos to dataframe
#   pos_df <- as.data.frame(pos)
#
#   # Convert wide to long format
#   pos_df <- pivot_longer(pos_df,
#                          cols = variables,
#                          names_to = "variable",
#                          values_to = "pos")
#
#   # Reorder pos_df by position
#   pos_df <- pos_df %>% arrange(pos)
#
#
#   #final plot dataframe
#   plot_df <- merge(plot_df, pos_df, by = "variable")
#
#
#   # Add lower and upper bounds for the ribbons
#   plot_df <- plot_df %>%
#     mutate(lower = mean - sd, upper = mean + sd)
#
#
#   # Reorder plot_df by position
#   plot_df <- plot_df %>% arrange(pos)
#
#
#
#   # Close the plotted loop by repeating rows 1 & 2 at the end and setting last two positions to 2pi
#   plot_df <- rbind(plot_df, plot_df[1:no_clusters,])
#   plot_df$pos[(nrow(plot_df) - (no_clusters - 1)):nrow(plot_df)] <- 2 * pi
#
#
#
#
#   #plotting
#
#   # Determine y-axis limit
#   y_max <- max(plot_df$upper, na.rm = TRUE)
#   y_min <- min(plot_df$lower, na.rm = TRUE)
#
#   # Plot using ggplot2
#   plot <- ggplot(plot_df) +
#     geom_ribbon(aes(x = pos, ymin = lower, ymax = upper, group = .data[[cluster.by]], fill = as.factor(.data[[cluster.by]])), alpha = 0.2, show.legend = T) +
#     geom_path(aes(x = pos, y = mean, group = .data[[cluster.by]], color = as.factor(.data[[cluster.by]])), size = 1, show.legend = T) +
#     scale_x_continuous(name = "", limits = c(0, 2 * pi), breaks = pos_df$pos, labels = pos_df$variable, minor_breaks = NULL,) +
#     scale_y_continuous(name = "", limits = c(y_min, y_max), breaks = NULL) +
#     coord_polar(start = -pi/2, direction = -1) +
#     ggtitle(plot.title) +
#     theme_minimal(base_size = 10) +
#     theme(axis.text.x = element_text(angle = 0)) +
#     scale_color_discrete(name = cluster.by) +  # Label for the color legend
#     scale_fill_discrete(name = cluster.by)    # Label for the fill legend
#
#
#   plot
#
#
# }
