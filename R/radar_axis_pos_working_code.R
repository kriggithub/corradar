
data <- iris
vars <- NULL
max.iteration <- 2000
minimum.degrees <- 3




# select numeric variables if they aren't specified
if (is.null(vars)) {
  numeric_columns <- sapply(data, is.numeric)
  vars <- names(data)[numeric_columns]
}

# ensure data only contains numeric columns
data <- dplyr::select_if(data[, vars, drop = FALSE], is.numeric)

# calculate correlation matrix for selected variables
corr_mat <- stats::cor(data, use='complete.obs')


# convert correlation matrix to target length difference matrix
target_length_mat <- (1-abs(corr_mat))*pi


# create matrix of current position from origin (initially equidistant)
num_var <- nrow(corr_mat)
names_var <- rownames(corr_mat)
cur_pos <- data.frame(matrix(ncol=num_var, nrow=1), row.names = NULL)
colnames(cur_pos) <- names_var
init_pos <- 2*pi / num_var
cur_pos[1,] <- cumsum(rep(init_pos, num_var))
cur_pos[cur_pos == 2*pi] <- 0


# setup initializers for loop (current axis distance matrix, difference matrix
# between axis distance and target distance left to be applied (remaining forces
# based on annealing left to apply), true difference matrix with final remaining force)
iteration_count <- 1
var_distance <- matrix(ncol=num_var, nrow=num_var)
colnames(var_distance) <- names_var
rownames(var_distance) <- names_var
stored_placements <- new.env(hash = TRUE, parent = emptyenv())
applied_force_mat <- (var_distance - target_length_mat)/2
final_force_mat <- (var_distance - target_length_mat)/2
sum_diff_stored <- numeric()

min_dist <- ((2*minimum.degrees)/360)*pi
anneal_factor <- max.iteration

# create list for all iteration details/store forces
iteration <- list()
iteration[[iteration_count]] <- list(
  Iteration = iteration_count,
  Current_Placement = cur_pos,
  Remaining_Force = final_force_mat,
  Sum_Remaining_Force = sum_diff_stored
)
placement_hash <- digest::digest(cur_pos)
assign(placement_hash, iteration_count, envir = stored_placements)



# create difference matrix to find either attractive or repulsive force
for (i in 1:num_var) {
  for (j in 1:num_var) {
    diff <- abs(cur_pos[1, i] - cur_pos[1, j])
    if (diff > pi) {
      var_distance[i,j] <- (2*pi) - diff
    } else {
      var_distance[i,j] <- diff
    }
  }
}





# start loop
while (iteration_count <= max.iteration ) {
  #Compare target (2) vs. current (4) and apply to variables to make new current.position
  #subtract tables (4-2) (difference matrix and apply annealing)
  #positive values mean attract
  #negative values means repel
  applied_force_mat <- (var_distance - target_length_mat)/2
  #final_force_mat not adjusted for annealing
  final_force_mat <- (var_distance - target_length_mat)/2
  applied_force_mat <- applied_force_mat*(anneal_factor/max.iteration)
  #go through each value in difference matrix and apply force based on criteria:

  #if attract:
  #AND distance <= pi
  for (i in 1:num_var) {
    for (j in 1:num_var) {
      if (applied_force_mat[i, j] > 0) {
        if (cur_pos[1, i] > cur_pos[1, j]
            & cur_pos[1, i] - cur_pos[1, j] <= pi) {
          cur_pos[1, i] <- cur_pos[1, i] - abs(applied_force_mat[i, j])
          cur_pos[1, j] <- cur_pos[1, j] + abs(applied_force_mat[i, j])
        }
      }
    }
  }
  #if attract:
  #AND distance > pi
  for (i in 1:num_var) {
    for (j in 1:num_var) {
      if (applied_force_mat[i, j] > 0) {
        if (cur_pos[1, i] > cur_pos[1, j]
            & cur_pos[1, i] - cur_pos[1, j] > pi) {
          cur_pos[1, i] <- cur_pos[1, i] + abs(applied_force_mat[i, j])
          cur_pos[1, j] <- cur_pos[1, j] - abs(applied_force_mat[i, j])
        }
      }
    }
  }
  #if repel:
  #AND distance <= pi
  for (i in 1:num_var) {
    for (j in 1:num_var) {
      if (applied_force_mat[i, j] < 0) {
        if (cur_pos[1, i] > cur_pos[1, j]
            & cur_pos[1, i] - cur_pos[1, j] <= pi) {
          cur_pos[1, i] <- cur_pos[1, i] + abs(applied_force_mat[i, j])
          cur_pos[1, j] <- cur_pos[1, j] - abs(applied_force_mat[i, j])
        }
      }
    }
  }
  #if repel:
  #AND distance > pi
  for (i in 1:num_var) {
    for (j in 1:num_var) {
      if (applied_force_mat[i, j] < 0) {
        if (cur_pos[1, i] > cur_pos[1, j]
            & cur_pos[1, i] - cur_pos[1, j] > pi) {
          cur_pos[1, i] <- cur_pos[1, i] - abs(applied_force_mat[i, j])
          cur_pos[1, j] <- cur_pos[1, j] + abs(applied_force_mat[i, j])
        }
      }
    }
  }
  #make sure position stays within the range of 0~2pi after applying difference.matrix
  cur_pos[cur_pos == 2*pi] <- 0
  while (any(cur_pos < 0)) {
    cur_pos[cur_pos < 0] <- cur_pos[cur_pos < 0] + (2 * pi)
  }
  while (any(cur_pos > 2*pi)) {
    cur_pos[cur_pos > 2*pi] <- cur_pos[cur_pos > 2*pi] - (2 * pi)
  }

  first.var.pos.c <- cur_pos[[1]]
  cur_pos[1] <- 0
  cur_pos[-1] <- cur_pos[-1] - first.var.pos.c



  cur_pos[cur_pos == 2*pi] <- 0
  while (any(cur_pos < 0)) {
    cur_pos[cur_pos < 0] <- cur_pos[cur_pos < 0] + (2 * pi)
  }
  while (any(cur_pos > 2*pi)) {
    cur_pos[cur_pos > 2*pi] <- cur_pos[cur_pos > 2*pi] - (2 * pi)
  }

  #recalculate difference matrix
  for (i in 1:num_var) {
    for (j in 1:num_var) {
      difference <- abs(cur_pos[1, i] - cur_pos[1, j])
      if (difference > pi) {
        var_distance[i,j] <- (2*pi) - difference
      } else {
        var_distance[i,j] <- difference
      }
    }
  }

  # add summed remaining force to list
  sum_diff_stored <- sum(abs(final_force_mat))/2

  #store iteration information
  iteration[[iteration_count]] <- list(
    Iteration = iteration_count,
    Current_Placement = cur_pos,
    Remaining_Force = final_force_mat,
    Sum_Remaining_Force = sum_diff_stored
  )


  anneal_factor <- (anneal_factor-1)
  iteration_count <- iteration_count + 1


}


# use vapply
sum_forces <- vapply(iteration, function(x) x$Sum_Remaining_Force, numeric(1))
min_iteration <- which.min(sum_forces)


cat(sprintf("Minimum remaining force found at iteration %d with value %.5f",
            min_iteration, iteration[[min_iteration]]$Sum_Remaining_Force))




# to plot remainign forces across iterations
# iteration_numbers <- vapply(iteration, function(x) x$Iteration, numeric(1))
# plot(iteration_numbers, sum_forces, type = "o", col = "blue", pch = 16,
#      main = "Sum Remaining Force Across Iterations",
#      xlab = "Iteration Number", ylab = "Sum Remaining Force",
#      xlim = c(0, min_iteration + 10))





##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################






# set list for to adjust for final adjusted positions (after minimum degrees)
adjusted_cur_pos <- iteration[[min_iteration]]$Current_Placement
# badjusted_cur_pos <- adjusted_cur_pos


adjusted_var_dist <- matrix(ncol=num_var, nrow=num_var)
colnames(adjusted_var_dist) <- names_var
rownames(adjusted_var_dist) <- names_var



for (i in 1:num_var) {
  for (j in 1:num_var) {
    adjusted_diff <- abs(adjusted_cur_pos[1, i] - adjusted_cur_pos[1, j])
    if (adjusted_diff > pi) {
      adjusted_var_dist[i,j] <- (2*pi) - adjusted_diff
    } else {
      adjusted_var_dist[i,j] <- adjusted_diff
    }
  }
}







# apply charged forces for minimum degrees
for (i in 1:num_var) {
  for (j in 1:num_var) {
    if (i != j) {  # exclude diagonals
      if (adjusted_var_dist[i, j] < min_dist) {
        if (badjusted_cur_pos[1, i] > badjusted_cur_pos[1, j]) {
          badjusted_cur_pos[1, i] <- (badjusted_cur_pos[1, i] + badjusted_cur_pos[1, j])/2
          badjusted_cur_pos[1, j] <- (badjusted_cur_pos[1, i] + badjusted_cur_pos[1, j])/2
          badjusted_cur_pos[1, i] <- badjusted_cur_pos[1, i] + (0.5*min_dist)
          badjusted_cur_pos[1, j] <- badjusted_cur_pos[1, j] - (0.5*min_dist)
        }
      }
    }
  }
}



badjusted_cur_pos[badjusted_cur_pos == 2*pi] <- 0
while (any(badjusted_cur_pos < 0)) {
  badjusted_cur_pos[badjusted_cur_pos < 0] <- badjusted_cur_pos[badjusted_cur_pos < 0] + (2 * pi)
}
while (any(badjusted_cur_pos > 2*pi)) {
  badjusted_cur_pos[badjusted_cur_pos > 2*pi] <- badjusted_cur_pos[badjusted_cur_pos > 2*pi] - (2 * pi)
}




#rotate it so the first variable in afinal.current.placement is at origin
true.final.placement <- badjusted_cur_pos
first.var.pos <- badjusted_cur_pos[[1]]



true.final.placement[1] <- 0
true.final.placement[-1] <- badjusted_cur_pos[-1] - first.var.pos



#make sure positions are back in range

true.final.placement[true.final.placement == 2*pi] <- 0
while (any(true.final.placement < 0)) {
  true.final.placement[true.final.placement < 0] <- true.final.placement[true.final.placement < 0] + (2 * pi)
}
while (any(true.final.placement > 2*pi)) {
  true.final.placement[true.final.placement > 2*pi] <- true.final.placement[true.final.placement > 2*pi] - (2 * pi)
}


print("True Final Placement:")
print(true.final.placement)

return(true.final.placement = true.final.placement)











#sample use of the function
#iris
#iris.pos <- radar_axis_pos(iris, max.iteration = 2000, minimum.degrees = 5)
#iris.pos


#asthma
#library(readxl)
#asthma_data_set_2 <- read_excel("~/CoSIBS/Research_Project/Data/asthma-data-set-2.xlsx")
#asthma_data_set_2[asthma_data_set_2 == 9999] <- NA
#asthma_data_set_2[asthma_data_set_2 == 999] <- NA
#asthma.pos <- radar_plot_pos(asthma_data_set_2, variables = c("Waist", "BMI", "Hip", "Neck", "HDL", "Age", "SBP", "DBP", "Glucose"), max.iteration = 1000)
#as.numeric(asthma.pos)



#gapminder (for fun)
#library(gapminder)
#gapminder.pos <- radar_plot_pos(gapminder)
#gapminder.pos
