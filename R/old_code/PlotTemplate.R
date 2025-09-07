
library(tidyverse)
data <- iris
variables <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
cluster.by <- "Species"
max.iteration = 1000
minimum.degrees = 3


#ensure variable data is numeric and no NAs
data <- data %>% 
  mutate(across(all_of(variables), ~ as.numeric(.)))
data <- na.omit(data)


#create dataframe to store mean and sd for each axis

means <- data %>%
  group_by(.data[[cluster.by]]) %>% 
  summarize(across(all_of(variables), mean, na.rm = TRUE))


sds <- data %>%
  group_by(.data[[cluster.by]]) %>%
  summarize(across(all_of(variables), sd, na.rm = TRUE))






#find # of clusters
no_clusters <- n_distinct(sds[[cluster.by]])

#find max observed value for each variable
max.values <- apply(data[variables], 2, max, na.rm = TRUE)


#make data proportionate to axis data frame

means_df <- means %>%
  mutate(across(-all_of(cluster.by), ~ .x / max.values[deparse(substitute(.x))]))
sds_df <- sds %>%
  mutate(across(-all_of(cluster.by), ~ .x / max.values[deparse(substitute(.x))]))




#convert means and sd to long format

means_long <- pivot_longer(
  as.data.frame(means_df), 
  cols = -all_of(cluster.by), 
  names_to = "variable", 
  values_to = "mean"
  )

sds_long <- pivot_longer(
  as.data.frame(sds_df), 
  cols = -all_of(cluster.by), 
  names_to = "variable", 
  values_to = "sd"
)

                           
plot_df <- merge(means_long, sds_long, by = c("variable", cluster.by))


# Get positions using the radar_plot_pos function
pos <- radar_plot_pos(data, variables, max.iteration, minimum.degrees)

# Convert pos to dataframe
pos_df <- as.data.frame(pos)

# Convert wide to long format
pos_df <- pivot_longer(pos_df, 
                       cols = variables, 
                       names_to = "variable", 
                       values_to = "pos")

# Reorder pos_df by position
pos_df <- pos_df %>% arrange(pos)


#final plot dataframe
plot_df <- merge(plot_df, pos_df, by = "variable")


# Add lower and upper bounds for the ribbons
plot_df <- plot_df %>%
  mutate(lower = mean - sd, upper = mean + sd)


# Reorder plot_df by position
plot_df <- plot_df %>% arrange(pos)



# Close the plotted loop by repeating rows 1 & 2 at the end and setting last two positions to 2pi
plot_df <- rbind(plot_df, plot_df[1:no_clusters,])
plot_df$pos[(nrow(plot_df) - (no_clusters - 1)):nrow(plot_df)] <- 2 * pi




#plotting

# Determine y-axis limit
y_max <- max(plot_df$upper, na.rm = TRUE)
y_min <- min(plot_df$lower, na.rm = TRUE)

# Plot using ggplot2
plot <- ggplot(plot_df) +
  geom_ribbon(aes(x = pos, ymin = lower, ymax = upper, group = .data[[cluster.by]], fill = as.factor(.data[[cluster.by]])), alpha = 0.2, show.legend = T) +
  geom_path(aes(x = pos, y = mean, group = .data[[cluster.by]], color = as.factor(.data[[cluster.by]])), size = 1, show.legend = T) +
  scale_x_continuous(name = "", limits = c(0, 2 * pi), breaks = pos_df$pos, labels = pos_df$variable, minor_breaks = NULL,) +
  scale_y_continuous(name = "", limits = c(y_min, y_max), breaks = NULL) +
  coord_polar(start = -pi/2, direction = -1) +
  ggtitle("Clinical Data from Days 0-3 Stratified by VAP Diagnosis") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_color_discrete(name = "VAP3") +  # Label for the color legend
  scale_fill_discrete(name = "VAP3")    # Label for the fill legend


plot






