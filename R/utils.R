# UTILS FUNCTIONS
# NONE OF THESE FUNCTIONS ARE EXPORTED


# Round half up a number
round_half_up <- function(x, digits=0) {

  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  z

}


# Update the user configuration file
update_user_config_file <- function(config_list) {

  # Get the user project config directory
  user_config_path <- tools::R_user_dir("eurostatRTool", which = "config")
  if (!dir.exists(user_config_path)) {
    dir.create(user_config_path, recursive=TRUE)
  }
  config_path <- file.path(user_config_path, "config.yaml")
  # Read the old config and merge it with the config_list
  if (file.exists(config_path)) {
    old_config <- configr::read.config(file = config_path)
    merged_config <- configr::config.list.merge(list.left = old_config,
                                                list.right = config_list)
  } else {
    merged_config <- config_list
  }
  # Write YAML format configuration file with the merged list
  success <- configr::write.config(config.dat = merged_config,
                                   file.path = config_path, write.type = "yaml")
  if (!success) {
    print("error")
  }

}


# Read a parameter from the user configuration file
read_user_config_param <- function(param) {

  # Get the user project config directory
  user_config_path <- tools::R_user_dir("eurostatRTool", which = "config")
  if (!dir.exists(user_config_path)) {
    dir.create(user_config_path, recursive=TRUE)
  }
  config_path <- file.path(user_config_path, "config.yaml")
  # Read the config and return the param value, if exists
  if (file.exists(config_path)) {
    config <- configr::read.config(file = config_path)
    if (is.null(config[[param]])) {
      return(NULL)
    } else {
      return(config[[param]])
    }
  }
  return(NULL)

}


# Transform an absolute path into relative
relative_path <- function(absolute_path, base_path) {
  # Normalize the paths to remove any redundant components
  absolute_path <- normalizePath(absolute_path, winslash = "/", mustWork = FALSE)
  base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)

  # Split the paths into their components
  absolute_parts <- strsplit(absolute_path, "/")[[1]]
  base_parts <- strsplit(base_path, "/")[[1]]

  # Find the length of the shorter path
  common_length <- min(length(absolute_parts), length(base_parts))

  # Find the index where the paths start to differ
  diff_index <- which(absolute_parts[1:common_length] != base_parts[1:common_length])

  if (length(diff_index) == 0) {
    # No difference found within the length of the shortest path
    common_prefix_length <- common_length
  } else {
    # Difference found, get the first difference index
    common_prefix_length <- diff_index[1] - 1
  }

  # Calculate the number of ".." needed to go up from the base path
  up_steps <- rep("..", length(base_parts) - common_prefix_length)

  # Combine the ".." steps with the remaining parts of the absolute path
  remaining_parts <- absolute_parts[(common_prefix_length + 1):length(absolute_parts)]
  relative_parts <- c(up_steps, remaining_parts)

  # Construct the relative path
  relative_path <- do.call(file.path, as.list(relative_parts))

  return(relative_path)
}
