get_file_shape <- function(file) {
  
  # Get dimensions of array
  file_dims <- minc.dimensions.sizes(file) # Returns lengths as z, y, x 
  names(file_dims) <- c("z", "y", "x")
  
  # Get step size (resolution) of array
  file_steps <- minc.separation.sizes(file) # Returns steps as z, y, x
  file_step_signs <- sign(file_steps) # Also z, y, x
  names(file_steps) <- c("z", "y", "x")
  names(file_step_signs) <- c("z", "y", "x")
  
  # Get coordinate ranges of array
  file_starts <- mincConvertVoxelToWorld(file, 0, 0, 0) # Input z, y, x; returns as x, y, z
  file_ends <- mincConvertVoxelToWorld(file, file_dims[1]-1, file_dims[2]-1, file_dims[3]-1) # Input z, y, x; returns as x, y, z
  file_range <- file_ends - file_starts
  names(file_starts) <- c("x", "y", "z")
  names(file_ends) <- c("x", "y", "z")
  names(file_range) <- c("x", "y", "z")
  
  # Object to return
  out <- list(file=file,
              dims=rev(file_dims),
              steps=rev(file_steps)*rev(file_step_signs),
              direction=rev(file_step_signs),
              starts=c(ifelse(file_step_signs["x"] > 0, file_starts["x"], file_ends["x"]),
                       ifelse(file_step_signs["y"] > 0, file_starts["y"], file_ends["y"]),
                       ifelse(file_step_signs["z"] > 0, file_starts["z"], file_ends["z"])
              ),
              ends=c(ifelse(file_step_signs["x"] > 0, file_ends["x"], file_starts["x"]),
                     ifelse(file_step_signs["y"] > 0, file_ends["y"], file_starts["y"]),
                     ifelse(file_step_signs["z"] > 0, file_ends["z"], file_starts["z"])
              ),
              range=rev(file_step_signs)*file_range
  )
  
  # Return
  return(out)
}

#############################

get_grid_sequence <- function(file, grid_padding=-0.5, line_points=100, grid_spacing=NULL, grid_lines=NULL) {
  
  file_shape <- get_file_shape(file)
  
  # Get starting coordinate
  start_x <- file_shape$starts["x"] + grid_padding
  start_y <- file_shape$starts["y"] + grid_padding
  start_z <- file_shape$starts["z"] + grid_padding
  
  
  # Get ending coordinate
  end_x <- file_shape$ends["x"] - grid_padding
  end_y <- file_shape$ends["y"] - grid_padding
  end_z <- file_shape$ends["z"] - grid_padding
  
  # Define position of bounding box for gridlines
  box_sequence <- list(x=c(start_x, end_x),
                       y=c(start_y, end_y),
                       z=c(start_z, end_z)
  )
  
  # Define position of gridlines along each axis
  if (!is.null(grid_lines) & !is.null(grid_spacing)) {
    stop("You must only specify exactly one of grid_lines or grid_spacing!")
  }
  
  if (!is.null(grid_lines) & is.null(grid_spacing)) {
    grid_sequence <- list(x=seq(from=start_x, 
                                to=end_x, 
                                length.out=grid_lines),
                          y=seq(from=start_y, 
                                to=end_y, 
                                length.out=grid_lines),
                          z=seq(from=start_z, 
                                to=end_z, 
                                length.out=grid_lines)
    )
  } else if (!is.null(grid_spacing) & is.null(grid_lines)) {
    grid_sequence <- list(x=seq(from=start_x, 
                                to=end_x, 
                                by=grid_spacing),
                          y=seq(from=start_y, 
                                to=end_y, 
                                by=grid_spacing),
                          z=seq(from=start_z, 
                                to=end_z, 
                                by=grid_spacing)
    )
  } else {
    stop("You must only specify exactly one of grid_lines or grid_spacing!")
  }
  
  
  # Define position of points along each grid and box line
  point_sequence <- list(x=seq(from=start_x, 
                               to=end_x, 
                               length.out=line_points),
                         y=seq(from=start_y, 
                               to=end_y, 
                               length.out=line_points),
                         z=seq(from=start_z, 
                               to=end_z, 
                               length.out=line_points)
  )
  
  # Output object
  out <- list(box_sequence=box_sequence,
              grid_sequence=grid_sequence,
              point_sequence=point_sequence)
  
  # Return
  return(out)
}

#############################

# Function to create a series points along lines at a sequence of points
# Creates lines along one axis (half of what's needed for a rectangular grid array)
get_half_grid <- function(grid_coordinates, # Sequence of points along main "grid" axis
                          line_coordinates, # Sequence of points along each grid line
                          grid_type="grid", # Type of grid (full grid or boundingbox ?)
                          grid_space="native", # Space in which grid is defined
                          grid_axis=NA,  # Which axis is the "grid" axis?
                          slice_axis=NA, # Which axis is the slice axis?
                          slice_axis_coordinate=NA) {
  
  # Get axes
  all_axes <- c("x", "y", "z")
  line_axis <- setdiff(all_axes, c(grid_axis, slice_axis))
  
  # Map along each of the axis coordinates to draw a line
  out <- seq_along(grid_coordinates) %>%
    map_dfr(function(lc) {
      
      # Map over points in the line
      seq_along(line_coordinates) %>%
        map_dfr(function(i) {
          tibble(grid_type=grid_type, 
                 grid_space=grid_space,
                 slice_axis=slice_axis,
                 grid_axis=grid_axis, 
                 gridline=lc, 
                 slice_axis_coordinate=slice_axis_coordinate,
                 grid_axis_coordinate=grid_coordinates[[lc]],
                 line_axis_coordinate=line_coordinates[[i]])
        })
      
    }) 
  
  # Rename axes
  out <- out %>%
    rename(!!sym(slice_axis):=slice_axis_coordinate,
           !!sym(grid_axis):=grid_axis_coordinate,
           !!sym(line_axis):=line_axis_coordinate) %>%
    select(grid_type, grid_space,
           slice_axis, grid_axis,
           gridline, 
           x, y, z)
  
  # Return
  return(out)
}

#############################

get_full_grid <- function(grid_sequence,  # Sequence of points along main "grid" axis (for all axes)
                          point_sequence, # Sequence of points along each grid line (for all axes)
                          grid_type="grid", #  Type of grid (full grid or boundingbox ?) - depends on grid_sequence input type
                          grid_space=NA, # Space in which grid is defined)
                          slice_axis="y", # Slice plane
                          slice_axis_coordinate=NA # Which slice is this?
                          ) {
  
  # Get axes
  all_axes <- c("x", "y", "z")
  grid_and_line_axes <- setdiff(all_axes, slice_axis)
  
  # Compute vertical grid
  grid_axis <- grid_and_line_axes[[1]]
  line_axis <- grid_and_line_axes[[2]]
  
  vertical_grid <- get_half_grid(grid_coordinates = grid_sequence[[grid_axis]],
                                 line_coordinates = point_sequence[[line_axis]], 
                                 grid_type=grid_type, 
                                 grid_space=grid_space,
                                 grid_axis=grid_axis,  
                                 slice_axis=slice_axis,
                                 slice_axis_coordinate=slice_axis_coordinate)
  
  # Compute horizontal grid
  grid_axis <- grid_and_line_axes[[2]]
  line_axis <- grid_and_line_axes[[1]]
  
  horizontal_grid <- get_half_grid(grid_coordinates = grid_sequence[[grid_axis]],
                                   line_coordinates = point_sequence[[line_axis]], 
                                   grid_type=grid_type,
                                   grid_space=grid_space,
                                   grid_axis=grid_axis, 
                                   slice_axis=slice_axis, 
                                   slice_axis_coordinate=slice_axis_coordinate)
  
  # Bind perpendicular grids together
  out <- rbind(vertical_grid, horizontal_grid)
  
  # Return
  return(out)
  
}

#############################

get_base_grid <- function(gs, # Output of get_grid_sequence()
                          grid_space=NA,
                          slice_axis="y",
                          slice_axis_coordinate=0
) {
  
  # Get the point sequences
  box_sequence <- gs$box_sequence
  grid_sequence <- gs$grid_sequence
  point_sequence <- gs$point_sequence
  
  # Get the grid and box sequence
  grid_df <- get_full_grid(grid_sequence = gs$grid_sequence, 
                           point_sequence = gs$point_sequence,
                           grid_type = "grid", 
                           grid_space = grid_space, 
                           slice_axis = slice_axis, 
                           slice_axis_coordinate = slice_axis_coordinate)
  
  box_df <- get_full_grid(grid_sequence = gs$box_sequence, 
                          point_sequence = gs$point_sequence,
                          grid_type = "box", 
                          grid_space = grid_space, 
                          slice_axis = slice_axis, 
                          slice_axis_coordinate = slice_axis_coordinate)
  
  # Output
  out <- list(grid=grid_df,
              box=box_df,
              grid_space=grid_space,
              
              slice_axis=slice_axis,
              grid_axis=grid_axis,
              line_axis=line_axis)
  
  # Return
  return(out)
}

#############################

prepare_anatomy <- function() {
  
}

#############################

# Visualize grid, and underlying anatomy if df provided
visualize_grid <- function(grid_df) {
  
  all_axes <- c("x", "y", "z")
  grid_and_line_axes <- setdiff(all_axes, slice_axis)
  
  grid_df$grid
  
  grid_df %>%
    ggplot(aes(x=, y=))
}

#############################

gs <- get_grid_sequence(file = file, grid_padding = -0.5, line_points = 100, grid_lines = 20)
grid_df <- get_base_grid(gs, grid_space = "native", slice_axis = "y", slice_axis_coordinate = 0)





  
  


write_tag <- function(df, outfile, clobber=F) {
  
  # Check file exists
  if (file.exists(outfile)) {
    print(glue("Tag file exists: {outfile}"))
    if (clobber) {
      print(glue("Overwriting tag file..."))
    } else {
      stop("Set clobber=TRUE to overwrite existing tag file")
    }
  } else {
    print(glue("Creating tag file: {outfile}"))
  }
  
  # Open connection to file
  fcon <- file(outfile, open = "wt")
  
  # Header
  writeLines("MNI Tag Point File", con = fcon, sep = "\n")
  writeLines("Volumes = 1;", con = fcon, sep = "\n")
  writeLines("", con = fcon, sep = "\n")
  writeLines("Points =", con = fcon, sep = "\n")
  
  # Write points
  prog <- txtProgressBar(max=nrow(df), style=3)
  for (i in 1:nrow(df)) {
    writeLines(paste0(" ", df$x[i], " ", df$z[i], " ", df$y[i], " ", "\"\""), con = fcon, sep = "\n")
    setTxtProgressBar(prog, i)
  }
  writeLines(";", con = fcon, sep = "\n")
  
  # Close file connection
  close(fcon)
  
  # Done
  print(glue("Done writing tag file: {outfile}"))
}