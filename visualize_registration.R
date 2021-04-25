source("functions.R")

#############################

# slice_axis = "x"
# nlin_slice_axis_coordinate = 1.15 
# native_file = "resources/HLHS/subject1_native.mnc"
# lsq6_file = "resources/HLHS/subject1_lsq6.mnc"
# nlin_file = "resources/HLHS/subject1_nlin.mnc"
# study_template_file = "resources/HLHS/study_template.mnc"
# study_mask_file = "resources/HLHS/study_mask.mnc"
# native_to_lsq6_xfm = "resources/HLHS/subject1_lsq6.xfm"
# lsq6_to_nlin_xfm = "resources/HLHS/subject1_N_I_lsq6_lsq12_and_nlin.xfm"
# nlin_abs_jd_file = "resources/HLHS/subject1_absjd.mnc"
# nlin_contour_levels <- c(600, 800, 1000, 1500)
# grid_padding = 0.5
# line_points = 200
# grid_spacing = 0.2
# highres_grid_lines = 12
# tmpdir = "/tmp"
# plot_progress=T

subject <- "subject10"
slice_axis = "y"
nlin_slice_axis_coordinate = -0.288
native_file = glue("resources/HLHS/{subject}_native.mnc")
lsq6_file = glue("resources/HLHS/{subject}_lsq6.mnc")
nlin_file = glue("resources/HLHS/{subject}_nlin.mnc")
study_template_file = "resources/HLHS/study_template.mnc"
study_mask_file = "resources/HLHS/study_mask.mnc"
native_to_lsq6_xfm = glue("resources/HLHS/{subject}_lsq6.xfm")
lsq6_to_nlin_xfm = glue("resources/HLHS/{subject}_N_I_lsq6_lsq12_and_nlin.xfm")
nlin_abs_jd_file = glue("resources/HLHS/{subject}_absjd.mnc")
nlin_contour_levels <- c(800, 1300, 1500)
grid_padding = 0.5
line_points = 200
grid_spacing = 0.2
highres_grid_lines = 12
tmpdir = "/tmp"
plot_progress=T

# subject <- "subject10"
# slice_axis = "y"
# nlin_slice_axis_coordinate = -0.288
# native_file = ""
# lsq6_file = glue("/hpf/largeprojects/MICe/arahman/PhD/CHD_mutants/HLHS_surgical/papertwo_experiments/MRI/MRI_brains/registration_jan132021/hlhs_jan132021_processed/{subject}/resampled/{subject}_N_I_lsq6.mnc")
# nlin_file = glue("/hpf/largeprojects/MICe/arahman/PhD/CHD_mutants/HLHS_surgical/papertwo_experiments/MRI/MRI_brains/registration_jan132021/hlhs_jan132021_processed/{subject}/resampled/{subject}_N_I_lsq6_lsq12_and_nlin-resampled.mnc")
# study_template_file = "resources/HLHS/study_template.mnc"
# study_mask_file = "resources/HLHS/study_mask.mnc"
# native_to_lsq6_xfm = glue("/hpf/largeprojects/MICe/arahman/PhD/CHD_mutants/HLHS_surgical/papertwo_experiments/MRI/MRI_brains/registration_jan132021/hlhs_jan132021_processed/{subject}/transforms/{subject}_lsq6.xfm")
# lsq6_to_nlin_xfm = glue("/hpf/largeprojects/MICe/arahman/PhD/CHD_mutants/HLHS_surgical/papertwo_experiments/MRI/MRI_brains/registration_jan132021/hlhs_jan132021_processed/{subject}/transforms/{subject}_N_I_lsq6_lsq12_and_nlin.xfm")
# nlin_abs_jd_file = glue("/hpf/largeprojects/MICe/arahman/PhD/CHD_mutants/HLHS_surgical/papertwo_experiments/MRI/MRI_brains/registration_jan132021/hlhs_jan132021_processed/{subject}/stats-volumes/{subject}_N_I_lsq6_lsq12_and_nlin_inverted_displ_log_det_abs.mnc")
# nlin_contour_levels <- c(600, 800, 1000, 1500)
# grid_padding = 0.5
# line_points = 200
# grid_spacing = 0.2
# highres_grid_lines = 12
# tmpdir = "/tmp"
# plot_progress=T

dat <- get_pipeline_grids(
  slice_axis = slice_axis, 
  nlin_slice_axis_coordinate = nlin_slice_axis_coordinate, 
  native_file = native_file, lsq6_file = lsq6_file, nlin_file = nlin_file, 
  study_template_file = study_template_file, study_mask_file = study_mask_file, 
  native_to_lsq6_xfm = native_to_lsq6_xfm, lsq6_to_nlin_xfm = lsq6_to_nlin_xfm, 
  nlin_abs_jd_file = nlin_abs_jd_file, 
  nlin_contour_levels = nlin_contour_levels,
  grid_padding = grid_padding, line_points = line_points, grid_spacing = grid_spacing, highres_grid_lines = highres_grid_lines, 
  tmpdir = tmpdir, plot_progress=plot_progress
)

# plot native
dat_space <- dat$native
plt1 <- dat_space$anatomy$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity)) +
  scale_fill_gradient(low="black", high="white", limits=c(10, 50), oob=squish, guide=F) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='#FFAAAA', size=0.5, alpha=0.5, 
            data=dat_space$grid$grid %>% filter(!!sym(dat_space$grid$slice_axis) <= dat_space$grid$slice_axis_coordinate)) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='orange', size=1, alpha=1, 
            data=dat_space$grid$box %>% filter(!!sym(dat_space$grid$slice_axis) <= dat_space$grid$slice_axis_coordinate)) +
  coord_fixed() + 
  labs(title=glue("{subject}"), subtitle="Brain in native space") +
  theme_void()
print(plt1)

# Plot lsq6
dat_space <- dat$lsq6
plt2 <- dat_space$anatomy$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity)) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='#FFAAAA', size=0.5, alpha=0.5, data = dat_space$grid$grid) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='orange', size=1, alpha=1, data=dat_space$grid$box) +
  geom_path(aes(group=obj), color='green', size=0.2, data=dat$study$contours$contours_df) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='red', size=1, alpha=1, data=dat_space$highres_grid_expansion$box) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='blue', size=1, alpha=1, data=dat_space$highres_grid_contraction$box) +
  scale_alpha(range=c(0, 1)) +
  coord_fixed() + 
  labs(title=glue("{subject}"), subtitle="Brain aligned to LSQ6 template") +
  theme_void()
print(plt2)

# Plot nlin
dat_space <- dat$nlin
plt3 <- dat_space$anatomy$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity)) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='#FFAAAA', size=0.5, alpha=0.5, data = dat_space$grid$grid) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='orange', size=1, alpha=1, data=dat_space$grid$box) +
  geom_path(aes(group=obj), color='green', size=0.2, data=dat$study$contours$contours_df) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='red', size=1, alpha=1, data=dat_space$highres_grid_expansion$box) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='blue', size=1, alpha=1, data=dat_space$highres_grid_contraction$box) +
  scale_alpha(range=c(0, 1)) +
  coord_fixed() + 
  labs(title=glue("{subject}"), subtitle="Brain aligned to study (nonlinear) average template") +
  theme_void()
print(plt3)

# Plot template
dat_space <- dat$study
plt4 <- dat_space$template$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity)) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  geom_path(aes(group=obj), color='green', size=0.2, data=dat$study$contours$contours_df) +
  coord_fixed() + 
  labs(title=glue("Target (average template)")) +
  theme_void()
print(plt4)

# Plot JD
plt5 <- dat_space$template$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity), interpolate = T) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  new_scale_fill() +
  geom_raster(aes(fill=intensity), alpha=0.25, data=dat$jd$jd_data$anatomy_df, interpolate = T) +
  scale_fill_gradient2(low="blue", mid="white", high="red", name="Log-transformed\nJacobian determinant", limits=c(-0.2, 0.2), oob=squish) +
  coord_fixed() + 
  labs(title=glue("{subject}"), subtitle=glue("Jacobian determinant (absolute scaling factor, log-transformed)")) +
  theme_void()
print(plt5)

# Plot JD at expansion
plt6 <- dat_space$template$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity), interpolate = T) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  new_scale_fill() +
  geom_raster(aes(fill=intensity), alpha=0.25, data=dat$jd$jd_data$anatomy_df, interpolate = T) +
  scale_fill_gradient2(low="blue", mid="white", high="red", name="Log-transformed\nJacobian determinant", limits=c(-0.2, 0.2), oob=squish, guide=F) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='#FF6666', size=0.5, alpha=0.5, data=dat$nlin$highres_grid_expansion$grid) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='red', size=1, alpha=1, data=dat$nlin$highres_grid_expansion$box) +
  xlim(min(dat$nlin$highres_grid_expansion$box$x)-0.5, max(dat$nlin$highres_grid_expansion$box$x)+0.5) +
  ylim(min(dat$nlin$highres_grid_expansion$box$z)-0.5, max(dat$nlin$highres_grid_expansion$box$z)+0.5) +
  coord_fixed() + 
  theme_void()
print(plt6)

# Plot JD at expansion
plt7 <- dat_space$template$anatomy_df %>%
  ggplot(aes(x=x, y=z)) +
  geom_raster(aes(fill=intensity), interpolate = T) +
  scale_fill_gradient(low="black", high="white", limits=c(250, 2000), oob=squish, guide=F) +
  new_scale_fill() +
  geom_raster(aes(fill=intensity), alpha=0.25, data=dat$jd$jd_data$anatomy_df, interpolate = T) +
  scale_fill_gradient2(low="blue", mid="white", high="red", name="Log-transformed\nJacobian determinant", limits=c(-0.2, 0.2), oob=squish, guide=F) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='#6666FF', size=0.5, alpha=0.5, data=dat$nlin$highres_grid_contraction$grid) +
  geom_path(aes(group=interaction(grid_axis, gridline)), color='blue', size=1, alpha=1, data=dat$nlin$highres_grid_contraction$box) +
  xlim(min(dat$nlin$highres_grid_contraction$box$x)-0.5, max(dat$nlin$highres_grid_contraction$box$x)+0.5) +
  ylim(min(dat$nlin$highres_grid_contraction$box$z)-0.5, max(dat$nlin$highres_grid_contraction$box$z)+0.5) +
  coord_fixed() + 
  theme_void()
print(plt7)


plt_layout <- 
  "
AABBCCDD
AABBCCDD
##FFGGEE
##FFGGEE
"
plt <- plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plt7 + plot_layout(design = plt_layout)
print(plt)

#############################

filename <- "resources/HLHS/subject1_lsq6.mnc"

# lsq6
gs <- get_grid_sequence(file = filename, grid_padding = 0.5, line_points = 500, grid_spacing = 0.2)
gs$grid_sequence$y
base_grid <- get_base_grid(gs, grid_space = "lsq6", slice_axis = "y", slice_axis_coordinate = -2.0)
visualize_grid(base_grid, flip_axes = F)

xfmfile <- "resources/HLHS/subject1_lsq6.xfm"
transformed_space <- "native"
transformed_file <- "resources/HLHS/subject1_native.mnc"
invert <- T

# native
transformed_grid_native <- transform_grids(base_grid, 
                                           xfmfile = xfmfile, 
                                           invert = invert, transformed_space = transformed_space, 
                                           transformed_file = transformed_file, 
                                           tmpdir = "tmp")
visualize_grid(transformed_grid_native, filter_grid = "negative")

xfmfile <- "resources/HLHS/subject1_N_I_lsq6_lsq12_and_nlin.xfm"
transformed_space <- "nlin"
transformed_file <- "resources/HLHS/subject1_nlin.mnc"
invert <- F

# nlin
transformed_grid_nlin <- transform_grids(base_grid, 
                                         xfmfile = xfmfile, 
                                         invert = invert, transformed_space = transformed_space, 
                                         transformed_file = transformed_file, 
                                         tmpdir = "tmp")

visualize_grid(transformed_grid_nlin, filter_grid = "none", interpolate_anatomy = F)