## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  fig.retina = 2
)
library(fdacluster)
library(future)
load("../R/sysdata.rda")

## ----fdahclust-overview, eval=FALSE-------------------------------------------
#  fdahclust(
#    x,
#    y,
#    n_clusters = 1L,
#    warping_class = c("affine", "dilation", "none", "shift", "srsf"),
#    centroid_type = c("mean", "medoid", "lowess", "poly"),
#    metric = c("l2", "pearson"),
#    linkage_criterion = c("complete", "average", "single", "ward.D2"),
#    cluster_on_phase = FALSE,
#    use_verbose = TRUE,
#  
#    warping_options = c(0.15, 0.15),
#    maximum_number_of_iterations = 100L,
#    number_of_threads = 1L,
#    parallel_method = 0L,
#    distance_relative_tolerance = 0.001,
#    use_fence = FALSE,
#    check_total_dissimilarity = TRUE,
#    compute_overall_center = FALSE
#  )

## ----data, echo=FALSE---------------------------------------------------------
matplot(
  x = t(simulated30_sub$x),
  y = t(simulated30_sub$y[, 1,]),
  type = "l",
  xlab = "x",
  ylab = "y"
)

## ----hac-amplitude-run--------------------------------------------------------
out1 <- fdahclust(
  simulated30_sub$x,
  simulated30_sub$y,
  n_clusters = 2,
  centroid_type = "mean",
  warping_class = "affine",
  metric = "l2", 
  cluster_on_phase = FALSE
)

## ----hac-amplitude-viz-amplitude----------------------------------------------
plot(out1, type = "amplitude")

## ----hac-amplitude-viz-phase--------------------------------------------------
plot(out1, type = "phase")

## ----hac-amplitude-viz-diag---------------------------------------------------
diagnostic_plot(out1)

## ----hac-phase-run------------------------------------------------------------
out2 <- fdahclust(
  simulated30_sub$x,
  simulated30_sub$y,
  n_clusters = 3,
  centroid_type = "mean",
  warping_class = "affine",
  metric = "l2", 
  cluster_on_phase = TRUE
)

## ----hac-phase-viz------------------------------------------------------------
plot(out2, type = "amplitude")
plot(out2, type = "phase")
diagnostic_plot(out2)

## ---- eval=FALSE--------------------------------------------------------------
#  ncores <- max(parallel::detectCores() - 1L, 1L)
#  plan(multisession, workers = ncores)
#  amplitude_data <- compare_caps(
#    x = simulated30_sub$x,
#    y = simulated30_sub$y,
#    n_clusters = 1:5,
#    metric = "l2",
#    warping_class = c("none", "shift", "dilation", "affine"),
#    clustering_method = "hclust-complete",
#    centroid_type = "mean",
#    cluster_on_phase = FALSE
#  )
#  plan(sequential)

## -----------------------------------------------------------------------------
plot(amplitude_data, validation_criterion = "wss", what = "mean")

## ---- eval=FALSE--------------------------------------------------------------
#  ncores <- max(parallel::detectCores() - 1L, 1L)
#  plan(multisession, workers = ncores)
#  phase_data <- compare_caps(
#    x = simulated30_sub$x,
#    y = simulated30_sub$y,
#    n_clusters = 1:5,
#    metric = "l2",
#    warping_class = c("shift", "dilation", "affine"),
#    clustering_method = "hclust-complete",
#    centroid_type = "mean",
#    cluster_on_phase = TRUE
#  )
#  plan(sequential)

## -----------------------------------------------------------------------------
plot(phase_data, validation_criterion = "wss", what = "mean")

