#' @importFrom dplyr arrange filter left_join mutate slice
#' @importFrom dplyr matches select
#' @import grid
#' @importFrom grDevices dev.cur cairo_pdf dev.off dev.set
#' @importFrom piecepackr as_pp_cfg pp_cfg pp_shape pmap_piece crosshairGrob
#' @importFrom rlang abort .data
#' @importFrom stringr str_glue str_replace str_sub
NULL

# Inches
CARD_WIDTH <- 2.25 # Bridge Card
CARD_HEIGHT <- 3.5
BLEED <- 1/8
INDEX_WIDTH <- 0.25
INDEX_HEIGHT <- 1.25
PIP_WIDTH <- CARD_WIDTH - 2 * BLEED - 2 * INDEX_WIDTH
PIP_HEIGHT <- 2 * PIP_WIDTH
