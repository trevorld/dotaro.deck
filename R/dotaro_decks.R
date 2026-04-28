#' Return piecepackr configurations of Dotaro Decks
#'
#' `dotaro_decks()` returns a list of piecepackr configurations
#'
#' @param ... Ignored
#' @param round Should the card's corners be rounded
#' @param border Should the card have a black border
#' @return A list of [piecepackr::pp_cfg()] objects.
#' @examples
#' decks <- dotaro_decks()
#' if (piecepackr:::device_supports_unicode()) {
#'   cfg <- decks$dotaro_full_traditional
#'   grid::grid.newpage()
#'   piecepackr::grid.piece("card_face", suit = 4, rank = 4, cfg = cfg)
#' }
#' if (piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   piecepackr::grid.piece("card_face", suit = 8, rank = 14, cfg = cfg)
#' }
#' if (piecepackr:::device_supports_unicode()) {
#'   cfg <- decks$dotaro_full_number
#'   grid::grid.newpage()
#'   piecepackr::grid.piece("card_face", suit = 4, rank = 5, cfg = cfg)
#' }
#' if (piecepackr:::device_supports_unicode()) {
#'   grid::grid.newpage()
#'   piecepackr::grid.piece("card_face", suit = 9, rank = 9, cfg = cfg)
#' }
#' if (piecepackr:::device_supports_unicode()) {
#'   cfg <- decks$dotaro_full_fool
#'   grid::grid.newpage()
#'   piecepackr::grid.piece("card_face", suit = 2, rank = 2, cfg = cfg)
#' }
#' @export
dotaro_decks <- function(..., round = TRUE, border = TRUE) {
    check_dots_empty()
    rect_shape <- ifelse(round, "roundrect", "rect")
    if (border) {
        border_color <- "black"
        border_lex <- 4
    } else {
        border_color <- "transparent"
        border_lex <- 0
    }
    list(
         dotaro_full_fool = dotaro_full_fool(rect_shape, border_color, border_lex),
         dotaro_full_number = dotaro_full_number(rect_shape, border_color, border_lex),
         dotaro_full_traditional = dotaro_full_traditional(rect_shape, border_color, border_lex),
         dotaro_corner_fool = dotaro_corner_fool(rect_shape, border_color, border_lex),
         dotaro_corner_number = dotaro_corner_number(rect_shape, border_color, border_lex),
         dotaro_corner_traditional = dotaro_corner_traditional(rect_shape, border_color, border_lex)
    )
}

dotaro_full_fool <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 2L, n_ranks = 2L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = 3.5, width = 2.25, # bridge size
              grob_fn.card_face = dotaroFoolFaceGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaro_full_traditional <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 8L, n_ranks = 14L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = 3.5, width = 2.25, # bridge size
              grob_fn.card_face = dotaroTradFaceGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaro_full_number <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 10L, n_ranks = 10L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = 3.5, width = 2.25, # bridge size
              grob_fn.card_face = dotaroNumFaceGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaro_corner_traditional <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 8L, n_ranks = 14L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = INDEX_HEIGHT + 2 * BLEED,
              width = INDEX_WIDTH + 2 * BLEED,
              grob_fn.card_face = dotaroTradCornerGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaro_corner_number <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 10L, n_ranks = 10L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = INDEX_HEIGHT + 2 * BLEED,
              width = INDEX_WIDTH + 2 * BLEED,
              grob_fn.card_face = dotaroNumCornerGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaro_corner_fool <- function(rect_shape, border_color, border_lex) {
    l <- list(n_suits = 2L, n_ranks = 2L,
              border_color = border_color, border_lex = border_lex, shape.card = rect_shape,
              height = INDEX_HEIGHT + 2 * BLEED,
              width = INDEX_WIDTH + 2 * BLEED,
              grob_fn.card_face = dotaroFoolCornerGrob)
    cfg <- pp_cfg(l)
    cfg$has_piecepack <- FALSE
    cfg$has_cards <- TRUE
    cfg
}

dotaroFoolFaceGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_fool_face")
}

#' @export
makeContext.dotaro_fool_face <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_fool_face <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_fool_face <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("1", "O") |>
        str_replace("2", "F")
    suit <- x$suit
    light <- ifelse(suit < 2L, "D", "L")
    df_card <- filter(card_info, .data$blight == light, is.na(.data$bsuit), .data$brank == rank)
    main_grob <- grobTree(do.call(card_grob, df_card),
                          vp = viewport(angle = 180))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}

dotaroNumFaceGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_num_face")
}

#' @export
makeContext.dotaro_num_face <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_num_face <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_num_face <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("10", "0")
    suit <- x$suit
    light <- ifelse(suit <= 5L, "D", "L")
    suit <- (ifelse(suit > 5L, suit - 5L, suit) - 1L) |> as.character()
    df_card <- filter(card_info, .data$blight == light, .data$bsuit == suit, .data$brank == rank)
    main_grob <- grobTree(do.call(card_grob, df_card),
                          vp = viewport(angle = 180))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}

dotaroTradFaceGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_trad_face")
}

#' @export
makeContext.dotaro_trad_face <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_trad_face <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_trad_face <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("10", "0") |>
        str_replace("11", "J") |>
        str_replace("12", "N") |>
        str_replace("13", "Q") |>
        str_replace("14", "K")
    suit <- x$suit
    light <- ifelse(suit <= 4L, "D", "L")
    suit <- switch((suit %% 4L) + 1L, "D", "H", "S", "C")
    df_card <- filter(card_info, .data$tlight == light, .data$tsuit == suit, .data$trank == rank)
    main_grob <- do.call(card_grob, df_card)
    if (rank == "N" && light == "L")
        main_grob <- grobTree(main_grob, vp = viewport(angle = 180))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}

dotaroTradCornerGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_trad_corner")
}

#' @export
makeContext.dotaro_trad_corner <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_trad_corner <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_trad_corner <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("10", "0") |>
        str_replace("11", "J") |>
        str_replace("12", "N") |>
        str_replace("13", "Q") |>
        str_replace("14", "K")
    suit <- x$suit
    light <- ifelse(suit <= 4L, "D", "L")
    suit <- switch((suit %% 4L) + 1L, "D", "H", "S", "C")
    if (rank == "N" && light == "L") {
        df_card <- filter(card_info, .data$tlight == "D", .data$tsuit == suit, .data$trank == rank)
        main_grob <- do.call(bot_corner_grob, df_card)
    } else {
        df_card <- filter(card_info, .data$tlight == light, .data$tsuit == suit, .data$trank == rank)
        main_grob <- do.call(top_corner_grob, df_card)
    }
    # main_grob <- grobTree(main_grob, vp = viewport(y = 0.4))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}

dotaroNumCornerGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_num_corner")
}

#' @export
makeContext.dotaro_num_corner <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_num_corner <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_num_corner <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("10", "0")
    suit <- x$suit
    light <- ifelse(suit <= 5L, "D", "L")
    suit <- ifelse(suit > 5L, suit - 5L, suit)
    suit <- as.character(suit - 1L)

    df_card <- filter(card_info, .data$blight == light, .data$bsuit == suit, .data$brank == rank)
    main_grob <- do.call(bot_corner_grob, df_card)
    # main_grob <- grobTree(main_grob, vp = viewport(y = 0.4))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}

dotaroFoolCornerGrob <- function(piece_side, suit, rank, cfg=pp_cfg()) {
    cfg <- as_pp_cfg(cfg)
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    gTree(opt=opt, border=TRUE, flip=FALSE, scale=1,
          suit=suit, rank=rank,
          name=NULL, gp=gpar(), vp=NULL, cl="dotaro_fool_corner")
}

#' @export
makeContext.dotaro_fool_corner <- function(x) {
    x <- piecepackr:::update_gp(x, gp = gpar(cex = x$scale, lex = x$scale))
    x
}

#' @export
grobCoords.dotaro_fool_corner <- function(x, closed, ...) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    grobCoords(shape$shape(vp=x$vp), closed=closed, ...)
}

#' @export
makeContent.dotaro_fool_corner <- function(x) {
    opt <- x$opt
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back, width = opt$shape_w, height = opt$shape_h)
    # Possibly shrink background and gridlines to not overlap mat
    # which sometimes prevents visual glitch if no border line
    # but do not do this if mat color is transparent.
    if (any(as.logical(grDevices::col2rgb(opt$mat_color, alpha = TRUE)[4, ] < 255)))
        bg_mat_width <- 0
    else
        bg_mat_width <- opt$mat_width

    # Background
    background_grob <- shape$shape(gp=gpar(col=NA, fill=opt$background_color),
                                   name = "background", mat_width = bg_mat_width)
    gp_mat <- gpar(col = NA, lwd = 0, fill = opt$mat_color)
    mat_grob <- shape$mat(opt$mat_width, gp = gp_mat, name = "mat")

    # Main grob
    rank <- as.character(x$rank) |>
        str_replace("1", "O") |>
        str_replace("2", "F")
    suit <- x$suit
    light <- ifelse(suit < 2L, "D", "L")

    df_card <- filter(card_info, .data$blight == light, is.na(.data$bsuit), .data$brank == rank)
    main_grob <- do.call(bot_corner_grob, df_card)
    # main_grob <- grobTree(main_grob, vp = viewport(y = 0.4))

    if (x$border) {
        gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
        border_grob <- shape$shape(gp=gp_border, name = "border")
    } else {
        border_grob <- nullGrob(name = "border")
    }
    if (x$flip)
        gl <- gList(main_grob, mat_grob, background_grob, border_grob)
    else
        gl <- gList(background_grob, mat_grob, main_grob, border_grob)

    setChildren(x, gl)
}
