
number_suits <- paste0(rep(c("L", "D"), each = 5L),
                    rep(0:4, 2L))
face_ranks <- c("J", "N", "Q", "K")
fool_ranks <- c("O", "F")
number_ranks <- as.character(0:9)

glyphs <- list("0" = "0", "1" = "1", "2" = "2", "3" = "3", "4" = "4",
               "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9",
               "J" = "♟", "N" = "♞", "Q" = "♛", "K" = "♚",
               LH = "♡", LS = "♤", LC = "♧", LD = "♢",
               DH = "♥", DS = "♠", DC = "♣", DD = "♦",
               "L0" = "🄋", "L1" = "➀", "L2" = "➁", "L3" = "➂", "L4" = "➃",
               "D0" = "🄌", "D1" = "➊", "D2" = "➋", "D3" = "➌", "D4" = "➍",
               "O" = "●", "F" = "★")
light_glyphs <- list("♟" = "♙", "♞" = "♘", "♛" = "♕", "♚" = "♔",
                     "●" = "○", "★" = "☆")

gp_trad_suit <- function(red, suit, light) {
    col <- ifelse(red == "R", red_color(), black_color())
    light_suit <- paste0(light, suit)
    if (light_suit %in% number_suits) {
        gpar(fontsize = 20, fontfamily = "Dejavu Sans", col = col)
    } else {
        gpar(fontsize = 21, fontfamily = "Dejavu Sans", col = col)
    }
}
gp_rank <- function(red, rank, light) {
    col <- ifelse(red == "R", red_color(), black_color())
    fill <- ifelse(light == "D", col, light_color())
    if (rank %in% fool_ranks) {
        gp_rank <- gpar(fontsize = 25, fontfamily = "Dejavu Sans", col = col, fill = fill)
    } else {
        gpar(fontsize = 30, fontfamily = "Dejavu Sans", col = col, fill = fill)
    }
}

top_suit_grob <- function(tsuit, tlight, red, ...) {
    light_suit <- paste0(tlight, tsuit)
    glyph <- glyphs[[light_suit]]
    gp_tsuit <- gp_trad_suit(red, tsuit, tlight)
    tsuit_grob <- textGrob(glyph, gp = gp_tsuit)
    tsuit_grob
}

bot_suit_grob <- function(bsuit, blight, red, ...) {
    if (is.na(bsuit)) return(nullGrob())
    light_suit <- paste0(blight, bsuit)
    glyph <- glyphs[[light_suit]]
    gp_bsuit <- gp_trad_suit(red, bsuit, blight)
    bsuit_grob <- textGrob(glyph, gp = gp_bsuit)
    bsuit_grob
}

top_rank_grob <- function(trank, tlight, red, ...) {
    glyph <- glyphs[[trank]]
    trank_grob <- textGrob(glyph, gp = gp_rank(red, trank, tlight))
    if (tlight == "L") {
        gp <- gp_rank(red, trank, tlight)
        if (trank %in% face_ranks) {
            trank_grob <- textGrob(light_glyphs[[glyph]], gp = gp)
        } else {
            trank_grob <- strokeGrob(trank_grob, gp = gp)
        }
    }
    trank_grob
}

bot_rank_grob <- function(brank, blight, red, ...) {
    glyph <- glyphs[[brank]]
    brank_grob <- textGrob(glyph, gp = gp_rank(red, brank, blight))
    if (blight == "L") {
        gp <- gp_rank(red, brank, blight)
        if (brank %in% face_ranks) {
            brank_grob <- textGrob(light_glyphs[[glyph]], gp = gp)
        } else {
            brank_grob <- strokeGrob(brank_grob, gp = gp)
        }
    }
    brank_grob
}

top_border_grob <- function(...) {
    l <- list(...)
    bar_width <- unit(INDEX_WIDTH - 0.05, "in")
    y_top <- unit(1, "npc") - unit(BLEED, "in") - 0.5 * bar_width
    width <- unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_WIDTH + 0.2, "in")
    if (l$red == "R") {
        col <- red_color()
    } else {
        col <- black_color()
    }
    # col <- "black"
    if(l$tlight == "D") {
        gp <- gpar(col = col, fill = col, lwd = 1)
        gp_label <- gp_label(col = light_color())
        fill <- "white"
        colour <- col
    } else {
        gp <- gpar(col = col, fill = light_color(), lwd = 1)
        gp_label <- gp_label(col = col)
        fill <- col
        colour <- light_color()
    }
    pat <- gridpattern::patternFill("weave", units = "cm",
                                     spacing = 0.26, angle = 0,
                                     xoffset = 0.13, yoffset = 0.15,
                                     density = 0.25, type = "plain",
                                     fill = fill, fill2 = fill,
                                     colour = colour, linewidth = 0.4)
    tt_grob <- rectGrob(y = y_top, width = width, height = bar_width,
                        gp = gp, name = "top_bar")
    tt_grob <- grobTree(tt_grob,
                        rectGrob(y = y_top, width = width, height = bar_width,
                        gp = gpar(col = col, fill = pat)))
    # label_grob <- textGrob("Test top label", y = y_top, gp = gp_label)
    label_grob <- nullGrob()

    height <- 0.5 * (unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_HEIGHT + 0.2, "in"))
    y_side <- unit(0.5, "npc") + 0.5 * height
    x_left <- unit(BLEED + 0.5 * INDEX_WIDTH, "in")
    x_right <- unit(1, "npc") - x_left
    tl_grob <- rectGrob(x = x_left, y = y_side,
                        width = bar_width, height = height,
                        gp = gp)
    tl_grob <- grobTree(tl_grob,
                        rectGrob(x = x_left, y = y_side,
                            width = bar_width, height = height,
                            gp = gpar(col = col, fill = pat)))
    tr_grob <- rectGrob(x = x_right, y = y_side,
                        width = bar_width, height = height,
                        gp = gp)
    tr_grob <- grobTree(tr_grob,
                        rectGrob(x = x_right, y = y_side,
                            width = bar_width, height = height,
                            gp = gpar(col = col, fill = pat)))
    grobTree(tt_grob, label_grob, tl_grob, tr_grob, name = "top_half_bars")
}

gp_label <- function(col) {
    gpar(col = col, fontsize = 11, fontfamily = "EB Garamond")
}

bot_border_grob <- function(...) {
    l <- list(...)
    width <- unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_WIDTH + 0.2, "in")
    bar_width <- unit(INDEX_WIDTH - 0.05, "in")
    y_bot <- unit(BLEED, "in") + 0.5 * bar_width
    if (l$red == "R") {
        col <- red_color()
    } else {
        col <- black_color()
    }
    # col <- "black"
    if(l$blight == "D") {
        gp <- gpar(col = col, fill = col, lwd = 1)
        gp_label <- gp_label(col = light_color())
        fill <- "white"
        colour <- col
    } else {
        gp <- gpar(col = col, fill = light_color(), lwd = 1)
        gp_label <- gp_label(col = col)
        fill <- col
        colour <- light_color()
    }
    pat <- gridpattern::patternFill("weave", units = "cm",
                                     spacing = 0.26, angle = 0,
                                     xoffset = 0.13, yoffset = 0.15,
                                     density = 0.2, type = "plain",
                                     fill = fill, fill2 = fill,
                                     colour = colour, linewidth = 0.4)
    bb_grob <- rectGrob(y = y_bot, width = width, height = bar_width,
                        gp = gp, name = "top_bar")
    if (l$brank == "N") {
        bb_grob <- grobTree(bb_grob,
                            rectGrob(y = y_bot, width = width, height = bar_width,
                            gp = gpar(col = col, fill = pat)))
    }
    # label_grob <- textGrob("Test bottom label", y = y_bot, gp = gp_label, rot = 180)
    label_grob <- nullGrob()

    height <- 0.5 * (unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_HEIGHT + 0.2, "in"))
    y_side <- unit(0.5, "npc") - 0.5 * height
    x_left <- unit(BLEED + 0.5 * INDEX_WIDTH, "in")
    x_right <- unit(1, "npc") - x_left
    bl_grob <- rectGrob(x = x_left, y = y_side,
                        width = bar_width, height = height,
                        gp = gp)
    if (l$brank == "N") {
        bl_grob <- grobTree(bl_grob,
                            rectGrob(x = x_left, y = y_side,
                                width = bar_width, height = height,
                                gp = gpar(col = col, fill = pat)))
    }
    br_grob <- rectGrob(x = x_right, y = y_side,
                        width = bar_width, height = height,
                        gp = gp)
    if (l$brank == "N") {
        br_grob <- grobTree(br_grob,
                            rectGrob(x = x_right, y = y_side,
                                width = bar_width, height = height,
                                gp = gpar(col = col, fill = pat)))
    }
    grobTree(bb_grob, label_grob, bl_grob, br_grob, name = "bottom_half_bars")
}

card_grob <- function(...) {
    l <- list(...)
    top_corner <- do.call(top_corner_grob, l)
    bot_corner <- do.call(bot_corner_grob, l)

    top_inner <- do.call(top_inner_grob, l)
    bot_inner <- do.call(bot_inner_grob, l)

    # top_border <- do.call(top_border_grob, l)
    # bot_border <- do.call(bot_border_grob, l)

    # Bleed zone, safe zone, and/or middle line
    # if (l$double9) {
    #     dot_grob <- circleGrob(r = unit(0.075, "in"), gp = gpar(col = NA, fill = "black"))
    # } else {
    #     dot_grob <- nullGrob()
    # }
    grob_other <- grobTree(
          # rectGrob(gp = gpar(col = NA, fill = "yellow")), # bleed
          rectGrob(width = 2, height = 3.25, default.units = "in", gp = gpar(col = NA, fill = "white")),
          # rectGrob(width = PIP_WIDTH, height = PIP_HEIGHT, default.units = "in", gp = gpar(col = NA, fill = "magenta")), # inner pip drawing area
          linesGrob(x = c(0.2, 0.8), y = 0.5, default.units = "npc", gp = gpar(col = "black", lwd = 4)),
          # top_border, bot_border,
          top_inner, bot_inner
    )

    gp_small <- gpar(cex = 0.5, lex = 0.5)
    y_top_corner <- unit(1, "npc") - unit(BLEED + 0.5 * INDEX_HEIGHT, "in")
    # Top
    x_index_l <- unit(BLEED + 0.5 * INDEX_WIDTH, "in")
    x_index_r <- unit(1, "npc") - x_index_l
    grob_top <- grobTree(
          grobTree(top_corner, vp = viewport(x = x_index_l, y = y_top_corner)),
          grobTree(top_corner, vp = viewport(x = x_index_r, y = y_top_corner)),
          name = "top"
    )
    grob_bot <- grobTree(
          grobTree(bot_corner, vp = viewport(x = x_index_l, y = y_top_corner)),
          grobTree(bot_corner, vp = viewport(x = x_index_r, y = y_top_corner)),
          vp = viewport(angle = 180), name = "bottom"
    )
    gList(grob_other, grob_top, grob_bot)
}

top_inner_grob <- function(...) {
    l <- list(...)
    trank <- l$trank
    if (trank %in% number_ranks) {
        grob <- do.call(top_pip_grob, l)
    } else {
        grob <- do.call(top_face_grob, l)
    }
    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"),
                   y = unit(0.5, "npc") + unit(0.25 * PIP_HEIGHT, "in"))
    grobTree(grob, vp = vp)
}

bot_inner_grob <- function(...) {
    l <- list(...)
    brank <- l$brank
    if (brank %in% number_ranks) {
        grob <- do.call(bot_pip_grob, l)
    } else {
        grob <- do.call(bot_face_grob, l)
    }
    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"),
                   y = unit(0.5, "npc") - unit(0.25 * PIP_HEIGHT, "in"),
                   angle = 180)
    grobTree(grob, vp = vp)
}
