
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

gp_trad_suit <- gpar(fontsize = 21, fontfamily = "Dejavu Sans")
gp_rank <- gpar(fontsize = 30, fontfamily = "Dejavu Sans")

top_suit_grob <- function(tsuit, tlight, ...) {
    light_suit <- paste0(tlight, tsuit)
    glyph <- glyphs[[light_suit]]
    gp_tsuit <- gp_trad_suit
    tsuit_grob <- textGrob(glyph, gp = gp_tsuit)
    tsuit_grob
}

bot_suit_grob <- function(bsuit, blight, ...) {
    if (is.na(bsuit)) return(nullGrob())
    light_suit <- paste0(blight, bsuit)
    glyph <- glyphs[[light_suit]]
    if (light_suit %in% number_suits) {
        gp_bsuit <- gpar(fontsize = 20, fontfamily = "Dejavu Sans")
    } else {
        gp_bsuit <- gp_trad_suit
    }
    bsuit_grob <- textGrob(glyph, gp = gp_bsuit)
    bsuit_grob
}

top_rank_grob <- function(trank, tlight, ...) {
    glyph <- glyphs[[trank]]
    trank_grob <- textGrob(glyph, gp = gp_rank)
    if (tlight == "L") {
        if (trank %in% face_ranks) {
            trank_grob <- textGrob(light_glyphs[[glyph]], gp = gp_rank)
        } else {
            trank_grob <- strokeGrob(trank_grob)
        }
    }
    trank_grob
}

bot_rank_grob <- function(brank, blight, ...) {
    glyph <- glyphs[[brank]]
    if (brank %in% fool_ranks) {
        gp_rank <- gpar(fontsize = 25, fontfamily = "Dejavu Sans")
    }
    brank_grob <- textGrob(glyph, gp = gp_rank)
    if (blight == "L") {
        if (brank %in% face_ranks) {
            brank_grob <- textGrob(light_glyphs[[glyph]], gp = gp_rank)
        } else {
            brank_grob <- strokeGrob(brank_grob)
        }
    }
    brank_grob
}


card_grob <- function(...) {
    l <- list(...)
    top_corner <- do.call(top_corner_grob, l)
    bot_corner <- do.call(bot_corner_grob, l)

    top_inner <- do.call(top_inner_grob, l)
    bot_inner <- do.call(bot_inner_grob, l)

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
          top_inner,
          bot_inner
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
