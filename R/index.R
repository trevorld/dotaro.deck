top_corner_grob <- function(...) {
    l <- list(...)
    tsuit_grob <- do.call(top_suit_grob, l)
    trank_grob <- do.call(top_rank_grob, l)

    bsuit_grob <- do.call(bot_suit_grob, l)
    brank_grob <- do.call(bot_rank_grob, l)
    gp_small <- gpar(cex = 0.6, lex = 0.6)
    y_top_rank <- unit(1, "npc") - unit(0.15, "in")
    y_top_suit <- y_top_rank - unit(0.35, "in")
    y_line <- y_top_suit - unit(0.2, "in")
    y_bot_rank <- y_line - unit(0.2, "in")
    y_bot_suit <- y_bot_rank - unit(0.20, "in")
    small_lines <- linesGrob(y = 0.5)
    grobTree(
             # rectGrob(gp = gpar(col = NA, fill = "cyan")), # index area
             grobTree(tsuit_grob, vp = viewport(y = y_top_suit)),
             grobTree(trank_grob, vp = viewport(y = y_top_rank)),
             grobTree(small_lines, vp = viewport(y = y_line, width = unit(INDEX_WIDTH * 0.5, "in"))),
             grobTree(bsuit_grob, vp = viewport(y = y_bot_suit), gp = gp_small),
             grobTree(brank_grob, vp = viewport(y = y_bot_rank), gp = gp_small),
             vp = viewport(width = unit(INDEX_WIDTH, "in"),
                           height = unit(INDEX_HEIGHT, "in")))
}

bot_corner_grob <- function(...) {
    l <- list(...)
    tsuit_grob <- do.call(top_suit_grob, l)
    trank_grob <- do.call(top_rank_grob, l)

    bsuit_grob <- do.call(bot_suit_grob, l)
    brank_grob <- do.call(bot_rank_grob, l)
    gp_small <- gpar(cex = 0.6, lex = 0.6)
    y_top_rank <- unit(1, "npc") - unit(0.15, "in")
    y_top_suit <- y_top_rank - unit(0.35, "in")
    y_line <- y_top_suit - unit(0.2, "in")
    y_bot_rank <- y_line - unit(0.2, "in")
    y_bot_suit <- y_bot_rank - unit(0.20, "in")
    small_lines <- linesGrob(y = 0.5)
    grobTree(
             # rectGrob(gp = gpar(col = NA, fill = "cyan")), # index area
             grobTree(bsuit_grob, vp = viewport(y = y_top_suit)),
             grobTree(brank_grob, vp = viewport(y = y_top_rank)),
             grobTree(small_lines, vp = viewport(y = y_line, width = unit(0.10, "in"))),
             grobTree(tsuit_grob, vp = viewport(y = y_bot_suit), gp = gp_small),
             grobTree(trank_grob, vp = viewport(y = y_bot_rank), gp = gp_small),
             vp = viewport(width = unit(INDEX_WIDTH, "in"),
                           height = unit(INDEX_HEIGHT, "in")))
}
