

top_face_grob <- function(...) {
    l <- list(...)
    tsuit_grob <- do.call(top_suit_grob, l)
    trank_grob <- do.call(top_rank_grob, l)
    trank <- l$trank
    if (trank == "N") {
        return(knight_grob(tsuit_grob, trank_grob))
    }
    meeple_grob <- pp_shape("meeple")$shape(
        vp = viewport(y = 0.4, width = 0.7, height = 0.5),
        gp = gpar(col = "black", fill = NA, lwd = 4))
    tsuit_grob <- grobTree(tsuit_grob,
        vp = viewport(y = 0.42),
        gp = gpar(lex = 1.1, cex = 1.1))
    trank_grob <- grobTree(trank_grob,
        vp = viewport(y = 0.78),
        gp = gpar(lex = 1.2, cex = 1.2))
    gl <- gList(meeple_grob, tsuit_grob, trank_grob)

    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"))
    gp <- gpar(cex = 1.2, lex = 1.2)
    gTree(children = gl, vp = vp, gp = gp)
}

bot_face_grob <- function(...) {
    l <- list(...)
    bsuit_grob <- do.call(bot_suit_grob, l)
    brank_grob <- do.call(bot_rank_grob, l)
    brank <- l$brank
    blight <- l$blight
    if (brank == "N") {
        return(knight_grob(bsuit_grob, brank_grob))
    } else if (brank %in% c("O", "F")) {
        return(fool_grob(brank, blight))
    }
    meeple_grob <- pp_shape("meeple")$shape(
        vp = viewport(y = 0.4, width = 0.7, height = 0.5),
        gp = gpar(col = "black", fill = NA, lwd = 4))
    bsuit_grob <- grobTree(bsuit_grob,
        vp = viewport(y = 0.42),
        gp = gpar(lex = 1.1, cex = 1.1))
    brank_grob <- grobTree(brank_grob,
        vp = viewport(y = 0.78),
        gp = gpar(lex = 1.2, cex = 1.2))
    gl <- gList(meeple_grob, bsuit_grob, brank_grob)

    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"))
    gp <- gpar(cex = 1.2, lex = 1.2)
    gTree(children = gl, vp = vp, gp = gp)
}

knight_grob <- function(suit_grob, rank_grob) {
    circle_grob <- circleGrob(x = 0.34, r = 0.4, y = 0.29,
                              gp = gpar(fill = "white", col = NA),
                              vp = viewport(height = 0.9, mask = rectGrob(gp = gpar(fill =  "white", col = NA))))
    meeple_grob <- pp_shape("meeple")$shape(
        vp = viewport(x = 0.58, y = 0.7, width = 0.4, height = 0.3),
        gp = gpar(col = "black", fill = NA, lwd = 4))
    suit_grob <- grobTree(suit_grob,
        vp = viewport(x = 0.58, y = 0.7),
        gp = gpar(lex = 0.8, cex = 0.8))
    rank_grob <- grobTree(rank_grob,
        vp = viewport(y = 0.42),
        gp = gpar(lex = 3.0, cex = 3.0))
    gl <- gList(meeple_grob, circle_grob, suit_grob, rank_grob)

    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"))
    gp <- gpar(cex = 1.2, lex = 1.2)
    gTree(children = gl, vp = vp, gp = gp)
}

fool_grob <- function(rank, light) {

    gp_meeple <- gpar(col = "black", lwd = 4)
    vp_meeple <- viewport(height=unit(0.7, "in"),
                          width=unit(0.6, "in"), y = 0.3)
    meeple_grob <- pp_shape("meeple")$shape(gp=gp_meeple, vp=vp_meeple)
    gp_triangle <- gpar(fill = "black", col = NA_character_)
    if (light == "L") {
        if (rank == "O") {
            gp_circle <- gpar(fill = NA, col = "black", lwd = 1.5)
        } else {
            gp_circle <- gpar(fill = NA, col = "black", lwd = 1.5)
        }
        gp_star <- gpar(fill = NA, col = "black", lwd=1.5)
    } else {
        if (rank == "O") {
            gp_circle <- gpar(fill = "black", col = "black", lwd = 1.5)
        } else {
            gp_circle <- gpar(fill = NA, col = "black", lwd = 1.5)
        }
        gp_star <- gpar(fill = "black", col = "black", lwd=1.5)
    }
    y_triangle <- unit(0.3, "npc") + unit(0.5 * 0.7 + 0.5 * 0.4 -0.10, "in")
    vp_triangle <- viewport(height=unit(0.4, "in"),
                            width=unit(0.30, "in"), y = y_triangle)
    triangle_grob <- pp_shape("pyramid")$shape(gp=gp_triangle, vp=vp_triangle)
    y_circle <- y_triangle + unit(0.5 * 0.4 + 0.5 * 0.2 -0.02, "in")
    vp_circle <- viewport(height=unit(0.2, "in"), width=unit(0.2, "in"),
                          y = y_circle)
    circle_grob <- circleGrob(vp = vp_circle, gp = gp_circle)
    if (rank == "F") {
        star_grob <- pp_shape("concave5")$shape(gp = gp_star, vp = vp_circle)
    } else {
        star_grob <- NULL
    }
    grobTree(meeple_grob, triangle_grob, circle_grob, star_grob,
             vp = viewport(width=0.8, height=0.9), cl = "fool")

}
