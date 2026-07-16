top_face_grob <- function(...) {
	l <- list(...)
	tsuit_grob <- do.call(top_suit_grob, l)
	trank_grob <- do.call(top_rank_grob, l)
	trank <- l$trank
	red <- l$red
	if (red == "R") {
		col <- red_color()
	} else {
		col <- black_color()
	}
	if (trank == "N") {
		return(knight_grob(tsuit_grob, trank_grob, col))
	}
	meeple_grob <- pp_shape("meeple")$shape(
		vp = viewport(y = 0.35, width = 0.55, height = 0.46),
		gp = gpar(col = col, fill = NA, lwd = 2)
	)
	tsuit_grob <- grobTree(tsuit_grob, vp = viewport(y = 0.37), gp = gpar(lex = 0.83, cex = 0.83))
	trank_grob <- grobTree(trank_grob, vp = viewport(y = 0.70), gp = gpar(lex = 1.11, cex = 1.11))
	gl <- gList(meeple_grob, tsuit_grob, trank_grob)

	vp <- viewport(width = unit(PIP_WIDTH, "in"), height = unit(0.5 * PIP_HEIGHT, "in"))
	gp <- gpar(cex = 1.2, lex = 1.2)
	gTree(children = gl, vp = vp, gp = gp)
}

bot_face_grob <- function(...) {
	l <- list(...)
	bsuit_grob <- do.call(bot_suit_grob, l)
	brank_grob <- do.call(bot_rank_grob, l)
	brank <- l$brank
	blight <- l$blight
	red <- l$red
	if (red == "R") {
		col <- red_color()
	} else {
		col <- black_color()
	}
	if (brank == "N") {
		return(knight_grob(bsuit_grob, brank_grob, col))
	} else if (brank %in% c("O", "F")) {
		return(fool_grob(brank, blight, col))
	}
	meeple_grob <- pp_shape("meeple")$shape(
		vp = viewport(y = 0.35, width = 0.55, height = 0.46),
		gp = gpar(col = col, fill = NA, lwd = 2)
	)
	bsuit_grob <- grobTree(bsuit_grob, vp = viewport(y = 0.37), gp = gpar(lex = 0.83, cex = 0.83))
	brank_grob <- grobTree(brank_grob, vp = viewport(y = 0.70), gp = gpar(lex = 1.11, cex = 1.11))
	gl <- gList(meeple_grob, bsuit_grob, brank_grob)

	vp <- viewport(width = unit(PIP_WIDTH, "in"), height = unit(0.5 * PIP_HEIGHT, "in"))
	gp <- gpar(cex = 1.2, lex = 1.2)
	gTree(children = gl, vp = vp, gp = gp)
}

knight_grob <- function(suit_grob, rank_grob, col = "black") {
	# The meeple is drawn from `dotaro.font`'s meeple glyph (same shape as
	# `pp_shape("meeple")`, just registered in font em-space) rather than a
	# piecepackr shape, so it composites with the knight/horse-head rank glyph
	# via ordinary z-order: knight drawn last, on top, so its solid fill
	# obscures the meeple's lower body -- evoking the meeple riding the horse.
	# meeple_grob <- grobTree(
	# 	dotaro.font:::suitGrob("\U000FC431", col = col, fill = "transparent"),
	# 	vp = viewport(y = 0.70),
	# 	gp = gpar(cex = 2.0, lex = 1.2)
	# )
	meeple_grob <- pp_shape("meeple")$shape(
		vp = viewport(y = 0.74, width = 0.6 * 0.7, height = 0.7 * 0.5),
		gp = gpar(col = col, fill = NA, lwd = 2)
	)
	suit_grob <- grobTree(suit_grob, vp = viewport(y = 0.75), gp = gpar(lex = 0.65, cex = 0.65))
	knight_grob <- grobTree(rank_grob, vp = viewport(y = 0.44), gp = gpar(lex = 1.2, cex = 2.10))
	gl <- gList(meeple_grob, suit_grob, knight_grob)
	vp <- viewport(width = unit(PIP_WIDTH, "in"), height = unit(0.5 * PIP_HEIGHT, "in"))
	gp <- gpar(cex = 1.2, lex = 1.2)
	gTree(children = gl, vp = vp, gp = gp)
}

fool_grob <- function(rank, light, col = "black") {
	gp_meeple <- gpar(col = col, lwd = 2)
	vp_meeple <- viewport(height = unit(0.7, "in"), width = unit(0.6, "in"), y = 0.3)
	meeple_grob <- pp_shape("meeple")$shape(gp = gp_meeple, vp = vp_meeple)
	gp_triangle <- gpar(fill = col, col = NA_character_)
	if (light == "L") {
		if (rank == "O") {
			gp_circle <- gpar(fill = NA, col = col, lwd = 1.5)
		} else {
			gp_circle <- gpar(fill = NA, col = col, lwd = 1.5)
		}
		gp_star <- gpar(fill = NA, col = col, lwd = 1.5)
	} else {
		if (rank == "O") {
			gp_circle <- gpar(fill = col, col = col, lwd = 1.5)
		} else {
			gp_circle <- gpar(fill = NA, col = col, lwd = 1.5)
		}
		gp_star <- gpar(fill = col, col = col, lwd = 1.5)
	}
	y_triangle <- unit(0.3, "npc") + unit(0.5 * 0.7 + 0.5 * 0.4 - 0.10, "in")
	vp_triangle <- viewport(height = unit(0.4, "in"), width = unit(0.30, "in"), y = y_triangle)
	triangle_grob <- pp_shape("pyramid")$shape(gp = gp_triangle, vp = vp_triangle)
	y_circle <- y_triangle + unit(0.5 * 0.4 + 0.5 * 0.2 - 0.02, "in")
	vp_circle <- viewport(height = unit(0.2, "in"), width = unit(0.2, "in"), y = y_circle)
	circle_grob <- circleGrob(vp = vp_circle, gp = gp_circle)
	if (rank == "F") {
		star_grob <- pp_shape("concave5")$shape(gp = gp_star, vp = vp_circle)
	} else {
		star_grob <- NULL
	}
	grobTree(
		meeple_grob,
		triangle_grob,
		circle_grob,
		star_grob,
		vp = viewport(width = 0.8, height = 0.9),
		cl = "fool"
	)
}
