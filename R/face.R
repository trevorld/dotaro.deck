top_face_grob <- function(...) {
	l <- list(...)
	tsuit_grob <- do.call(top_suit_grob, l)
	trank_grob <- do.call(top_rank_grob, l)
	trank <- l$trank
	red <- l$red
	if (red == "R") {
		col <- hearts_diamonds_color()
	} else {
		col <- spades_clubs_color()
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
	# The fool isn't really part of either red/black French-suit family (it
	# just borrows whichever suit it happens to be paired with on the top
	# half), so it gets the same dedicated color as the number suits instead.
	if (brank %in% c("O", "F")) {
		return(fool_grob(brank, blight, number_suits_color()))
	}
	if (red == "R") {
		col <- hearts_diamonds_color()
	} else {
		col <- spades_clubs_color()
	}
	if (brank == "N") {
		return(knight_grob(bsuit_grob, brank_grob, col))
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
	# The knight/horse-head rank glyph is drawn last (on top of the meeple, via
	# ordinary z-order in `gl` below), so its solid fill obscures the meeple's
	# lower body --
	# evoking the meeple riding the horse.
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
	# The border follows the same split as the suit badges (see
	# `top_suit_grob()`): black on the dark half (whose icon fill is already
	# the accent color), accent on the light half (whose icon fill is the
	# light color instead). The meeple's own outline stays flat accent,
	# same as every other face rank's meeple.
	border <- if (light == "D") "black" else col
	# See `top_suit_grob()`: the dark half's fill gets lightened when `col`
	# (== `number_suits_color()`) isn't black, so the still-black border
	# keeps good contrast against it.
	fill_col <- if (light == "D") lighten_number_fill(col) else col
	if (suit_style() == "hybrid") {
		icon_fill <- if (light == "D") fill_col else light_color()
		# The fool has its own dedicated icon (star/plain ring), so -- like the
		# number suits -- it doesn't get the "hbinary" shading effect.
		return(hybrid_fool_grob(rank, border, col, icon_fill, shading = "none"))
	}
	gp_meeple <- gpar(col = col, lwd = 2)
	vp_meeple <- viewport(height = unit(0.7, "in"), width = unit(0.6, "in"), y = 0.3)
	meeple_grob <- pp_shape("meeple")$shape(gp = gp_meeple, vp = vp_meeple)
	# Match the fool corner index's own col/fill (see `bot_rank_grob()`):
	# accent fill on the dark half, light-color fill on the light half.
	index_fill <- if (light == "D") fill_col else light_color()
	gp_triangle <- gpar(fill = index_fill, col = border, lwd = 1.5)
	if (rank == "O") {
		# Same index-matching fill as the triangle.
		gp_circle <- gpar(fill = index_fill, col = border, lwd = 1.5)
	} else {
		# White (not left hollow), same trick as `hybrid_fool_grob()`'s ring,
		# so the star's concave notches don't let the triangle's tip poke
		# through visually.
		gp_circle <- gpar(fill = "white", col = border, lwd = 1.5)
	}
	if (light == "L") {
		gp_star <- gpar(fill = NA, col = border, lwd = 1.5)
	} else {
		gp_star <- gpar(fill = fill_col, col = border, lwd = 1.5)
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

hybrid_fool_grob <- function(rank, border, accent, icon_fill, shading) {
	# Unlike the triangle/ring/icon border (black on the dark half), the
	# meeple's outline always stays the accent color.
	gp_meeple <- gpar(col = accent, lwd = 2)
	vp_meeple <- viewport(height = unit(0.7, "in"), width = unit(0.6, "in"), y = 0.3)
	meeple_grob <- pp_shape("meeple")$shape(gp = gp_meeple, vp = vp_meeple)

	# Match the fool corner index's own col/fill (see `bot_rank_grob()`):
	# `border`/`icon_fill` are already that same black-or-accent border and
	# accent-or-light-color fill, computed by the caller.
	gp_triangle <- gpar(fill = icon_fill, col = border, lwd = 1.5)
	y_triangle <- unit(0.3, "npc") + unit(0.5 * 0.7 + 0.5 * 0.4 - 0.10, "in")
	vp_triangle <- viewport(height = unit(0.4, "in"), width = unit(0.30, "in"), y = y_triangle)
	triangle_grob <- pp_shape("pyramid")$shape(gp = gp_triangle, vp = vp_triangle)

	y_circle <- y_triangle + unit(0.5 * 0.4 + 0.5 * 0.2 - 0.02, "in")
	vp_circle <- viewport(height = unit(0.2, "in"), width = unit(0.2, "in"), y = y_circle)
	gp_icon <- gpar(fill = dotaro.font:::shaded_fill(icon_fill, shading), col = border, lwd = 1.5)
	if (rank == "F") {
		# `pp_shape("concave5")`'s regular 5-point star polygon fits the ring
		# circle exactly (a font star glyph doesn't line up as cleanly).
		# The ring is filled white (rather than left hollow) so the star's
		# concave notches don't let the triangle's tip show through.
		ring_grob <- circleGrob(vp = vp_circle, gp = gpar(fill = "white", col = border, lwd = 1.5))
		icon_grob <- pp_shape("concave5")$shape(gp = gp_icon, vp = vp_circle)
	} else {
		ring_grob <- NULL
		icon_grob <- circleGrob(vp = vp_circle, gp = gp_icon)
	}

	grobTree(
		meeple_grob,
		triangle_grob,
		ring_grob,
		icon_grob,
		vp = viewport(width = 0.8, height = 0.9),
		cl = "fool"
	)
}
