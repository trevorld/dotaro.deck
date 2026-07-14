number_ranks <- as.character(0:9)

glyphs <- list(
	"0" = "0",
	"1" = "1",
	"2" = "2",
	"3" = "3",
	"4" = "4",
	"5" = "5",
	"6" = "6",
	"7" = "7",
	"8" = "8",
	"9" = "9",
	"J" = "\u265F", # ♟
	"N" = "\u265E", # ♞
	"Q" = "\u265B", # ♛
	"K" = "\u265A", # ♚
	DH = "\u2665", # ♥
	DS = "\u2660", # ♠
	DC = "\u2663", # ♣
	DD = "\u2666", # ♦
	"L0" = "\u24EA", # ⓪
	"L1" = "\u2460", # ①
	"L2" = "\u2461", # ②
	"L3" = "\u2462", # ③
	"L4" = "\u2463", # ④
	"D0" = "\u24FF", # ⓿
	"D1" = "\u2776", # ❶
	"D2" = "\u2777", # ❷
	"D3" = "\u2778", # ❸
	"D4" = "\u2779", # ❹
	"O" = "\u25CF", # ●
	"F" = "\u2605" # ★
)

# `dotaro.font`'s negative circled digit glyphs (dark number suits) draw the
# interior digit as a thin hole, which doesn't survive being shrunk to the
# secondary index's cex=0.6 scale.  Draw the digit as a second, separate
# "counter" glyph on top in the background color instead, matching how the
# newer number-suit glyphs stay legible at any scale.
counter_glyphs <- list(
	D0 = "\uF5E0", # digit 0 counter
	D1 = "\uF5E1", # digit 1 counter
	D2 = "\uF5E2", # digit 2 counter
	D3 = "\uF5E3", # digit 3 counter
	D4 = "\uF5E4" # digit 4 counter
)

# Mathematical bold digits, used for ranks on number-suit cards (tsuit/bsuit
# one of `number_suits` below) to visually set them apart from the plain
# digit ranks on traditional (French-suited) pip cards.
bold_digit_glyphs <- list(
	"0" = "\U0001D7CE", # 𝟎
	"1" = "\U0001D7CF", # 𝟏
	"2" = "\U0001D7D0", # 𝟐
	"3" = "\U0001D7D1", # 𝟑
	"4" = "\U0001D7D2", # 𝟒
	"5" = "\U0001D7D3", # 𝟓
	"6" = "\U0001D7D4", # 𝟔
	"7" = "\U0001D7D5", # 𝟕
	"8" = "\U0001D7D6", # 𝟖
	"9" = "\U0001D7D7" # 𝟗
)

# French suits have no separate "light" glyph: draw the (dark) glyph in both
# cases and let `fill` alone distinguish light from dark, same as every other
# rank/suit glyph.
french_suits <- c("H", "S", "C", "D")

number_suits <- c("0", "1", "2", "3", "4")

suit_glyph_key <- function(suit, light) {
	if (suit %in% french_suits) paste0("D", suit) else paste0(light, suit)
}

suit_grob <- function(key, col, fill) {
	glyph <- glyphs[[key]]
	grob <- dotaro.font:::suitGrob(glyph, col = col, fill = fill)
	counter <- counter_glyphs[[key]]
	if (!is.null(counter)) {
		grob <- grobTree(
			grob,
			dotaro.font:::suitGrob(counter, col = light_color(), fill = light_color())
		)
	}
	grob
}

top_suit_grob <- function(tsuit, tlight, red, ...) {
	col <- ifelse(red == "R", red_color(), black_color())
	fill <- ifelse(tlight == "D", col, light_color())
	suit_grob(suit_glyph_key(tsuit, tlight), col = col, fill = fill)
}

bot_suit_grob <- function(bsuit, blight, red, ...) {
	if (is.na(bsuit)) {
		return(nullGrob())
	}
	col <- ifelse(red == "R", red_color(), black_color())
	fill <- ifelse(blight == "D", col, light_color())
	suit_grob(suit_glyph_key(bsuit, blight), col = col, fill = fill)
}

rank_glyph <- function(rank, suit) {
	if (suit %in% number_suits && rank %in% number_ranks) {
		bold_digit_glyphs[[rank]]
	} else {
		glyphs[[rank]]
	}
}

top_rank_grob <- function(trank, tlight, red, tsuit, ...) {
	glyph <- rank_glyph(trank, tsuit)
	col <- ifelse(red == "R", red_color(), black_color())
	fill <- ifelse(tlight == "D", col, light_color())
	dotaro.font:::rankGrob(glyph, col = col, fill = fill)
}

bot_rank_grob <- function(brank, blight, red, bsuit, ...) {
	glyph <- rank_glyph(brank, bsuit)
	col <- ifelse(red == "R", red_color(), black_color())
	fill <- ifelse(blight == "D", col, light_color())
	dotaro.font:::rankGrob(glyph, col = col, fill = fill)
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
	if (l$tlight == "D") {
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
	pat <- gridpattern::patternFill(
		"weave",
		units = "cm",
		spacing = 0.26,
		angle = 0,
		xoffset = 0.13,
		yoffset = 0.15,
		density = 0.25,
		type = "plain",
		fill = fill,
		fill2 = fill,
		colour = colour,
		linewidth = 0.4
	)
	tt_grob <- rectGrob(y = y_top, width = width, height = bar_width, gp = gp, name = "top_bar")
	tt_grob <- grobTree(
		tt_grob,
		rectGrob(y = y_top, width = width, height = bar_width, gp = gpar(col = col, fill = pat))
	)
	# label_grob <- textGrob("Test top label", y = y_top, gp = gp_label)
	label_grob <- nullGrob()

	height <- 0.5 * (unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_HEIGHT + 0.2, "in"))
	y_side <- unit(0.5, "npc") + 0.5 * height
	x_left <- unit(BLEED + 0.5 * INDEX_WIDTH, "in")
	x_right <- unit(1, "npc") - x_left
	tl_grob <- rectGrob(x = x_left, y = y_side, width = bar_width, height = height, gp = gp)
	tl_grob <- grobTree(
		tl_grob,
		rectGrob(
			x = x_left,
			y = y_side,
			width = bar_width,
			height = height,
			gp = gpar(col = col, fill = pat)
		)
	)
	tr_grob <- rectGrob(x = x_right, y = y_side, width = bar_width, height = height, gp = gp)
	tr_grob <- grobTree(
		tr_grob,
		rectGrob(
			x = x_right,
			y = y_side,
			width = bar_width,
			height = height,
			gp = gpar(col = col, fill = pat)
		)
	)
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
	if (l$blight == "D") {
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
	pat <- gridpattern::patternFill(
		"weave",
		units = "cm",
		spacing = 0.26,
		angle = 0,
		xoffset = 0.13,
		yoffset = 0.15,
		density = 0.2,
		type = "plain",
		fill = fill,
		fill2 = fill,
		colour = colour,
		linewidth = 0.4
	)
	bb_grob <- rectGrob(y = y_bot, width = width, height = bar_width, gp = gp, name = "top_bar")
	if (l$brank == "N") {
		bb_grob <- grobTree(
			bb_grob,
			rectGrob(y = y_bot, width = width, height = bar_width, gp = gpar(col = col, fill = pat))
		)
	}
	# label_grob <- textGrob("Test bottom label", y = y_bot, gp = gp_label, rot = 180)
	label_grob <- nullGrob()

	height <- 0.5 * (unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_HEIGHT + 0.2, "in"))
	y_side <- unit(0.5, "npc") - 0.5 * height
	x_left <- unit(BLEED + 0.5 * INDEX_WIDTH, "in")
	x_right <- unit(1, "npc") - x_left
	bl_grob <- rectGrob(x = x_left, y = y_side, width = bar_width, height = height, gp = gp)
	if (l$brank == "N") {
		bl_grob <- grobTree(
			bl_grob,
			rectGrob(
				x = x_left,
				y = y_side,
				width = bar_width,
				height = height,
				gp = gpar(col = col, fill = pat)
			)
		)
	}
	br_grob <- rectGrob(x = x_right, y = y_side, width = bar_width, height = height, gp = gp)
	if (l$brank == "N") {
		br_grob <- grobTree(
			br_grob,
			rectGrob(
				x = x_right,
				y = y_side,
				width = bar_width,
				height = height,
				gp = gpar(col = col, fill = pat)
			)
		)
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
		rectGrob(
			width = 2,
			height = 3.25,
			default.units = "in",
			gp = gpar(col = NA, fill = "white")
		),
		# rectGrob(width = PIP_WIDTH, height = PIP_HEIGHT, default.units = "in", gp = gpar(col = NA, fill = "magenta")), # inner pip drawing area
		linesGrob(
			x = c(0.2, 0.8),
			y = 0.5,
			default.units = "npc",
			gp = gpar(col = "black", lwd = 4)
		),
		# top_border, bot_border,
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
		vp = viewport(angle = 180),
		name = "bottom"
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
	vp <- viewport(
		width = unit(PIP_WIDTH, "in"),
		height = unit(0.5 * PIP_HEIGHT, "in"),
		y = unit(0.5, "npc") + unit(0.25 * PIP_HEIGHT, "in")
	)
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
	vp <- viewport(
		width = unit(PIP_WIDTH, "in"),
		height = unit(0.5 * PIP_HEIGHT, "in"),
		y = unit(0.5, "npc") - unit(0.25 * PIP_HEIGHT, "in"),
		angle = 180
	)
	grobTree(grob, vp = vp)
}
