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
# secondary index's cex=0.6 scale.
# Draw the digit as a second, separate "counter" glyph on top in the
# background color instead, matching how the newer number-suit glyphs stay
# legible at any scale.
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

# French suits have no separate "light" glyph:
# draw the (dark) glyph in both cases and let `fill` alone distinguish light
# from dark, same as every other rank/suit glyph.
french_suits <- c("H", "S", "C", "D")

number_suits <- c("0", "1", "2", "3", "4")

suit_glyph_key <- function(suit, light) {
	if (suit %in% french_suits) paste0("D", suit) else paste0(light, suit)
}

suit_grob <- function(key, col, fill, counter_color) {
	glyph <- glyphs[[key]]
	grob <- dotaro.font:::suitGrob(glyph, col = col, fill = fill)
	counter <- counter_glyphs[[key]]
	if (!is.null(counter)) {
		grob <- grobTree(
			grob,
			dotaro.font:::suitGrob(counter, col = counter_color, fill = counter_color)
		)
	}
	grob
}

# Experimental "hybrid" suits (`dotaro.deck.suits = "hybrid"`), per
# `dotaro.font`'s own "Dotaro Deck hybrid traditional suits" design.
# The light half is a fused glyph (a base shape plus a gold accent-colored
# detail overlay) pairing each French suit with a corresponding suit from
# another system:
# hearts with Spanish cups, spades with Spanish swords, clubs with both
# Spanish clubs (bastos) and German acorns, and diamonds with both Spanish
# coins and German bells.
# The dark half is just the plain French glyph, but for hearts and spades
# specifically it doubles as the corresponding German suit too (hearts and
# leaves respectively, which share the same outline) --
# marked by the "hbinary" shaded fill rather than a second overlay layer.
# Every layer is outlined in black, never an accent color.
# Layer colors reuse the same options as every other glyph:
# "red"/"green" are the hearts/diamonds and spades/clubs ink colors, and
# "gold" (the hybrid suits' shared accent, not tied to either suit family)
# is the light color.
hybrid_suits_dark <- list(
	H = list(glyph = "\u2665", color = "red", shading = "hbinary"), # heart
	S = list(glyph = "\u2660", color = "green", shading = "hbinary"), # spade
	C = list(glyph = "\u2663", color = "green", shading = NA), # club
	D = list(glyph = "\u2666", color = "red", shading = NA) # diamond
)

hybrid_suits_light <- list(
	H = list(
		list(glyph = "\uF5C6", color = "gold", shading = NA), # chalice
		list(glyph = "\uF5C8", color = "red", shading = NA) # chalice heart
	),
	S = list(
		list(glyph = "\uF5C0", color = "green", shading = NA), # small spade
		list(glyph = "\uF5C1", color = "gold", shading = NA) # sword hilt
	),
	C = list(
		list(glyph = "\uF5C4", color = "green", shading = "hbinary"), # club top
		list(glyph = "\uF5C5", color = "gold", shading = "hbinary") # acorn tip
	),
	D = list(
		list(glyph = "\uF5B8", color = "gold", shading = "hbinary"), # coin
		list(glyph = "\u2B29", color = "red", shading = "hbinary") # small diamond
	)
)

hybrid_layer_color <- function(role) {
	switch(role, red = hearts_diamonds_color(), green = spades_clubs_color(), gold = light_color())
}

hybrid_suit_grob <- function(suit, light) {
	layers <- if (light == "D") list(hybrid_suits_dark[[suit]]) else hybrid_suits_light[[suit]]
	# Trying the light half's layers bordered in a darkened shade of the
	# suit's own accent color instead of black --
	# the same darkened half already used by the "hbinary"-shaded layers'
	# (clubs, diamonds) own two-tone fill, now applied uniformly to all 4
	# suits' light borders.
	# The dark half's border stays black.
	accent <- if (suit %in% c("H", "D")) hearts_diamonds_color() else spades_clubs_color()
	col <- if (light == "D") {
		"black"
	} else {
		dotaro.font:::darken(accent, dotaro.font:::BINARY_SHADING_AMOUNT)
	}
	grobs <- lapply(layers, function(layer) {
		dotaro.font:::suitGrob(
			layer$glyph,
			col = col,
			fill = hybrid_layer_color(layer$color),
			shading = layer$shading
		)
	})
	do.call(grobTree, grobs)
}

# The dark half's fill for number suits (and the fool and number-suit rank
# glyphs, whose color also tracks `number_suits_color()`) gets lightened
# when that color isn't black, so their still-black border/counter/icon
# keeps good contrast against it instead of nearly vanishing into a
# same-color fill.
lighten_number_fill <- function(accent) {
	if (normalize_color(number_suits_color()) == "#000000") {
		accent
	} else {
		dotaro.font:::lighten(accent, 0.4)
	}
}

top_suit_grob <- function(tsuit, tlight, red, ..., pip = FALSE) {
	if (suit_style() == "hybrid" && tsuit %in% french_suits) {
		return(hybrid_suit_grob(tsuit, tlight))
	}
	# Under the hybrid style, reaching here always means a number suit
	# (French suits are always diverted above); under the default "french"
	# style, French suits reach here too and keep the red/black split.
	hybrid <- suit_style() == "hybrid"
	accent <- if (tsuit %in% number_suits) {
		number_suits_color()
	} else {
		ifelse(red == "R", hearts_diamonds_color(), spades_clubs_color())
	}
	fill <- ifelse(tlight == "D", accent, light_color())
	# The dark half's border is black since its fill is already the accent
	# color and a same-color border would be invisible; the light half's own
	# border takes the accent color instead.
	col <- if (tlight == "D") "black" else accent
	# Number suits always use the "negated" (solid disc + counter) circled-digit
	# glyph, even on the light half:
	# the "positive" glyph's digit is stroked but its interior is a true hole,
	# so almost none of `fill` ever actually shows.
	if (hybrid) {
		counter_color <- col
	} else {
		# The counter needs to contrast against `fill`, so it takes the *other*
		# color of the pair:
		# `light_color()` on the dark half (solid accent disc, light digit) and
		# `accent` on the light half (accent-ringed light disc, accent digit) --
		# matching how French suits' light half is purely accent + light with no
		# black, instead of a literal "white"/"black".
		counter_color <- if (tlight == "D") light_color() else accent
	}
	if (tsuit %in% number_suits && normalize_color(number_suits_color()) != "#000000") {
		if (tlight == "D") {
			# Lighten the disc fill itself instead of the digit counter, so
			# the counter can stay plain black (matching the border) while
			# still contrasting well against the fill.
			fill <- lighten_number_fill(fill)
			counter_color <- "black"
		} else {
			# The light half's counter tracks the number-suit color directly
			# (its disc is `light_color()`-filled, so the plain color already
			# contrasts fine).
			counter_color <- number_suits_color()
		}
	}
	if (pip && tsuit %in% number_suits) {
		# Repeated pip dots stay plain circles, matching how French suits'
		# pips are just their plain suit symbol with no extra embellishment --
		# the digit is only spelled out in the single corner-index/suit-badge
		# instance drawn below.
		return(dotaro.font:::suitGrob(glyphs[["O"]], col = col, fill = fill))
	}
	suit_grob(suit_glyph_key(tsuit, "D"), col = col, fill = fill, counter_color = counter_color)
}

bot_suit_grob <- function(bsuit, blight, red, ..., pip = FALSE) {
	if (is.na(bsuit)) {
		return(nullGrob())
	}
	if (suit_style() == "hybrid" && bsuit %in% french_suits) {
		return(hybrid_suit_grob(bsuit, blight))
	}
	hybrid <- suit_style() == "hybrid"
	accent <- if (bsuit %in% number_suits) {
		number_suits_color()
	} else {
		ifelse(red == "R", hearts_diamonds_color(), spades_clubs_color())
	}
	fill <- ifelse(blight == "D", accent, light_color())
	# See `top_suit_grob()`.
	col <- if (blight == "D") "black" else accent
	if (hybrid) {
		counter_color <- col
	} else {
		counter_color <- if (blight == "D") light_color() else accent
	}
	if (bsuit %in% number_suits && normalize_color(number_suits_color()) != "#000000") {
		# See `top_suit_grob()`.
		if (blight == "D") {
			fill <- lighten_number_fill(fill)
			counter_color <- "black"
		} else {
			counter_color <- number_suits_color()
		}
	}
	if (pip && bsuit %in% number_suits) {
		# See `top_suit_grob()`.
		return(dotaro.font:::suitGrob(glyphs[["O"]], col = col, fill = fill))
	}
	suit_grob(suit_glyph_key(bsuit, "D"), col = col, fill = fill, counter_color = counter_color)
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
	accent <- if (tsuit %in% number_suits) {
		number_suits_color()
	} else {
		ifelse(red == "R", hearts_diamonds_color(), spades_clubs_color())
	}
	hybrid <- suit_style() == "hybrid"
	if (hybrid && tlight == "L") {
		# Trying the light half's rank glyph (number-suit digit or
		# traditional-suit digit/letter alike) in the suit's accent color
		# (border) against the theme's light color (fill), instead of the
		# "always black border, always accent fill" rule the dark half uses.
		return(dotaro.font:::rankGrob(glyph, col = accent, fill = light_color()))
	}
	# The dark half's border is black (see `top_suit_grob()`); the light
	# half's border takes the accent color instead.
	col <- if (tlight == "D") "black" else accent
	# Hybrid ranks stay in the accent color on both halves rather than
	# switching to the light color --
	# unlike the suit glyphs, there's no separate light/dark rank glyph to
	# fill instead.
	fill <- if (hybrid) accent else ifelse(tlight == "D", accent, light_color())
	if (tsuit %in% number_suits && tlight == "D") {
		# See `top_suit_grob()`.
		fill <- lighten_number_fill(fill)
	}
	dotaro.font:::rankGrob(glyph, col = col, fill = fill)
}

bot_rank_grob <- function(brank, blight, red, bsuit, ...) {
	glyph <- rank_glyph(brank, bsuit)
	# Fools borrow whichever suit they're paired with (see `bot_face_grob()`),
	# so like the number suits they get the dedicated color instead.
	accent <- if (bsuit %in% number_suits || brank %in% c("O", "F")) {
		number_suits_color()
	} else {
		ifelse(red == "R", hearts_diamonds_color(), spades_clubs_color())
	}
	hybrid <- suit_style() == "hybrid"
	if (hybrid && blight == "L") {
		# See `top_rank_grob()`.
		return(dotaro.font:::rankGrob(glyph, col = accent, fill = light_color()))
	}
	# See `top_rank_grob()`. This also covers the fool ranks (O/F): their
	# suit-badge-like border/fill split (see `fool_grob()`) already matches
	# this same dark/light rule, so they no longer need a separate branch.
	col <- if (blight == "D") "black" else accent
	fill <- if (hybrid) accent else ifelse(blight == "D", accent, light_color())
	if ((bsuit %in% number_suits || brank %in% c("O", "F")) && blight == "D") {
		# See `top_suit_grob()`.
		fill <- lighten_number_fill(fill)
	}
	dotaro.font:::rankGrob(glyph, col = col, fill = fill)
}

top_border_grob <- function(...) {
	l <- list(...)
	bar_width <- unit(INDEX_WIDTH - 0.05, "in")
	y_top <- unit(1, "npc") - unit(BLEED, "in") - 0.5 * bar_width
	width <- unit(1, "npc") - unit(2 * BLEED + 2 * INDEX_WIDTH + 0.2, "in")
	if (l$red == "R") {
		col <- hearts_diamonds_color()
	} else {
		col <- spades_clubs_color()
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
		col <- hearts_diamonds_color()
	} else {
		col <- spades_clubs_color()
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

# Card back:
# a "4.8*.4**.8*" polygon tiling (big squares + small diamonds rotated 45
# degrees between them) reworked as a mini scoring track.
# Every card in the deck shares this one design, so `card_back_grob()`
# (unlike `card_grob()`) takes no suit/rank arguments.
CARD_BACK_NROW <- 5L
CARD_BACK_NCOL <- 3L
CARD_BACK_SQUARE_WIDTH <- unit(1, "cm")
CARD_BACK_SQUARE_GAP <- unit(0.8, "cm")
CARD_BACK_LWD <- 2

# The fool "star" rank glyph, rendered via the "Dotaro Suits" font/grob (not
# "Dotaro Ranks", which is what actually renders it elsewhere in the
# package) so it sits stylistically with the 4 French suits alongside it.
# Left column of the back, bottom-to-top.
card_back_suit_keys <- c("star", "S", "C", "D", "H")

normalize_color <- function(color) {
	rgb(t(col2rgb(color)), maxColorValue = 255L)
}

# The "light" rendering of a suit symbol, tracking whatever color theme is
# currently active (including the experimental "hybrid" suits, which swap
# in fused Spanish- or German-suit-style glyphs for the 4 French suits, per
# `hybrid_suits_light`).
# The fool "star" has no hybrid variant of its own, so it always falls back
# to the plain light suit-glyph treatment.
card_back_suit_grob <- function(key) {
	if (key == "star") {
		# Matches the fool's own dedicated color everywhere else (see
		# `bot_rank_grob()`/`bot_face_grob()`), rather than the red/black
		# family it happens to be paired with on the top half.
		return(dotaro.font:::suitGrob(
			glyphs[["F"]],
			col = number_suits_color(),
			fill = light_color()
		))
	}
	if (suit_style() == "hybrid") {
		return(hybrid_suit_grob(key, "L"))
	}
	accent <- if (key %in% c("H", "D")) hearts_diamonds_color() else spades_clubs_color()
	dotaro.font:::suitGrob(glyphs[[paste0("D", key)]], col = accent, fill = light_color())
}

card_back_grob <- function() {
	nrow <- CARD_BACK_NROW
	ncol <- CARD_BACK_NCOL
	width <- CARD_BACK_SQUARE_WIDTH
	gap <- CARD_BACK_SQUARE_GAP

	# `side` marks a cribbage-style score path (left/right columns), `middle`
	# a possible snaking score path (center column), and the checkerboard
	# colors are just possible board-game cells --
	# shape (square vs. star vs. diamond) and the black borders between every
	# piece already separate these categories, so color only has to
	# distinguish *within* each one:
	# side vs. middle, and checker1 vs. checker2.
	side_fill <- light_color()
	middle_fill <- light_color()
	if (normalize_color(spades_clubs_color()) == "#000000") {
		if (normalize_color(hearts_diamonds_color()) == "#000000") {
			checker_fill1 <- checker_fill2 <- "white"
		} else {
			checker_fill1 <- checker_fill2 <- hearts_diamonds_color()
		}
	} else {
		checker_fill1 <- hearts_diamonds_color()
		checker_fill2 <- number_suits_color()
	}
	if (suit_style() == "hybrid") {
		middle_fill <- hearts_diamonds_color()
		checker_fill1 <- "white"
		checker_fill2 <- spades_clubs_color()
	}
	marker_fill <- middle_fill
	diamond_fill <- "black"
	bg_fill <- "black" # doesn't need to be distinguished from `diamond_fill`

	gap_in <- convertWidth(gap, "in", valueOnly = TRUE)
	width_in <- convertWidth(width, "in", valueOnly = TRUE)
	pitch_in <- width_in + gap_in
	pitch <- width + gap
	total_width <- ncol * width + (ncol - 1) * gap
	total_height <- nrow * width + (nrow - 1) * gap
	X <- function(j) (j - 1) * pitch_in + 0.5 * width_in
	Y <- function(i) (i - 1) * pitch_in + 0.5 * width_in

	bg_grob <- rectGrob(gp = gpar(fill = bg_fill, col = NA))

	# Checkerboard-colored bounding box squares at the old tiling's
	# eight-pointed-star positions:
	# the (nrow + 1) x (ncol + 1) lattice sitting half a pitch diagonally off
	# of every big square, i.e. at the shared corner of (up to) 4 neighboring
	# big squares.
	# The 4 diamonds directly above/below/left/right of a star position are
	# each a distance `pitch / 2` away (axis-aligned) -
	# that's exactly how far a star's own points used to reach, so a box
	# side of `pitch` (reaching from the star's center out to each of those
	# 4 diamond centers) is what bounds it.
	# This is drawn under the diamonds and big squares, which cover most of
	# it, leaving just its corner tips showing through.
	#
	# Only draw a box where all 4 of those neighboring diamonds actually
	# exist (`di` in [2, nrow], `dj` in [2, ncol]):
	# a star position on the perimeter is missing 1 or more of its 4 points
	# to begin with, so its box would just be a mostly-cropped rectangle
	# sticking out past the card edge.
	# Skip it and let the plain background color show instead.
	star_grid <- expand.grid(di = seq(2, nrow), dj = seq(2, ncol))
	star_grobs <- Map(
		function(di, dj) {
			cx <- (dj - 1.5) * pitch_in + 0.5 * width_in
			cy <- (di - 1.5) * pitch_in + 0.5 * width_in
			fill <- if ((di + dj) %% 2 == 0) checker_fill1 else checker_fill2
			rectGrob(
				x = unit(cx, "in"),
				y = unit(cy, "in"),
				width = unit(pitch_in, "in"),
				height = unit(pitch_in, "in"),
				gp = gpar(fill = fill, col = "black", lwd = CARD_BACK_LWD)
			)
		},
		star_grid$di,
		star_grid$dj
	)

	# Diamond ("small rotated square") positions sit directly *between* two
	# edge-adjacent big squares -
	# i.e. in the same gap that a big square's neighbor would occupy, not
	# diagonally off of it.
	# That means two distinct families:
	# `ncol - 1` of them between each row's squares (same y as that row, x
	# centered in each horizontal gap), and `ncol` of them between each pair
	# of adjacent rows (same x as that column, y centered in each vertical
	# gap).
	#
	# The diamond's corner nearest a neighboring big square, and that
	# square's nearest edge, are both a distance `gap / 2` from the
	# diamond's center (one along the diagonal, one along the axis) -
	# so a diamond side of `gap * sqrt(2) / 2` is exactly the size at which
	# the diamond's corner touches that edge's midpoint, with no gap and no
	# overlap.
	# The diamond's other two corners (pointing where there is no adjacent
	# square) poke the same distance into empty space, where the big
	# squares (drawn on top, below) don't reach.
	diamond_side_in <- gap_in / sqrt(2)
	within_row <- expand.grid(i = seq_len(nrow), j = seq_len(ncol - 1))
	between_row <- expand.grid(i = seq_len(nrow - 1), j = seq_len(ncol))
	diamond_cx <- c(0.5 * (X(within_row$j) + X(within_row$j + 1)), X(between_row$j))
	diamond_cy <- c(Y(within_row$i), 0.5 * (Y(between_row$i) + Y(between_row$i + 1)))

	# Most (within-row) diamonds are a single plain color, except the topmost
	# (then leftmost) and bottommost (then rightmost) diamond, which get a
	# distinct marker color -
	# e.g. to mark the start/end of a scoring path that continues onto other
	# cards.
	# The between-row diamonds sit in a column directly above/below a big
	# square, so they instead take that column's own color:
	# `side_fill` under the leftmost/rightmost column (matching the big
	# squares there), and `marker_fill` under the interior column(s)
	# (turning the center column into a second, vertical marker path).
	diamond_fills <- c(
		rep(diamond_fill, nrow(within_row)),
		ifelse(between_row$j %in% c(1, ncol), side_fill, marker_fill)
	)
	is_top_marker <- order(-diamond_cy, diamond_cx)[1]
	is_bot_marker <- order(diamond_cy, -diamond_cx)[1]
	diamond_fills[c(is_top_marker, is_bot_marker)] <- marker_fill

	diamond_grobs <- Map(
		function(cx, cy, fill) {
			grobTree(
				rectGrob(gp = gpar(fill = fill, col = "black", lwd = CARD_BACK_LWD)),
				vp = viewport(
					x = unit(cx, "in"),
					y = unit(cy, "in"),
					width = unit(diamond_side_in, "in"),
					height = unit(diamond_side_in, "in"),
					angle = 45
				)
			)
		},
		diamond_cx,
		diamond_cy,
		diamond_fills
	)

	# Functional grid of big squares, on top of the diamond lattice (covering
	# all but the diamonds' corner tips).
	# The left column carries the suit symbols bottom-to-top;
	# the right column carries the same symbols top-to-bottom, each rotated
	# 180 degrees -
	# so the whole card looks identical if rotated 180 degrees (a common
	# card-back trait), and the two columns can be read as a snaking,
	# connectable path.
	left <- unit(0.5, "npc") - 0.5 * total_width
	bottom <- unit(0.5, "npc") - 0.5 * total_height
	square_grid <- expand.grid(i = seq_len(nrow), j = seq_len(ncol))
	square_grobs <- Map(
		function(i, j) {
			x <- left + (j - 1) * pitch + 0.5 * width
			y <- bottom + (i - 1) * pitch + 0.5 * width
			fill <- if (j %in% c(1, ncol)) side_fill else middle_fill
			square_grob <- rectGrob(
				x = x,
				y = y,
				width = width,
				height = width,
				gp = gpar(fill = fill, col = "black", lwd = CARD_BACK_LWD)
			)
			if (j != 1 && j != ncol) {
				return(square_grob)
			}
			key <- if (j == 1) card_back_suit_keys[i] else card_back_suit_keys[nrow - i + 1]
			angle <- if (j == 1) 0 else 180
			symbol_grob <- grobTree(
				card_back_suit_grob(key),
				vp = viewport(
					x = x,
					y = y,
					width = 0.7 * width,
					height = 0.7 * width,
					angle = angle
				)
			)
			grobTree(square_grob, symbol_grob)
		},
		square_grid$i,
		square_grid$j
	)

	block_border_grob <- rectGrob(
		width = total_width,
		height = total_height,
		gp = gpar(fill = NA, col = "black", lwd = CARD_BACK_LWD)
	)

	gl <- do.call(
		gList,
		c(list(bg_grob), star_grobs, diamond_grobs, square_grobs, list(block_border_grob))
	)
	grobTree(children = gl, vp = viewport(width = total_width, height = total_height, clip = TRUE))
}
