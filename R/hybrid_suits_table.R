# The eight hybrid suits (see `hybrid_suits_dark`/`hybrid_suits_light` in
# `R/grob.R`), numbered 1-8 with the same dark/light French suit indices used
# throughout `README.Rmd`'s other example charts (1-4 dark Hearts/Spades/
# Clubs/Diamonds, 5-8 their light "extra" counterparts):
# * the four "golden" hybrids (5-8) double as the Spanish suits
# * the four shaded (hbinary) suits (1, 2, 7, 8) double as the German suits
# * Hanafuda card types: 3 chaff (plants), 3 banners, 1 animal, 1 bright
hybrid_suits_table_labels <- function() {
	data.frame(
		french = c(
			"Hearts (dark)",
			"Spades (dark)",
			"Clubs (dark)",
			"Diamonds (dark)",
			"Hearts (light)",
			"Spades (light)",
			"Clubs (light)",
			"Diamonds (light)"
		),
		german = c("Hearts", "Leaves", "", "", "", "", "Acorns", "Bells"),
		spanish = c("", "", "", "", "Cups", "Swords", "Clubs", "Coins"),
		hanafuda = c(
			"Ribbon (plain)",
			"Chaff",
			"Chaff",
			"Bright",
			"Ribbon (embellished)",
			"Ribbon (dark)",
			"Chaff (yellow)",
			"Animal"
		)
	)
}

# Reference table cross-referencing the eight experimental "hybrid" suits
# (`dotaro.deck.suits = "hybrid"`) with the French, Spanish, German, and
# Hanafuda suits each one doubles as.  The "Suit" column is drawn with the
# same `top_suit_grob()` used to render actual cards, keyed off of the suit
# index (1-8) rather than a separately maintained glyph/color/shading table,
# so it always matches what the cards themselves look like.
hybrid_suits_table_grob <- function(name = NULL) {
	labels <- hybrid_suits_table_labels()
	headers <- c("Suit", "French", "German", "Spanish", "Hanafuda")

	n <- 8L
	fontsize <- 18
	# Size each label column to fit its widest cell (header or body) via a
	# "strwidth" unit, resolved lazily against whatever device this actually
	# draws on, instead of a fixed "null" ratio: the same nominal fontsize
	# renders at a different width on different devices (e.g. cairo_pdf,
	# used by the pdf manual, vs. the png device README.Rmd knits with), so a
	# ratio tuned against one device can clip a label on the other.
	col_width <- function(header, column) {
		strs <- c(header, column)
		do.call(unit.pmax, lapply(strs, function(s) unit(1, "strwidth", s))) + unit(6, "mm")
	}
	widths <- unit.c(
		unit(1, "null"),
		col_width(headers[2], labels$french),
		col_width(headers[3], labels$german),
		col_width(headers[4], labels$spanish),
		col_width(headers[5], labels$hanafuda)
	)
	lay <- grid.layout(nrow = n + 1L, ncol = 5L, widths = widths)
	x_text <- unit(0, "npc") + unit(2, "mm")

	gl <- gList()
	for (j in seq_along(headers)) {
		vp <- viewport(layout.pos.row = 1L, layout.pos.col = j)
		if (j == 1L) {
			gl[[j]] <- textGrob(
				headers[j],
				gp = gpar(fontface = "bold", fontsize = fontsize),
				vp = vp
			)
		} else {
			gl[[j]] <- textGrob(
				headers[j],
				x = x_text,
				just = "left",
				gp = gpar(fontface = "bold", fontsize = fontsize),
				vp = vp
			)
		}
	}
	for (i in seq_len(n)) {
		# Same suit index -> (tsuit, tlight) mapping used by `dotaroTradFaceGrob()`
		# and every suit = 1:8 example chart in README.Rmd.
		tsuit <- switch((i %% 4L) + 1L, "D", "H", "S", "C")
		tlight <- if (i <= 4L) "D" else "L"
		red <- if (tsuit %in% c("H", "D")) "R" else "B"
		vp_glyph <- viewport(layout.pos.row = i + 1L, layout.pos.col = 1L)
		gl[[length(gl) + 1L]] <- gTree(
			children = gList(top_suit_grob(tsuit, tlight, red)),
			gp = gpar(cex = 1.2, lex = 1.2),
			vp = vp_glyph
		)
		for (j in 2L:5L) {
			vp <- viewport(layout.pos.row = i + 1L, layout.pos.col = j)
			gl[[length(gl) + 1L]] <- textGrob(
				labels[i, j - 1L],
				x = x_text,
				just = "left",
				vp = vp,
				gp = gpar(fontsize = fontsize)
			)
		}
	}
	# The layout is established here, on the outer `gTree`'s own viewport; the
	# cell viewports above are nested *inside* it (as each child's own `vp`),
	# so their `layout.pos.row`/`layout.pos.col` resolve against this layout
	# instead of needing (invalidly) to declare it themselves.  Its own `gp`
	# also carries the same `fontsize` so the `strwidth` units in `widths`
	# above resolve against the fontsize the cells actually draw at.
	gTree(
		children = gl,
		vp = viewport(width = 0.95, height = 0.95, layout = lay, gp = gpar(fontsize = fontsize)),
		name = name
	)
}
