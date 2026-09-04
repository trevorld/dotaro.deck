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
# `cex` scales the fontsize (and, with it, the `strwidth` column widths and
# the suit glyphs) along with the absolute-mm cell padding, so the same table
# can be drawn at README size or shrunk onto a booklet page.
# `columns` picks which of the 4 label columns to keep; the "Suit" glyph
# column is always kept, so a subset still reads as a table on its own (e.g.
# splitting the 4 label columns across the two pages of a booklet spread,
# which keeps every column clear of the fold).
# `glyph_cex` sizes the suit glyphs relative to the table's own fontsize.
# They are font glyphs, so their cell's dimensions only position them -- this
# multiplier is what actually scales them.  The glyph column is sized off it
# (see `widths` below), so shrinking one shrinks the other.
hybrid_suits_table_grob <- function(
	name = NULL,
	cex = 1,
	columns = c("French", "German", "Spanish", "Hanafuda"),
	glyph_cex = 1.2
) {
	labels <- hybrid_suits_table_labels()
	all_headers <- c("French", "German", "Spanish", "Hanafuda")
	columns <- match.arg(columns, all_headers, several.ok = TRUE)
	labels <- labels[, match(columns, all_headers), drop = FALSE]
	headers <- c("Suit", columns)

	n <- 8L
	fontsize <- cex * 18
	# Size each label column to fit its widest cell (header or body) via a
	# "strwidth" unit, resolved lazily against whatever device this actually
	# draws on, instead of a fixed "null" ratio: the same nominal fontsize
	# renders at a different width on different devices (e.g. cairo_pdf,
	# used by the pdf manual, vs. the png device README.Rmd knits with), so a
	# ratio tuned against one device can clip a label on the other.
	col_width <- function(header, column) {
		strs <- c(header, column)
		do.call(unit.pmax, lapply(strs, function(s) unit(1, "strwidth", s))) + unit(cex * 6, "mm")
	}
	label_widths <- lapply(seq_along(columns), function(j) col_width(headers[j + 1L], labels[[j]]))
	# With every column shown the glyph column takes the slack (`unit(1,
	# "null")`) and the table fills its viewport.  Dropping columns frees up
	# width that the glyph column would otherwise absorb, stranding the
	# glyphs far from their labels -- so size it off the fontsize instead and
	# split the slack between equal spacer columns either side, which centers
	# the narrower table rather than pinning it to one edge.
	# `col0` is the glyph column's index, which those spacers shift.
	if (length(columns) == length(all_headers)) {
		widths <- do.call(unit.c, c(list(unit(1, "null")), label_widths))
		col0 <- 1L
	} else {
		widths <- do.call(
			unit.c,
			c(
				list(unit(1, "null"), unit(3.5 * glyph_cex * fontsize, "points")),
				label_widths,
				list(unit(1, "null"))
			)
		)
		col0 <- 2L
	}
	ncol <- length(headers)
	lay <- grid.layout(nrow = n + 1L, ncol = length(widths), widths = widths)
	x_text <- unit(0, "npc") + unit(cex * 2, "mm")

	gl <- gList()
	for (j in seq_along(headers)) {
		vp <- viewport(layout.pos.row = 1L, layout.pos.col = col0 + j - 1L)
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
		vp_glyph <- viewport(layout.pos.row = i + 1L, layout.pos.col = col0)
		gl[[length(gl) + 1L]] <- gTree(
			children = gList(top_suit_grob(tsuit, tlight, red)),
			gp = gpar(cex = glyph_cex, lex = glyph_cex),
			vp = vp_glyph
		)
		for (j in seq_len(ncol)[-1L]) {
			vp <- viewport(layout.pos.row = i + 1L, layout.pos.col = col0 + j - 1L)
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
