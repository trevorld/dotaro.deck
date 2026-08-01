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
			"Hearts",
			"Spades",
			"Clubs",
			"Diamonds",
			"Hearts (extra)",
			"Spades (extra)",
			"Clubs (extra)",
			"Diamonds (extra)"
		),
		spanish = c("", "", "", "", "Cups", "Swords", "Clubs", "Coins"),
		german = c("Hearts", "Leaves", "", "", "", "", "Acorns", "Bells"),
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
	headers <- c("Suit", "French", "Spanish", "German", "Hanafuda")

	n <- 8L
	lay <- grid.layout(nrow = n + 1L, ncol = 5L, widths = unit(c(1, 2.2, 1.1, 1.1, 3.2), "null"))
	x_text <- unit(0, "npc") + unit(2, "mm")

	gl <- gList()
	for (j in seq_along(headers)) {
		vp <- viewport(layout.pos.row = 1L, layout.pos.col = j)
		if (j == 1L) {
			gl[[j]] <- textGrob(headers[j], gp = gpar(fontface = "bold", fontsize = 18), vp = vp)
		} else {
			gl[[j]] <- textGrob(
				headers[j],
				x = x_text,
				just = "left",
				gp = gpar(fontface = "bold", fontsize = 18),
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
				gp = gpar(fontsize = 18)
			)
		}
	}
	# The layout is established here, on the outer `gTree`'s own viewport; the
	# cell viewports above are nested *inside* it (as each child's own `vp`),
	# so their `layout.pos.row`/`layout.pos.col` resolve against this layout
	# instead of needing (invalidly) to declare it themselves.
	gTree(children = gl, vp = viewport(width = 0.95, height = 0.95, layout = lay), name = name)
}
