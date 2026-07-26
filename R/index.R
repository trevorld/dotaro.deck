# Vertical layout of a corner index, expressed as distances below the top of
# the safe zone (mirroring `dotaro.font:::indexGrob()`'s margins for the
# primary rank/suit:
# rank (3/8in cap height) flush to the top, suit (1/4in cap height) after a
# 0.10in gap).
# The secondary (mirrored, `cex_small`-scaled) rank/suit pair below the
# divider line gets its own, smaller gaps:
# `line_gap` on both sides of the line (symmetric), `small_gap` between the
# pair itself.
corner_index_y <- function() {
	cex_small <- 0.6
	rank_h <- 3 / 8
	suit_h <- 0.25
	gap <- 0.10
	line_gap <- 0.075
	small_gap <- 0.06

	suit_bottom <- rank_h + gap + suit_h
	rank2_top <- suit_bottom + 2 * line_gap
	rank2_bottom <- rank2_top + cex_small * rank_h

	list(
		cex_small = cex_small,
		rank = unit(1, "npc") - unit(0.5 * rank_h, "in"),
		suit = unit(1, "npc") - unit(rank_h + gap + 0.5 * suit_h, "in"),
		line = unit(1, "npc") - unit(suit_bottom + line_gap, "in"),
		rank2 = unit(1, "npc") - unit(rank2_top + 0.5 * cex_small * rank_h, "in"),
		suit2 = unit(1, "npc") - unit(rank2_bottom + small_gap + 0.5 * cex_small * suit_h, "in")
	)
}

top_corner_grob <- function(...) {
	dotaro_fonts_available()
	l <- list(...)
	tsuit_grob <- do.call(top_suit_grob, l)
	trank_grob <- do.call(top_rank_grob, l)

	bsuit_grob <- do.call(bot_suit_grob, l)
	brank_grob <- do.call(bot_rank_grob, l)
	y <- corner_index_y()
	gp_small <- gpar(cex = y$cex_small, lex = y$cex_small)
	small_lines <- linesGrob(y = 0.5)
	grobTree(
		# rectGrob(gp = gpar(col = NA, fill = "cyan")), # index area
		grobTree(tsuit_grob, vp = viewport(y = y$suit)),
		grobTree(trank_grob, vp = viewport(y = y$rank)),
		grobTree(small_lines, vp = viewport(y = y$line, width = unit(INDEX_WIDTH * 0.5, "in"))),
		grobTree(bsuit_grob, vp = viewport(y = y$suit2), gp = gp_small),
		grobTree(brank_grob, vp = viewport(y = y$rank2), gp = gp_small),
		vp = viewport(width = unit(INDEX_WIDTH, "in"), height = unit(INDEX_HEIGHT, "in"))
	)
}

bot_corner_grob <- function(...) {
	dotaro_fonts_available()
	l <- list(...)
	tsuit_grob <- do.call(top_suit_grob, l)
	trank_grob <- do.call(top_rank_grob, l)

	bsuit_grob <- do.call(bot_suit_grob, l)
	brank_grob <- do.call(bot_rank_grob, l)
	y <- corner_index_y()
	gp_small <- gpar(cex = y$cex_small, lex = y$cex_small)
	small_lines <- linesGrob(y = 0.5)
	grobTree(
		# rectGrob(gp = gpar(col = NA, fill = "cyan")), # index area
		grobTree(bsuit_grob, vp = viewport(y = y$suit)),
		grobTree(brank_grob, vp = viewport(y = y$rank)),
		grobTree(small_lines, vp = viewport(y = y$line, width = unit(0.10, "in"))),
		grobTree(tsuit_grob, vp = viewport(y = y$suit2), gp = gp_small),
		grobTree(trank_grob, vp = viewport(y = y$rank2), gp = gp_small),
		vp = viewport(width = unit(INDEX_WIDTH, "in"), height = unit(INDEX_HEIGHT, "in"))
	)
}
