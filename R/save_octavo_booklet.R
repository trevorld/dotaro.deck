# Prototype "octavo" booklet (see `pnpmisc::layout_octavo()`) that documents
# the Dotaro Deck *system* itself -- unlike `save_dotaro_rules()`'s
# (unpublished) game rules -- folded down to bridge card size (2.25" x 3.5")
# to match the deck. Kept internal/unexported -- see GitHub issue #15.
#
# Spread layout (booklet page numbers 1-16, read in order; see
# `pnpmisc::layout_octavo()`'s own docs for how these map onto the printed
# sheet's sheetwise imposition):
# * 1: front cover -- title plus the four-card compact cribbage board
# * 2-3: overview of the deck and how its halves pair up
# * 4-5: the annotated card diagram, one card per page (see
#   `octavo_anatomy_grob()` for why nothing sits near the fold)
# * 6-7: `variant = "hybrid"` gets the hybrid suit reference; the two French
#   variants instead get the double-nine and Chinese domino subdecks,
#   reflowed into a page-height-friendly grid (see `octavo_domino_grob()`)
# * 8-9, 10-11, 12-13, 14-15: the four "orientation" reference charts also
#   shown in `README.Rmd` (dark/light/traditional-up/traditional-down
#   halves), each split left/right across its spread via
#   `dotaro_orientation_*()`'s own `side` argument -- built for exactly this
#   purpose (see `R/orientation.R`).
# * 16: back cover -- highlights of the shared card back
save_octavo_booklet <- function(
	filename = "dotaro_octavo_booklet.pdf",
	...,
	variant = c("french_bw", "french_color", "hybrid"),
	size = c("letter", "a4"),
	bolt_padding = 0
) {
	check_dots_empty()
	variant <- match.arg(variant)
	size <- tolower(size)
	size <- match.arg(size, c("letter", "a4"))
	local_options(!!!dotaro_deck_options(variant))

	xmp <- xmpdf::xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Dotaro Deck: Octavo Booklet"
	)

	# `round = FALSE`: at this booklet's small `scale`, a rounded corner's
	# fixed (unscaled) radius exceeds half the shrunk card box, which makes
	# the border rectangle degenerate and silently fail to draw.
	envir <- dotaro_decks(round = FALSE)
	pages <- octavo_booklet_pages(envir, variant = variant)

	layout1 <- pnpmisc::layout_octavo(
		width = CARD_WIDTH,
		height = CARD_HEIGHT,
		page = 1L,
		paper = size,
		bolt_padding = bolt_padding
	)
	layout2 <- pnpmisc::layout_octavo(
		width = CARD_WIDTH,
		height = CARD_HEIGHT,
		page = 2L,
		paper = size,
		bolt_padding = bolt_padding
	)

	# Cropmarks at every page's own edges would mark the fold between the two
	# pages of a spread as if it needed trimming, which it doesn't (or, for
	# the centerfold, actively shouldn't). A `layout_octavo()` row's 4 page
	# columns pair up into 2 spread-wide columns of exactly `2 * CARD_WIDTH`,
	# so re-tiling the same sheet with `layout_grid()` at that width gives one
	# box per spread instead of one per page, its edges landing on exactly the
	# same lines as each spread's outer page edges (verified: box x/width
	# reduce to the union of its two pages' x-ranges). `bleed` is halved
	# because it's meant per-edge, but `bolt_padding` already doubled it (one
	# `bolt_padding`'s worth on each side of every trimmed fold).
	cropmark_layout <- pnpmisc::layout_grid(
		nrow = 2L,
		ncol = 2L,
		width = 2 * CARD_WIDTH,
		height = CARD_HEIGHT,
		bleed = bolt_padding / 2,
		paper = size
	)

	# `layout_octavo()` always imposes landscape, so the physical sheet's
	# width/height are the paper's *portrait* height/width, swapped.
	paper_width <- switch(size, letter = 8.5, a4 = pnpmisc:::A4_WIDTH)
	paper_height <- switch(size, letter = 11, a4 = pnpmisc:::A4_HEIGHT)

	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	cairo_pdf(filename, onefile = TRUE, width = paper_height, height = paper_width)
	grid.newpage()
	pnpmisc::grid_add_layout(pages, layout = layout1)
	pnpmisc::grid_add_cropmarks(layout = cropmark_layout)
	grid.newpage()
	pnpmisc::grid_add_layout(pages, layout = layout2)
	dev.off()

	tmpfile1 <- pnpmisc::pdf_set_xmp(filename, xmp = xmp)
	on.exit(unlink(tmpfile1), add = TRUE)
	tmpfile2 <- pnpmisc::pdf_set_docinfo(tmpfile1, docinfo = xmpdf::as_docinfo(xmp))
	on.exit(unlink(tmpfile2), add = TRUE)
	pnpmisc::pdf_compress(tmpfile2, filename, linearize = TRUE)

	invisible(filename)
}

# The four `README.Rmd` "orientation" reference charts, each paired with the
# two booklet page numbers (left half, right half) its spread occupies.
#
# Pages 8-9 are the signature's centerfold: the one spread whose facing pages
# are a single continuous piece of paper, rather than two separate leaves
# joined at the spine.  So the widest chart goes there and gets laid out
# straight across the fold (`continuous`), while the other three are centered
# on each page independently.
#
# `decks` are the `README.Rmd` example decks played in that orientation, listed
# at the top of the spread's right-hand page (opposite `label`).  `hybrid` adds
# the examples that only the hybrid-suited deck can build, so the two French
# variants list fewer.
#
# Assignments follow each example section's own "Orient ..." bullet, except the
# number-suit-up spread, which takes two of the `README.Rmd` overview's modern
# designs instead.  The domino and dice subdecks deliberately aren't listed
# there: they read their number suits, but `README.Rmd` notes that for domino
# games every corner already carries the whole card, so no one orientation is
# theirs.
octavo_orientation_charts <- function(variant) {
	charts <- list(
		list(
			fn = dotaro_orientation_trad_up,
			label = "Traditional suit halves up top",
			pages = c(8L, 9L),
			continuous = TRUE,
			decks = "Two French-suited decks",
			# Both use all 8 traditional suits, dark and light alike.
			hybrid = c("German-suited deck", "Hanafuda deck")
		),
		list(
			fn = dotaro_orientation_num_up,
			label = "Traditional suit halves down below",
			pages = c(10L, 11L),
			continuous = FALSE,
			decks = c(
				str_glue("Pyramid decks (1 one, {ellipsis}, 10 tens)"),
				"Sequential decks (up to 100 cards)"
			),
			hybrid = character()
		),
		list(
			fn = dotaro_orientation_dark_up,
			label = "Dark halves up top",
			pages = c(12L, 13L),
			continuous = FALSE,
			decks = c("French-suited (tarot) deck"),
			hybrid = character()
		),
		list(
			fn = dotaro_orientation_light_up,
			label = "Light halves up top",
			pages = c(14L, 15L),
			continuous = FALSE,
			# Nothing in the French variants reaches for the light halves on
			# their own -- both of these need the hybrid suits.
			decks = character(),
			hybrid = c("Spanish-suited deck", "Latin-suited tarot deck")
		)
	)
	lapply(charts, function(chart) {
		if (variant == "hybrid") {
			chart$decks <- c(chart$decks, chart$hybrid)
		}
		chart
	})
}

# Builds all 16 named ("page_1".."page_16") page content grobs consumed by
# `pnpmisc::grid_add_layout()`.
octavo_booklet_pages <- function(envir, variant, scale = 0.3) {
	pages <- vector("list", 16L)
	names(pages) <- paste0("page_", 1:16)

	pages[["page_1"]] <- octavo_cover_grob(envir)
	pages[["page_16"]] <- octavo_card_back_grob()

	pages[["page_2"]] <- octavo_notes_grob(OCTAVO_OVERVIEW_1, cex = 0.85)
	pages[["page_3"]] <- octavo_notes_grob(OCTAVO_OVERVIEW_2, cex = 0.85)

	pages[["page_4"]] <- octavo_anatomy_grob(envir, side = "left")
	pages[["page_5"]] <- octavo_anatomy_grob(envir, side = "right")

	if (variant == "hybrid") {
		# The three playing-card suit systems the hybrid deck bridges belong
		# together, and they fit one page; Hanafuda is a different sort of
		# thing (card categories, not suits) so it takes the facing page --
		# as the actual chart of which cards to pull for a Hanafuda deck,
		# rather than just its suit-name correspondence.
		pages[["page_6"]] <- octavo_hybrid_grob(
			c("French", "German", "Spanish"),
			title = "Hybrid French-German-Spanish traditional suits"
		)
		pages[["page_7"]] <- octavo_hanafuda_grob(envir)
	} else {
		# The hybrid suit reference these carry has no counterpart in the two
		# French variants, so they get a different pair of subdecks instead:
		# the two domino sets, each reflowed into a shorter, wider grid than
		# `README.Rmd`'s own (a left-justified triangle, and 2 x 16) so they
		# clear a single page's height -- see `octavo_domino_grob()`.
		pages[["page_6"]] <- octavo_domino_grob(
			"double9",
			nrow = 5L,
			ncol = 11L,
			label = "One set of double-nine dominoes",
			envir = envir
		)
		pages[["page_7"]] <- octavo_domino_grob(
			"chinese",
			nrow = 4L,
			ncol = 8L,
			label = "One set of Chinese dominoes",
			envir = envir
		)
	}

	for (chart in octavo_orientation_charts(variant)) {
		if (chart$continuous) {
			df <- chart$fn(
				scale = octavo_spread_scale(chart$fn, envir),
				side = "both",
				envir = envir
			)
			grobs <- octavo_spread_grobs(df, chart$label, chart$decks, envir)
		} else {
			grobs <- lapply(c("left", "right"), function(side) {
				df <- chart$fn(scale = scale, side = side, envir = envir)
				octavo_chart_grob(
					df,
					if (side == "left") chart$label,
					if (side == "right") chart$decks else character(),
					envir
				)
			})
		}
		pages[[paste0("page_", chart$pages[1L])]] <- grobs[[1L]]
		pages[[paste0("page_", chart$pages[2L])]] <- grobs[[2L]]
	}

	pages
}

octavo_open_page_grob <- function() {
	textGrob(
		"(open)",
		x = unit(CARD_WIDTH / 2, "in"),
		y = unit(CARD_HEIGHT / 2, "in"),
		default.units = "in",
		gp = gpar(col = "grey70", fontsize = 10, fontface = "italic")
	)
}

# ---------------------------------------------------------------------------
# Markdown page bodies
# ---------------------------------------------------------------------------

# Typographic punctuation for the page bodies below.  Two conventions meet
# here:
# * spelled as `\u` escapes with the literal in a trailing comment, so the R
#   source stays ASCII -- the same way `glyphs` and friends are written in
#   `R/grob.R`
# * referenced from the markdown as `{en_dash}` etc. and substituted by the
#   `marquee::marquee_glue()` call in `octavo_notes_grob()`, which resolves
#   them by lexical scope -- the same way `sbgjackets` does it
# A plain `textGrob()` label doesn't pass through glue, so those paste the
# constant in directly instead.
en_dash <- "\u2013" # – (ranges: 8mm–10mm)
em_dash <- "\u2014" # — (parenthetical breaks)
ellipsis <- "\u2026" # …
times <- "\u00d7" # × (products and grid dimensions)
middot <- "\u00b7" # ·
rotate_sign <- "\u2b6e" # ⭮

# Number of leading tabs shared by every indented line, so `trim_multistring()`
# can strip exactly the indentation that came from the R source's own nesting.
n_indents <- function(x) {
	lines <- strsplit(x, "\n", fixed = TRUE)[[1L]]
	n_tabs <- nchar(gsub("[^\t]", "", lines))
	n_tabs <- n_tabs[n_tabs > 0L]
	if (length(n_tabs) == 0L) {
		return(0L)
	}
	min(n_tabs)
}

# Lets the markdown page bodies below live in R raw strings indented to match
# the surrounding source (so `air format` leaves them alone) without that
# indentation reaching marquee, which would read it as code blocks.
trim_multistring <- function(x) {
	x <- trimws(x)
	lines <- strsplit(x, "\n", fixed = TRUE)[[1L]]
	lines <- str_replace(lines, str_glue("^\t{{{n_indents(x)}}}"), "")
	paste(lines, collapse = "\n")
}

# A card-sized `marquee` style, mirroring `sbgjackets::sbgjackets_style(size =
# "wallet")` -- the same recipe `credits_style()` uses for the print-and-play
# sheet, retuned from that one's 10pt/1.6 (a full letter page) to the 8pt/1.3
# that suits a 2.25" x 3.5" page.  Copied rather than depended on: sbgjackets
# is not an import here, and this is ~15 lines of it.
octavo_style <- function(cex = 1, align = NULL) {
	style <- marquee::classic_style(
		base_size = cex * 8,
		body_font = "Carlito",
		header_font = "Carlito",
		lineheight = 1.3,
		margin = marquee::trbl(0, bottom = marquee::rem(0.7))
	) |>
		marquee::modify_style(
			"h1",
			border = NA,
			size = marquee::relative(1.4),
			border_size = marquee::trbl(NULL),
			margin = marquee::trbl(NULL),
			padding = marquee::trbl(NULL)
		) |>
		marquee::modify_style("ul", padding = marquee::trbl(right = marquee::em(1)))
	if (!is.null(align)) {
		style <- marquee::modify_style(style, "base", align = align)
	}
	style
}

# `margin` is inset from all four page edges; on pages 2-7 the inner edge is
# the fold, so this doubles as the gutter allowance.
octavo_notes_grob <- function(text, ..., cex = 1, margin = 0.16) {
	check_dots_empty()
	notes <- marquee::marquee_glue(trim_multistring(text), .trim = FALSE)
	marquee::marquee_grob(
		notes,
		style = octavo_style(cex = cex),
		width = unit(CARD_WIDTH - 2 * margin, "in"),
		x = unit(margin, "in"),
		y = unit(CARD_HEIGHT - margin, "in"),
		hjust = "left",
		vjust = "top"
	)
}

OCTAVO_OVERVIEW_1 <- r"(
	# Overview

	The *Dotaro Deck* is an 108 card deck that can play games from a variety of card game systems.

	Traditional decks:

	* 1{en_dash}2 decks of French-suited playing cards
	* one deck of French-suited tarot cards
	* one set of double-nine dominoes
	* one set of Chinese dominoes

	Modern designs:

	* 10 suits with up to 10 ranks
	* 8 suits with up to 13 ranks
	* 5 suits with up to 20 ranks
	* 4 suits with up to 27 ranks
	* pyramid decks e.g. 1 one, 2 twos, {ellipsis}, 10 tens
	* sequential decks of up to 100 cards
	* one set of d6 dice cards
	* a 2 {times} 5 {times} 10 dimensional deck
	* a 2 {times} 2 {times} 2 {times} 13 dimensional deck
)"

OCTAVO_OVERVIEW_2 <- r"(
	# Halves

	Every card is vertically asymmetric {em_dash} a light half and a dark
	half {em_dash} for 2 {times} 108 = 216 halves:

	* 112 traditional suit halves

	  = 2 {times} 4 French suits {times} 14 ranks

	* 100 number suit halves

	  = 2 {times} 5 number suits {times} 10 ranks

	* 4 fool halves = 2 {times} 2 (circle, star)

	The halves are paired up three ways:

	* each number suit half joins a traditional suit half, allocated so there are double-9 domino, Chinese domino, and d6 dice card subdecks
	* each knight half joins the other knight half of the same French suit
	* each fool half joins a Queen of Spades or a Jack of Diamonds half
)"

OCTAVO_CARD_BACK <- r"(
	# The card back

	Every card shares one back: a tiling of squares, diamonds and
	eight-pointed stars, carrying the four light suit pips plus the
	fool's star down each side.

	* The pips are grouped in fives, like the holes of a cribbage board.

	  Stack six cards for a 60-hole board.

	  One card alone is a 10-hole board; each card can represent a base-10 digit.

	* Four backs set at right angles, corners touching where the star
	  squares meet, make a compact 60-square board also snaking through the center five squares.
	* Mark the trump suit with a counter on that suit's pip{em_dash}star means No Trump.
	* Each back has a 2{times}4 grid of stars:

	  4{times}2 grid of cards = 8{times}8 board of stars
	* The squares are 10mm, so 8mm{en_dash}10mm cubes or a meeple suit best
	  (a 16mm die is the upper limit).
)"

# ---------------------------------------------------------------------------
# Covers
# ---------------------------------------------------------------------------

# The four card backs of `README.Rmd`'s `pinwheel` chunk, centered on
# (`x`, `y`).  Card centers are in units of the *tiling's* own square-plus-gap
# pitch (not the card box), which is what makes each card's squares run
# seamlessly into its neighbor's; see that chunk for the arrangement.
octavo_pinwheel_df <- function(envir, scale, x, y) {
	cfg <- envir$dotaro_full_traditional
	back_width <- scale * cfg$get_width("card_back")
	back_height <- scale * cfg$get_height("card_back")
	pitch <- scale *
		convertWidth(CARD_BACK_SQUARE_WIDTH + CARD_BACK_SQUARE_GAP, "in", valueOnly = TRUE)

	gx <- pitch * c(0, 4, 5, 1)
	gy <- pitch * c(0, 1, -3, -4)
	angle <- c(0, 90, 0, 90)
	half_w <- ifelse(angle == 0, back_width / 2, back_height / 2)
	half_h <- ifelse(angle == 0, back_height / 2, back_width / 2)

	data.frame(
		piece_side = "card_back",
		cfg = "dotaro_full_traditional",
		suit = 1L,
		rank = 1L,
		angle = angle,
		scale = scale,
		x = gx - mean(range(c(gx - half_w, gx + half_w))) + x,
		y = gy - mean(range(c(gy - half_h, gy + half_h))) + y
	)
}

octavo_cover_grob <- function(envir, scale = 0.35) {
	pinwheel_y <- 1.45
	df <- octavo_pinwheel_df(envir, scale = scale, x = CARD_WIDTH / 2, y = pinwheel_y)
	gList(
		textGrob(
			"Dotaro Deck",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(CARD_HEIGHT - 0.38, "in"),
			default.units = "in",
			gp = gpar(fontsize = 15, fontface = "bold")
		),
		textGrob(
			"By Trevor L. Davis",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(CARD_HEIGHT - 0.60, "in"),
			default.units = "in",
			gp = gpar(fontsize = 7, fontface = "italic")
		),
		textGrob(
			"Released under a CC-BY 4.0 license",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(CARD_HEIGHT - 0.80, "in"),
			default.units = "in",
			gp = gpar(fontsize = 7, fontface = "italic")
		),
		pmap_piece(df, default.units = "in", envir = envir, draw = FALSE),
		textGrob(
			"A compact snaking 60-square score board from four card backs",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(0.18, "in"),
			default.units = "in",
			gp = gpar(fontsize = 5, fontface = "italic")
		)
	)
}

octavo_card_back_grob <- function() {
	gList(
		octavo_notes_grob(OCTAVO_CARD_BACK, cex = 0.95)
	)
}

# ---------------------------------------------------------------------------
# Pages 4-5: the annotated card diagram
# ---------------------------------------------------------------------------

# Label heights above/below the card's own center, in units of card height.
# Ported from `README.Rmd`'s `anatomy` chunk, whose positions are npc of a
# 4"-tall figure holding a 3.5" card (so e.g. its y = 0.86 is
# (0.86 * 4 - 2) / 3.5 of a card height above center).
OCTAVO_ANATOMY_Y <- c(0.4114, 0.3200, 0.2171, 0.1486, -0.2286, -0.3429)

# `README.Rmd` draws both orientations of one card side by side and puts a
# third column of labels ("Top half" / "Bottom half") down the middle.  Across
# a booklet spread that middle column would land squarely on the fold, so here
# each page carries one orientation on its own, the labels go to the *outer*
# edge, and the two cards stay far enough inboard to leave a clear band either
# side of the fold.
octavo_anatomy_grob <- function(envir, side = c("left", "right"), scale = 0.45) {
	side <- match.arg(side)
	dark_up <- side == "right"

	labels <- c("Top rank", "Top suit", "Bottom rank", "Bottom suit")
	labels <- c(
		labels,
		if (dark_up) {
			c("Dark half", "Traditional suit half")
		} else {
			c("Light half", "Number suit half")
		}
	)

	card_y <- 1.58
	# Mirror the whole page for the recto so both cards sit inboard of the
	# fold and both label blocks sit against the outer trim.
	card_x <- if (dark_up) CARD_WIDTH - 1.45 else 1.45
	label_x <- if (dark_up) CARD_WIDTH - 0.88 else 0.88
	just <- if (dark_up) "left" else "right"

	df <- data.frame(
		piece_side = "card_face",
		cfg = "dotaro_full_traditional",
		suit = 3L,
		rank = 9L,
		angle = if (dark_up) 180 else 0,
		scale = scale,
		x = card_x,
		y = card_y
	)

	gList(
		textGrob(
			if (dark_up) paste("The same card, rotated", rotate_sign) else "Anatomy of a card",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(CARD_HEIGHT - 0.22, "in"),
			default.units = "in",
			gp = gpar(fontsize = 8, fontface = "bold")
		),
		pmap_piece(df, default.units = "in", envir = envir, draw = FALSE),
		textGrob(
			labels,
			x = unit(label_x, "in"),
			y = unit(card_y + OCTAVO_ANATOMY_Y * scale * CARD_HEIGHT, "in"),
			just = just,
			default.units = "in",
			gp = gpar(fontsize = 6)
		),
		# One note for the whole spread, on the left page only.
		if (dark_up) {
			nullGrob()
		} else {
			textGrob(
				"Indices repeat on all four corners, so cards fan either way",
				x = unit(CARD_WIDTH / 2, "in"),
				y = unit(0.20, "in"),
				default.units = "in",
				gp = gpar(fontsize = 5, fontface = "italic")
			)
		}
	)
}

# ---------------------------------------------------------------------------
# Pages 6-7: the hybrid suit reference
# ---------------------------------------------------------------------------

# Half of `hybrid_suits_table_grob()`'s label columns, so the 8 suit rows read
# straight across a single page instead of over the fold.
# `cex` is set by the widest page of the spread (French/German/Spanish, which
# measures 1.93" against the table viewport's 2.14"), and both pages share it
# so their type matches even though Hanafuda alone would take more.
octavo_hybrid_grob <- function(
	columns,
	title = NULL,
	cex = 0.35,
	glyph_cex = 0.85,
	bottom = 0.26
) {
	gList(
		# Both pages reserve the same `bottom` band, titled or not, so the 8
		# suit rows stay level with each other across the spread.
		grobTree(
			hybrid_suits_table_grob(cex = cex, columns = columns, glyph_cex = glyph_cex),
			vp = viewport(
				y = unit(0.5 * (CARD_HEIGHT + bottom), "in"),
				height = unit(CARD_HEIGHT - bottom, "in")
			)
		),
		if (is.null(title)) {
			nullGrob()
		} else {
			textGrob(
				title,
				x = unit(CARD_WIDTH / 2, "in"),
				y = unit(bottom / 2, "in"),
				default.units = "in",
				gp = gpar(fontsize = 5, fontface = "italic")
			)
		}
	)
}

# The same chart as `README.Rmd`'s "Deck of Hanafuda cards" section (see
# `dotaro_hanafuda_chart()`), shrunk onto a single booklet page with
# `HANAFUDA_ROW_LABELS` down the right instead of `README.Rmd`'s wide margin.
# `scale` is picked off the height alone: 8 rows is this chart's binding
# dimension (12 columns, even with the label column added, still clears a
# page's width at that scale) -- unlike the orientation charts, which are
# always width-bound.
octavo_hanafuda_grob <- function(envir, top_margin = 0.3, label_fontsize = 5.5) {
	cfg <- envir$dotaro_corner_traditional
	scale <- (CARD_HEIGHT - top_margin - 0.1) / (8 * cfg$get_height("card_face"))

	df <- dotaro_hanafuda_chart(scale = scale, envir = envir)
	content_height <- CARD_HEIGHT - top_margin
	df$x <- df$x + 0.1 - min(df$x)
	df$y <- df$y + content_height / 2 - mean(range(df$y))

	gList(
		octavo_label_grob("Deck of Hanafuda cards", top_margin),
		pmap_piece(df, default.units = "in", envir = envir, draw = FALSE),
		textGrob(
			HANAFUDA_ROW_LABELS,
			x = unit(max(df$x) + scale * cfg$get_width("card_face"), "in"),
			y = unit(dotaro_hanafuda_row_y(df), "in"),
			just = "left",
			default.units = "in",
			gp = gpar(fontsize = label_fontsize)
		)
	)
}

# ---------------------------------------------------------------------------
# Pages 6-7 (French variants only): the domino subdecks
# ---------------------------------------------------------------------------

# Reflows a `card_info` domino subset (its `column`, e.g. `"double9"` or
# `"chinese"` -- see `R/dotaro_decks.R`) into an `nrow` x `ncol` grid instead
# of `README.Rmd`'s own shape for that subset (a left-justified triangle for
# `"double9"`, 2 x 16 for `"chinese"`), filled row-major so reading order
# (top to bottom, left to right) still matches `README.Rmd`'s own
# `arrange(lrank, urank)` card order. Only meant for shapes that use every
# cell exactly once (`nrow * ncol` equal to the subset's size); a shape that
# didn't would leave a ragged, unfilled last row.
dotaro_domino_chart <- function(column, nrow, ncol, scale = 1, envir = dotaro_decks()) {
	cfg_corner_trad <- envir$dotaro_corner_traditional
	IW <- cfg_corner_trad$get_width("card_face")
	IH <- cfg_corner_trad$get_height("card_face")
	X0 <- 0.1 + 0.5 * IW
	Y0 <- 0.1 - 0.5 * IH

	df <- filter(card_info, .data[[column]]) |>
		mutate(
			lrank = pmin(trank, brank),
			urank = pmax(trank, brank),
			top = trank == urank
		) |>
		arrange(lrank, urank) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		mutate(
			piece_side = "card_face",
			x = X0 + rep(seq(0, by = IW, length.out = ncol), nrow),
			y = Y0 + rep(nrow:1, each = ncol) * IH
		)
	dotaro_scale_xy(df, scale)
}

# Picks `scale` off whichever of width/height binds first for an `nrow` x
# `ncol` grid of corner pieces on one page -- the same min-of-two-constraints
# approach as `octavo_spread_scale()`, just for a single page instead of a
# two-page spread.
octavo_domino_scale <- function(nrow, ncol, envir, margin = 0.15, top_margin = 0.3) {
	cfg <- envir$dotaro_corner_traditional
	min(
		(CARD_WIDTH - 2 * margin) / (ncol * cfg$get_width("card_face")),
		(CARD_HEIGHT - top_margin - margin) / (nrow * cfg$get_height("card_face"))
	)
}

octavo_domino_grob <- function(column, nrow, ncol, label, envir, top_margin = 0.3, margin = 0.15) {
	scale <- octavo_domino_scale(nrow, ncol, envir, margin, top_margin)
	df <- dotaro_domino_chart(column, nrow, ncol, scale = scale, envir = envir)
	content_height <- CARD_HEIGHT - top_margin
	df$x <- df$x + (CARD_WIDTH - diff(range(df$x))) / 2 - min(df$x)
	df$y <- df$y + content_height / 2 - mean(range(df$y))

	gList(
		octavo_label_grob(label, top_margin),
		pmap_piece(df, default.units = "in", envir = envir, draw = FALSE)
	)
}

# ---------------------------------------------------------------------------
# Pages 8-15: the orientation charts
# ---------------------------------------------------------------------------

# `df` (as built by a `dotaro_orientation_*()` helper, already scaled and
# split to one page's worth) carries its own arbitrary-origin `x`/`y`
# positions; re-center it into the booklet's `CARD_WIDTH` x `CARD_HEIGHT`
# page, leaving `top_margin` inches of headroom for `label`.
# The per-page `scale` is tuned for a chart squeezed onto one page; the
# centerfold has both to play with, so size its chart to whichever of the two
# dimensions runs out first.  Every corner cfg shares one index size, so a
# single piece's width/height covers the whole data frame.
octavo_spread_scale <- function(fn, envir, margin = 0.18, top_margin = 0.3) {
	df <- fn(scale = 1, side = "both", envir = envir)
	cfg <- envir$dotaro_corner_traditional
	full_width <- diff(range(df$x)) + cfg$get_width("card_face")
	full_height <- diff(range(df$y)) + cfg$get_height("card_face")
	min(
		(2 * CARD_WIDTH - 2 * margin) / full_width,
		(CARD_HEIGHT - top_margin - margin) / full_height
	)
}

# The centerfold treatment: centre the *whole* chart across both pages, then
# hand each page the pieces that land on it, so the columns keep their true
# spacing straight through the fold.  Safe because every
# `dotaro_orientation_*()` layout already opens a gap at its own left/right
# cutoff, so no piece straddles the fold and needs drawing twice.
octavo_spread_grobs <- function(df, label, decks, envir, top_margin = 0.3) {
	dx <- (2 * CARD_WIDTH - diff(range(df$x))) / 2 - min(df$x)
	content_height <- CARD_HEIGHT - top_margin
	dy <- content_height / 2 - mean(range(df$y))
	df$x <- df$x + dx
	df$y <- df$y + dy

	left <- df[df$x < CARD_WIDTH, ]
	right <- df[df$x >= CARD_WIDTH, ]
	right$x <- right$x - CARD_WIDTH

	list(
		gList(
			octavo_label_grob(label, top_margin),
			pmap_piece(left, default.units = "in", envir = envir, draw = FALSE)
		),
		gList(
			octavo_decks_grob(decks, top_margin),
			pmap_piece(right, default.units = "in", envir = envir, draw = FALSE)
		)
	)
}

# One title per spread, on its left page, with `NULL` on the right.  Sized so
# the longest of the four chart labels ("Traditional suit halves down below",
# 1.76" here) still clears a page's 1.93" of usable width -- the title has to
# fit on one page, since on every spread but the centerfold the two pages are
# separate leaves and centering it over the pair would run it into the spine.
OCTAVO_LABEL_FONTSIZE <- 8

octavo_label_grob <- function(label, top_margin) {
	if (is.null(label)) {
		return(nullGrob())
	}
	textGrob(
		label,
		x = unit(CARD_WIDTH / 2, "in"),
		y = unit(CARD_HEIGHT - top_margin / 2, "in"),
		default.units = "in",
		gp = gpar(fontsize = OCTAVO_LABEL_FONTSIZE, fontface = "bold")
	)
}

# The example decks played in this orientation, heading the spread's right-hand
# page so it balances `label` on the left.  The heading sits on `label`'s own
# baseline and the list runs down from there into the slack the charts leave
# above themselves; empty `decks` draws nothing at all.
octavo_decks_grob <- function(decks, top_margin, fontsize = 6) {
	if (length(decks) == 0L) {
		return(nullGrob())
	}
	heading_y <- CARD_HEIGHT - top_margin / 2
	line_height <- convertHeight(unit(1.3 * fontsize, "points"), "in", valueOnly = TRUE)
	gList(
		textGrob(
			"Example decks",
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(heading_y, "in"),
			default.units = "in",
			gp = gpar(fontsize = fontsize, fontface = "italic")
		),
		textGrob(
			decks,
			x = unit(CARD_WIDTH / 2, "in"),
			y = unit(heading_y - seq_along(decks) * line_height, "in"),
			default.units = "in",
			gp = gpar(fontsize = fontsize)
		)
	)
}

# `label` is `NULL` on a spread's right-hand page, which carries `decks`
# instead.  `top_margin` is reserved on both, so the halves of a chart stay
# level with each other.
octavo_chart_grob <- function(df, label, decks, envir, top_margin = 0.3) {
	dx <- (CARD_WIDTH - diff(range(df$x))) / 2 - min(df$x)
	content_height <- CARD_HEIGHT - top_margin
	dy <- content_height / 2 - mean(range(df$y))
	df$x <- df$x + dx
	df$y <- df$y + dy

	gList(
		octavo_label_grob(label, top_margin),
		octavo_decks_grob(decks, top_margin),
		pmap_piece(df, default.units = "in", envir = envir, draw = FALSE)
	)
}
