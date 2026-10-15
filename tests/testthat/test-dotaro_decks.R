write_svg <- function(plot, file, title = "") {
	svglite::svglite(file)
	plot()
	invisible(grDevices::dev.off())
}

# The suit/rank glyphs are drawn as font outlines, which differ slightly between FontForge
# versions (e.g. Ubuntu 24.04's writes some straight segments as degenerate quadratics), so the
# fonts that CI builds with `dotaro.font` don't reproduce the committed snapshots.  On CI we
# still render each card (exercising the code for coverage) but skip the snapshot comparison.
expect_card_doppelganger <- function(title, fig) {
	if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
		f <- tempfile(fileext = ".svg")
		on.exit(unlink(f), add = TRUE)
		write_svg(fig, f)
		expect_true(file.exists(f))
	} else {
		vdiffr::expect_doppelganger(title, fig, writer = write_svg)
	}
}

draw_card <- function(cfg, suit, rank) {
	function() {
		grid::grid.newpage()
		piecepackr::grid.piece("card_face", suit = suit, rank = rank, cfg = cfg)
	}
}

test_that("dotaro_full_traditional renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"full-trad-dark-pip",
		draw_card(decks$dotaro_full_traditional, suit = 4, rank = 4)
	)
	expect_card_doppelganger(
		"full-trad-light-king",
		draw_card(decks$dotaro_full_traditional, suit = 8, rank = 14)
	)

	dev_cur <- grDevices::dev.cur()
	if (dev_cur > 1) {
		on.exit(grDevices::dev.set(dev_cur), add = TRUE)
	}
	pdf(NULL)
	on.exit(grDevices::dev.off(), add = TRUE)
	expect_s3_class(
		grid::grobCoords(
			piecepackr::pieceGrob("card_face", cfg = decks$dotaro_full_traditional),
			closed = TRUE
		),
		"GridGrobCoords"
	)
})

test_that("dotaro_full_number renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"full-num-dark-pip",
		draw_card(decks$dotaro_full_number, suit = 4, rank = 5)
	)
	expect_card_doppelganger(
		"full-num-light-high",
		draw_card(decks$dotaro_full_number, suit = 9, rank = 9)
	)
})

test_that("dotaro_full_fool renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"full-fool-ordinary",
		draw_card(decks$dotaro_full_fool, suit = 1, rank = 1)
	)
	expect_card_doppelganger(
		"full-fool-fool",
		draw_card(decks$dotaro_full_fool, suit = 2, rank = 2)
	)
})

test_that("dotaro_corner_traditional renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"corner-trad-dark-pip",
		draw_card(decks$dotaro_corner_traditional, suit = 4, rank = 4)
	)
	expect_card_doppelganger(
		"corner-trad-light-king",
		draw_card(decks$dotaro_corner_traditional, suit = 8, rank = 14)
	)
})

test_that("dotaro_corner_number renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"corner-num-dark-pip",
		draw_card(decks$dotaro_corner_number, suit = 4, rank = 5)
	)
	expect_card_doppelganger(
		"corner-num-light-high",
		draw_card(decks$dotaro_corner_number, suit = 9, rank = 9)
	)
})

test_that("dotaro_corner_fool renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	local_dotaro_deck_default_options()
	decks <- dotaro_decks()
	expect_card_doppelganger(
		"corner-fool-ordinary",
		draw_card(decks$dotaro_corner_fool, suit = 1, rank = 1)
	)
	expect_card_doppelganger(
		"corner-fool-fool",
		draw_card(decks$dotaro_corner_fool, suit = 2, rank = 2)
	)
})
