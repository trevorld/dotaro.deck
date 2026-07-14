write_svg <- function(plot, file, title = "") {
	svglite::svglite(file)
	plot()
	invisible(grDevices::dev.off())
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
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"full-trad-dark-pip",
		draw_card(decks$dotaro_full_traditional, suit = 4, rank = 4),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"full-trad-light-king",
		draw_card(decks$dotaro_full_traditional, suit = 8, rank = 14),
		writer = write_svg
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
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"full-num-dark-pip",
		draw_card(decks$dotaro_full_number, suit = 4, rank = 5),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"full-num-light-high",
		draw_card(decks$dotaro_full_number, suit = 9, rank = 9),
		writer = write_svg
	)
})

test_that("dotaro_full_fool renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"full-fool-ordinary",
		draw_card(decks$dotaro_full_fool, suit = 1, rank = 1),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"full-fool-fool",
		draw_card(decks$dotaro_full_fool, suit = 2, rank = 2),
		writer = write_svg
	)
})

test_that("dotaro_corner_traditional renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"corner-trad-dark-pip",
		draw_card(decks$dotaro_corner_traditional, suit = 4, rank = 4),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"corner-trad-light-king",
		draw_card(decks$dotaro_corner_traditional, suit = 8, rank = 14),
		writer = write_svg
	)
})

test_that("dotaro_corner_number renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"corner-num-dark-pip",
		draw_card(decks$dotaro_corner_number, suit = 4, rank = 5),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"corner-num-light-high",
		draw_card(decks$dotaro_corner_number, suit = 9, rank = 9),
		writer = write_svg
	)
})

test_that("dotaro_corner_fool renders", {
	skip_if_not_installed("svglite")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	decks <- dotaro_decks()
	vdiffr::expect_doppelganger(
		"corner-fool-ordinary",
		draw_card(decks$dotaro_corner_fool, suit = 1, rank = 1),
		writer = write_svg
	)
	vdiffr::expect_doppelganger(
		"corner-fool-fool",
		draw_card(decks$dotaro_corner_fool, suit = 2, rank = 2),
		writer = write_svg
	)
})
