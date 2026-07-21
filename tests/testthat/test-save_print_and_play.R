test_that("save_print_and_play() works", {
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("DejaVu Sans"))
	local_dotaro_deck_default_options()
	f <- tempfile(fileext = ".pdf")
	on.exit(unlink(f), add = TRUE)
	save_print_and_play(f)
	expect_equal(xmpdf::n_pages(f) |> as.integer(), 26L)
})
