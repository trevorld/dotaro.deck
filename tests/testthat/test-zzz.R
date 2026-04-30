test_that("save_images() works", {
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("DejaVu Sans"))
	dir <- tempfile()
	dir.create(dir)
	on.exit(unlink(dir, recursive = TRUE), add = TRUE)
	f <- save_images(dir = dir)
	expect_equal(xmpdf::n_pages(f) |> as.integer(), 108L)
})
