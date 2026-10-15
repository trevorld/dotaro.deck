test_save_octavo_booklet <- function(variant, size = "letter") {
	skip_if_not_installed("bittermelon")
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("DejaVu Sans"))
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	f <- tempfile(fileext = ".pdf")
	on.exit(unlink(f), add = TRUE)
	save_octavo_booklet(f, variant = variant, size = size)
	# one sheet, printed on both sides
	expect_equal(xmpdf::n_pages(f) |> as.integer(), 2L)
}

test_that("save_octavo_booklet() works (hybrid)", {
	test_save_octavo_booklet("hybrid")
})

test_that("save_octavo_booklet() works (french_color)", {
	test_save_octavo_booklet("french_color", size = "a4")
})
