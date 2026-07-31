test_save_manual <- function(variant, size = "letter") {
	skip_if_not_installed("systemfonts")
	skip_if_not(piecepackr::has_font("DejaVu Sans"))
	skip_if_not(piecepackr::has_font("Dotaro Ranks"))
	skip_if_not(piecepackr::has_font("Dotaro Suits"))
	skip_if_not(has_xelatex(), "Doesn't have suitable xelatex setup")
	f <- tempfile(fileext = ".pdf")
	on.exit(unlink(f), add = TRUE)
	save_manual(f, variant = variant, size = size)
	expect_true(file.exists(f))
	expect_gt(xmpdf::n_pages(f), 5)
}

test_that("save_manual() works (french_bw)", {
	test_save_manual("french_bw")
})

test_that("save_manual() works (french_color)", {
	test_save_manual("french_color")
})

test_that("save_manual() works (hybrid)", {
	test_save_manual("hybrid", size = "a4")
})
