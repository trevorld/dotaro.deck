# Save the (work-in-progress, unpublished) "Dotaro" game rules to a pdf,
# knit from `inst/Rtex/dotaro_rules.Rtex` via `knitr` and compiled with
# `xelatex`. Kept internal (no `@export`) since the game/rules aren't
# finalized yet -- see GitHub issue #17. Mirrors `save_manual()`/
# `save_manual_impl()` in "save_manual.R", including knitting in a
# throwaway `Rscript` subprocess for the same knitr-state-isolation reasons
# documented there.
save_dotaro_rules <- function(
	filename = "dotaro_rules.pdf",
	...,
	variant = c("french_bw", "french_color", "hybrid"),
	size = c("letter", "a4"),
	quietly = TRUE
) {
	check_dots_empty()
	variant <- match.arg(variant)
	size <- tolower(size)
	size <- match.arg(size, c("letter", "a4"))

	if (!file.exists(filename)) {
		file.create(filename)
	}
	filename <- normalizePath(filename)

	tr <- tempfile(fileext = ".R")
	on.exit(unlink(tr))
	trdata <- tempfile(fileext = ".RData")
	on.exit(unlink(trdata), add = TRUE)
	save(filename, variant, size, quietly, file = trdata)
	code <- c(
		"library(dotaro.deck)",
		sprintf("load(%s)", shQuote(trdata)),
		"tryCatch(",
		"  dotaro.deck:::save_dotaro_rules_impl(filename, variant, size, quietly),",
		"  error = function(e) {",
		"    message(conditionMessage(e))",
		"    quit(status = 1L, save = \"no\")",
		"  }",
		")"
	)
	writeLines(code, tr)
	out <- suppressWarnings(system2("Rscript", tr, stdout = TRUE, stderr = TRUE))
	if (!quietly) {
		writeLines(out)
	}
	status <- attr(out, "status")
	if (!is.null(status) && status != 0L) {
		abort(c("`save_dotaro_rules()` failed in a subprocess:", out))
	}

	invisible(filename)
}

save_dotaro_rules_impl <- function(filename, variant, size, quietly) {
	dir <- setup_tempdir(filename)
	wd <- setwd(dir)
	on.exit(setwd(wd))

	rtex_dir <- system.file("Rtex", package = "dotaro.deck")
	file.copy(file.path(rtex_dir, "dotaro_rules.Rtex"), dir, overwrite = TRUE)

	local_options(!!!dotaro_deck_options(variant))

	tex <- knit("dotaro_rules.Rtex", quiet = quietly)
	pdf <- xelatex(tex, quietly)

	xmp <- xmpdf::xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = "Dotaro: Official Rules"
	)
	file.copy(pdf, filename, overwrite = TRUE)
	tmpfile1 <- pnpmisc::pdf_set_xmp(filename, xmp = xmp)
	on.exit(unlink(tmpfile1), add = TRUE)
	tmpfile2 <- pnpmisc::pdf_set_docinfo(tmpfile1, docinfo = xmpdf::as_docinfo(xmp))
	on.exit(unlink(tmpfile2), add = TRUE)
	pnpmisc::pdf_compress(tmpfile2, filename, linearize = TRUE)

	invisible(filename)
}
