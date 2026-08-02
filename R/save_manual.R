#' Save manual of Dotaro Deck
#'
#' `save_manual()` saves a pdf manual explaining the Dotaro Deck, knit from
#' the `inst/Rtex` Rtex files via `knitr` and compiled with `xelatex`.
#' @param filename Filename
#' @param ... Ignored
#' @param variant One of `"french_bw"`, `"french_color"`, or `"hybrid"`.
#'   Which suit style and color scheme to render
#'   the manual's example figures in.  See [dotaro_deck_options()].
#' @param size Either `"letter"` or `"a4"`.
#' @param quietly Whether to hide document compilation output.
#' @return The filename invisibly.  As a side effect creates a pdf file.
#' @export
save_manual <- function(
	filename = "dotaro_manual.pdf",
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

	# `knit()` only auto-detects a file's chunk syntax (e.g. Rtex's
	# `%% begin.rcode`) -- and the plot/output hooks that go with it (e.g.
	# LaTeX `\includegraphics` vs. an HTML `<embed>` tag) -- when no chunk
	# pattern/hooks are currently active; otherwise it silently inherits
	# whatever is already set, since that's what `knit_child()` relies on for
	# same-format nesting. So when `save_manual()` is itself called from
	# inside an *unrelated* enclosing knitr session (e.g. a website's own
	# `.Rmd`/`.Rrst` page), it would otherwise inherit that session's
	# leftover, wrong-format state. Saving and restoring the affected knitr
	# globals in-process is fragile -- `render_latex()`/`render_markdown()`
	# touch more than just the hooks (`opts_knit`'s `out.format`, `opts_chunk`'s
	# `out.width`/`dev`, ...) and there's no guarantee that's the complete
	# list. Instead do the actual knitting in a throwaway `Rscript`
	# subprocess, which always starts with pristine knitr state, same as
	# `pprules`' own `save_ruleset()`/`save_rulebook()`/`save_pamphlet()`
	# (see websites/piecepackr/set_knitr_opts.R).
	tr <- tempfile(fileext = ".R")
	on.exit(unlink(tr))
	trdata <- tempfile(fileext = ".RData")
	on.exit(unlink(trdata), add = TRUE)
	save(filename, variant, size, quietly, file = trdata)
	code <- c(
		"library(dotaro.deck)",
		sprintf("load(%s)", shQuote(trdata)),
		# `tryCatch()` + explicit `quit()` rather than letting the error
		# propagate to R's default handler: some `Rscript` sessions configure
		# `options(error = recover)` (or similar) in a site/user `.Rprofile`,
		# which otherwise causes even a fatal error to exit with status 0.
		"tryCatch(",
		"  dotaro.deck:::save_manual_impl(filename, variant, size, quietly),",
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
		abort(c("`save_manual()` failed in a subprocess:", out))
	}

	invisible(filename)
}

save_manual_impl <- function(filename, variant, size, quietly) {
	dir <- setup_tempdir(filename)
	wd <- setwd(dir)
	on.exit(setwd(wd))

	rtex_dir <- system.file("Rtex", package = "dotaro.deck")
	file.copy(list.files(rtex_dir, full.names = TRUE), dir, overwrite = TRUE)

	# Set *before* knitting "shared.Rtex" below since that's where the example
	# figures actually get rendered -- setting these inside "manual.Rtex"
	# (knit afterwards) would be too late.
	local_options(!!!dotaro_deck_options(variant))
	title <- switch(
		variant,
		french_bw = "Dotaro Deck Manual (Black and White French Suits)",
		french_color = "Dotaro Deck Manual (French Suits)",
		hybrid = "Dotaro Deck Manual (Hybrid Suits)"
	)

	knit("shared.Rtex", quiet = quietly)
	tex <- knit("manual.Rtex", quiet = quietly)
	pdf <- xelatex(tex, quietly)

	xmp <- xmpdf::xmp(
		creator = "Trevor L. Davis",
		date_created = "2026",
		spdx_id = "CC-BY-4.0",
		title = title
	)
	file.copy(pdf, filename, overwrite = TRUE)
	tmpfile1 <- pnpmisc::pdf_set_xmp(filename, xmp = xmp)
	on.exit(unlink(tmpfile1), add = TRUE)
	tmpfile2 <- pnpmisc::pdf_set_docinfo(tmpfile1, docinfo = xmpdf::as_docinfo(xmp))
	on.exit(unlink(tmpfile2), add = TRUE)
	pnpmisc::pdf_compress(tmpfile2, filename, linearize = TRUE)

	invisible(filename)
}

# Adapted from `pprules:::xelatex()`/`pprules:::has_xelatex()`/
# `pprules:::check_xelatex()`/`pprules:::setup_tempdir()`/
# `pprules:::set_knitr_opts()` (github.com/piecepackr/pprules).

xelatex <- function(tex, quietly = TRUE) {
	stdout <- if (quietly) NULL else ""
	pdf <- sub("tex$", "pdf", tex)
	args <- c("-halt-on-error", "-interaction=nonstopmode")
	suppressWarnings(system2("xelatex", c(args, tex), stdout = stdout))
	error <- suppressWarnings(system2("xelatex", c(args, tex), stdout = TRUE))
	if (!file.exists(pdf)) {
		error <- grep("LaTeX Error", error, value = TRUE)
		error <- gsub("^! ", "", error)
		names(error) <- rep_len("x", length(error))
		msg <- c(str_glue("`xelatex` failed to compile `{tex}`"), error)
		abort(msg)
	}
	pdf
}

has_xelatex <- function() {
	tryCatch(
		{
			check_xelatex()
			TRUE
		},
		error = function(x) FALSE
	)
}

check_xelatex <- function() {
	stopifnot(nzchar(Sys.which("xelatex")))
	dir <- setup_tempdir("hello")
	wd <- setwd(dir)
	on.exit(setwd(wd), add = TRUE)
	on.exit(unlink(dir, recursive = TRUE), add = TRUE)
	test_file <- system.file("hello.tex", package = "dotaro.deck")
	file.copy(test_file, "hello.tex")
	xelatex("hello.tex")
	xelatex("hello.tex")
	invisible(NULL)
}

setup_tempdir <- function(output) {
	dir <- file.path(tempdir(), paste0(basename(output), "_tempdir"))
	unlink(dir, recursive = TRUE)
	dir.create(dir)
	dir
}

set_knitr_opts <- function(name) {
	opts_chunk$set(
		dev = "cairo_pdf",
		echo = FALSE,
		fig.align = "center",
		fig.path = paste0(name, "-"),
		fig.pos = "ht!"
	)
	invisible(NULL)
}
