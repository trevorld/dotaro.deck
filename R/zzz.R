#' @importFrom dplyr arrange filter left_join mutate slice
#' @importFrom dplyr matches select
#' @import grid
#' @importFrom grDevices dev.cur cairo_pdf dev.off dev.set
#' @importFrom piecepackr as_pp_cfg has_font pp_cfg pp_shape pmap_piece crosshairGrob
#' @importFrom rlang abort check_dots_empty .data
#' @importFrom stringr str_glue str_replace str_sub
#' @importFrom utils packageDescription packageVersion
NULL

# Inches
CARD_WIDTH <- 2.25 # Bridge Card
CARD_HEIGHT <- 3.5
BLEED <- 1 / 8
INDEX_WIDTH <- 0.25
INDEX_HEIGHT <- 1.25
PIP_WIDTH <- CARD_WIDTH - 2 * BLEED - 2 * INDEX_WIDTH
PIP_HEIGHT <- 2 * PIP_WIDTH

suit_style <- function() {
	getOption("dotaro.deck.suits", "french")
}
hearts_diamonds_color <- function() {
	getOption("dotaro.deck.hearts_diamonds_color", "black")
}
spades_clubs_color <- function() {
	getOption("dotaro.deck.spades_clubs_color", "black")
}
light_color <- function() {
	getOption("dotaro.deck.light", "white")
}

dotaro_fonts_available <- local({
	checked <- FALSE
	function() {
		if (!checked) {
			stopifnot(
				`'Dotaro Ranks' font must be installed` = has_font("Dotaro Ranks"),
				`'Dotaro Suits' font must be installed` = has_font("Dotaro Suits")
			)
			checked <<- TRUE
		}
		invisible(TRUE)
	}
})

save_images <- function(label = "dotaro", dir = "tmp") {
	envir <- dotaro_decks(border = FALSE)
	current_dev <- dev.cur()
	if (current_dev > 1) {
		on.exit(dev.set(current_dev), add = TRUE)
	}

	filename <- file.path(
		dir,
		paste0(label, "_v", packageDescription("dotaro.deck")$Version, ".pdf")
	)
	cairo_pdf(filename, width = CARD_WIDTH, height = CARD_HEIGHT, onefile = TRUE)
	df <- half_info |>
		filter(.data$top) |>
		select("card", "top") |>
		left_join(full_info, by = c("card", "top")) |>
		arrange(.data$suit, .data$rank)
	for (i in seq_len(nrow(df))) {
		grid.newpage()
		dfx <- df[i, ] |>
			select("suit", "rank", "cfg") |>
			mutate(piece_side = "card_face")
		pmap_piece(dfx, envir = envir)
	}
	dev.off()
	invisible(filename)
}
