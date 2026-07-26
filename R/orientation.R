# Display order (top to bottom, or left to right) for the French suits in the
# README's illustrations:
# hearts, diamonds, clubs, spades.
# Indexed by `corner_info$suit`'s own integer code (1:4 = dark H,S,C,D; 5:8 =
# light H,S,C,D), giving each code's rank in the desired H,D,C,S display
# order.
dotaro_french_suit_order <- rep(c(1, 4, 3, 2), 2)

#' Split a positioned `pmap_piece()` data frame into left/right halves
#'
#' Splits a data frame of piece positions (as built by the
#' `dotaro_orientation_*()` helpers) into a left and right half at `cutoff`,
#' re-anchoring the right half's `x` so it starts back at the left half's own
#' starting `x` --
#' e.g. so each half can be laid out on its own booklet page.
#'
#' @param df A data frame with an `x` column.
#' @param cutoff An x-coordinate (in `df`'s own units) to split on;
#'   pieces with `x < cutoff` go left, the rest go right.
#' @return A list with `left` and `right` data frames.
#' @noRd
dotaro_split_lr <- function(df, cutoff) {
	left <- df[df$x < cutoff, ]
	right <- df[df$x >= cutoff, ]
	right$x <- right$x - min(right$x) + min(left$x)
	list(left = left, right = right)
}

#' Scale a positioned `pmap_piece()` data frame's coordinates (and pieces)
#'
#' Multiplies `df`'s `x`/`y` columns by `scale` and sets a `scale` column
#' (consumed by `pieceGrob()`/`pmap_piece()` to shrink the pieces themselves)
#' to the same value, so the whole layout shrinks proportionally -- pieces
#' and their spacing alike -- rather than just the pieces in place.
#'
#' @param df A data frame with `x`/`y` columns.
#' @param scale A scale factor.
#' @return `df`, with `x`/`y` scaled and a `scale` column added.
#' @noRd
dotaro_scale_xy <- function(df, scale) {
	df$x <- df$x * scale
	df$y <- df$y * scale
	df$scale <- scale
	df
}

#' Build a `pmap_piece()` data frame for one of the "orientation" layouts
#'
#' These lay out all 108 *Dotaro Deck* cards' corner indices by a shared
#' orientation (which half is "up top"), as shown in `README.Rmd`.
#' Each returns a data frame ready for `pmap_piece(df, default.units = "in",
#' envir = envir)`, e.g. for a bridge-sized booklet where a layout needs to
#' be split across two facing pages.
#'
#' @param scale A scale factor, applied to the `x`/`y` positions and passed
#'   through to `pieceGrob()`/`pmap_piece()` to shrink the pieces themselves
#'   by the same amount (see `dotaro_scale_xy()`) --
#'   e.g. to shrink the whole layout to fit a smaller page.
#' @param side Either `"both"` (the full layout) or `"left"`/`"right"` (just
#'   that half, as split by `dotaro_split_lr()`, re-anchored back to the same
#'   starting `x` as the full layout's left edge).
#' @param envir A named list of `piecepackr::pp_cfg()` objects, as returned
#'   by `dotaro_decks()`.
#' @return A data frame with `suit`, `rank`, `cfg`, `piece_side`, `x`, `y`,
#'   and `scale` columns.
#' @name dotaro_orientation
#' @noRd
NULL

#' @rdname dotaro_orientation
#' @noRd
dotaro_orientation_dark_up <- function(
	scale = 1,
	side = c("both", "left", "right"),
	envir = dotaro_decks()
) {
	dotaro_orientation_dark_light_up("D", scale, match.arg(side), envir)
}

#' @rdname dotaro_orientation
#' @noRd
dotaro_orientation_light_up <- function(
	scale = 1,
	side = c("both", "left", "right"),
	envir = dotaro_decks()
) {
	dotaro_orientation_dark_light_up("L", scale, match.arg(side), envir)
}

# Shared by `dotaro_orientation_dark_up()`/`dotaro_orientation_light_up()`:
# the traditional-suit block (left) and number-suit block (right), with the
# 2 fool cards floating above the traditional-suit block.
# The natural left/right cutoff sits in the gap between the two blocks.
dotaro_orientation_dark_light_up <- function(light, scale, side, envir) {
	cfg_corner_trad <- envir$dotaro_corner_traditional
	IW <- cfg_corner_trad$get_width("card_face")
	IH <- cfg_corner_trad$get_height("card_face")
	X0 <- 0.1 + 0.5 * IW
	Y0 <- 0.1 - 0.5 * IH

	df_trad <- filter(half_info, .data$light == !!light, .data$suit %in% c("H", "S", "C", "D")) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		arrange(dotaro_french_suit_order[.data$suit], .data$rank) |>
		select("suit", "rank", "cfg") |>
		mutate(
			piece_side = "card_face",
			x = X0 + rep(seq(0, by = IW, length.out = 14L), 4L),
			y = Y0 + rep(4:1, each = 14L) * IH
		)

	df_num <- filter(half_info, .data$light == !!light, .data$suit %in% as.character(0:4)) |>
		arrange(.data$suit, .data$rank) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		mutate(
			piece_side = "card_face",
			x = max(df_trad$x) + IW + X0 + rep(seq(0, by = IW, length.out = 10L), 5L),
			y = Y0 + rep(5:1, each = 10L) * IH
		)

	df_fool <- filter(half_info, .data$light == !!light, is.na(.data$suit)) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		arrange(.data$rank) |>
		mutate(
			piece_side = "card_face",
			x = X0 + 6 * IW + seq(0, by = IW, length.out = 2L),
			y = max(df_trad$y) + IH + 0.2
		)

	df <- bind_rows(df_trad, df_num, df_fool)
	if (side != "both") {
		cutoff <- mean(c(max(df_trad$x), min(df_num$x)))
		df <- dotaro_split_lr(df, cutoff)[[side]]
	}
	dotaro_scale_xy(df, scale)
}

#' @rdname dotaro_orientation
#' @noRd
dotaro_orientation_trad_up <- function(
	scale = 1,
	side = c("both", "left", "right"),
	envir = dotaro_decks()
) {
	side <- match.arg(side)
	cfg_corner_trad <- envir$dotaro_corner_traditional
	IW <- cfg_corner_trad$get_width("card_face")
	IH <- cfg_corner_trad$get_height("card_face")
	X0 <- 0.1 + 0.5 * IW
	Y0 <- 0.1 - 0.5 * IH

	# Suit index order swaps clubs (3, 7) and diamonds (4, 8) from their
	# "natural" order so the left/right split below groups the 2 red suits
	# (hearts, diamonds) on one page and the 2 black suits (spades, clubs) on
	# the other, instead of mixing a red and a black suit on each page.
	df <- data.frame(
		piece_side = "card_face",
		cfg = "dotaro_corner_traditional",
		x = X0 + rep(seq(0, by = IW, length.out = 28L), 4L),
		y = Y0 + rep(4:1, each = 28L) * IH,
		rank = rep(1:14, 8L),
		suit = rep(c(1, 2, 5, 6, 4, 3, 8, 7), each = 14L)
	)
	df <- filter(df, .data$rank != 12L | .data$suit <= 4)
	df$y[which(df$rank == 12L)] <- df$y[which(df$rank == 12L)] - 0.5 * IH
	df$x[which(df$x > median(df$x))] <- df$x[which(df$x > median(df$x))] + 0.1
	df$y[which(df$y > median(df$y))] <- df$y[which(df$y > median(df$y))] + 0.1

	if (side != "both") {
		# The gap just inserted above (`+ 0.1`) sits right at the post-shift
		# median, so it's already a safe left/right cutoff.
		cutoff <- median(df$x)
		df <- dotaro_split_lr(df, cutoff)[[side]]
	}
	dotaro_scale_xy(df, scale)
}

#' @rdname dotaro_orientation
#' @noRd
dotaro_orientation_num_up <- function(
	scale = 1,
	side = c("both", "left", "right"),
	envir = dotaro_decks()
) {
	side <- match.arg(side)
	cfg_corner_trad <- envir$dotaro_corner_traditional
	IW <- cfg_corner_trad$get_width("card_face")
	IH <- cfg_corner_trad$get_height("card_face")
	X0 <- 0.1 + 0.5 * IW
	Y0 <- 0.1 - 0.5 * IH

	df_num_d <- filter(half_info, .data$light == "D", .data$suit %in% as.character(0:4)) |>
		arrange(.data$suit, .data$rank) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		mutate(
			piece_side = "card_face",
			x = X0 + rep(seq(0, by = IW, length.out = 10L), 5L),
			y = Y0 + rep(5:1, each = 10L) * IH
		)

	df_fool_d <- filter(half_info, .data$light == "D", is.na(.data$suit)) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		arrange(.data$rank) |>
		mutate(
			piece_side = "card_face",
			x = X0 + IW * 4 + seq(0, by = IW, length.out = 2L),
			y = max(df_num_d$y) + IH + 0.2
		)

	df_num_l <- filter(half_info, .data$light == "L", .data$suit %in% as.character(0:4)) |>
		arrange(.data$suit, .data$rank) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		mutate(
			piece_side = "card_face",
			x = X0 + IW * 10 + 0.2 + rep(seq(0, by = IW, length.out = 10L), 5L),
			y = Y0 + rep(5:1, each = 10L) * IH
		)

	df_fool_l <- filter(half_info, .data$light == "L", is.na(.data$suit)) |>
		select("card", "top") |>
		left_join(corner_info, by = c("card", "top")) |>
		select("suit", "rank", "cfg") |>
		arrange(.data$rank) |>
		mutate(
			piece_side = "card_face",
			x = X0 + IW * 14 + 0.2 + seq(0, by = IW, length.out = 2L),
			y = max(df_num_d$y) + IH + 0.2
		)

	df <- bind_rows(df_num_d, df_fool_d, df_num_l, df_fool_l)
	if (side != "both") {
		cutoff <- mean(c(max(df_num_d$x), min(df_num_l$x)))
		df <- dotaro_split_lr(df, cutoff)[[side]]
	}
	dotaro_scale_xy(df, scale)
}
