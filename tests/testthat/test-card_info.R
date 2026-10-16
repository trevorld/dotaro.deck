# Guards on the shipped deck in `R/sysdata.rda`, mirroring the post-solve `stopifnot()` in
# `data-raw/0_solve_monolith.R` so that a bad rebuild by `data-raw/4_clean.R` (e.g. from a
# stale or re-sorted csv) fails `devtools::test()` without needing Gurobi.

FRENCH_SUITS <- c("H", "S", "C", "D")

# the 100 cards with a number half (i.e. not the 4 knights or 4 fools)
number_cards <- function() {
	df <- card_info[!is.na(card_info$bsuit) & card_info$brank %in% 0:9, ]
	df$nvalue <- paste0(df$bsuit, df$brank)
	df$nlight_suit <- paste0(df$blight, df$bsuit)
	df$tlight_suit <- paste0(df$tlight, df$tsuit)
	df
}

# both halves of each card as a (French suit, rank) pair; a card's domino ranks are its
# traditional rank and its number rank
suit_rank_halves <- function(df) {
	paste0(rep(df$tsuit, 2L), c(df$trank, df$brank))
}

# `x` splits into two halves as even as `length(x)` allows (27/28 for the 55 double-nine cards)
expect_even_split <- function(x) {
	n <- length(x)
	tab <- table(x)
	expect_length(tab, 2L)
	expect_setequal(as.vector(tab), c(n %/% 2L, n - n %/% 2L))
}

test_that("the deck has 108 uniquely identified cards", {
	expect_equal(nrow(card_info), 108L)
	expect_false(anyDuplicated(card_info$card) > 0L)
	# the traditional half-suit glyph plus rank is a unique id for all 108 cards
	expect_false(anyDuplicated(paste(card_info$tlight, card_info$tsuit, card_info$trank)) > 0L)
	expect_true(all(card_info$tlight != card_info$blight))
	expect_equal(sum(card_info$brank == "N"), 4L)
	expect_equal(sum(is.na(card_info$bsuit)), 4L)
})

test_that("shaded suits are the dark hearts/spades and light clubs/diamonds", {
	shaded <- (card_info$tlight == "D" & card_info$tsuit %in% c("H", "S")) |
		(card_info$tlight == "L" & card_info$tsuit %in% c("C", "D"))
	expect_equal(card_info$tshaded, shaded)
	expect_true(all(card_info$red == ifelse(card_info$tsuit %in% c("H", "D"), "R", "B")))
})

test_that("number halves and traditional halves are each used once", {
	df <- number_cards()
	expect_equal(nrow(df), 100L)
	expect_false(anyDuplicated(paste(df$blight, df$nvalue)) > 0L)
	expect_false(anyDuplicated(paste(df$tlight_suit, df$trank)) > 0L)
	# the fools take both Diamond Jacks and both Spade Queens
	expect_false(any(paste0(df$tsuit, df$trank) %in% c("SQ", "DJ")))
	expect_equal(as.vector(table(df$tsuit)[FRENCH_SUITS]), c(26L, 24L, 26L, 24L))
})

test_that("each number value 00--49 appears once plain and once shaded", {
	df <- number_cards()
	values <- sprintf("%d%d", rep(0:4, each = 10L), rep(0:9, 5L))
	expect_setequal(df$nvalue[!df$tshaded], values)
	expect_setequal(df$nvalue[df$tshaded], values)
	expect_false(anyDuplicated(df$nvalue[!df$tshaded]) > 0L)
	# so both cards of a value fall on the same side of the hearts/spades vs clubs/diamonds split
	hs <- tapply(df$tsuit %in% c("H", "S"), df$nvalue, unique)
	expect_true(all(lengths(hs) == 1L))
	expect_equal(sum(unlist(hs)), 25L)
})

test_that("number suits pair evenly with the traditional suits", {
	df <- number_cards()
	# deviation 4 is the floor imposed by the 26/24/26/24 French suit totals
	expect_equal(sum(abs(table(df$bsuit, df$tsuit) - 5L)), 4L)
	cells <- table(df$tlight_suit, df$bsuit)
	expect_true(all(cells >= 2L & cells <= 3L))
	expect_true(all(table(df$nlight_suit, df$red) == 5L))
	expect_true(all(table(df$nlight_suit, df$tshaded) == 5L))
})

test_that("number ranks pair evenly with the traditional suits", {
	df <- number_cards()
	red <- table(df$brank, df$red)[, "R"]
	expect_true(all(red >= 4L & red <= 6L))
	expect_true(all(table(df$brank, df$tsuit) >= 1L))
})

test_that("the 20 doubles are balanced and uniquely identified", {
	df <- number_cards()
	df <- df[df$trank == df$brank, ]
	expect_equal(nrow(df), 20L)
	expect_true(all(table(df$tsuit)[FRENCH_SUITS] == 5L))
	expect_true(all(table(df$nlight_suit) == 2L))
	expect_false(anyDuplicated(paste0(df$blight, df$brank)) > 0L)
	expect_false(anyDuplicated(df$nvalue) > 0L)
})

test_that("the 20 face-rank cards are balanced", {
	df <- number_cards()
	df <- df[df$trank %in% c("J", "Q", "K"), ]
	expect_equal(nrow(df), 20L)
	expect_true(all(table(df$nlight_suit) == 2L))
	expect_true(all(table(df$bsuit) == 4L))
	expect_false(anyDuplicated(df$nvalue) > 0L)
})

test_that("the d6 dice have 3 halves of each rank per French suit", {
	df <- number_cards()
	df <- df[df$d6, ]
	expect_equal(nrow(df), 36L)
	expect_true(all(table(df$tsuit)[FRENCH_SUITS] == 9L))
	expect_true(all(table(df$tlight_suit) %in% 4:5))
	halves <- table(suit_rank_halves(df))
	expect_setequal(names(halves), paste0(rep(FRENCH_SUITS, each = 6L), 1:6))
	expect_true(all(halves == 3L))
})

test_that("the d6 dominoes have 2 halves of each rank per French suit", {
	df <- number_cards()
	df <- df[df$double9 & df$trank %in% 0:6 & df$brank %in% 0:6, ]
	expect_equal(nrow(df), 28L)
	halves <- table(suit_rank_halves(df))
	expect_setequal(names(halves), paste0(rep(FRENCH_SUITS, each = 7L), 0:6))
	expect_true(all(halves == 2L))
})

test_that("the Chinese dominoes are balanced", {
	df <- number_cards()
	df <- df[df$chinese, ]
	expect_equal(nrow(df), 32L)
	expect_true(all(table(df$tlight_suit) == 4L))
	halves <- suit_rank_halves(df)
	expect_true(all(paste0(rep(FRENCH_SUITS, each = 6L), 1:6) %in% halves))
	# each of the 11 doubled civil tiles has one light and one dark traditional half
	tile <- paste(pmin(df$trank, df$brank), pmax(df$trank, df$brank))
	doubled <- names(which(table(tile) == 2L))
	expect_length(doubled, 11L)
	for (t in doubled) {
		expect_setequal(df$tlight[tile == t], c("D", "L"))
	}
})

test_that("the double-nine dominoes are spread across the French suits", {
	df <- number_cards()
	df <- df[df$double9, ]
	expect_equal(nrow(df), 55L)
	expect_true(all(table(df$tsuit)[FRENCH_SUITS] %in% 13:14))
})

test_that("every subdeck halves evenly along light/dark, red/black, and shaded/plain", {
	df <- number_cards()
	subdecks <- list(
		doubles = df$trank == df$brank,
		dom6 = df$double9 & df$trank %in% 0:6 & df$brank %in% 0:6,
		chinese = df$chinese,
		d6 = df$d6,
		double9 = df$double9
	)
	for (s in names(subdecks)) {
		sub <- df[subdecks[[s]], ]
		for (axis in c("tlight", "red", "tshaded")) {
			expect_even_split(sub[[axis]])
		}
	}
})
