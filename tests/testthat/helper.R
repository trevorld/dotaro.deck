# The `dotaro.deck.*` option defaults baked into `R/zzz.R` (`suit_style()`,
# `hearts_diamonds_color()`, `spades_clubs_color()`, `light_color()`). Tests
# that render cards should pin these explicitly, so a `dotaro.deck.*` option
# left set in the developer's own session doesn't change test output.
dotaro_deck_default_options <- function() {
	list(
		dotaro.deck.suits = "french",
		dotaro.deck.hearts_diamonds_color = "black",
		dotaro.deck.spades_clubs_color = "black",
		dotaro.deck.light = "white"
	)
}

local_dotaro_deck_default_options <- function(.frame = parent.frame()) {
	rlang::local_options(!!!dotaro_deck_default_options(), .frame = .frame)
}
