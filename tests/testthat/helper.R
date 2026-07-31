# `dotaro_deck_options()` (see `R/zzz.R`) bundles the three canonical
# `dotaro.deck.*` option combinations. Tests that render cards should pin one
# explicitly (default the monochrome "french_bw" scheme), so a `dotaro.deck.*`
# option left set in the developer's own session doesn't change test output.
local_dotaro_deck_default_options <- function(
	variant = c("french_bw", "french_color", "hybrid"),
	.frame = parent.frame()
) {
	variant <- match.arg(variant)
	rlang::local_options(!!!dotaro_deck_options(variant), .frame = .frame)
}
