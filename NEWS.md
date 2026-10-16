dotaro.deck v0.4.0 (development)
================================

Deck changes
------------

* The number halves and traditional halves are paired differently than in v0.3.0, so cards printed from this version should not be mixed with cards printed from earlier versions:

  + The shaded (German-suited) trump run is unbroken from 00 to 49 again. There are exactly 50 plain traditional halves (light hearts/spades and dark clubs/diamonds), so every number value now appears exactly once on a plain-suited card and once on a shaded-suited card.
  + Restoring the full run costs one of v0.3.0's balance features: each number rank now pairs with 4--6 red traditional halves instead of exactly 5 (Gurobi proved the full run cannot keep that exactly alongside the other features). Every other subdeck property from v0.3.0 is kept, including every (traditional half-suit, number suit) pairing holding 2--3 cards and the exact d6 dice rank balance.

dotaro.deck v0.3.0
==================

Deck changes
------------

* The number halves and traditional halves are paired differently than in v0.2.0, so cards printed from this version should not be mixed with cards printed from earlier versions:

  + Every subdeck feature improves except the shaded trump run:

    - The d6 dominoes have two halves of every rank per French suit (previously only 7 of the 28 rank/suit combinations did).
    - The d6 dice have 9 cards per French suit with exactly 3 halves of each rank per suit (previously the suits were uneven).
    - The Chinese dominoes have 8 cards per French suit, 4 per traditional half-suit, each suit seeing every rank.
    - The number suits pair as evenly with the French suits as the deck's 26/24/26/24 suit totals allow (the total deviation from 5 per pairing drops from 26 to 4, its arithmetic floor).
    - Every number rank pairs with 5 red and 5 black traditional halves and shows all four French suits (previously 3--7 red, and some ranks had no cards of one suit).
    - The 20 doubles have 2 per half-number-suit and 20 distinct number values, so a double's number half identifies it uniquely.
    - Every subdeck halves evenly along each of light/dark, red/black, and shaded/plain, so "take the light halves" or "take the shaded halves" names an even half of any subdeck.

  + The shaded trump run shrinks from 00--49 to 00--23. This still covers trumps 1--21 for French and Latin tarot and leaves 0 usable as the lowest trump in older games.

dotaro.deck v0.2.0
==================

Breaking changes
----------------

* The `dotaro.deck.red`, `dotaro.deck.black`, and `dotaro.deck.dark` options are replaced by `dotaro.deck.hearts_diamonds_color` and `dotaro.deck.spades_clubs_color`, which name the suit family rather than its color.

New features
------------

* `save_manual()` generates a pdf manual explaining the Dotaro Deck.
* `dotaro_deck_options()` returns the `dotaro.deck.*` options for one of three canonical (suit style, color palette) variants: `"french_bw"`, `"french_color"`, or `"hybrid"`.
* New `dotaro.deck.suits` option. The default `"french"` is unchanged; the experimental `"hybrid"` style draws the French suits as fused two-layer glyphs on the light half and gives the number suits their own pip shapes.  Under the `"french"` style the dark half's suit, rank, and fool glyphs get a black border instead of blending into their own accent-colored fill.
* New `dotaro.deck.number_suits_color` option colors the number suits and the fool independently of the hearts/diamonds and spades/clubs colors, since those cards are only paired with a French suit family for balance rather than belonging to it.
* Card glyphs are rendered with the Dotaro Ranks and Dotaro Suits fonts from the `{dotaro.font}` package instead of DejaVu Sans.
* Every card now has a shared card back: a scoring-track tiling of squares, diamonds, and eight-pointed stars that doubles as a cribbage board, counter track, or 8x8 grid. `save_print_and_play()` interleaves front and back pages for double-sided printing, and its new `crosshairs` argument chooses which side(s) of each sheet get crosshairs and cropmarks (default `"back"`).

dotaro.deck v0.1.0
===================

New features
------------

* `dotaro_decks()` returns `piecepackr::pp_cfg()` objects to draw Dotaro Deck cards and indices.
* `save_print_and_play()` generates a print-and-play file.
* The `dotaro.deck.red`, `dotaro.deck.black`, `dotaro.deck.dark`, and `dotaro.deck.light` options set the colors of the hearts/diamonds, the spades/clubs, the dark half, and the light half. They default to black on white.
