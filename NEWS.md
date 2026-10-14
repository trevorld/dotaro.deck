dotaro.deck v0.4.0
==================

Deck changes
------------

* The number halves and traditional halves are paired differently than in v0.3.0, so cards printed from this version should not be mixed with cards printed from earlier versions.
* The shaded (German-suited) trump run is unbroken from 00 to 49 again. There are exactly 50 plain traditional halves (light hearts/spades and dark clubs/diamonds), so every number value now appears exactly once on a plain-suited card and once on a shaded-suited card. If one group takes the 50 shaded-suit cards to play a German-suited game the other group is left with the four plain suits *and* a complete 00--49 number deck, the way Euchre players leave a Crazy Eights deck behind. Both a shaded-suited and a plain-suited tarot deck have complete trumps.
* Restoring the full run costs two of v0.3.0's balance features: four of the 40 (traditional half-suit, number suit) pairings now hold 1 or 4 cards instead of 2--3, and each number rank pairs with 4--6 red traditional halves instead of exactly 5. Gurobi proved the full run cannot keep both of those exactly, and keeping the pairings exact alongside the exact d6 dice rank balance was never found, so the deck gives up a little of each and keeps the d6 balance exact. Every other subdeck property from v0.3.0 is kept.
* `data-raw/0_solve_monolith.R` warm-starts from the previous `gamma.csv`, only overwrites it on a strict improvement, and reads its time limit from the `DOTARO_TIME_LIMIT` environment variable. Its header records which further constraints were proven infeasible or never found.

dotaro.deck v0.2.0 (development)
================================

New features
------------

* `save_manual()` generates a pdf manual explaining the Dotaro Deck.
* `dotaro_deck_options()` returns the `dotaro.deck.*` options for one of three canonical (suit style, color palette) variants: `"french_bw"`, `"french_color"`, or `"hybrid"`.

dotaro.deck v0.1.0
===================

New features
------------

* `dotaro_decks()` returns `piecepackr::pp_cfg()` objects to draw Dotaro Deck cards and indices.
* `save_print_and_play()` generates a print-and-play file.
