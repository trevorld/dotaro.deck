# Dotaro Deck

### Table of Contents

* [Overview](#overview)
* [Examples](#examples)
* [Frequently Asked Questions](#faq)
* [Prior art](#prior)



**Warning:** The *Dotaro Deck* is currently at the **prototype** stage of development.  There is a [letter-sized print-and-play file](https://trevorldavis.com/piecepackr/share/pnp/dotaro_french_bw_letter.pdf) and a [A4-sized print-and-play file](https://trevorldavis.com/piecepackr/share/pnp/dotaro_french_bw_a4.pdf) that can be used to craft a deck.

## <a name="overview">Overview</a>

* The *Dotaro Deck* is a deck of 108 cards that can be used to play games from a variety of card game systems including:

  + several traditional card game systems:

    + one deck of traditional French-suited playing cards
    + two decks of traditional French-suited playing cards
    + one deck of traditional French-suited tarot cards
    + one double-nine set of dominoes
    + one set of Chinese dominoes

  + several modern card games including:

    - 10 suits with up to 10 ranks
    - 8 suits with up to 13 ranks
    - 5 suits with up to 20 ranks
    - 4 suits with up to 27 ranks
    - Pyramid decks e.g. 1 one, 2 twos, ..., 10 tens
    - Sequential decks with an ordinal ordering of up to 100 cards
    - A set of d6 dice cards
    - 2 x 5 x 10 dimensional deck
    - 2 x 2 x 2 x 13 dimensional deck

* Each card of the *Dotaro Deck* is vertically asymmetric with a light half and a dark half for a total of 2 x 108 = 216 card halves:

  + 112 "traditional suit" halves = 2 (light vs. dark) x 4 (French suits) x 14 (10 number ranks plus 4 face ranks)
  + 100 "number suit" halves = 2 (light vs. dark) x 5 (number suits) x 10 (10 number ranks)
  + 4 "fool" halves = 2 (light vs. dark) x 2 (circle vs. star)

* Each light half and dark half are combined to create the three types of cards in the *Dotaro Deck*:

  + Each of the hundred "number suit" halves is paired with a "traditional suit" half.  These halves are allocated such that there are double-9 domino, Chinese domino, and d6 dice card subdecks.

<img src="man/figures/README-number-cards-1.png" alt="The four fool cards"  />

  + Each of the four "knight" rank halves is combined with the other "knight" rank half of the same French suit.

<img src="man/figures/README-knight-cards-1.png" alt="The four knight cards"  />

  + Each of the four unsuited "fool" halves is paired with either a Queen of Spades half or a Jack of Diamonds half (which are special cards in Bezique, Hearts, Pinochle, Scabby Queen, etc.).

<img src="man/figures/README-fool-cards-1.png" alt="The four fool cards"  />

* Each index shows the top orientated half's rank and suit in a larger size and then beneath it shows the bottom orientated half's rank and suit in a smaller size.
  Cards are ambidextrous (i.e. left and right corner indices are the same) so cards can be fanned either left or right as each player prefers.
  Thus if playing domino games all relevant card information is available on any of the four corners but for traditional card games one can just focus on the larger top most rank and suit symbol (on either the left or right side) after orienting the card halves to the correct side for the game (usually either traditional suit halves, number suit halves, dark halves, or light halves orientated up top).

<img src="man/figures/README-anatomy-1.png" alt="Annotated Dotaro Deck card"  />

## <a name="examples">Examples</a>

### Rotate dark halves up top

* 4 traditional suits x 14 traditional ranks
* 5 number suits x 10 number ranks

  + If the number suit number is treated as a tens digit and the rank as the ones digit then the number suit cards go from 00 to 49

* 2 fools



![](man/figures/README-dark_up-1.png)

### Rotate light halves up top

* 4 traditional suits x 14 traditional ranks
* 5 number suits x 10 number ranks

  + If the number suit number is treated as a tens digit and the rank as the ones digit then the number suit cards go from 00 to 49

* 2 fools



![](man/figures/README-light_up-1.png)

### Rotate traditional suit halves up top

* Note both halves of the four knights has a traditional suit.
* Can be treated in the following ways:

  * 8 suits up to 13 ranks (i.e. treat light/dark suits as different suits)
  * 2 x 4 suits x 13 ranks (i.e. treat each French suit card as the same)
  * 4 suits x 27 ranks (i.e. treat each French suit as the same suit but let dark cards break ties with light cards)



![](man/figures/README-trad_up-1.png)

### Rotate traditional suit halves down below

* Can be treated in the following ways:

  + 10 suits up to 10 ranks (i.e. treat light/dark suits as different suits).
  + 2 x 5 suits x 10 ranks (i.e. treat light/dark suits as the same).
  + 5 suits x 20 ranks (i.e. treat light/dark suits as the same but let dark suit cards break ties with light suit cards).
  + Create a pyramid deck i.e. 1 one, 2 twos, ..., 10 zeros.
  + If you treat each number suit as the tens digit and each rank as the ones digit then you have two copies of two digit numbers going from 00 to 49.  If you let the dark suit card break ties with light cards then you have an ordinal sequence of up to one hundred cards (some prefer to treat dark cards as a "+50").

* Note below omits the four knights which has a traditional suit on both halves.



![](man/figures/README-num_up-1.png)

### One deck of traditional French-suited playing cards

* Treat the 0 rank as tens and the pawn rank as jacks.
* If necessary use the two fools as jokers (if you need more jokers you also have four knights and fifty number cards).



![](man/figures/README-one_french-1.png)


### Two decks of traditional French-suited playing cards

* Treat each dark/light French suit as the same suit.
* If necessary use the four knights as jokers e.g. the two hearts/diamonds knights can serve as "red" jokers and the two spades/clubs knights can serve as "black" jokers.
* The four Pinochle legs (i.e. Queens of Spades / Jacks of Diamonds) are opposite the four Fool cards.



![](man/figures/README-two_french-1.png)

### One deck of traditional French-suited tarot cards

* The traditional "0" rank is a ten and the "pawn" rank is a jack/page.
* For the trump cards treat each of the number suits as a two digit number with the number suit as the ten digit number and the rank as the one digit number.
* Use the star fool as the Fool/Excuse (in French Tarot Nouveau decks the Fool is usually represented by a star index).  However in some tarot card games the fool is the lowest or highest trump in which case it may make sense to instead use the number suit card corresponding to "00" or "22".



![](man/figures/README-french_tarot-1.png)

### One double-nine set of dominoes

* Not only does each of the five number suits (ignoring light/dark distinctions) have exactly 11 cards but for this subset of cards each number suit equals the total number of pips on each card modulo 5.  In particular a *necessary* condition that a card is a scoring card in games like Texas 42 is that the number suit is zero.



![](man/figures/README-double_nine-1.png)

### One set of Chinese dominoes



![](man/figures/README-chinese-1.png)

### One set of d6 dice cards

* Each of the 36 possible combinations of throwing two d6 dice.
* For this subset of cards the number suit equals the total number of pips modulo five.



![](man/figures/README-d6-1.png)

## <a name="faq">Frequently Asked Questions</a>

### Why the name Dotaro?

* The *Dotaro Deck* is inspired by domino sets and tarot decks and Dotaro is an abbreviation of Domino Tarot.
* In Esperanto the word "dotaro" would mean something like "collection of endowments" and the *Dotaro Deck* can be used to play a wide variety of the playing card legacies our ancestors have endowed us with.

## <a name="prior">Prior Art</a>

### Overviews

* [Crafting Additional Playing Card Suits](https://www.dragoncompany.org/crafting-additional-playing-card-suits/)
* [Playing Card Decks with Five or More Suits](http://www.solitairelaboratory.com/cards/FiveOrMoreSuits.html)
* [Playing Card Game Systems](https://boardgamegeek.com/geeklist/252876/playing-card-game-systems) Geeklist
* [Ranks and Suits - The Anatomy of Decks of Cards](https://boardgamegeek.com/geeklist/165733/ranks-and-suits-the-anatomy-of-decks-of-cards) Geeklist
* [Suits and Patterns](https://www.altacarta.com/english/collect/suits/index.html)

### Selected Multidecks

* [Banjo Deck](https://medium.com/@banjoanton/update-i-created-my-own-minimalistic-deck-and-now-its-available-to-print-or-buy-741d8362b30a)
* [DIY Multideck](https://diymultideck.mauri.app/)
* [The Everdeck](https://boardgamegeek.com/boardgame/291951/the-everdeck)
* [The Micaya Deck](https://micaya.selfthinker.org/)
* [Rainbow deck](https://boardgamegeek.com/boardgame/59655/rainbow-deck)
* [Singularity Deck](https://www.singularity.games/singularity-deck)
* [Skeleton Deck](https://kevan.org/games/skeleton)
* [Wil Su's Numbered double-deck experiment](https://boardgamegeek.com/thread/1867714/article/36786465#36786465)

### Pyramid decks

* [Pairs](https://crabfragmentlabs.com/pairs)

### Dark/light suits

* [Dual piecepacks](https://trevorldavis.com/piecepackr/dual-piecepacks-pnp.html)
* [Duplhex Playing Cards](https://www.thegamecrafter.com/games/duplhex-playing-cards?dept_uri=playing-cards&dept_name=Playing%20Cards)
* [Gamelib Pocket](https://www.gamelibpocket.com/en/)
* [Rainbow deck](https://boardgamegeek.com/boardgame/59655/rainbow-deck)

### Asymmetric decks

* [Dark Hand deck](https://www.jankendeck.com/dark)
* [Dual Deck](https://boardgamegeek.com/boardgame/112802/dual-deck)
* [Singularity Deck: Asymmetric Layout](https://www.singularity.games/singularity-deck)
* [Split Suit Playing Cards](https://boardgamegeek.com/boardgame/156181/split-suit-playing-cards)

* Domino decks:

  + [Bicycle Double Dominoes Card Game](https://boardgamegeek.com/thread/1478158/k6t-a-new-playing-cards-deck)
  + [Chinese Domino Cards](http://anthonylesq.blogspot.com/2019/02/chinese-domino-cards.html) and [Antique Chinese Domino Cards](https://www.criticaleyefinds.com/products/antique-chinese-domino-cards?variant=40655289647300)
  + [Double-6 Pokadeck](https://www.thegamecrafter.com/games/double-6-pokadeck-1x-deck-no-box)
  + [Flexdeck](https://www.amazon.com/Flexdeck-Dominoes-Combined-Playing-Adults/)
  + Cartesian Cards' [One Deck](https://cartesiancards.com/products/one-deck-game-cards)
  + [Quadkopf](https://www.thegamecrafter.com/games/quadkopf-double-9-domino-tin-deck)
  + [Scout](https://boardgamegeek.com/image/6764326/scout)

### Chess-related decks:

* [Chess Cards](https://boardgamegeek.com/geeklist/199391/chess-cards-using-cards-or-tiles-as-pieces-to-play) Geeklist
* [CardChess](https://boardgamegeek.com/boardgame/19062/cardchess)
* Cartesian Cards' [One Deck](https://cartesiancards.com/products/one-deck-game-cards)
* [En Prise](https://boardgamegeek.com/boardgame/153791/en-prise-chess-cards)
* [K6T](https://boardgamegeek.com/thread/1478158/k6t-a-new-playing-cards-deck)
* [The King's Caste](https://boardgamegeek.com/boardgame/38417/the-kings-caste)

### Tarot decks

* French Bourgeois Tarot Nouveau

  + <https://www.wopc.co.uk/france/catel-farcy/jeu-de-tarot>
  + <https://i-p-c-s.org/pattern/ps-18.html>
  + <https://en.wikipedia.org/wiki/Bourgeois_Tarot>
  + <https://l-pollett.tripod.com/cards28.htm>

* [The King's Caste](https://boardgamegeek.com/boardgame/38417/the-kings-caste)
* [Rider Waite Playing Card Deck](https://www.amazon.com/Rider-Waite-Playing-Card-Games-Systems/dp/1646710185/)
* [Trio Play Tarot Deck](https://boardgamegeek.com/boardgame/386643/trio-play-tarot-deck)
