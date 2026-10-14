# Solve steps 1--3 exactly, as a single mixed-integer program, and write `data-raw/gamma.csv`
#
# This script supersedes `1_evolve_domino_scheme.R`, `2_filter_doubles.R` and
# `3_evolve_suit_schemes.R`.  Its output has the same columns as `data-raw/alpha.csv`, so
# `4_clean.R` consumes it directly:
#
#   Rscript data-raw/0_solve_monolith.R   # writes data-raw/gamma.csv (needs Gurobi; ~hours)
#   Rscript data-raw/4_clean.R            # rebuilds R/sysdata.rda
#
# If `data-raw/gamma.csv` already exists it is checked against every constraint and handed
# to Gurobi as a warm start, so a re-run is feasible from the first second and only spends
# its time limit looking for something better (a deck that no longer satisfies the model
# stops the script with the offending row, rather than being dropped silently).  `beta.csv`
# is the previous deck (shaded run 00--23 only), kept for reference.
#
# ## Why a single program
#
# The staged pipeline searches randomly at both ends: step 1 samples number suit halves
# until one satisfies the domino/dice rules (~200,000 attempts, about an hour), then step 3
# hill-climbs the traditional suit halves for up to 30 minutes per candidate.  Both stages
# report failure by running out of time, which cannot distinguish "hard" from "impossible".
#
# Worse, the *decomposition itself* loses solutions.  Step 1 commits to an allocation before
# step 3's subdeck features are visible, so a step 1 candidate can satisfy every one of its
# own requirements and still be unable to support step 3 (of the 57 `step_2` candidates that
# were generated, only 13 admitted the stricter chinese count).  Solving both stages jointly
# dissolves those conflicts.
#
# ## Solver
#
# This needs Gurobi rather than HiGHS.  Several features below are individually easy but
# jointly hard: HiGHS ran for hours on the d6 rank balance and the per-rank colour split
# without finding a solution *or* proving infeasibility, while Gurobi settles the same
# models.  A free academic licence covers this.
#
# Finding a feasible deck is dominated by the random seed.  On one probe a seed found a
# solution in 11 minutes where another found nothing in 25 minutes, on a model with a known
# solution; the old evolutionary scripts showed the same seed dependence.  So a timeout is
# weak evidence of infeasibility, and `ConcurrentMIP` (several independent solves racing on
# different seeds) is used in preference to one long single-seed run.
#
# ## The model
#
# One binary per allowed (card, number suit half, French suit) triple.  Indexing by the
# triple is what keeps the program linear: which *traditional* half a card consumes depends
# on both its number half (which fixes the shade and rank it needs) and its French suit, so
# separate y[card, half] and x[card, suit] variables would multiply together.
#
# Structural constraints:
#
# * each card takes exactly one (number half, French suit) triple
# * each of the 100 number suit halves is used exactly once
# * each of the 100 available traditional halves is used exactly once.  There are exactly 100
#   (112 = 2 x 4 x 14, less 8 for the knights and 4 for the fools' Diamond Jacks and Spade
#   Queens), so this also *implies* `trad_ranks_freq.csv` --- that table is a consequence of
#   the structure rather than an independent requirement.
# * the 70 cards with a pre-allocated number suit in `basic_domino_scheme.csv` may only take
#   halves of that suit (the "pips modulo 5" rule); this is imposed by restricting which
#   triples exist at all.
#
# Subdeck features, all linear in the triples:
#
# * "shaded trumps"    - *every one* of the 50 number values 00--49 appears on a card whose
#                        traditional half is plain (light hearts/spades or dark
#                        clubs/diamonds).  There are exactly 50 plain halves, so each value
#                        appears exactly once plain and once shaded.  Hence one group can take
#                        the 50 shaded-suit cards (the German suits) and the other group is
#                        left with the four plain suits *and* a complete 00--49 number deck
#                        --- the way Euchre players left a Crazy Eights deck behind.  It also
#                        gives an unbroken trump run for tarot, with 0 usable as the lowest
#                        trump in older games and the four fool cards existing regardless.
#                        Structurally, this forces both cards of a value onto the same side of
#                        the hearts/spades vs clubs/diamonds split (25 values each way).
# * "chinese dominoes" - each of the 8 trad. half-suits has 4 cards (so each of the 4 fr.
#                        suits has 8), and each fr. suit sees each of 6 ranks at least once
# * "d6 dice"          - each of 4 fr. suits has 9 cards, 4-5 per trad. half-suit, and
#                        exactly 3 halves of each rank (72 halves / 4 suits / 6 ranks)
# * "d6 dominoes"      - each of 4 fr. suits has two halves for each of 7 ranks
# * "doubles"          - each of 4 fr. suits has 5 cards
# * "doubles balance"  - each of the 10 half-number-suits has 2 of the 20 doubles
# * "d100 doubles"     - the 20 doubles use 20 distinct shade + rank pairs (what
#                        `2_filter_doubles.R` screened for) *and* 20 distinct number values,
#                        so a double's number half identifies it uniquely
# * "suit balance"     - the 5 number suits pair evenly with the 4 fr. suits (deviation 4,
#                        its floor -- see below); every (trad. half-suit, number suit) cell
#                        holds 1-4 cards, 2-3 wherever the objective can manage it (4 cells
#                        at 1 or 4 in practice); and each of the 10 half-number-suits pairs
#                        with 5 red and 5 black traditional halves
# * "rank balance"     - every number rank pairs with 4-6 red traditional halves, 5 wherever
#                        the objective can manage it (5 6 4 6 5 4 5 5 4 6 in practice), and
#                        shows at least one card of each fr. suit (1-5 in practice), so the
#                        deck laid out by rank looks even
# * "face cards"       - the 20 face-rank cards have 2 per half-number-suit, 4 per number
#                        suit, and 20 distinct number values
# * "civil pairs"      - each of the 11 doubled Chinese civil tiles has one light and one
#                        dark traditional half, so its two copies read as a matched pair
# * "even subdecks"    - 13-14 of the 55 double-nine cards per fr. suit
# * "three even axes"  - every subdeck halves evenly along each of light/dark (the Spanish
#                        suits are the light ones), red/black, and shaded/plain (the German
#                        suits are the shaded ones): doubles 10/10, dom6 14/14, chinese
#                        16/16, d6 18/18 and double-nine 27/28 (55 being odd).  So "take the
#                        light halves" or "take the shaded halves" names an even half of any
#                        subdeck.  The traditional half-suit glyph plus rank is also a unique
#                        id for all 108 cards -- including the fools and knights, which have
#                        no number half -- and needs no constraint.
#
# The two "wherever the objective can manage it" features are the only soft ones.  The
# objective minimises 64 x (cell deviation from 2-3) + 1 x (per-rank red deviation from 5),
# which is lexicographic since the per-rank total cannot exceed 10 under its 4-6 bounds.
# Everything else is a hard constraint.
#
# `suit balance` cannot be perfectly uniform.  The fools consume both Diamond Jacks and both
# Queens of Spades, leaving 24 diamonds and 24 spades against 26 hearts and 26 clubs, so the
# 20 (number suit, fr. suit) counts must deviate from 5 by at least
# |26 - 25| * 2 + |24 - 25| * 2 = 4.  The program reaches that floor.
#
# ## What is *not* achievable, or was not found
#
# * The stricter `8L + 48L` chinese variant discussed at `F_CHI` in
#   `3_evolve_suit_schemes.R`.  Its coverage half is impossible for any deck: rank 2 is on
#   only seven of the 32 Chinese domino cards, while covering all 8 traditional half-suits
#   needs at least eight.  That is a counting proof, not a failed search.
# * The full shaded run together with cells at exactly 2-3 *and* 5 red / 5 black on every
#   number rank --- proven infeasible by Gurobi.  Some loss on the colour split is therefore
#   unavoidable once the run is required.
# * The full shaded run together with cells at exactly 2-3 *and* the exact d6 rank balance.
#   Not refuted, but roughly ten hours of solver time across many seeds (single-seed runs,
#   4-seed concurrent runs, warm starts, and a tightened reformulation) never found one,
#   while either feature alone with the run is found within an hour or two.  The deck here
#   keeps the d6 balance exact and lets four cells slip to 1 or 4, on the grounds that an
#   uneven dice subdeck is visible at the table and four pairing cells are not.  The other
#   trade (cells exact, d6 off by one in four cells) is also reachable.
# * Dropping the shaded-axis symmetry constraints does not help: the deck that recovered the
#   exact d6 balance was found with them removed, yet satisfies all of them anyway.  They are
#   kept as hard constraints.
# * Giving the two copies of every number value different French suits (equivalently, one
#   red and one black, since the run puts both copies in the same hearts/spades or
#   clubs/diamonds family).  Impossible for all 50 values, by counting: it would need 25
#   cards per fr. suit, and the fools leave 26/24/26/24.  The ceiling is 48 values, with one
#   value doubled on hearts and one on clubs.  Even 48 was not found: 2 hours as a hard
#   constraint found nothing, and 2 hours with the mismatches penalised instead returned the
#   warm start (30 of 50 values split) unimproved.  Not refuted.
# * One red and one black traditional half for the two copies of each double (and the five
#   Chinese military pairs sharing a colour, probed alongside it).  No counting obstruction
#   is known, but 2 hours as hard constraints found nothing, 2 hours penalised returned the
#   warm start (4 of 10 doubles split, 1 of 5 military pairs matched) unimproved, and a
#   further 2 hours penalising the doubles alone on another seed did the same.  Runs from
#   scratch with the penalties found no deck at all in 2 hours, so they say nothing; the
#   base model is already near the edge of what Gurobi finds in that time.  Not refuted.

library("dplyr")
library("stringr")
library("Matrix")
stopifnot(requireNamespace("gurobi", quietly = TRUE))

SUITS <- c("H", "S", "C", "D")
OUTFILE <- "data-raw/gamma.csv"

# ---------------------------------------------------------------------------------------
# Reference tables (as in `3_evolve_suit_schemes.R`, so `gamma.csv` matches `alpha.csv`)
# ---------------------------------------------------------------------------------------

dft <- tibble::tibble(
	tlight = rep(c("L", "D"), each = 52),
	tsuit = rep(rep(SUITS, 13), 2),
	trank = rep(c(as.character(0:9), "J", "Q", "K"), 8),
	tred = rep(c("R", "B", "B", "R"), 26),
	tshaded = c(rep(c(F, F, T, T), 13), rep(c(T, T, F, F), 13)),
	tlabel = paste0(tlight, tsuit, " ", trank),
	tlight_rank = paste0(tlight, trank),
	tlight_red = paste0(tlight, tred),
	tlight_suit = paste0(tlight, tsuit),
	tsuit_rank = paste0(tsuit, trank)
)
# the fool cards took both Jacks of Diamonds and both Queens of Spades
dft <- filter(dft, tsuit_rank != "SQ", tsuit_rank != "DJ")

dfn <- tibble::tibble(
	nsuit = rep(as.character(rep(0:4, each = 2L)), 10L),
	nrank = as.character(rep(0:9, each = 10L)),
	nlight = rep(c("L", "D"), 50),
	nlabel = paste0(nlight, nsuit, " ", nrank),
	nlight_suit = paste0(nlight, nsuit),
	nsuit_rank = paste0(nsuit, nrank)
)

# ---------------------------------------------------------------------------------------
# Cards, subdeck membership (all determined by `label` alone), and number suit halves
# ---------------------------------------------------------------------------------------

NUM <- as.character(0:9)
cards <- read.csv("data-raw/basic_domino_scheme.csv", na.strings = "", colClasses = "character") |>
	mutate(
		is_double = grepl("^(.)-\\1[ab]$", label),
		dom9 = grepl("a$", label) & lrank %in% NUM,
		dom6 = dom9 & lrank %in% as.character(0:6) & rrank %in% as.character(0:6),
		d6 = lrank %in%
			as.character(1:6) &
			rrank %in% as.character(1:6) &
			((lrank != rrank) | grepl("a$", label))
	)
# the 32 Chinese domino cards: the 11 civil tiles twice, the 10 military tiles once
cards <- mutate(
	cards,
	chi = (d6 | label %in% paste0(1:6, "-", 1:6, "b")) &
		!(label %in%
			c("4-5b", "3-6b", "3-5b", "2-6b", "3-4b", "2-5b", "2-4b", "2-3b", "1-4b", "1-2b"))
)
stopifnot(sum(cards$chi) == 32L, sum(cards$d6) == 36L, sum(cards$dom6) == 28L)

halves <- expand.grid(
	nrank = NUM,
	nsuit = as.character(0:4),
	nlight = c("L", "D"),
	stringsAsFactors = FALSE
) |>
	mutate(
		nlabel = paste0(nlight, nsuit, " ", nrank),
		nlight_suit = paste0(nlight, nsuit),
		nsuit_rank = paste0(nsuit, nrank)
	)

# ---------------------------------------------------------------------------------------
# Enumerate the allowed (card, number half, French suit) triples
# ---------------------------------------------------------------------------------------

triples <- do.call(
	rbind,
	lapply(seq_len(nrow(cards)), function(i) {
		rk <- unique(c(cards$lrank[i], cards$rrank[i]))
		rk <- rk[rk %in% NUM] # a face rank can only take the other half's rank
		hh <- which(
			halves$nrank %in% rk & (is.na(cards$nsuit[i]) | halves$nsuit == cards$nsuit[i])
		)
		do.call(
			rbind,
			lapply(hh, function(h) {
				# the traditional half must be the opposite shade, and the *other* rank
				other <- ifelse(halves$nrank[h] == cards$rrank[i], cards$lrank[i], cards$rrank[i])
				need <- paste0(ifelse(halves$nlight[h] == "L", "D", "L"), other)
				g <- dft[dft$tlight_rank == need, ]
				data.frame(
					i = i,
					h = h,
					s = match(g$tsuit, SUITS),
					tsuit = g$tsuit,
					need = need,
					shaded = g$tshaded,
					nsuit = halves$nsuit[h],
					nlight = halves$nlight[h],
					nrank = halves$nrank[h],
					nsuit_rank = halves$nsuit_rank[h],
					nlight_suit = halves$nlight_suit[h],
					stringsAsFactors = FALSE
				)
			})
		)
	})
)

# ---------------------------------------------------------------------------------------
# Build and solve
# ---------------------------------------------------------------------------------------

# `time_limit` is spent looking for a better deck than the warm start (if any); a couple of
# minutes (e.g. `DOTARO_TIME_LIMIT=120`) just re-validates `gamma.csv`, while the deck here
# took about 70 minutes to find from scratch with `concurrent = 4` on 32 threads.
default_time_limit <- function() {
	tl <- Sys.getenv("DOTARO_TIME_LIMIT")
	if (nzchar(tl)) as.numeric(tl) else 7200
}

solve_monolith <- function(
	time_limit = default_time_limit(),
	concurrent = 4L,
	seed = 11L,
	start = OUTFILE
) {
	stopifnot(is.finite(time_limit), time_limit > 0)
	nz <- nrow(triples)
	cells <- expand.grid(nsuit = as.character(0:4), s = 1:4, stringsAsFactors = FALSE)
	hcells <- expand.grid(
		nsuit = as.character(0:4),
		s = 1:4,
		sh = c("L", "D"),
		stringsAsFactors = FALSE
	)
	hns <- sort(unique(triples$nlight_suit))

	# continuous slack variables, laid out after the binaries
	blocks <- c(
		surplus = nrow(cells),
		shortfall = nrow(cells), # suit balance, cap 4
		hns_over = length(hns),
		hns_under = length(hns), # half-number-suit red, cap 2
		cell_over = nrow(hcells),
		cell_under = nrow(hcells), # SOFT: cells outside 2-3
		rank_over = length(NUM),
		rank_under = length(NUM) # SOFT: per-rank red away from 5
	)
	offs <- nz + c(0, cumsum(blocks))[seq_along(blocks)]
	names(offs) <- names(blocks)
	NC <- nz + sum(blocks)
	slack_col <- function(block, k) offs[[block]] + k

	ii <- jj <- xx <- lo <- hi <- numeric(0)
	nr <- 0L
	add <- function(idx, coef, l, u) {
		nr <<- nr + 1L
		coef <- rep_len(coef, length(idx)) # scalars must be recycled for sparse triplets
		ii <<- c(ii, rep(nr, length(idx)))
		jj <<- c(jj, idx)
		xx <<- c(xx, coef)
		lo <<- c(lo, l)
		hi <<- c(hi, u)
	}
	suit_counts <- function(mask, target) {
		for (s in 1:4) {
			k <- which(mask[triples$i] & triples$s == s)
			add(k, 1, target, target)
		}
	}
	nsh <- str_sub(triples$need, 1, 1) # the traditional half's shade

	## structure
	for (i in seq_len(nrow(cards))) {
		add(which(triples$i == i), 1, 1, 1)
	}
	for (h in seq_len(nrow(halves))) {
		add(which(triples$h == h), 1, 1, 1)
	}
	for (g in unique(triples$need)) {
		for (s in 1:4) {
			k <- which(triples$need == g & triples$s == s)
			if (length(k)) add(k, 1, 1, 1)
		}
	}

	## doubles, d6 dice, chinese dominoes, d6 dominoes
	suit_counts(cards$is_double, 5L)
	suit_counts(cards$d6, 9L)
	# 4 per traditional *half*-suit, which implies the 8 per fr. suit of `fitness_chinese()`.
	# In the staged pipeline this needed the 32 Chinese cards to split 16/16 by shade, which
	# step 1 fixed blindly (only 13 of the 57 `step_2` candidates managed it); here the split
	# is chosen as part of the solve.
	for (sh in c("L", "D")) {
		for (s in 1:4) {
			k <- which(cards$chi[triples$i] & nsh == sh & triples$s == s)
			add(k, 1, 4, 4)
		}
	}
	for (s in 1:4) {
		for (rk in as.character(1:6)) {
			k <- which(
				cards$chi[triples$i] &
					triples$s == s &
					(cards$lrank[triples$i] == rk | cards$rrank[triples$i] == rk)
			)
			add(k, 1, 1, Inf)
		}
	}
	for (s in 1:4) {
		for (rk in as.character(0:6)) {
			# a double contributes two halves of its own rank, as in `fitness_dom6()`
			mult <- (cards$lrank[triples$i] == rk) + (cards$rrank[triples$i] == rk)
			k <- which(cards$dom6[triples$i] & triples$s == s & mult > 0)
			add(k, mult[k], 2, 2)
		}
	}

	## d6 dice: exactly 3 halves of each rank per fr. suit
	for (s in 1:4) {
		for (rk in as.character(1:6)) {
			mult <- (cards$lrank[triples$i] == rk) + (cards$rrank[triples$i] == rk)
			k <- which(cards$d6[triples$i] & triples$s == s & mult > 0)
			add(k, mult[k], 3, 3)
		}
	}

	## suit balance: count = 5 + surplus - shortfall, with the total deviation at its floor
	for (k in seq_len(nrow(cells))) {
		idx <- which(triples$nsuit == cells$nsuit[k] & triples$s == cells$s[k])
		add(
			c(idx, slack_col("surplus", k), slack_col("shortfall", k)),
			c(rep(1, length(idx)), -1, 1),
			5,
			5
		)
	}
	add(
		c(slack_col("surplus", seq_len(nrow(cells))), slack_col("shortfall", seq_len(nrow(cells)))),
		1,
		0,
		4
	)

	## each of the 5 number suits pairs with 10 red and 10 black traditional halves (each
	## number suit has 20 cards, so fixing red at 10 fixes black at 10 too)
	for (n in as.character(0:4)) {
		k <- which(triples$nsuit == n & triples$tsuit %in% c("H", "D"))
		add(k, 1, 10, 10)
	}

	## doubles balance, and the `2_filter_doubles.R` d100 property
	for (m in unique(triples$nlight_suit)) {
		add(which(cards$is_double[triples$i] & triples$nlight_suit == m), 1, 2, 2)
	}
	for (rk in NUM) {
		for (sh in c("L", "D")) {
			k <- which(cards$is_double[triples$i] & triples$nrank == rk & triples$nlight == sh)
			add(k, 1, 1, 1)
		}
	}
	## the 20 doubles use 20 distinct number values, so a double's number half identifies
	## it uniquely among the doubles (only 4 of the 57 `step_2` candidates managed this)
	for (v in unique(triples$nsuit_rank)) {
		add(which(cards$is_double[triples$i] & triples$nsuit_rank == v), 1, 0, 1)
	}

	## the 20 face-rank cards mirror the doubles: 2 per half-number-suit, 4 per number suit,
	## and 20 distinct number values.  (Their *traditional* suit split is not a choice: the
	## fools take both Diamond Jacks and both Spade Queens, so the available face halves are
	## H6 S4 C6 D4 and no deck can even them out.)
	is_face <- cards$lrank %in% c("J", "Q", "K")
	for (m in unique(triples$nlight_suit)) {
		add(which(is_face[triples$i] & triples$nlight_suit == m), 1, 2, 2)
	}
	for (n in as.character(0:4)) {
		add(which(is_face[triples$i] & triples$nsuit == n), 1, 4, 4)
	}
	for (v in unique(triples$nsuit_rank)) {
		add(which(is_face[triples$i] & triples$nsuit_rank == v), 1, 0, 1)
	}

	## SOFT: every one of the 40 (trad. half-suit, number suit) cells holds 2 or 3 cards
	## (100 / 40 is 2.5), as nearly as the objective can manage; 1-4 is a hard bound.
	## count - over + under is in [2, 3], so over/under measure the excursion outside it.
	for (q in seq_len(nrow(hcells))) {
		idx <- which(
			nsh == hcells$sh[q] & triples$s == hcells$s[q] & triples$nsuit == hcells$nsuit[q]
		)
		add(idx, 1, 1, 4)
		add(
			c(idx, slack_col("cell_over", q), slack_col("cell_under", q)),
			c(rep(1, length(idx)), -1, 1),
			2,
			3
		)
	}

	## each of the 11 doubled Chinese "civil" tiles gets one light and one dark traditional
	## half, so its two copies read as a matched pair.  The 6 civil doubles get this for free
	## (the doubles alternate shade by rank); this binds on 1-3, 1-5, 1-6, 4-6 and 5-6.
	CIVIL <- c("1-1", "2-2", "3-3", "4-4", "5-5", "6-6", "1-3", "1-5", "1-6", "4-6", "5-6")
	for (tile in CIVIL) {
		k <- which(substr(cards$label[triples$i], 1, 3) == tile & cards$chi[triples$i] & nsh == "L")
		add(k, 1, 1, 1)
	}

	## double-nine dominoes: 13 or 14 cards per fr. suit (55 cards cannot split evenly), and
	## 4 or 5 of the 36 d6 dice cards per trad. half-suit (36 / 8 is 4.5).
	for (s in 1:4) {
		add(which(cards$dom9[triples$i] & triples$s == s), 1, 13, 14)
	}
	for (sh in c("L", "D")) {
		for (s in 1:4) {
			add(which(cards$d6[triples$i] & nsh == sh & triples$s == s), 1, 4, 5)
		}
	}

	## every subdeck splits evenly between light and dark traditional halves, so "take the
	## light-half cards" hands a game designer an even, nameable half of any subdeck.  The
	## chinese (16/16) and doubles (10/10) splits already follow from constraints above; 55
	## is odd so the double-nine set can only reach 27/28.
	add(which(cards$dom6[triples$i] & nsh == "L"), 1, 14, 14)
	add(which(cards$d6[triples$i] & nsh == "L"), 1, 18, 18)
	add(which(cards$dom9[triples$i] & nsh == "L"), 1, 27, 28)

	## each of the 10 half-number-suits pairs with 5 red and 5 black traditional halves
	## (each holds 10 cards), which refines the 10/10 per number suit above.
	##
	## Note the slack: asking for this *exactly* --- as an equality, or as a cap of 0 on the
	## total deviation --- does not solve within an hour, while allowing 2 and letting the
	## solver settle finds a perfect 5/5.  The `stopifnot()` below asserts the result really
	## is 5/5, so a future change cannot silently degrade it.
	## count = 5 + over - under, as for the cells and ranks below
	for (q in seq_along(hns)) {
		idx <- which(triples$nlight_suit == hns[q] & triples$tsuit %in% c("H", "D"))
		add(
			c(idx, slack_col("hns_over", q), slack_col("hns_under", q)),
			c(rep(1, length(idx)), -1, 1),
			5,
			5
		)
	}
	add(c(slack_col("hns_over", seq_along(hns)), slack_col("hns_under", seq_along(hns))), 1, 0, 2)

	## the shaded partition axis
	add(which(cards$is_double[triples$i] & triples$shaded), 1, 10, 10)
	add(which(cards$dom6[triples$i] & triples$shaded), 1, 14, 14)
	add(which(cards$dom9[triples$i] & triples$shaded), 1, 27, 28)
	add(which(cards$d6[triples$i] & triples$shaded), 1, 18, 18)
	for (m in unique(triples$nlight_suit)) {
		add(which(triples$nlight_suit == m & triples$shaded), 1, 5, 5)
	}

	## the full shaded run: every one of the 50 number values on exactly one plain card.
	## (There are exactly 50 plain halves, so "at least one" would be equivalent, but the
	## equality is tighter for the solver.)
	for (v in unique(triples$nsuit_rank)) {
		add(which(triples$nsuit_rank == v & !triples$shaded), 1, 1, 1)
	}

	## every number rank shows at least one card of each fr. suit, so no rank is missing a
	## suit when the deck is laid out by rank
	for (rk in NUM) {
		for (s in 1:4) {
			add(which(triples$nrank == rk & triples$s == s), 1, 1, Inf)
		}
	}

	## SOFT: every number rank pairs with 5 red and 5 black traditional halves, as nearly as
	## the objective can manage; 4-6 is a hard bound.  count + under - over = 5.
	for (q in seq_along(NUM)) {
		k <- which(triples$nrank == NUM[q] & triples$tsuit %in% c("H", "D"))
		add(k, 1, 4, 6)
		add(
			c(k, slack_col("rank_over", q), slack_col("rank_under", q)),
			c(rep(1, length(k)), -1, 1),
			5,
			5
		)
	}

	stopifnot(length(ii) == length(xx), length(jj) == length(xx))
	A <- sparseMatrix(i = ii, j = jj, x = xx, dims = c(nr, NC))
	cat("variables:", NC, " constraints:", nr, " non-zeros:", length(xx), "\n")

	## lexicographic objective: cells first (64 exceeds the per-rank total's bound of 10)
	obj <- numeric(NC)
	obj[c(
		slack_col("cell_over", seq_len(nrow(hcells))),
		slack_col("cell_under", seq_len(nrow(hcells)))
	)] <- 64
	obj[c(slack_col("rank_over", seq_along(NUM)), slack_col("rank_under", seq_along(NUM)))] <- 1

	eq <- which(lo == hi)
	geq <- which(is.finite(lo) & lo != hi & !is.finite(hi))
	leq <- which(is.finite(hi) & lo != hi & !is.finite(lo))
	rng <- which(is.finite(lo) & is.finite(hi) & lo != hi)
	# a ranged row l <= a'x <= u is written as two rows
	model <- list(
		A = rbind(
			A[eq, , drop = FALSE],
			A[geq, , drop = FALSE],
			A[rng, , drop = FALSE],
			A[leq, , drop = FALSE],
			A[rng, , drop = FALSE]
		),
		obj = obj,
		sense = c(
			rep("=", length(eq)),
			rep(">=", length(geq) + length(rng)),
			rep("<=", length(leq) + length(rng))
		),
		rhs = c(lo[eq], lo[geq], lo[rng], hi[leq], hi[rng]),
		vtype = c(rep("B", nz), rep("C", NC - nz)),
		lb = numeric(NC),
		ub = c(rep(1, nz), rep(20, NC - nz)),
		modelsense = "min"
	)

	if (!is.null(start) && file.exists(start)) {
		st <- read.csv(start, colClasses = "character")
		st <- st[match(cards$label, st$label), ] # by label, not by row position
		stopifnot(!anyNA(st$label))
		key <- paste0(triples$nlight, triples$nsuit, " ", triples$nrank, "|", triples$tsuit)
		chosen <- which(key == paste0(st$nlabel, "|", st$tsuit)[triples$i])
		stopifnot(length(chosen) == nrow(cards))
		x <- numeric(NC)
		x[chosen] <- 1
		# Complete the start with the slack values the binaries imply, then check it against
		# every row.  Gurobi discards an infeasible start without a word under `OutputFlag = 0`,
		# and a short re-run would then report "no solution" as if the model were merely hard,
		# when the real news is that `start` no longer satisfies the constraints.
		ct <- triples[chosen, ]
		ct$red <- ct$tsuit %in% c("H", "D")
		ct$nsh <- nsh[chosen]
		set_slack <- function(over, under, count, lo_t, hi_t) {
			x[slack_col(over, seq_along(count))] <<- pmax(0, count - hi_t)
			x[slack_col(under, seq_along(count))] <<- pmax(0, lo_t - count)
		}
		set_slack(
			"surplus",
			"shortfall",
			vapply(
				seq_len(nrow(cells)),
				function(k) sum(ct$nsuit == cells$nsuit[k] & ct$s == cells$s[k]),
				0
			),
			5,
			5
		)
		set_slack(
			"hns_over",
			"hns_under",
			vapply(hns, function(m) sum(ct$nlight_suit == m & ct$red), 0),
			5,
			5
		)
		set_slack(
			"cell_over",
			"cell_under",
			vapply(
				seq_len(nrow(hcells)),
				function(q) {
					sum(ct$nsh == hcells$sh[q] & ct$s == hcells$s[q] & ct$nsuit == hcells$nsuit[q])
				},
				0
			),
			2,
			3
		)
		set_slack(
			"rank_over",
			"rank_under",
			vapply(NUM, function(rk) sum(ct$nrank == rk & ct$red), 0),
			5,
			5
		)
		Ax <- as.vector(A %*% x)
		bad <- which(Ax < lo - 1e-9 | Ax > hi + 1e-9)
		if (length(bad)) {
			stop(sprintf(
				"warm start %s violates %d model row(s); first is row %d with value %g outside [%g, %g] -- was a constraint added since it was written?",
				start,
				length(bad),
				bad[1],
				Ax[bad[1]],
				lo[bad[1]],
				hi[bad[1]]
			))
		}
		model$start <- x
		cat("warm start loaded from", start, "and verified feasible\n")
	}

	cat("gurobi rows:", nrow(model$A), "\n")
	flush.console()
	r <- gurobi::gurobi(
		model,
		params = list(
			TimeLimit = time_limit,
			OutputFlag = 0L,
			MIPFocus = 1L,
			ConcurrentMIP = concurrent,
			Seed = seed
		)
	)
	list(status_message = r$status, primal_solution = r$x, objective = r$objval)
}

# ---------------------------------------------------------------------------------------
# Score with the fitness functions of `3_evolve_suit_schemes.R`, so the solution is checked
# against the original definitions rather than against the model's own arithmetic
# ---------------------------------------------------------------------------------------

score <- function(dfj) {
	df6 <- filter(dfj, dom6)
	halves6 <- c(paste0(df6$tsuit, df6$lrank), paste0(df6$tsuit, df6$rrank))
	t6 <- table(df6$tsuit)
	dfc <- filter(dfj, chi)
	tc <- table(dfc$tsuit)
	td <- table(slice(dfj, 1:20)$tsuit)
	td6 <- table(filter(dfj, d6)$tsuit)
	c(
		shaded = length(unique(filter(dfj, !tshaded)$nsuit_rank)), #                    max 50
		dom6_old = length(t6) - sum(abs(t6 - 7)) + length(unique(halves6)), #           max 32
		dom6 = length(which(table(halves6) == 2L)), #                                   max 28
		chinese = length(tc) -
			sum(abs(tc - 8)) +
			length(unique(c(paste0(dfc$tsuit, dfc$lrank), paste0(dfc$tsuit, dfc$rrank)))), # 28
		d6 = length(td6) - sum(abs(td6 - 9)), #                                          max 4
		doubles = length(td) - sum(abs(td - 5)), #                                       max 4
		balance = sum(abs(table(dfj$nsuit, dfj$tsuit) - 5)) #                       floor 4 (!)
	)
}

# the objective, recomputed from a deck data frame (64 x cell deviation + per-rank red deviation)
soft_objective <- function(df) {
	cells <- table(paste0(df$tlight, df$tsuit), df$nsuit)
	red <- table(df$nrank, df$tred)[, "R"]
	64 * sum(pmax(0L, cells - 3L) + pmax(0L, 2L - cells)) + sum(abs(red - 5L))
}

start_objective <- if (file.exists(OUTFILE)) {
	soft_objective(read.csv(OUTFILE, colClasses = "character"))
} else {
	Inf
}

res <- solve_monolith()
cat("status:", res$status_message, "  objective:", res$objective, "\n")
if (is.null(res$primal_solution)) {
	stop(
		"gurobi returned '",
		res$status_message,
		"' with no solution -- raise `time_limit`, or relax a feature (see the notes at the ",
		"top of this file)"
	)
}

k <- which(round(res$primal_solution[seq_len(nrow(triples))]) == 1L)
stopifnot(length(k) == 100L)
z <- triples[k, ][order(triples$i[k]), ]

dfj <- cards |>
	mutate(
		nlabel = halves$nlabel[z$h],
		needs = z$need,
		tlabel = paste0(str_sub(z$need, 1, 1), z$tsuit, " ", str_sub(z$need, 2, 2))
	) |>
	select("label", "lrank", "rrank", "nlabel", "needs", "tlabel", "dom9", "dom6", "d6", "chi") |>
	left_join(dfn, by = "nlabel") |>
	left_join(dft, by = "tlabel")

df6 <- filter(dfj, d6)
d6_halves <- table(c(paste0(df6$tsuit, df6$lrank), paste0(df6$tsuit, df6$rrank)))
cell_counts <- table(paste0(dfj$tlight, dfj$tsuit), dfj$nsuit)
red_per_rank <- table(dfj$nrank, dfj$tred)[, "R"]

stopifnot(
	nrow(dfj) == 100L,
	n_distinct(dfj$nlabel) == 100L, # every number suit half used once
	n_distinct(dfj$tlabel) == 100L, # every traditional half used once
	!any(dfj$tsuit_rank %in% c("SQ", "DJ")), # the fools' halves are untouched
	all(dfj$tlight != dfj$nlight), # the two halves are opposite shades
	all(table(dfj$tsuit)[SUITS] == c(26L, 24L, 26L, 24L)),
	all(table(paste0(dfj$nlight, dfj$nsuit)[1:20]) == 2L), # doubles balance
	n_distinct(paste0(dfj$nlight, dfj$nrank)[1:20]) == 20L, # d100 doubles
	all(table(dfj$nlight_suit, dfj$tred) == 5L), # half-number-suit x red/black
	all(table(dfj$nlight_suit, dfj$tshaded) == 5L), # half-number-suit x shaded/plain
	all(table(dfj$nrank, dfj$tsuit) >= 1L), # every rank shows every fr. suit
	all(d6_halves == 3L), # 3 d6 halves per fr. suit x rank
	all(sprintf("%d%d", rep(0:4, each = 10), rep(0:9, 5)) %in% filter(dfj, !tshaded)$nsuit_rank), # full run
	all(cell_counts >= 1L & cell_counts <= 4L),
	all(red_per_rank >= 4L & red_per_rank <= 6L),
	# the soft objective must be at least as good as the deck adopted in 2026 (4 cells off,
	# red deviation 6).  One bound, not one per term: a deck with 3 cells off and a worse
	# colour split is strictly better under the lexicographic objective and must not abort.
	soft_objective(dfj) <= 64L * 4L + 6L
)
print(score(dfj))

cat("d6 halves per suit x rank:", paste(range(d6_halves), collapse = "-"), "\n")
cat("cells outside 2-3:", sum(cell_counts < 2L | cell_counts > 3L), "\n")
cat("red per number rank:", paste(red_per_rank, collapse = " "), "\n")
cat("pairing deviation:", sum(abs(table(dfj$nsuit, dfj$tsuit) - 5)), "\n")
# Gurobi often returns a *different* deck with the same objective (the face cards can be
# permuted freely), so only replace the adopted deck when the solver strictly improves on it.
if (round(soft_objective(dfj)) < start_objective) {
	write.csv(dfj, OUTFILE, row.names = FALSE)
	cat("wrote", OUTFILE, "\n")
} else {
	cat("no improvement on", OUTFILE, "(objective", start_objective, ") -- left unchanged\n")
}
