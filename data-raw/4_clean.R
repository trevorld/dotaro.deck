library("dplyr")
library("stringr")

# Reshapes a solved deck into the three data frames saved in `R/sysdata.rda`.
#
# `beta.csv` is written by `0_solve_monolith.R`, which solves steps 1--3 exactly as a single
# mixed-integer program.  To recreate it:
#
#   Rscript data-raw/0_solve_monolith.R   # writes data-raw/beta.csv
#   Rscript data-raw/4_clean.R            # rebuilds R/sysdata.rda
#
# `alpha.csv` is the earlier deck, found by the evolutionary prototype in
# `1_evolve_domino_scheme.R` -> `2_filter_doubles.R` -> `3_evolve_suit_schemes.R`.  Those
# scripts are kept for reference but are no longer part of the pipeline: `beta.csv` matches
# or beats `alpha.csv` on every subdeck feature (notably "d6 dominoes" 28/28 vs 7/28,
# "d6 dice" 4/4 vs 0/4, and the number/fr. suit pairing balance at its floor of 4 vs 26).

df <- read.csv("data-raw/beta.csv") |>
	arrange(tlabel) |>
	select(
		"tsuit",
		"trank",
		"tlight",
		bsuit = "nsuit",
		brank = "nrank",
		blight = "nlight",
		double9 = "dom9",
		chinese = "chi",
		"d6",
		red = "tred",
		"tshaded"
	) |>
	mutate(brank = as.character(brank), bsuit = as.character(bsuit))

df_knights <- data.frame(
	double9 = FALSE,
	chinese = FALSE,
	d6 = FALSE,
	red = c("R", "B", "B", "R"),
	tsuit = c("H", "S", "C", "D"),
	trank = "N",
	tlight = "D",
	bsuit = c("H", "S", "C", "D"),
	brank = "N",
	blight = "L",
	tshaded = c(TRUE, TRUE, FALSE, FALSE)
)

df <- bind_rows(df, df_knights)
df <- mutate(df, card = str_glue("{tlight}{tsuit} {trank}-{blight}{bsuit} {brank}"))

df_fools <- data.frame(
	double9 = FALSE,
	chinese = FALSE,
	d6 = FALSE,
	red = c("R", "R", "B", "B"),
	tsuit = c("D", "D", "S", "S"),
	trank = c("J", "J", "Q", "Q"),
	tlight = c("D", "L", "D", "L"),
	bsuit = NA_character_,
	brank = c("O", "O", "F", "F"),
	blight = c("L", "D", "L", "D"),
	tshaded = c(FALSE, TRUE, TRUE, FALSE),
	card = c("DD J-L  O", "LD J-D  O", "DS Q-L  F", "LS Q-D  F")
)

df <- bind_rows(df, df_fools) |>
	select(
		"card",
		"tlight",
		"tsuit",
		"trank",
		"blight",
		"bsuit",
		"brank",
		"red",
		"double9",
		"chinese",
		"d6",
		"tshaded"
	)

df_top <- select(
	df,
	light = "tlight",
	suit = "tsuit",
	rank = "trank",
	"card",
	"red",
	shaded = "tshaded"
) |>
	mutate(top = TRUE)
df_bot <- select(
	df,
	light = "blight",
	suit = "bsuit",
	rank = "brank",
	"card",
	"red",
	shaded = "tshaded"
) |>
	mutate(shaded = !shaded, top = FALSE)

card_info <- df
half_info <- bind_rows(df_top, df_bot)

corner_trad1 <- df_top |>
	mutate(
		suit = str_replace(suit, "H", "1"),
		suit = str_replace(suit, "S", "2"),
		suit = str_replace(suit, "C", "3"),
		suit = str_replace(suit, "D", "4"),
		suit = as.integer(suit),
		suit = ifelse(light == "L", suit + 4L, suit),
		rank = str_replace(rank, "0", "10"),
		rank = str_replace(rank, "J", "11"),
		rank = str_replace(rank, "N", "12"),
		rank = str_replace(rank, "Q", "13"),
		rank = str_replace(rank, "K", "14"),
		rank = as.integer(rank),
		cfg = "dotaro_corner_traditional"
	)

corner_trad2 <- filter(df_bot, rank == "N") |>
	mutate(
		suit = str_replace(suit, "H", "1"),
		suit = str_replace(suit, "S", "2"),
		suit = str_replace(suit, "C", "3"),
		suit = str_replace(suit, "D", "4"),
		suit = as.integer(suit),
		suit = ifelse(light == "L", suit + 4L, suit),
		rank = str_replace(rank, "0", "10"),
		rank = str_replace(rank, "J", "11"),
		rank = str_replace(rank, "N", "12"),
		rank = str_replace(rank, "Q", "13"),
		rank = str_replace(rank, "K", "14"),
		rank = as.integer(rank),
		cfg = "dotaro_corner_traditional"
	)

corner_num <- filter(df_bot, rank != "N", !is.na(suit)) |>
	mutate(
		suit = as.integer(suit) + 1L,
		suit = ifelse(light == "L", suit + 5L, suit),
		rank = str_replace(rank, "0", "10"),
		rank = as.integer(rank),
		cfg = "dotaro_corner_number"
	)

corner_fool <- filter(df_bot, is.na(suit)) |>
	mutate(
		suit = 1L,
		suit = ifelse(light == "L", suit + 1L, suit),
		rank = str_replace(rank, "O", "1"),
		rank = str_replace(rank, "F", "2"),
		rank = as.integer(rank),
		cfg = "dotaro_corner_fool"
	)

corner_info <- bind_rows(corner_trad1, corner_trad2, corner_num, corner_fool) |>
	select("card", "top", "suit", "rank", "cfg")

full_info <- mutate(corner_info, cfg = str_replace(cfg, "corner", "full"))

save(card_info, half_info, corner_info, full_info, file = "R/sysdata.rda", version = 3)
