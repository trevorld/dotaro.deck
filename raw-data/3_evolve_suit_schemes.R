# Here we allocate the remaining traditional suit halves and apply
# evolutionary algorithms to try to achieve further pleasing subdeck features
#
# * "shaded tarot" - If flip "shaded" side up do we have numbers 00--49
# * "chinese dominoes" - Each 8 t. suits has 4 cards, each of 6 ranks at least once
# * "d6 dice" - Each of 4 fr. suits has 9 cards
# * "d6 dominoes" - Each of 4 fr. suits has 7 cards, each of 7 ranks at least once
# * "doubles" - Each of 4 fr. suits has 5 cards
library("dplyr")
library("parallel")

# filename <- "raw-data/step_2/candidate_98_77435.csv" # shaded + dom-6 + first 20 with seed 42
# seed <- 42
# filename <- "raw-data/step_2/candidate_101_3774175.csv" # shaded + dom-6 with seed 1234
# seed <- 1234
# filename <- "raw-data/step_2/candidate_103_6422195.csv" # shaded + dom-6 with seed 1942
# seed <- 1942
# filename <- "raw-data/step_2/candidate_102_15642136.csv"
# filename <- "raw-data/step_2/candidate_108_16090053.csv" # shaded + dom-6 + first 20 with seed 42
# seed <- 42
# filename <- "raw-data/step_2/candidate_109_14459887.csv" # shaded + dom-6 with seed 66, 72
# seed <- 1834344
# filename <- "raw-data/step_2/candidate_103_32007877.csv" # shaded + dom-6 + first 20 with seed 12
# seed <- 12
seed2 <- 1111

# filename <- "raw-data/step_2/candidate_9373927_5080362.csv"
filename <- "raw-data/step_2/candidate_42_1030905.csv"
filename <- "raw-data/step_2/candidate_42_6446791.csv"
filename <- "raw-data/step_2/candidate_42_6775971.csv"
filename <- "raw-data/step_2/candidate_84_3635820.csv"
seed <- 5 # Found one that did shaded + dom-6 hard

# seed <- 3210123
# seed2 <- 1111

dft <- tibble::tibble(tlight = rep(c("L", "D"), each = 52),
                      tsuit = rep(rep(c("H", "S", "C", "D"), 13), 2),
                      trank = rep(c(as.character(0:9), "J", "Q", "K"), 8),
                      tred = rep(c("R", "B", "B", "R"), 26),
                      tshaded = c(rep(c(F, F, T, T), 13),
                                  rep(c(T, T, F, F), 13)),
                      tlabel = paste0(tlight, tsuit, " ", trank),
                      tlight_rank = paste0(tlight, trank),
                      tlight_red = paste0(tlight, tred),
                      tlight_suit = paste0(tlight, tsuit),
                      tsuit_rank = paste0(tsuit, trank))
dft <- filter(dft, tsuit_rank != "SQ", tsuit_rank != "DJ")

select_df <- function(dfj) {
	select(dfj, "label", "lrank", "rrank", "nlabel", "needs", "tlabel", "dom9", "dom6", "d6", "chi")
}

build_df <- function(filename) {
    df_raw <- read.csv(filename)

    df <- df_raw |>
        select(-"nsuit") |>
        mutate(tlabel = NA_character_,
               dom9 = grepl("a$", label) & lrank %in% as.character(0:9),
               dom6 = dom9 & lrank %in% as.character(0:6) & rrank %in% as.character(0:6),
               d6 = lrank %in% as.character(1:6) & rrank %in% as.character(1:6) &
                    ((lrank != rrank) | grepl("a$", label))
        )

    df_chi <- filter(df,
                     d6 | label %in% c("1-1b", "2-2b", "3-3b", "4-4b", "5-5b", "6-6b"),
                     !(label %in% c("4-5b", "3-6b", "3-5b", "2-6b", "3-4b", "2-5b", "2-4b", "2-3b", "1-4b", "1-2b")))

    df <- df |> mutate(chi = label %in% df_chi$label)

    for (need in unique(df$needs)) {
        idx <- which(df$needs == need)
        idt <- which(dft$tlight_rank == need)
        df$tlabel[idx] <- sample(dft$tlabel[idt])
    }

    df
}

mod <- as.character(rep(0:4, each = 2L))
dfn <- tibble::tibble(nsuit = rep(mod, 10L),
                      nrank = as.character(rep(0:9, each = 10L)),
                      nlight = rep(c("L", "D"), 50),
                      nlabel = paste0(nlight, nsuit, " ", nrank),
                      nlight_suit = paste0(nlight, nsuit),
                      nsuit_rank = paste0(nsuit, nrank))


# evolve function
# genetic algorithm

# shaded tarot
#   if flip shaded up do we have numbers 00--49
#   note however historical shaded tarot deck doesn't exist...
F_SHADED <- 50L
fitness_shaded <- function(dfj) {
    length(unique(filter(dfj, !tshaded)$nsuit_rank))
}

# F_CHI <- 8L + 48L
# chinese dominoes
#   each of 8 t. suits has 4 cards, each of 6 ranks at least once
F_CHI <- 4L + 24L
fitness_chinese <- function(dfj) {
    dfc <- filter(dfj, chi)
    # tc <- table(dfc$tlight_suit)
    # sc <- union(paste0(dfc$tlight_suit, dfc$lrank),
    #             paste0(dfc$tlight_suit, dfc$rrank))
    # length(tc) - sum(abs(tc - 4)) + length(sc)
    tc <- table(dfc$tsuit)
    sc <- union(paste0(dfc$tsuit, dfc$lrank),
                paste0(dfc$tsuit, dfc$rrank))
    length(tc) - sum(abs(tc - 8)) + length(sc)
    # length(tc) - sum(abs(tc - 4))
}

# d6 dice
#   each of 4 fr. suits has 9 cards
F_D6 <- 4L
fitness_d6 <- function(dfj) {
    td6 <- table(filter(dfj, d6)$tsuit)
    length(td6) - sum(abs(td6 - 9))
}

# d-6 dominoes
#   each of 4 fr. suits has 7 cards, at least one card for each rank
# F_DOM6 <- 4L + 28L
F_DOM6 <- 28L
fitness_dom6 <- function(dfj) {
    df6 <- filter(dfj, dom6)
    tdom6 <- table(df6$tsuit)
    # each French suit has at least one card for each rank
    s6 <- union(paste0(df6$tsuit, df6$lrank),
                paste0(df6$tsuit, df6$rrank))
    # length(tdom6) - sum(abs(tdom6 - 7)) + length(s6)

    # each French suit has two halves for each rank
    tdom6 <- table(c(paste0(df6$tsuit, df6$lrank),
                     paste0(df6$tsuit, df6$rrank)))
	length(which(tdom6 == 2L))
}

# doubles cards
#   each of 4 fr. suits has 5 cards
F_DOUB <- 4L
fitness_doubles <- function(dfj) {
    dfd <- slice(dfj, 1:20)
    td <- table(dfd$tsuit)
    length(td) - sum(abs(td - 5))
}

fitness_1 <- function(df) {
    dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
    # fitness_shaded(dfj) + fitness_chinese(dfj) + fitness_d6(dfj) + fitness_dom6(dfj)
    # fitness_chinese(dfj) + fitness_d6(dfj) + fitness_dom6(dfj)
    # fitness_shaded(dfj)
    # fitness_shaded(dfj) + fitness_chinese(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_d6(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj)
    fitness_shaded(dfj) + fitness_dom6(dfj)
}

# F_1 <- F_SHADED + F_CHI
F_1 <- F_SHADED + F_DOM6

fitness_2 <- function(df) {
    dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_d6(dfj)
    fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj) + fitness_d6(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj) + fitness_doubles(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj) + fitness_d6(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj) + fitness_d6(dfj)
}

# F_2 <- (F_1 + F_CHI)
# F_2 <- (F_1 + F_CHI + F_DOUB)
# F_2 <- (F_1 + F_CHI + F_D6)
# F_2 <- (F_1 + F_DOUB + F_D6)
F_2 <- (F_1 + F_DOUB)

fitness_3 <- function(df) {
    fitness_2(df) + fitness_chinese(df)
}
F_3 <- F_2 + F_CHI

needs <- c("D0", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9",
    "DJ", "DQ", "DK", "L0", "L1", "L2", "L3", "L4", "L5", "L6", "L7",
    "L8", "L9", "LJ", "LQ", "LK")

swap <- function(df) {
    id_df <- which(df$needs == sample(needs, 1L))
    id_t <- sample(id_df)
    df$tlabel[id_t] <- df$tlabel[id_df]
    # id_t <- sample(id_df, 2L)
    # df$tlabel[id_t] <- rev(df$tlabel[id_t])
    df
}


set.seed(seed)
df <- build_df(filename)
dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")

military_suit_labels <- df |>
        mutate(label = substr(label, 1, 3)) |>
        group_by(label) |>
        mutate(chi_n = n(), chi_l = sum(chi)) |>
        ungroup() |>
        filter(chi_n == 2L & chi_l == 1L) |>
        pull(label) |>
        unique()
swap_chi <- function(df) {
    label <- sample(military_suit_labels, 1L)
    i_a <- which(df$label == paste0(label, "a"))
    i_b <- i_a + 1L
    df$chi[i_a] <- !df$chi[i_a]
    df$chi[i_b] <- !df$chi[i_b]
    df
}

evolve <- function(filename) {
    set.seed(seed)
    df <- build_df(filename)
    f <- fitness_1(df)
    # cat(filename, ",", f, "\n",
    #      sep = "", append = TRUE, file = "raw-data2/fitness.txt")
    start_time <- Sys.time()
    while(f < F_1) {
        df_new <- swap(df)
        f_new <- fitness_1(df_new)
        if (f_new > f) {
            cat("fitness: ", f_new, "/", F_1, "\n")
        }
        if (f_new >= f) {
            f <- f_new
            df <- df_new
        }
        if (as.numeric(difftime(Sys.time(), start_time), units = "mins") > 30) return(invisible(NULL))
    }
    # cat(filename, ",", f_new, "\n",
    #     sep = "", append = TRUE, file = "raw-data2/fitness.txt")

    if (FALSE) {
    start_time <- Sys.time()
    f <- fitness_2(df)
    cat("fitness 2: ", f, "/", F_2, "\n")
    set.seed(seed2)
    while(f < F_2) {
        df_new <- swap(df)
        f_new <- fitness_2(df_new)
        if (f_new > f) {
            cat("fitness 2: ", f_new, "/", F_2, "\n")
        }
        if (f_new >= f) {
            f <- f_new
            df <- df_new
        }
        if (as.numeric(difftime(Sys.time(), start_time), units = "mins") > 30) return(invisible(NULL))
    }

    # f <- fitness_3(df)
    # cat("fitness 3: ", f, "/", F_3, "\n")
    # while(f < F_3) {
    #     df_new <- swap_chi(df)
    #     f_new <- fitness_3(df_new)
    #     if (f_new > f) {
    #         cat("fitness 3: ", f_new, "/", F_3, "\n")
    #     }
    #     if (f_new >= f) {
    #         f <- f_new
    #         df <- df_new
    #     }
    #     # if (as.numeric(difftime(Sys.time(), start_time), units = "mins") > 45) return(invisible(NULL))
    # }
    }

    # cat(filename, ",", f_new, "\n",
    #     sep = "", append = TRUE, file = "raw-data2/fitness.txt")
    return(invisible(df))
}

if (TRUE) {
    filenames <- list.files("raw-data/step_2", full.names = TRUE)

    cl <- makeCluster(10L)
    clusterExport(cl, ls())
    clusterEvalQ(cl, {
      library("dplyr")
    })
    l <- parLapply(cl, filenames, evolve) |> 
		Filter(f = Negate(is.null)) |>
		lapply(function(df) {
			left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
		})
	if (length(l)) {
		cat(paste0("Found ", length(l), " candidates\n"))
		dfj <- l[[1L]]
	} else {
		cat("Found zero candidates\n")
	}
} else {
    df <- evolve(filename)
    dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
}

# slice(dfj, 1:20) |> pull(tsuit_rank) |> table() |> as.data.frame()

# write.csv(dfj, "raw-data/alpha_candidate2.csv", row.names = FALSE)
