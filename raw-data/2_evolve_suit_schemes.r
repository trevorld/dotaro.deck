library("dplyr")
library("parallel")

# filename <- "tmp/candidate.csv"
filename <- "raw-data/step_1/candidate_342_761823.csv" # shaded + dom-6
# filename <- "raw-data/step_1/candidate_242_1985528.csv"
# filename <- "raw-data/step_1/candidate_242_2590921.csv"
# filename <- "raw-data/step_1/candidate_311_2641571.csv"
# filename <- "raw-data/step_1/candidate_911_2720203.csv"
filename <- "raw-data/step_1/candidate_720_2042613.csv" # shaded + dom-6
# filename <- "raw-data/step_1/candidate_732_2872840.csv"
# filename <- "raw-data/step_1/candidate_732_3012693.csv"
# filename <- "raw-data/step_1/candidate_411_4176423.csv"
filename <- "raw-data/step_1/candidate_98_77435.csv" # shaded + dom-6 + first 20

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

# fitness functions
# shaded tarot
#   if flip shaded up do we have numbers 01--49
#   historical shaded tarot deck doesn't exist
# d-6 dominoes
#   each of 4 fr. suits has 7 cards, each of 7 ranks
# d-9 dominoes
#   each of 5 nu. suits has 11 cards
# double-6 dice
#   each of 4 fr. suits has 9 cards
# doubles cards
#   each of 4 fr. suits has 5 cards
# chinese dominoes
#   each of 8 t. suits has 4 cards, each of 6 ranks at least once
# 3D 1
# 3D 2
# 3D 3
#   each of 5 n. suits has 4 cards: dark/light x red/black

# evolve function
# genetic algorithm

# Best possible is 50
F_SHADED <- 50L
fitness_shaded <- function(dfj) {
    length(unique(filter(dfj, !tshaded)$nsuit_rank))
}

# Best possible is 8 + 48
# F_CHI <- 8L + 48L
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

# Best possible is 4
F_D6 <- 4L
fitness_d6 <- function(dfj) {
    td6 <- table(filter(dfj, d6)$tsuit)
    length(td6) - sum(abs(td6 - 9))
}

# Best possible is 4 + 28
F_DOM6 <- 4L + 28L
fitness_dom6 <- function(dfj) {
    df6 <- filter(dfj, dom6)
    tdom6 <- table(df6$tsuit)
    # each French suit has at least one card for each rank
    s6 <- union(paste0(df6$tsuit, df6$lrank),
                paste0(df6$tsuit, df6$rrank))
    length(tdom6) - sum(abs(tdom6 - 7)) + length(s6)
}

# Best possible is 8 * 10 = 80
# Best possible is 4 * 10 = 40
F_DOM9 <- 8L * 10L
fitness_dom9 <- function(dfj) {

    # each suit has at least one card for each rank
    df9 <- filter(dfj, dom9)
    s9 <- union(paste0(df9$tlight_suit, df9$lrank),
                paste0(df9$tlight_suit, df9$rrank))
    # s9 <- union(paste0(df9$tsuit, df9$lrank),
    #             paste0(df9$tsuit, df9$rrank))
    length(s9)
}

F_DOUB <- 4L
fitness_doubles <- function(dfj) {
    dfd <- slice(dfj, 1:20)
    td <- table(dfd$tsuit)
    length(td) - sum(abs(td - 5))
}

# Best possible is 50 + 8 + 4 + 4
fitness <- function(df) {
    dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
    # fitness_shaded(dfj) + fitness_chinese(dfj) + fitness_d6(dfj) + fitness_dom6(dfj)
    # fitness_chinese(dfj) + fitness_d6(dfj) + fitness_dom6(dfj)
    # fitness_shaded(dfj)
    # fitness_shaded(dfj) + fitness_chinese(dfj)
    # fitness_dom9(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_d6(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj)
    fitness_shaded(dfj) + fitness_dom6(dfj)
}

fitness_2 <- function(df) {
    dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_d6(dfj)
    fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj) + fitness_doubles(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_chinese(dfj) + fitness_d6(dfj)
    # fitness_shaded(dfj) + fitness_dom6(dfj) + fitness_doubles(dfj) + fitness_d6(dfj)
}

# needs <- c("D1", "D2", "L2", "D3", "L3", "L4", "L5", "D5", "L6", "D6",
# "D7", "D8", "D9", "L9", "L0", "D0", "D4", "L1", "L7", "L8", "LJ",
# "DJ", "DQ", "LQ", "DK", "LK")

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

# Target is 66L
# while(f < (48L + 8L)) {
# while(f < (50L + 32L + 56L)) {
# while(f < (50L + 32L)) {
# while(f < (50L + 56L)) {
# while(f < (80L)) {
# while(f < (F_SHADED + F_DOM6 + F_DOUB)) {

set.seed(42)
df <- build_df(filename)
dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")

evolve <- function(filename) {
    set.seed(42)
    df <- build_df(filename)
    f <- fitness(df)
    cat(filename, ",", f, "\n",
         sep = "", append = TRUE, file = "raw-data2/fitness.txt")
    start_time <- Sys.time()
    while(f < (F_SHADED + F_DOM6)) {
    # while(f < (F_SHADED + F_CHI)) {
        df_new <- swap(df)
        f_new <- fitness(df_new)
        if (f_new > f) {
            cat("fitness: ", f_new, "\n")
        }
        if (f_new >= f) {
            f <- f_new
            df <- df_new
        }
        if (as.numeric(difftime(Sys.time(), start_time), units = "mins") > 30) return(invisible(NULL))
    }
    # cat(filename, ",", f_new, "\n",
    #     sep = "", append = TRUE, file = "raw-data2/fitness.txt")

    if (TRUE) {
    f <- fitness_2(df)
    cat("fitness 2: ", f, "\n")
    # set.seed(93)
    # set.seed(428)
    set.seed(1111)
    # set.seed(11811)
    # while(f < (F_SHADED + F_DOM6 + F_CHI)) {
    while(f < (F_SHADED + F_DOM6 + F_DOUB)) {
    # while(f < (F_SHADED + F_DOM6 + F_CHI + F_DOUB)) {
    # while(f < (F_SHADED + F_DOM6 + F_CHI + F_D6)) {
    # while(f < (F_SHADED + F_DOM6 + F_DOUB + F_D6)) {
    # while(f < (86L)) {
        df_new <- swap(df)
        f_new <- fitness_2(df_new)
        if (f_new > f) {
            cat("fitness 2: ", f_new, "\n")
        }
        if (f_new >= f) {
            f <- f_new
            df <- df_new
        }
        if (as.numeric(difftime(Sys.time(), start_time), units = "mins") > 45) return(invisible(NULL))
    }
    }

    # cat(filename, ",", f_new, "\n",
    #     sep = "", append = TRUE, file = "raw-data2/fitness.txt")
    return(invisible(df))
}

df <- evolve(filename)
dfj <- left_join(df, dfn, by = "nlabel") |> left_join(dft, by = "tlabel")

# filenames <- list.files("raw-data2", full.names = TRUE)
# 
# cl <- makeCluster(10L)
# clusterExport(cl, ls())
# clusterEvalQ(cl, {
#   library("dplyr")
# })
# l <- parLapply(cl, filenames, evolve)

# write.csv(dfj, "raw-data/alpha.csv", row.names = FALSE)
