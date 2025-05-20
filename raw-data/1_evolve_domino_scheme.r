library("dplyr")
library("stringr")
library("tibble")

df_nt <- read.csv("raw-data/trad_ranks_freq.csv") |> arrange(Var1)

df <- read.csv("raw-data/basic_domino_scheme.csv",
               na.strings = "",
               colClasses = "character")


nsuit <- rep(as.character(0:4), each = 2L)
dfn <- tibble::tibble(nsuit = rep(nsuit, 10L),
                      nrank = as.character(rep(0:9, each = 10L)),
                      light = rep(c("L", "D"), 50),
                      nlabel = paste0(light, nsuit, " ", nrank))


df$takes <- vector("list", 100L)
df$nlabel <- NA_character_
df$needs <- NA_character_
for (i in 1:100) {
    if (is.na(df$nsuit[i])) {
        # either a double or `lrank` is a face rank so just `rrank` is fine
        df$takes[[i]] <- filter(dfn, nrank == df$rrank[i]) |> pull(nlabel)
    } else {
        df$takes[[i]] <- filter(dfn,
                                nrank == df$lrank[i] | nrank == df$rrank[i],
                                nsuit == df$nsuit[i]) |> pull(nlabel)
    }
}
names(df$takes) <- df$label

dfn$gives <- vector("list", 100L)
for (i in 1:100) {
    ltakes <- vapply(1:100, \(j) dfn$nlabel[i] %in% df$takes[[j]], FUN.VALUE = logical(1L))
    dfn$gives[[i]] <- df$label[which(ltakes)]
}

names(dfn$gives) <- dfn$nlabel

vn <- dfn$nlabel
vt <- rep.int(df_nt$Var1, df_nt$Freq)
vl <- df$label

to_needs <- function(takes, i, df, dfn) {
    vapply(takes,
           function(nlabel) {
              idn <- which(vn == nlabel)
              needs_rank <- ifelse(df$lrank[i] == dfn$nrank[idn], df$rrank[i], df$lrank[i])
              ifelse(str_detect(nlabel, "^L"),
                     paste0("D", needs_rank),
                     paste0("L", needs_rank))
           }, FUN.VALUE = character(1L))
}

n_needs_left <- function(takes, i, df, dfn, vt) {
    vapply(to_needs(takes, i, df, dfn),
           function(x) {
               length(which(vt == x))
           }, FUN.VALUE = integer(1L))
}

idxs <- c(which(!is.na(df$nsuit)), which(is.na(df$nsuit)))

df0 <- df
vt0 <- vt

if (length(commandArgs(TRUE))) {
    seed <- as.numeric(commandArgs(TRUE))
} else {
    seed <- 1111 # 1111 fastest to give first candidate
}
set.seed(seed) 
iter <- 0L
matches <- FALSE
max_i <- 1
offset <- Inf
while (TRUE) {
    df <- df0
    vt <- vt0
    iter <- iter + 1L
    failed <- FALSE
    offset <- Inf
    # sample weights based on feasible ratios of traditional?
    # recursive function kick back up if fails?
    for (i in idxs) {
        if (!is.na(df$nlabel[i])) next
        needs_left <- n_needs_left(df$takes[[i]], i, df, dfn, vt)
        if (!failed && sum(needs_left) > 0) {
            nlabel <- sample(df$takes[[i]], 1L, prob = needs_left)
        } else {
            # i_in_idx <- which(idxs == i)
            # if (i_in_idx > max_i) {
            #     cat("iteration: ", iter, " i_in_idxs: ", i_in_idx, "\n")
            #     max_i <- i_in_idx
            # }
            break
        }
        idn <- which(vn == nlabel)
        for (l in dfn$gives[[idn]]) {
            idl <- which(df$label == l)
            df$takes[[idl]] <- setdiff(df$takes[[idl]], nlabel)
        }
        df$nlabel[i] <- nlabel
        df$takes[[i]] <- character(0L)
        df$needs[i] <- to_needs(nlabel, i, df, dfn)
        vt <- vt[-which(vt == df$needs[i])[[1L]]]

        # If only one left in a `takes` we know what it must be
        for (j in which(lengths(df$takes) == 1L)) {
            nlabel <- df$takes[[j]]
            if (length(nlabel) == 0L)
                break
            idn <- which(vn == nlabel)
            for (l in dfn$gives[[idn]]) {
                idl <- which(df$label == l)
                df$takes[[idl]] <- setdiff(df$takes[[idl]], nlabel)
            }
            df$nlabel[j] <- nlabel
            df$takes[[j]] <- character(0L)
            df$needs[j] <- to_needs(nlabel, j, df, dfn)
            ivs <- which(vt == df$needs[j])
            if (length(ivs)) {
                vt <- vt[-ivs[[1L]]]
            } else {
                failed <- TRUE
            }
        }
    }
    df_tab <- as.data.frame(table(df$needs)) |> arrange(Var1)
    if (nrow(df_tab) == 26L) {
        offset <- sum(abs(df_tab$Freq - df_nt$Freq))
    }
    if (offset == 0) { # Meets basic needs for dominoes/dice
        if (!dir.exists("raw-data/step_1"))
            dir.create("raw-data/step_1")

        df <- select(df, -"takes")
        f <- paste0("raw-data/step_1/candidate_", seed, "_", iter, ".csv")
        cat("seed:", seed, "iter:", iter, "\n")
        write.csv(df, f, row.names = FALSE)
    }
}
