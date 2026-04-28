# In this file we only look at the subset of files in `step_1`
# where we have unique number suit ranks in both light/dark
# this is helpful for e.g. randomly drawing d100's with the deck
#
# successful candidates get written to `step_2`

library("stringr")
files <- list.files("raw-data/step_1", full.names = TRUE)

if (!dir.exists("raw-data/step_2"))
    dir.create("raw-data/step_2")

for (file in files) {

    df <- read.csv(file, colClasses = "character")

    light <- str_sub(df$nlabel[1:20], 1, 1)
    rank <- str_sub(df$nlabel[1:20], 4, 4)

    tlr <- table(paste0(light, rank))
    l <- length(tlr)
    if (l >= 20) {
        cat(file, ":", l , "\n")
        file2 <- gsub("step_1", "step_2", file)
        file.copy(file, file2)
    }
}
