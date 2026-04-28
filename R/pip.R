xya_pips_dominoes <- function(n_pips, die = FALSE, chinese = FALSE, other_pips = NULL) {
    if (n_pips == 0) {
        return(data.frame(x = numeric(0), y = numeric(0), angle = numeric(0)))
    }
    if (die && !chinese) {
        high <- 0.75
    } else if (chinese && die) {
        if (n_pips == 4) {
            high <- 0.65
        } else if (n_pips == 6) {
            high <- 0.75
        } else {
            high <- 0.68
        }
    } else {
        high <- 0.78
    }
    low <- 1 - high
    if (chinese && !die) {
        low <- 0.2
        if (n_pips == 3 && other_pips < 5) {
            high <- 1.0
        } else if (n_pips == 4 && other_pips == 4) {
            high <- 0.75
        } else {
            high <- 0.84
        }
    }
    if (chinese && die && n_pips == 6) {
        left <- 0.36
    } else if (chinese && !die) {
        left <- 0.30
    } else {
        left <- low
    }
    right <- 1 - left
    mid <- 0.5 * (high + low)
    if (n_pips > 9L) {
       abort(paste0("Don't know pip pattern for ", n_pips, " pips"))
    }
    switch(n_pips,
           if (chinese && !die)
               data.frame(x = 0.5, y = low, angle = 0)
           else
               data.frame(x = 0.5, y = mid, angle = 0), # 1
           if (chinese)
               if (die)
                   data.frame(x = 0.5, y = c(low, high), angle = c(0, 180))
                else
                   data.frame(x = c(left, right), y = low, angle = c(0, 180))
           else
               data.frame(x = c(left, right), y = c(high, low), angle = c(0, 180)), # 2
           data.frame(x = c(left, 0.5, right),
                  y = c(high, mid, low),
                  angle = c(0, 0, 180)), # 3
           if (chinese && !die && other_pips != 4)
               data.frame(x = rep(c(left, right), 2), # 4
                      y = rep(c(low, mid), each=2),
                  angle = rep(c(180, 0), each=2))
           else
               data.frame(x = rep(c(left, right), 2), # 4
                      y = rep(c(low, high), each=2),
                  angle = rep(c(180, 0), each=2)),
           data.frame(x = c(rep(c(left, right), 2), 0.5), # 5
                  y = c(rep(c(low, high), each=2), mid),
                  angle = c(rep(180, 2), rep(0, 3))),
           data.frame(x = rep(c(left, right), 3),
                  y = rep(c(low, mid, high), each=2), # 6
                  angle = c(rep(180, 2), rep(0, 4))),
           data.frame(x = c(rep(c(left, right), 3), 0.5), # 7
                  y = c(rep(c(low, 0.5, high), each=2), 0.50),
                  angle = c(rep(180, 2), rep(0, 5))),
           data.frame(x = c(0.5, rep(c(left, right), 3), 0.5), # 8
                  y = c(low, rep(c(low, 0.5, high), each=2), high),
                  angle = c(rep(180, 3), rep(0, 5))),
           data.frame(x = c(rep(c(left, 0.5, right), 3)), # 9
                  y = c(rep(c(low, 0.5, high), each=3)),
                  angle = c(rep(180, 3), rep(0, 6)))
           )
}

top_pip_grob <- function(...) {
    l <- list(...)
    tsuit_grob <- do.call(top_suit_grob, l)

    n_pips <- as.integer(l$trank)

    xya <- xya_pips_dominoes(n_pips)

    gl <- gList()
    for (i in seq_len(nrow(xya))) {
        vp <- viewport(x = xya$x[i], y = xya$y[i], angle = xya$angle[i])
        gl[[i]] <- grobTree(tsuit_grob, vp = vp)
    }

    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"))
    gp <- gpar(cex = 1.2, lex = 1.2)
    gTree(children = gl, vp = vp, gp = gp)
}

bot_pip_grob <- function(...) {
    l <- list(...)
    # bsuit_grob <- do.call(bot_suit_grob, l)
    if (l$red == "R") {
        col = red_color()
    } else {
        col = black_color()
    }
    if (l$blight == "L") {
        bsuit_grob <- circleGrob(r = unit(0.13, "in"),
                                 gp = gpar(col = col, fill = NA, lwd = 1.1))
    } else {
        bsuit_grob <- circleGrob(r = unit(0.13, "in"),
                                 gp = gpar(col = col, fill = col, lwd = 1.1))
    }

    n_pips <- as.integer(l$brank)

    xya <- xya_pips_dominoes(n_pips)

    gl <- gList()
    for (i in seq_len(nrow(xya))) {
        vp <- viewport(x = xya$x[i], y = xya$y[i], angle = xya$angle[i])
        gl[[i]] <- grobTree(bsuit_grob, vp = vp)
    }

    vp <- viewport(width = unit(PIP_WIDTH, "in"),
                   height = unit(0.5 * PIP_HEIGHT, "in"))
    gp <- gpar(cex = 1.2, lex = 1.2)
    gTree(children = gl, vp = vp, gp = gp)
}
