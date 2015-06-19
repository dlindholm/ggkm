################################################################################
#
#
#  ggkm.R
#     - draws survival curves from a survfit object
#     - uses colorspace to supply proper colors
#     - adds a 'number at risk' table below the graph
#
#
################################################################################
#
# Version 0.1
# 2015-06-19
# D. Lindholm
#
# Based upoon the work of Abhijit Dasgupta, Gil Tomas, Dieter Menne
# The original file can be found at: https://github.com/dmenne/dmisc2
#
################################################################################
#
# sfit          object of survfit class
# table         should a number at risk table be drawn?
# returns       if returns = TRUE, then an arrangeGlob object is returned
# xlim          limits of x-axis. If left blank, the upper limit will be the
#               longest time present in the survfit object
# ylim          limits of y axis
# xlabs         label of x axis
# ylabs         label of y axis
# main          main title of plot
# ystratalabs   labels of strata
# timeby        distance between ticks on x axis (which also translates into
#               distance between columns in number at risk table)
#
################################################################################

ggkm <- function(sfit, table = TRUE, returns = FALSE,
                      xlim = c(0, max(sfit$time)), ylim = c(0,1),
                      xlabs = "Time", ylabs = "Cumulative incidence", main = "",
                      ystratalabs = NULL,
                      timeby = 100) {
    require(ggplot2)
    require(colorspace)
    require(scales)
    require(dplyr)
    ystrataname <- NULL
    surv <- NULL
    n.risk <- NULL
    if (is.null(ystratalabs)) {
        ystratalabs <- as.character(levels(summary(sfit)$strata))
    }
    m <- max(nchar(ystratalabs))
    if (is.null(ystrataname))
        ystrataname <- "Strata"
    times <- seq(0, max(sfit$time), by = timeby)
    .df <- data.frame(
        time = sfit$time, n.risk = sfit$n.risk,
        n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,
        upper = sfit$upper, lower = sfit$lower
    )
    levels(.df$strata) <- ystratalabs
    zeros <-
        data.frame(
            time = 0, surv = 1, strata = factor(ystratalabs, levels = levels(.df$strata)),
            upper = 1, lower = 1
        )
    .df <- rbind_list(zeros, .df)
    d <- length(levels(.df$strata))
    col <- rev(rainbow_hcl(d))
    p <- ggplot(.df, aes(time, 1-surv, group = strata)) +
        geom_step(aes(color = strata), size = 0.7) +
        scale_color_manual(values=col) +
        scale_x_continuous(xlabs, breaks = times, limits = xlim) +
        scale_y_continuous(ylabs, limits = ylim, labels = percent_format()) +
        theme_bw() +
        theme(axis.title.x = element_text(vjust = 0.5)) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position = "none") +
        theme(plot.margin = unit(c(0, 1, .5, ifelse(m < 10, 1.5, 2.5)), "lines")) +
        ggtitle(main)

    ## Create a blank plot for place-holding
    ## .df <- data.frame()
    blank.pic <- ggplot(.df, aes(time, surv)) +
        geom_blank() +
        theme_bw() +
        theme(
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.ticks = element_blank(), panel.grid.major = element_blank(),
            panel.border = element_blank()
        )
    if (table) {
        ## Create table graphic to include at-risk numbers
        risk.data <-
            data.frame(
                strata = summary(sfit, times = times, extend = TRUE)$strata,
                time = summary(sfit, times = times, extend = TRUE)$time,
                n.risk = summary(sfit, times = times, extend = TRUE)$n.risk
            )
        data.table <-
            ggplot(risk.data, aes(
                x = time, y = strata, label = format(n.risk, nsmall = 0)
            )) +
            geom_text(size = 3.5, hjust=0.15)+
            theme_bw() +
            scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = ystratalabs) +
            scale_x_continuous("Number at risk", limits = xlim) +
            theme(
                axis.title.x = element_text(size = 10, vjust = 1), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.text.x = element_blank(), axis.ticks = element_blank(),
                axis.text.y = element_text(face = "bold", hjust = 1, color = col)
            )
        data.table <- data.table + theme(legend.position = "none") +
            xlab(NULL) + ylab(NULL)
        data.table <- data.table +
            theme(plot.margin = unit(c(
                -1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.28 * m
            ), "lines"))
        gridExtra::grid.arrange(
            p, blank.pic, data.table,
            clip = FALSE, nrow = 3, ncol = 1,
            heights = unit(c(2, .1, .25),c("null", "null", "null"))
        )
        if (returns) {
            a <- gridExtra::arrangeGrob(
                p, blank.pic, data.table, clip = FALSE,
                nrow = 3, ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null"))
            )
            return(a)
        }
    }
    else {
        print(p)
        if (returns)
            return(p)
    }
}
