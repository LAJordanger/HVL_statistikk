#' The plot function
#'
#' This function creates the plots to be used in the interactive investigation.
#'
#' @param .input The main argument, which is the \code{input}-list from
#'     the internal body of the \code{shiny}-application.
#'
#' @return A plot in accordance with the nodes detected in
#'     \code{.input}.
#'
#' @export


## .input <- list(
##     distribution = "binomial",
##     n = 23,
##     p = 0.35,
##     plot_type = "pdf")  ##  Alternative "cdf"

the_plot <- function(.input) {
    ##  Add code to convert '.input' from reactive to ordinary?

    ##  Initial test: Avodid errors (during initiation) when used
    ##  in the interactive shiny-environment.
    if (is.null(.input$distribution))
        return(invisible(NULL))
    
    ##  Later on, in order to accept specifications of values from the
    ##  input, it might be necessary to add an additional argument to
    ##  this function that updates a copy of 'distribution_defaults'.
    ##  The ugly ad hoc code below is included in order to prepare for
    ##  that.

    .distributional_defaults <- distribution_details

    ##  Identify the plot-components to show, and store the results as
    ##  logical values.
    
    .plot_components <- list(
        normal_approximation = identical(.input$normal_approximation, "on"),
        mean_pm_sd = identical(.input$mean_pm_sd, "on"),
        show_intervals_areals = identical(.input$show_intervals_areals, "on"))

    
    ##  Code for density-plot.

    ## ##  Identify the function to use.
    ## .pdf <- switch(
    ##     EXPR = .input$distribution,
    ##     binomial = dbinom,
    ##     normal = dnorm,
    ##     t = dt,
    ##     chi.squared = dchisq)

    ## ##  Would it be better to write this out explicitly?

    

    ##  I suppose I might want to create the 'aes' to be used based on
    ##  the selected input.
    
    
    ##  Different strategies for discrete and continuous
    ##  distributions...

    
    
    if (.input$distribution == "binomial") {
        ##  Identify the values for 'n' and 'p' based on the indices
        ##  delivered for the cases of interest.
        .par <- .distributional_defaults[[c(.input$distribution, "par")]]
        .n <- .par$n[.input$n]
        .p <- .par$p[.input$p]
        rm(.par)
        
        .x <- 0:.n
        .pdf <- dbinom(
            x = .x,
            size = .n,
            prob = .p)
        df <- data.frame(
            x = .x,
            pdf = .pdf,
            cdf = cumsum(.pdf))
        rm(.x,  .pdf)
        ##  Additional details.
        .mean <- .n * .p
        .sd <- sqrt(.n * .p * (1 - .p))
        .t <- seq(from = 0,
                  to = .n,
                  length.out = 251)
        .normal_approx_df <- data.frame(
            x = .t,
            pdf = dnorm(
                x = .t,
                mean = .mean,
                sd = .sd),
            cdf = pnorm(
                q = .t,
                mean = .mean,
                sd = .sd))
        rm(.t)

        ##  Select the aesthetics for the desired type of plot.
        .aes <- eval(
            bquote(aes(x = x,
                       y = .(as.symbol(.input$plot_type)))))
        ##  Create the plot
        
        ..p <- ggplot(
            data = df,
            mapping = .aes) +
            stat_summary(
                fun.y = identity,
                geom = 'bar',
                colour = "black",
                fill = "white",
                lwd = 0.2,
                alpha = 0.5)
        ###  Lines at the end, not to be included later on.
        ##   +
        ## geom_vline(xintercept = c(0, .n),
        ##            lty = 2,
        ##            col = "magenta",
        ##            alpha = 0.75)

        ##  Add lines for "mean" and "mean pluss/minus standard
        ##  deviation" when required.
        if (.plot_components$mean_pm_sd)
            ..p <- ..p +
                geom_vline(
                    xintercept = .mean + c(-1, 0, 1)*.sd,
                    colour = c("blue", "magenta", "blue"),
                    lwd = 0.5,
                    lty = 2)
        ##  Add normal-approximation when required.
        if (.plot_components$normal_approximation)
            ..p <- ..p +
                geom_line(
                    mapping = .aes,
                    data = .normal_approx_df,
                    colour = "brown",
                    alpha = 0.5,
                    lty = 1)

        ##  Add description of the distribution at hand to the plot,
        ##  i.e. plot-type (pdf/cdf), the name of the distribution,
        ##  the parameters and so on.  I guess this also should be
        ##  available as an attribute of the final plot, in order for
        ##  a description to be available later on.

        ##  Add a (very simple) title.
        ..p <- ..p +
            ggtitle(label = sprintf("bin(n,p), with n=%s and p=%s", .n, .p))
        
        
    }

    
    if (.input$distribution == "normal") {
        ##  Identify the values for 'n' and 'p' based on the indices
        ##  delivered for the cases of interest.
        .par <- .distributional_defaults[[c(.input$distribution, "par")]]
        .mean <- .par$mean[.input$mean]
        .sd <- .par$sd[.input$sd]
        ##  Use the value of '.input$scaling' to detect if adjustments
        ##  are to be used for the 'x' and 'y'-axes.
        .adjust_x <- .input$scaling %in% c("only x", "both x and y")
        .adjust_y <- .input$scaling %in% c("only y", "both x and y")
        ##  Identify the desired range for the x-axis.  Find suitable
        ##  values based on the standard normal distribution, and a
        ##  value of '.alpha' that is lower than those that can be
        ##  investigated later on.
        .alpha <- 0.0001
        .x_range <-
            if (.adjust_x) {
                c(min(.par$mean) -  max(.par$sd) * qnorm(p = 1-.alpha),
                  max(.par$mean) +  max(.par$sd) * qnorm(p = 1-.alpha))
        } else         
            .mean + c(-1,1) * .sd * qnorm(p = 1-.alpha)
        ##  Identify the desired range for the y-axis, note that this
        ##  (for the case of the normal distribution) only depends on
        ##  the shape parameter '.sd'
        .y_range <-
            if (.adjust_y) {
                c(0, 1/(sqrt(2*pi)*min(.par$sd)))
            } else
                c(0, 1/(sqrt(2*pi)*.sd))
        ##  Create the data-frame to be used by ggplot.  Note that the
        ##  width of the interval is based on the desire for the
        ##  inclusion of some alpha-quantiles.
        .upper_lower <- .mean + c(-1,1) * .sd * qnorm(p = 1-.alpha)
        ##  Reminder: Use an odd number of points to ensure that the
        ##  mean is included as one of the values to be plotted.
        .t <- seq(from = .upper_lower[1],
                  to   = .upper_lower[2],
                  length.out = 501)
        ##  When required, add some extra points to get a plot that
        ##  covers all of the shown x-axis.  Reminder: The
        ##  restrictions are done in order to avoid having the same
        ##  points included twice.
        if (.adjust_x) {
            if (.x_range[1] < head(x = .t, n = 1))
                .t <- c(
                    head(x = seq(from = .x_range[1],
                                 to = head(x = .t, n = 1),
                                 length.out = 10),
                         n = -1),
                        .t)
            if (.x_range[2] > tail(x = .t, n= 1))
                .t <- c(.t,
                        tail(x = seq(from = tail(x = .t, n= 1),
                                     to = .x_range[2],
                                     length.out = 10),
                             n = -1))
        }
        rm(.par, .alpha, .adjust_x, .x_range)
        df <- data.frame(
            x = .t,
            pdf = dnorm(
                x = .t,
                mean = .mean,
                sd = .sd),
            cdf = pnorm(
                q = .t,
                mean = .mean,
                sd = .sd))
        rm(.t)
        
        ##  Select the aesthetics for the desired type of plot.
        .aes <- eval(
            bquote(aes(x = x,
                       y = .(as.symbol(.input$plot_type)))))
        ##  Create the plot
        ..p <- ggplot(data = df,
                      mapping = .aes) +
            geom_path(lwd = .5)
        ##  Add lines for "mean" and "mean pluss/minus standard
        ##  deviation" when required.
        if (.plot_components$mean_pm_sd)
            ..p <- ..p +
                geom_vline(
                    xintercept = .mean + c(-1, 0, 1)*.sd,
                    colour = c("blue", "magenta", "blue"),
                    lwd = 0.5,
                    lty = 2)

        ##  Add description of the distribution at hand to the plot,
        ##  i.e. plot-type (pdf/cdf), the name of the distribution,
        ##  the parameters and so on.  I guess this also should be
        ##  available as an attribute of the final plot, in order for
        ##  a description to be available later on.

        ##  Reminder: Use the 'eval'+'bquote'+'.()' construction to
        ##  create the 'expression' needed for the title.
        .the_title <- eval(bquote(
            expression(N(mu,sigma)~-~with~mu==.(.mean)~and~sigma==.(.sd))))
        
        ..p <- ..p +
            ggtitle(label = .the_title) +
            theme(plot.title = element_text(size = 22,
                                            hjust = 0.5))


        ##  How to get the desired plots showing regions.  First of
        ##  all, it might for the time being be sufficient to ignore
        ##  the complementary setting, so it boils down to finding
        ##  from a combination of alpha and side_type what part of the
        ##  data-frame that should be selected.

        ##  Update the plot with the desired .y_range (when required).
        if (.input$plot_type == "pdf") {
            ..p <- ..p +  
                ylim(.y_range)
        }
        
        
        if (.plot_components$show_intervals_areals) {
            ##  Add information that shows intervals and areals.  The
            ##  desired region for the probability density function is
            ##  created with 'geom_area' and a subset of the
            ##  data-frame which originaly was used for the plotting
            ##  of the graph.  The plot for the cumulative density
            ##  function must instead be based on lines at the y-axis.
            alpha_value <- as.numeric(.input$alpha_quantile)
            .alpha_label <- eval(bquote(expression(
                .(100*(1-alpha_value))~'%')))
            if (.input$plot_type == "pdf") {
                .include <- switch(
                    EXPR = .input$side_type,
                    lower = {
                        df$x <= .mean + qnorm(p= 1-alpha_value) * .sd},
                    upper = {
                        df$x >= .mean - qnorm(p= 1-alpha_value) * .sd},
                    two_sided = local({
                        .z_alpha_half <- qnorm(p= 1-alpha_value/2)
                        ## Use product and 'as.logical' to produce the
                        ## logical operation "AND".
                        as.logical({df$x <= .mean + .z_alpha_half * .sd} *
                                   {df$x >= .mean - .z_alpha_half * .sd})
                    }))

                ##  Update the plot, with the desired .y_range
                ..p <- ..p +
                    geom_area(data=df[.include,],
                              col = "cyan",
                              lwd = 0.1,
                              fill = "cyan",
                              alpha = 0.5) +
                    annotation_custom(
                        grob = grid::textGrob(
                                         label = .alpha_label,
                                         gp = grid::gpar(
                                                        col = "brown",
                                                        fontsize = 50)),
                        xmin = .upper_lower[1],
                        xmax = .upper_lower[2],
                        ymin = 0,
                        ymax = max(df[,.input$plot_type])/4)
            }

            if (.input$plot_type == "cdf") {
                ##  Add lines to highlight the points of interest, the
                ##  configuration of the lines will differ depending
                ##  on the 'side_type'.
                .y_range <- switch(
                    EXPR = .input$side_type,
                    lower = c(0, 1 - alpha_value),
                    upper = c(alpha_value, 1),
                    two_sided = c(0,1) + c(1, -1) * alpha_value / 2)
                .x_range <- qnorm(
                    p = .y_range,
                    mean = .mean,
                    sd = .sd)
                ##  Add lines and describing text to the plot.
                ..p <- ..p  +
                    geom_segment( ##  x-axis
                        x = .x_range[1],
                        y = 0,
                        xend = .x_range[2],
                        yend = 0,
                        colour = "blue",
                        lwd = 1) +
                    geom_segment( ##  y-axis
                        x = -Inf,
                        y = .y_range[1],
                        xend = -Inf,
                        yend = .y_range[2],
                        colour = "brown",
                        lwd = 2) +
                    geom_segment( 
                        x = -Inf,
                        y = .y_range[1],
                        xend = .x_range[1],
                        yend = .y_range[1],
                        lwd = 0.05,
                        lty = 1,
                        colour = "brown") +
                    geom_segment( 
                        x = -Inf,
                        y = .y_range[2],
                        xend = .x_range[2],
                        yend = .y_range[2],
                        lwd = 0.05,
                        lty = 1,
                        colour = "brown") +
                    geom_segment( 
                        x = .x_range[1],
                        y = 0,
                        xend = .x_range[1],
                        yend = .y_range[1],
                        lwd = 0.05,
                        lty = 1,
                        colour = "blue") +
                    geom_segment( 
                        x = .x_range[2],
                        y = 0,
                        xend = .x_range[2],
                        yend = .y_range[2],
                        lwd = 0.05,
                        lty = 1,
                        colour = "blue") +
                    annotation_custom(
                        grob = grid::textGrob(
                                         label = .alpha_label,
                                         gp = grid::gpar(
                                                        col = "brown",
                                                        fontsize = 50)),
                        xmin = min(df$x),
                        xmax = min(df$x) + diff(range(df$x)/4),
                        ymin = .y_range[1],
                        ymax = .y_range[2])
                
            }

            
            
        }
        
        
    }
    
    ##  Placeholder
    
    if (! .input$distribution %in% c("binomial", "normal")) 
        stop("Not implemented yet...")
    


    
    
    return(..p)
    
    
}
