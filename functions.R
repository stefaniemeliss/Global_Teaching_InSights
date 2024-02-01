# rain cloud plot

# define variables for plotting
ambition_coral <- "#E94B58"
ambition_charcole <- "#2C3C3B"

# ambition colours 2023 #
navy = "#474C68"
white = "#FFFFFF"
black = "#000000"
cyan = "#14B4E9"
coral = "#E94B58"
yellow = "#FFCC00"
green = "#C1D10F"
teal = "#00987C"
blue = "#006FB7"
purple = "#6D2160"
orange = "#EC642D"
red = "#BF1C1D"

# source: https://rdrr.io/cran/MESS/src/R/colorfunctions.R
col.tint <- function(col, tint=.5) {
  
  if(missing(col))
    stop("a vector of colours is missing")
  
  if (tint<0 | tint>1)
    stop("shade must be between 0 and 1")
  
  mat <- t(col2rgb(col, alpha=TRUE)  +  c(rep(1-tint, 3), 0)*(255-col2rgb(col, alpha=TRUE)))
  rgb(mat, alpha=mat[,4], maxColorValue=255)
}

# define theme for plot
theme <- theme(
  plot.title = element_text(size=14, face="bold", hjust = 0.5),
  axis.title.y = element_text(size=10, face="bold"),
  legend.title = element_blank(),
  legend.position = "bottom"
) + theme_bw()


dominant_col <- coral
nondominant_col <- navy

plot_raincloud <- function(data = df, xvar = x, yvar = y,
                           xlower = NULL,
                           xupper = NULL,
                           ylower = NULL,
                           yupper = NULL,
                           ybreaks = NULL,
                           yintercept = NULL,
                           title = "",
                           ylab = "",
                           xlab = "",
                           note = ""){
  
  # create rain cloud plot, adapted from: https://z3tt.github.io/Rainclouds/
  plot <- 
    # define variables to plot based on input
    ggplot(data, aes(x = get(xvar), y = get(yvar))) +
    # create rain cloud
    ggdist::stat_halfeye(adjust = 1.5, width = .3, .width = 0, justification = -.3, point_colour = NA, fill = nondominant_col) + 
    # create boxplot
    geom_boxplot(width = .1, outlier.shape = NA) + # do not show outlier in boxplot
    # add stat mean + se to boxplt
    stat_summary(fun="mean", geom = "point", col = dominant_col) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .05, col = dominant_col) +    
    # add rain
    ggdist::stat_dots(side = "left", dotsize = 0.1, justification = 1.1, binwidth = .1, col = nondominant_col, fill = nondominant_col) + 
    
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab(paste0(ylab)) +
    labs(caption = note) +
    theme +
    theme(plot.caption = element_text(hjust=0))
  #+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
  # determine coord system + scales
  if (!is.null(ylower) | !is.null(yupper) | !is.null(xlower) | !is.null(xupper)) {
    
    if (!is.null(ylower) & is.null(xlower)) { # only modify y axis
      plot <- plot + coord_cartesian(ylim = c(ylower, yupper))
      
    } else if (!is.null(xlower) & is.null(ylower)) { # only modify x axis
      plot <- plot + coord_cartesian(xlim = c(xlower, xupper))
      
    } else if (!is.null(xlower) & !is.null(ylower)) { # modify both axes
      plot <- plot + coord_cartesian(xlim = c(xlower, xupper), ylim = c(ylower, yupper))
    }
  }
  
  if (!is.null(ybreaks)) {
    plot <- plot + scale_y_continuous(breaks=seq(ylower, yupper, ybreaks))
  }
  
  # add horizontal line
  if (!is.null(yintercept)) {
    plot <- plot + geom_hline(yintercept = yintercept, linetype="dashed")
  }
  
  print(plot)
}


table_desc <- function(data = df, group_var = "group", dep_var = "variable"){
  
  out <- rbind(
    psych::describe(data[, dep_var]), # get descriptives whole sample
    do.call("rbind",psych::describeBy(data[, dep_var], group = data[, group_var])) # get descriptives per group
  )
  # edit output
  out$vars <- NULL
  rownames(out)[1] <- "all"
  out <- round(out, 3)
  # print output
  kbl(out, caption = paste0("Descriptives of variable '", dep_var,"' for whole sample and within each country")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>% 
    htmltools::HTML() %>% 
    print
  cat("\n")
}

plot_scatter <- function(data = df, xvar = x, yvar = y,
                         title = "",
                         ylab = "",
                         xlab = "") {
  # create rain cloud plot, adapted from: https://z3tt.github.io/Rainclouds/
  plot <- 
    # define variables to plot based on input
    ggplot(data, aes(x = get(xvar), y = get(yvar))) +
    # add points
    geom_point(color = nondominant_col) +
    # add regression line
    stat_smooth(formula = y ~ x, method = "lm", fullrange = T, se = T, alpha=0.2, color = dominant_col) + 
    # add regression coefficient
    ggpubr::stat_cor(method = "pearson", cor.coef.name = "r",
                     p.accuracy = 0.001, r.accuracy = 0.01,
                     geom = "label") +
    # add theme
    theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab(paste0(ylab)) 

  

}

