# source ambition theme
devtools::source_url("https://github.com/stefaniemeliss/ambition_theme/blob/main/ambition_theme.R?raw=TRUE")

# combine to palette
ambition_palette_bright <- c(cyan, coral, teal, purple, orange) # bright palette
ambition_palette_accent <- c(yellow, blue, red)
ambition_palette <- c(coral, teal, purple, orange, blue, red, cyan, yellow) # de-prioritise cyan and yellow

# declare dominant and non-dominant colour in plots
dominant_col <- coral
nondominant_col <- navy



# raincloud plot function
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
    ambition_theme +
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
    ambition_theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab(paste0(ylab)) 
  
}

plot_scatter_jitter <- function(data = df, xvar = x, yvar = y,
                                title = "",
                                ylab = "",
                                xlab = "") {
  
  # compute appropriate jitfer
  
  
  # create rain cloud plot, adapted from: https://z3tt.github.io/Rainclouds/
  plot <- 
    # define variables to plot based on input
    ggplot(data, aes(x = get(xvar), y = get(yvar))) +
    # add points
    geom_point(color = nondominant_col, position = position_jitter(seed = 123)) +
    # add regression line
    stat_smooth(formula = y ~ x, method = "lm", fullrange = T, se = T, alpha=0.2, color = dominant_col) + 
    # add regression coefficient
    ggpubr::stat_cor(method = "pearson", cor.coef.name = "r",
                     p.accuracy = 0.001, r.accuracy = 0.01,
                     geom = "label") +
    # add theme
    ambition_theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab(paste0(ylab)) 
  
}

plot_histogram <- function(data = df, xvar = x,
                           title = "",
                           xlab = "") {
  plot <- 
    # define variables to plot based on input
    ggplot(df, aes(x = get(xvar))) +
    # basic histogram with density
    geom_histogram(aes(y=..density..), color = nondominant_col, fill = nondominant_col) +
    # add density plot
    geom_density(alpha = .2, fill = dominant_col, col = dominant_col) +
    # add mean as vertical line
    geom_vline(aes(xintercept = mean(get(xvar), na.rm = T)),
               color = dominant_col, linetype = "dashed", size = 1) +
    # add theme
    ambition_theme +
    # determine titles
    ggtitle(paste0(title)) + xlab(paste0(xlab)) + ylab("Density") 
}



extract_factorscores <- function(data_in = raw_data,
                                 cfa_model = lavaan_model,
                                 id_var = subject_identifier,
                                 file_out = output_filename # file.path(dir, "01_preproc", "SQF_feat.csv")
){
  
  # Fit Confirmatory Factor Analysis Model 
  cfa_baseline <- lavaan::cfa(cfa_model, ordered = T, data = data_in, missing = "pairwise")
  
  # Predict values of latent variables, i.e., factor scores
  scores <- lavaan::lavPredict(cfa_baseline, newdata = data_in, append.data = T)
  
  # convert lavaan matrix to df
  df_scores <- as.data.frame(scores)
  
  # add S_ID to df_scores
  # note: lavaan handles NA by listwise deletion
  # use case.idx stored in lavaan object to identify with rows of the original data were used to compute CFA
  df_scores[, paste0(id_var)] <- data_in[cfa_baseline@Data@case.idx[[1]], id_var]
  
  # reorder columns
  df_scores <- dplyr::relocate(df_scores, paste(id_var))
  
  # save file
  write.csv(df_scores, file = file_out, row.names = F)
}

create_cfa_plot_longitudinal <- function(model_baseline,
                                         model_posttest,
                                         construct_name,
                                         data,
                                         filename){
  
  # define output
  png(filename = filename, width = 960, height = 300)
  # modify plot
  par(cex.main = 1.8, # size of title
      mfrow = c(1, 2)) # plot as grid
  
  # fit baseline CFA model ORDINAL #
  base <-lavaan::cfa(model_baseline, ordered = T, missing = "pairwise", warn = FALSE, data = stud)
  
  # plot model
  scaling_factor <- 2.5
  
  # generate plot
  p <- mark_sig(semPaths(base, whatLabels="est",
                         thresholds = F,
                         DoNotPlot = T,
                         sizeMan = 6*scaling_factor,
                         sizeMan2 = 1.5*scaling_factor,
                         sizeLat = 5*scaling_factor,
                         sizeLat2 = 3*scaling_factor,
                         nCharNodes = 0,
                         edge.label.cex = .8* scaling_factor,
                         rotation = 4, # exogenuous variables placed on right side 
                         intercepts = F,
                         residuals = F
  ), base)
  # print plot
  plot(p)
  
  # fit posttest CFA model ORDINAL #
  base <-lavaan::cfa(model_posttest, ordered = T, missing = "pairwise", warn = FALSE, data = stud)
  
  # generate plot
  p <- mark_sig(semPaths(base, whatLabels="est",
                         thresholds = F,
                         DoNotPlot = T,
                         sizeMan = 5*scaling_factor,
                         sizeMan2 = 1.5*scaling_factor,
                         sizeLat = 5*scaling_factor,
                         sizeLat2 = 3*scaling_factor,
                         nCharNodes = 0,
                         edge.label.cex = .8* scaling_factor,
                         rotation = 4, # exogenuous variables placed on right side 
                         intercepts = F,
                         residuals = F
  ), base)
  # print plot
  plot(p)
  
  # add title
  title(paste(construct_name), line = -2, adj = 0, outer = TRUE)
  dev.off()
  
}

create_cfa_plot <- function(model,
                            construct_name,
                            data,
                            filename){
  
  # define output
  png(filename = filename, width = 960/2, height = 300)
  # modify plot
  # par(cex.main = 1.8, # size of title
  #     mfrow = c(1, 2)) # plot as grid
  par(cex.main = 1.8) # size of title
  
  # fit baseline CFA model ORDINAL #
  base <-lavaan::cfa(model, ordered = T, missing = "pairwise", warn = FALSE, data = stud)
  
  # plot model
  scaling_factor <- 2.5
  
  # generate plot
  p <- mark_sig(semPaths(base, whatLabels="est",
                         thresholds = F,
                         DoNotPlot = T,
                         sizeMan = 6*scaling_factor,
                         sizeMan2 = 1.5*scaling_factor,
                         sizeLat = 5*scaling_factor,
                         sizeLat2 = 3*scaling_factor,
                         nCharNodes = 0,
                         edge.label.cex = .8* scaling_factor,
                         rotation = 4, # exogenuous variables placed on right side 
                         intercepts = F,
                         residuals = F
  ), base)
  # print plot
  plot(p)
  
  # add title
  title(paste(construct_name), line = -2, adj = 0, outer = TRUE)
  dev.off()
  
}

# function to determine outliers
is_outlier_iqr <- function(x) {
  # +/- 1.5*IQR
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}

