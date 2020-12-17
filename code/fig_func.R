# functions for generating a slightly different figure 
# ben.williams@noaa.gov
# 2020-12

# note: Gretchen totally owes me...


plot_data_two_iso_new <- function (isotopes, mix, source, discr, filename, plot_save_pdf, 
                                   plot_save_png, return_obj = FALSE) 
{
  x <- y <- ymin <- ymax <- scolour <- xmin <- xmax <- label <- NULL
  df <- data.frame(x = mix$data_iso[, isotopes[1]], y = mix$data_iso[,isotopes[2]])
  if (length(grep("C", mix$iso_names[isotopes[1]])) == 1) x_label <- expression(paste(delta^13, "C (‰)", sep = ""))
  if (length(grep("N", mix$iso_names[isotopes[1]])) == 1) x_label <- expression(paste(delta^15, "N (‰)",sep = ""))
  if (length(grep("S", mix$iso_names[isotopes[1]])) == 1) x_label <- expression(paste(delta^34, "S (‰)",sep = ""))
  if (length(grep("O", mix$iso_names[isotopes[1]])) == 1) x_label <- expression(paste(delta^18, "O (‰)", sep = ""))
  if (length(grep("SP", mix$iso_names[isotopes[1]])) == 1) y_label <- expression(paste(delta^15, "N-SP (‰)",sep = ""))
  if (length(grep("C", mix$iso_names[isotopes[2]])) == 1) y_label <- expression(paste(delta^13, "C (‰)",sep = ""))
  if (length(grep("N", mix$iso_names[isotopes[2]])) == 1) y_label <- expression(paste(delta^15, "N (‰)", sep = ""))
  if (length(grep("S", mix$iso_names[isotopes[2]])) == 1) y_label <- expression(paste(delta^34, "S (‰)",sep = ""))
  if (length(grep("O", mix$iso_names[isotopes[2]])) == 1) y_label <- expression(paste(delta^18, "O (‰)", sep = ""))
  if (length(grep("SP", mix$iso_names[isotopes[2]])) == 1) y_label <- expression(paste(delta^15, "N-SP (‰)",sep = ""))
  if (!exists("x_label")) x_label <- mix$iso_names[isotopes[1]]
  if (!exists("y_label")) y_label <- mix$iso_names[isotopes[2]]
  
  if (!is.na(source$by_factor)) {
    source_linetype <- sort(rep(1:source$n.sources, source$S_factor_levels))
    source_color <- factor(as.numeric(source$S_factor1))
    index <- seq(from = 1, to = 1 + (source$n.sources - 1) * 
                   source$S_factor_levels, by = source$S_factor_levels)
    discr_mu_plot <- array(NA, dim = c(length(source$S_MU[, 
                                                          1]), mix$n.iso))
    discr_sig2_plot <- array(NA, dim = c(length(source$S_MU[, 
                                                            1]), mix$n.iso))
    for (i in 1:source$n.sources) {
      discr_mu_plot[index[i]:(index[i] + source$S_factor_levels - 
                                1), ] <- matrix(rep(discr$mu[i, ], source$S_factor_levels), 
                                                nrow = source$S_factor_levels, ncol = mix$n.iso, 
                                                byrow = T)
      discr_sig2_plot[index[i]:(index[i] + source$S_factor_levels - 
                                  1), ] <- matrix(rep(discr$sig2[i, ], source$S_factor_levels), 
                                                  nrow = source$S_factor_levels, ncol = mix$n.iso, 
                                                  byrow = T)
    }
  }
  else {
    source_linetype <- 1:source$n.sources
    source_color <- factor(rep("black", source$n.sources))
    index <- 1:source$n.sources
    discr_mu_plot <- discr$mu
    discr_sig2_plot <- discr$sig2
  }
  
  MU_plot <- array(NA, dim = c(length(source$S_MU[, 1]), 2))
  SIG_plot <- array(NA, dim = c(length(source$S_SIG[, 1]), 
                                2))
  for (iso in 1:2) {
    MU_plot[, iso] <- source$S_MU[, isotopes[iso]] + discr_mu_plot[,isotopes[iso]]
    SIG_plot[, iso] <- sqrt(source$S_SIG[, isotopes[iso]]^2 + 
                              discr_sig2_plot[, isotopes[iso]])
  }
  df_sources <- data.frame(x = MU_plot[, 1], y = MU_plot[,2], ymin = MU_plot[, 2] - 
                             SIG_plot[, 2], ymax = MU_plot[,2] + SIG_plot[, 2], xmin = MU_plot[, 1] - SIG_plot[,1], xmax = MU_plot[, 1] + 
                             SIG_plot[, 1], linetype = source_linetype, scolour = source_color)
  source.labels <- data.frame(x = MU_plot[index, 1] - rep(1,source$n.sources), y = MU_plot[index, 2] + 
                                rep(0.75, source$n.sources), label = source$source_names)
  .e <- environment()
  dev.new()
  if (mix$n.effects == 2) {
    shapes <- c(16, 17, 15, 3, 7, 8, 1, 6, 35, 36, 37, 4, 
                18, 14, 11, 9, 13)
    shapes <- shapes[1:mix$FAC[[2]]$levels]
    if (!is.na(source$by_factor)) {
      g <- ggplot2::ggplot(data = df, 
                           ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values), 
                                         shape = factor(mix$FAC[[2]]$values)), 
                            size = 2.5, 
                            show.legend = T) + 
        ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                       labels = mix$FAC[[1]]$labels) + 
        ggplot2::scale_shape_manual(values = shapes,
                                    labels = mix$FAC[[2]]$labels) + 
        ggplot2::geom_pointrange(data = df_sources,
                                 ggplot2::aes(ymin = ymin, ymax = ymax, colour = scolour),
                                 show.legend = F) + 
        ggplot2::geom_errorbarh(data = df_sources, 
                                ggplot2::aes(xmin = xmin, xmax = xmax, colour = scolour), 
                                height = 0, show.legend = F) + 
        ggplot2::geom_text(data = source.labels, 
                           ggplot2::aes(x = x,  y = y, label = label), show.legend = F) + 
        ggplot2::ylab(y_label) + 
        ggplot2::xlab(x_label) + 
        ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = c(0, 1), legend.justification = c(0, 
                                                                           1), legend.title = ggplot2::element_blank())
      print(g)
    }
    else {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, 
                                                   y = y), environment = .e) + ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values), 
                                                                                                                shape = factor(mix$FAC[[2]]$values)), size = 2.5, 
                                                                                                   show.legend = T) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)), 
                                                                                                                                                     labels = mix$FAC[[1]]$labels) + ggplot2::scale_shape_manual(values = shapes, 
                                                                                                                                                                                                                 labels = mix$FAC[[2]]$labels) + ggplot2::geom_pointrange(data = df_sources, 
                                                                                                                                                                                                                                                                          ggplot2::aes(ymin = ymin, ymax = ymax), size = 1, 
                                                                                                                                                                                                                                                                          linetype = source_linetype, show.legend = F) + 
        ggplot2::geom_errorbarh(data = df_sources, ggplot2::aes(xmin = xmin, 
                                                                xmax = xmax), size = 1, height = 0, linetype = source_linetype, 
                                show.legend = F) + ggplot2::geom_text(data = source.labels, 
                                                                      ggplot2::aes(x = x, y = y, label = label), show.legend = F) + 
        ggplot2::ylab(y_label) + ggplot2::xlab(x_label) + 
        ggplot2::theme_bw() + ggplot2::theme(legend.position = c(0, 
                                                                 1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank())
      print(g)
    }
  }
  if (mix$n.effects == 1) {
    if (!is.na(source$by_factor)) {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, 
                                                   y = y), environment = .e) + ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values)), 
                                                                                                   show.legend = T) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)), 
                                                                                                                                                     labels = mix$FAC[[1]]$labels) + ggplot2::geom_pointrange(data = df_sources, 
                                                                                                                                                                                                              ggplot2::aes(ymin = ymin, ymax = ymax, colour = scolour), 
                                                                                                                                                                                                              size = 1, linetype = source_linetype, show.legend = F) + 
        ggplot2::geom_errorbarh(data = df_sources, ggplot2::aes(xmin = xmin, 
                                                                xmax = xmax, colour = scolour), size = 1, height = 0, 
                                linetype = source_linetype, show.legend = F) + 
        ggplot2::geom_text(data = source.labels, ggplot2::aes(x = x, 
                                                              y = y, label = label), show.legend = F) + ggplot2::ylab(y_label) + 
        ggplot2::xlab(x_label) + ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = c(0, 1), legend.justification = c(0, 
                                                                           1), legend.title = ggplot2::element_blank())
      print(g)
    }
    else {
      g <- ggplot2::ggplot(data = df,ggplot2::aes(x = x,y = y)) +
        ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values)), size = 2) +  # Factor.1
        scico::scale_colour_scico_d(breaks = levels(factor(mix$FAC[[1]]$values)),  # Factor.1
                                    labels = c("2015", "2016", "2017", "2018", "2020"),
                                    palette = "roma") +    # factor1_names
        ggplot2::geom_pointrange(data=df_sources,
                                 ggplot2::aes(ymin=ymin,ymax=ymax),
                                 size=1,
                                 linetype=1,
                                 show.legend=F,
                                 fatten = 3,
                                 color = "darkgray") +
        ggplot2::geom_errorbarh(data=df_sources,
                                ggplot2::aes(xmin=xmin,xmax=xmax),
                                size=1,
                                height=0,
                                linetype=1,
                                show.legend=F,
                                color = "darkgray") +
        ggplot2::geom_text(data=source.labels, ggplot2::aes(x=x,y=y,label=label), show.legend=F, family = "sans") +
        ggplot2::ylab(y_label) +
        ggplot2::xlab(x_label) +
        funcr::theme_report(base_family = "sans") +
        ggplot2::theme(legend.position=c(0.1,0.9), 
                       legend.justification=c(0,1), 
                       legend.title=ggplot2::element_blank())
      g
    }
  }
  if (mix$n.effects == 0) {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) + 
      ggplot2::geom_point() + ggplot2::geom_pointrange(data = df_sources, 
                                                       ggplot2::aes(ymin = ymin, ymax = ymax), size = 1, 
                                                       linetype = source_linetype, show.legend = F) + ggplot2::geom_errorbarh(data = df_sources, 
                                                                                                                              ggplot2::aes(xmin = xmin, xmax = xmax), size = 1, 
                                                                                                                              height = 0, linetype = source_linetype, show.legend = F) + 
      ggplot2::geom_text(data = source.labels, ggplot2::aes(x = x, 
                                                            y = y, label = label), show.legend = F) + ggplot2::ylab(y_label) + 
      ggplot2::xlab(x_label) + ggplot2::theme_bw()
    print(g)
  }
  if (plot_save_pdf == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, 
                              "_", isotopes[1], "_", isotopes[2], ".pdf", 
                              sep = ""))
    cairo_pdf(filename = mypath, width = 7, height = 7)
    print(g)
    dev.off()
  }
  if (plot_save_png == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, 
                              "_", isotopes[1], "_", isotopes[2], ".png", 
                              sep = ""))
    png(filename = mypath)
    print(g)
    dev.off()
  }
  if (return_obj == TRUE) 
    return(g)
}
plot_data_new <- function (filename, plot_save_pdf, plot_save_png, mix, source, 
                           discr, return_obj = FALSE) 
{
  if (!identical(rownames(discr$mu), source$source_names)) {
    stop(paste("*** Error: Source names do not match in source and discr\n    data files. Please check your source and discr data file row names.", 
               sep = ""))
  }
  if (!identical(rownames(discr$sig2), source$source_names)) {
    stop(paste("*** Error: Source names do not match in source and discr\n    data files. Please check your source and discr data file row names.", 
               sep = ""))
  }
  if (mix$n.iso == 1) {
    plot_data_one_iso(mix, source, discr, filename, plot_save_pdf, 
                      plot_save_png, return_obj)
    if (return_obj == TRUE) {
      g = plot_data_one_iso(mix, source, discr, filename, 
                            plot_save_pdf, plot_save_png, return_obj = return_obj)
    }
  }
  else {
    for (iso1 in 1:(mix$n.iso - 1)) {
      for (iso2 in (iso1 + 1):mix$n.iso) {
        plot_data_two_iso_new(isotopes = c(iso1, iso2), mix = mix, 
                              source = source, discr = discr, filename = filename, 
                              plot_save_pdf = plot_save_pdf, plot_save_png = plot_save_png, 
                              return_obj = return_obj)
        if (return_obj == TRUE) {
          g = plot_data_two_iso_new(isotopes = c(iso1, iso2), 
                                    mix = mix, source = source, discr = discr, 
                                    filename = filename, plot_save_pdf = plot_save_pdf, 
                                    plot_save_png = plot_save_png, return_obj = return_obj)
        }
      }
    }
  }
  if (return_obj == TRUE) {
    return(g)
  }
}


