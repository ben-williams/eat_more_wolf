library(tidyverse)
library(funcr)
library(scico)
library(MixSIAR)
source("code/fig_func.R")

# Load the mixture/consumer data
mix <- load_mix_data(filename="data/wolf_mix_hair_2.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("bioyear"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)


# Load the source data
source <- load_source_data(filename="data/wolf_source2.csv",
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)


# Load the discrimination/TDF data
discr <- load_discr_data(filename= "data/wolf_discrimination.csv", mix)

# Make an isospace plot
plot_data(filename="isospace_plot_test1", plot_save_pdf=TRUE, plot_save_png=TRUE, mix,source,discr)

g = plot_data(filename="isospace_plot", plot_save_pdf=F, plot_save_png=F, mix,source,discr, return_obj=T)

# now modify plot using ggplot2 commands
g

g + 
  ggplot2::geom_text(data=source.labels, ggplot2::aes(x=x,y=y,label=label), show.legend=F, family = "sans") +
  funcr::theme_report(base_family = "sans") +
  ggplot2::theme(legend.position=c(0.1,0.9), 
                 legend.justification=c(0,1), 
                 legend.title=ggplot2::element_blank())

g + theme(panel.border = element_blank(), panel.background = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title=element_text(size=20), axis.text=element_text(size=14),
          legend.text=element_text(size=12))


ggsave("isoplot.png", dpi = 300, width = 6.5, height = 6.5, units = "in")

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#plot_prior(alpha.prior=1,source)
#informative prior
kw.alpha <- c(0.13, 0.87) # from fecal samples results
kw.alpha <- kw.alpha*length(kw.alpha)/sum(kw.alpha)
kw.alpha[which(kw.alpha==0)] <- 0.01
plot_prior(alpha.prior=kw.alpha,source=source,plot_save_pdf=TRUE,
           plot_save_png=FALSE,filename="prior_plot")

# Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

#Good idea to use run = "test" first to check if 1) the data are loaded correctly and 2) the model is specified correctly:
jags.1 <- run_model(run="test", mix, source, discr, model_filename, 
                    alpha.prior = 1, resid_err, process_err)
#After a test run works, increase the MCMC run to a value that may converge
jags.1 <- run_model(run="long", mix, source, discr, model_filename, 
alpha.prior = 1, resid_err, process_err)
output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)
#Then you can call output_JAGS to process diagnostics, summary statistics, and create posterior density plots:
output_JAGS(jags.1, mix, source, output_options)


# figure  ----
# code just for this figure
plot_data_new(filename="isospace_plot", plot_save_pdf=F, plot_save_png=F, mix, source, discr, return_obj=T)






