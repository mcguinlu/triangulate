#' Plot bias-adjusted and unadjusted estimates with RoB
#'
#' Creates a forest plot of both raw and adjusted effect estimates, alongside domain-level
#' risk of bias annotations for each study.
#'
#' @param dat A dataframe output from `tri_calculate_adjusted_estimates()`, with `yi`, `vi`, `yi_adj`, `vi_adj`, and domain columns.
#' @param title Optional plot title.
#' @param grouping Column by which to stratify subgroups (default = `"type"`).
#' @param dat_adj A data frame with adjusted estimates and variances, if different from the main input.
#' @param rma_method Meta-analysis method passed to metafor::rma (e.g., "REML", "FE"). We use metafor's default which random-effects
#' @param ... Additional arguments passed to `rob_direction()`.
#'
#' @return A forest plot is drawn (base graphics).
#' @export
tri_plot_bias_direction <- function(dat, dat_adj = NULL, title = NULL, grouping = "type", rma_method = "REML", ...) {

  # If adjusted estimates are provided separately, join them
  if (!is.null(dat_adj)) {
    if (!all(c("result_id", "yi_adj", "vi_adj") %in% names(dat_adj))) {
      stop("dat_adj must contain 'result_id', 'yi_adj', and 'vi_adj' columns.")
    }

    dat <- dplyr::left_join(dat, dat_adj[, c("result_id", "yi_adj", "vi_adj")], by = "result_id")
  }
  rob_direction(dat = dat, title = title, grouping = grouping, rma_method = rma_method, ...)
}

# Original function is from R package robvis https://github.com/mcguinlu/robvis/tree/main
# Modified by Chin Yang Shapland on 19/09/24 to visualize estimates, RoB and corrected estimates

# TODO add argument that prevents subgroup summary estimates - in this case, each subgroup will need to be one row closer to each other
# TODO Finish description of function

#' Bias direction plots
#'
#' @keywords internal
#'
#' @param dat Dataframe
#' @param sei Vector containing the corresponding standard errors (normally defined as the column within the dataset, i.e. dat$sei). Note: either vi or sei must be set.
#' @param title Graph title
#' @param legend_cex Expansion factor for figure legend.
#' @param grouping Variable of the provided dataset by which the resulting plot will be stratified. Often will study design or overall risk-of-bias level.
#' @param grouping_levels Ordering of grouping variable. Note: the levels will be plotted
#'   in order, starting at the bottom of the graph (i.e. the last item in the
#'   vector will be placed at the top of the graph)
#' @param label_subgroup_summary Annotation text for subgroup label
#' @param ... Other arguments to pass to metafor::forest
#'
#' @keywords internal

rob_direction <- function(dat,
                          sei = NULL,
                          title = NULL,
                          legend_cex = 0.9,
                          grouping = "type",
                          grouping_levels = c("MR", "NRSI", "Obs", "RCT"),
                          label_subgroup_summary = "RE Model for Subgroup",
                          rma_method = "REML",  # <- here
                          ...) {


  ### calculate log risk ratios and corresponding sampling variances (and use
  ### the 'slab' argument to store study labels as part of the data frame)

  if (("study" %in% colnames(dat)) == FALSE) {
    dat$study <- paste("Study", 1:nrow(dat))
  }

  rob_levels = c("Low","Moderate","High","Critical")

  dat <- dat %>%
    dplyr::mutate(type = factor(type, levels = grouping_levels)) %>%
    dplyr::mutate(overall = factor(overall, levels = rob_levels)) %>%
    dplyr::arrange(type, overall, dplyr::desc(study))

  #dat[is.na(dat)] <- "None"


  # Use this to define the gaps between different groups
  # Will be important when adding argument to prevent subgroup analyses
  offset_n <- 4

  dat_rob_vec <- dat %>%
    dplyr::mutate(row_n = 1:dplyr::n()) %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(n=dplyr::n(),max = max(row_n), min = min(row_n)) %>%
    dplyr::mutate(offset = seq(2,length(unique(.$type))*offset_n,by=offset_n)) %>%
    dplyr::mutate(min = min+offset, max =max+offset, heading = max+1, stats = min-1.25) %>%
    dplyr::mutate(min = ifelse(n==1,min-1,min),
                  max = ifelse(n==1,max-1,max),
                  heading = ifelse(n==1,heading-1,heading))

  if (length(unique(dat$type))==1) {
    dat_rob_vec <- dat_rob_vec %>%
      dplyr::mutate(dplyr::across(c(min, max, heading),~.-1))
  }

  rows <- c()

  for (i in 1:nrow(dat_rob_vec)) {

    rows <-c(rows, dat_rob_vec$min[i]:dat_rob_vec$max[i])

  }

  arg <- list(...)

  if (is.null(arg$at)) {
    x_adj <- log(3)
  } else {
    x_adj <- arg$at[3]
  }

  if (is.null(arg$x_min)) {
    x_min = -5 #changed from -10
  } else {
    x_min <- arg$x_min
  }

  x_max = 4.6 - log(3) + x_adj
  textpos <- c(x_min, x_max-2.2)
  # Dynamic vertical buffer based on number of studies and subgroups
  # More reproducible vertical buffer: ensures top space scales with distance to first heading
  buffer_above <- max(4, ceiling(nrow(dat) * 0.10))  # smaller buffer, but never below 4
  y_max <- max(max(rows) + buffer_above, max(dat_rob_vec$heading) + 2)


  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Deal with adding rob data
  dat_adj <- dat %>%
    dplyr::mutate(dplyr::across(-c(result_id,study,type, yi_adj, vi_adj), clean_data))

  dat <- dat %>%
    dplyr::mutate(dplyr::across(-c(result_id,study,type,yi,vi), clean_data))

  # Combine direction and type
  for (j in paste0("d",1:7)) {
    for (i in 1:nrow(dat)) {
      dat[i,paste0(j,"d")] <- paste0(dat[i,paste0(j,"d")],dat[i,paste0(j,"t")])
    }
  }

  x_pos <- seq(x_max-2, by = 0.35, length.out = 9 - 2)

  x_overall_pos <- max(x_pos) + .5

  # Convenience vector, specifying x-axis positions for all risk of bias columns
  header_row <- c(x_pos, x_overall_pos)

  legend_pos <- x_max+(max(header_row)-min(header_row))/2

  # New right-hand x-axis limit
  new_x_lim <- x_overall_pos + .5

  # Setting colours (changed)
  rob_colours <- c()
  rob_colours$na_colour <- "#cccccc"
  rob_colours$low_colour <- "#02C100"
  rob_colours$concerns_colour <- "#E2DF07"
  rob_colours$high_colour <- "#BF0000"
  rob_colours$critical_colour <- "#820000"
  rob_colours$ni_colour <- "#4EA1F7"

  judgements<-c("Very high risk",  #changed
                "High risk",
                "Moderate risk",
                "Low risk")
  cols <- c(
    c = rob_colours$critical_colour, #changed
    h = rob_colours$high_colour,
    m = rob_colours$concerns_colour,
    l = rob_colours$low_colour,
    n = rob_colours$ni_colour,
    x = "transparent"
  )

  syms <- c(ua = "?",
            up = "?",
            lp = "<",
            rp = ">",
            la = "\U2190",
            ra = "\U2192",
            l = "\U2190",
            r = "\U2192",
            xx = "",
            x = "")

  shapes <- c(c = 15,
              v = 15,
              h = 15,
              m = 15,
              l = 15,
              n = 15,
              x = 15)


  rob_psize = 3
  tsize <- rob_psize * 0.3

  #par(cex=0.9, mai=c(1.5,0.1,0,0.1))
  #par(cex=0.9, mai=c(2, 0.1, 0.5, 0.1))
  par(cex=0.9, mai=c(1.5,0.1,0.6,0.1))



  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Make forest plot

  res_all <- rma(yi, vi, data=dat, method = rma_method)

  #par(fig=c(0.01,0.7,0,1))
  #par(fig = c(0.01, 0.7, 0.01, 0.96))
  par(fig = c(0.01, 0.7, 0.00, 0.94))  # try lowering the top more if needed


  ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
  ### used to specify in which rows the outcomes will be plotted)
  metafor::forest(res_all,
                  #x = dat$yi,
                  #vi = dat$vi,
                  #sei = sei,
                  xlim=c(x_min, new_x_lim),
                  atransf=exp,
                  slab = paste0(" ", dat$study),
                  cex=1.2,
                  ylim=c(-2.5, y_max),
                  rows=rows,
                  textpos = textpos,
                  mlab = "RE Model for All Studies",
                  header="Studies",
                  ...
  )

  ### set font expansion factor (as in forest() above) and use a bold font
  op <- graphics::par(font=2)

  ### switch to bold italic font
  graphics::par(font=2)

  ### add text for the subgroups
  graphics::text(x_min, -2, pos=4, mlabfun(" ", res_all)[[1]], cex = 1.2)


  for (i in 1:nrow(dat_rob_vec)) {

    graphics::text(x_min, dat_rob_vec$heading[i], pos=4, dat_rob_vec$type[i], cex = 1.2)
  }

  ### set par back to the original settings
  graphics::par(op)

  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Add risk of bias data

  headers <- c("D1", "D2", "D3", "D4", "D5", "D6","D7", "O")

  graphics::par(font = 2)
  # Finer-tuned placement of RoB titles
  header_title_y <- y_max - 0.6
  domain_labels_y <- y_max - 1.5

  # Plot titles
  graphics::text(mean(header_row), header_title_y, labels = "Risk of Bias", cex = 1.2)
  graphics::text(header_row, domain_labels_y, labels = headers, cex = 1.2)
  graphics::par(op)

  # Plot domain points
  for (j in 1:length(x_pos)) {
    graphics::points(
      x = rep(x_pos[j], length(rows)),
      y = rows,
      pch = shapes[dat[[paste0("d", j,"j")]]],
      col = scales::alpha(cols[dat[[paste0("d", j,"j")]]],0.6),
      cex = rob_psize
    )
    graphics::text(x_pos[j], rows, syms[dat[[paste0("d", j,"d")]]], cex = tsize)
  }

  graphics::points(
    rep(x_overall_pos, length(rows)),
    rows,
    pch = 15,
    col = scales::alpha(cols[dat[["overall"]]],0.6),
    cex = rob_psize
  )
  # graphics::text(x_overall_pos, rows, syms[dat[["overall"]]], cex = tsize)
  graphics::par(op)

  # #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  #


  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Add sub-group, summary polygons & text

  rma_flexi <- function(x) {
    metafor::rma(yi, vi, subset = (type == x), data = dat, method = rma_method)
  }


  res <- purrr::map(dat_rob_vec$type, rma_flexi)

  if (length(unique(dat$type))>1) {

    ### add summary polygons for the three subgroups
    for (i in 1:nrow(dat_rob_vec)) {

      if (length(res[[i]]$slab) == 1) {
        next
      }
      #graphics::par(font = 1)
      metafor::addpoly(
        res[[i]],
        #fonts = 1,
        row = dat_rob_vec$stats[i],
        cex = 1.2,
        textpos=textpos,
        atransf = exp,
        annotate = F,
        mlab = label_subgroup_summary
      )

      graphics::par(font = 2)
      # Need to add handling of top here
      graphics::text(x_min, dat_rob_vec$stats[i]-1, pos=4, mlabfun(" ", res[[i]])[[1]], cex = 1.2)
      graphics::par(op)

      annotate_poly(res[[i]]$b,
                    res[[i]]$ci.lb,
                    res[[i]]$ci.ub,
                    textpos = textpos,
                    rows = dat_rob_vec$stats[[i]])

    }
  }

  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

  if(!is.null(title)){
    graphics::par(font = 2)
    graphics::text(x_min, y_max, pos=4, bquote(bold(underline(.(title)))), cex = 1.2)
    graphics::par(op)
  }

  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

  graphics::legend(
    legend_pos-3.5,                         #changed
    -4,                                   #changed
    c(judgements),
    pch = c(15,15,15,15,16,50),           #changed
    xjust = 0.5,
    col = c(cols[1:4],"white","white"),   #changed
    xpd = TRUE,
    title = parse(text = "bold(\"Extent of bias\")"),
    title.adj = 0.05,
    cex = legend_cex,
    pt.cex = legend_cex-.1,
    y.intersp = 0.9,                     #changed
    box.col = "white",
  )

  graphics::legend(
    legend_pos-1.5,                    #changed
    -4,                                #changed
    c("\U2190  \U2192  Additive","  <   >   Proportional", "    ?     Unpredictable"),
    xjust = 0.5,
    xpd = TRUE,
    adj = 0.15,
    title = parse(text = "bold(\"Type of bias\")"),
    title.adj = 0.05,
    cex = legend_cex,
    pt.cex = legend_cex,
    y.intersp = 0.9,
    box.col = "white"
  )
  #
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Make forest plot with adjusted estimates

  #par(fig=c(0.6,1,0,1), new=TRUE)
  #par(fig = c(0.6, 1, 0.01, 0.96), new = TRUE)
  par(fig = c(0.6, 1, 0.00, 0.94), new = TRUE)



  res_adj <- rma(yi_adj, vi_adj, data=dat_adj, method = rma_method)

  ### set up forest plot (with 2x2 table counts added; the 'rows' argument is
  ### used to specify in which rows the outcomes will be plotted)
  metafor::forest(res_adj,
                  #x = dat$yi,
                  #vi = dat$vi,
                  #sei = sei,
                  xlim=c(x_min, x_max),
                  atransf=exp,
                  slab = NA,
                  cex=1.2,
                  ylim=c(-2.5, y_max),
                  rows=rows,
                  textpos = c(x_min, x_max),
                  mlab = NA,
                  header=c(" ", "Bias-corrected Est. [95% CI]"),
                  ...
  )

  ### set font expansion factor (as in forest() above) and use a bold font
  op <- graphics::par(font=2)

  graphics::text(x_min-.4, -2, pos=4, mlabfun(" ", res_adj)[[1]], cex = 1.2)

  ### set par back to the original settings
  graphics::par(op)


  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  # Add sub-group, summary polygons & text

  rma_flexi_adj <- function(x) {
    metafor::rma(yi_adj, vi_adj, subset = (type == x), data = dat_adj, method = rma_method)
  }



  res_adj <- purrr::map(dat_rob_vec$type,  rma_flexi_adj)

  if (length(unique(dat$type))>1) {

    ### add summary polygons for the three subgroups
    for (i in 1:nrow(dat_rob_vec)) {

      if (length(res_adj[[i]]$slab) == 1) {
        next
      }
      #graphics::par(font = 1)
      metafor::addpoly(
        res_adj[[i]],
        #fonts = 1,
        row = dat_rob_vec$stats[i],
        cex = 1.2,
        textpos=textpos,
        atransf = exp,
        annotate = F,
        mlab = ""
      )

      graphics::par(font = 2)
      # Need to add handling of top here
      graphics::text(x_min-.4, dat_rob_vec$stats[i]-1, pos=4, mlabfun(" ", res_adj[[i]])[[1]], cex = 1.2)
      graphics::par(op)

      annotate_poly(res_adj[[i]]$b,
                    res_adj[[i]]$ci.lb,
                    res_adj[[i]]$ci.ub,
                    textpos = c(x_min, x_max),
                    rows = dat_rob_vec$stats[[i]])

    }
  }
}
