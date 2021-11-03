
plot_group_density <- function(df, group, y, title = "Distributions within Groups"){
  # plot some density things for visualizing data in groups
  # group, and y are strings
  
  p <- df %>% 
    ggplot(aes(.data[[group]],.data[[y]])) +
    geom_violin(aes(fill=.data[[group]]), alpha=0.2, draw_quantiles = c(.25, .5, .75), scale = "count") +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size=3, color="red") +
    ggtitle(title)
  
  return(p)
}