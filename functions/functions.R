heatplot = function(data, border = "white", border_size = 2, x_lab = "Outcome",
                    primary_outcome = 2){

  
  primary_outcome <- rlang::sym(colnames(data[, primary_outcome]))
  Treatment <- rlang::sym(colnames(data[,1]))
  Treatment <- enquo(Treatment)
  #Arrange treatments according to SUCRA in primary analysis
  data = data %>% arrange(- !!primary_outcome) 

  # Convert to long format, create labels for cells and factor variables in order of appearence (as_factor instead of as.factor). Aaron, I'm sure there is
  # a way to do this that doesn't require forcats? It's a Hadley package so I trust it to be maintained but base would be nicer

  long = data %>% gather(variable, value, !! -Treatment) %>% mutate(value2 = ifelse(!is.na(value),paste(round(value*100,0),"%",sep = ""),
                                                                               paste(round(value*100,0)))) %>% mutate(variable = as_factor(variable))

  #Orders treatments based on sucra. Rev needed because axis is flipped?
  long <- long %>% mutate(!! quo_name(Treatment) := 
                            factor(!! Treatment, levels = rev(unique(!! Treatment))))


  long %>% ggplot(aes(y = !! Treatment, x = variable)) +
    geom_tile(aes(fill = value), colour = "white", size = 2) +
    geom_text(aes(label = value2)) + scale_fill_gradient2(name="Legend\n",
                                                          midpoint = 0.5,
                                                          limits = c(0, 1),
                                                          breaks = 0.5*0:2,
                                                          labels = scales::percent(0.5*0:2),
                                                          na.value = I(rgb(255, 255, 255, maxColorValue=255)),
                                                          low = I(rgb(248, 105, 107, maxColorValue=255)),
                                                          mid = I(rgb(255, 235, 132, maxColorValue=255)),
                                                          high = I(rgb(0, 192, 82, maxColorValue=255))) +
    # move x-axis label to top
    scale_x_discrete(position = "top") + xlab(x_lab) +
    # use a white background, remove borders
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border=element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    theme(legend.title =  element_text(face = "bold", size = 10),
          legend.text = element_text(size = 7.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
}
