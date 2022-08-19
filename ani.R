
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
  S <- slicc %>% accumulate_by(~Year)


S <- S %>%
  plot_ly(
    x = ~Perc, 
    y = ~DALYs,
    text = ~Entity, 
    hoverinfo = "text",
    split = ~Entity,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines+markers', 
    line = list(simplyfy = F)
  )
S <- S %>% layout(
  xaxis = list(
    title = "Perecentage of policies implemented according to WHO",
    zeroline = F
  ),
  yaxis = list(
    title = "DALYs",
    zeroline = F
  )
) 


S