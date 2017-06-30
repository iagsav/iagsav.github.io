imshow = function(img)
  library(jpeg)
  library(grid)

  w <- convertUnit(unit(ncol(img),"pt"), "in", value = TRUE)
  h <- convertUnit(unit(nrow(img),"pt"), "in", value = TRUE)
  
  dev.new(width = w, height = h)    
  grid.raster(img, width = unit(1,"npc"), height = unit(1,"npc"))