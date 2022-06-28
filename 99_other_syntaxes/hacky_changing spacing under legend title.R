
TitleMargins = function(plot, Tmargin = unit(0, "mm"), Bmargin = unit(0, "mm")) { 
  library(gtable)
  library(grid)
  
  # Get the plot grob
  g = ggplotGrob(plot)
  
  # Get the legend
  index = which(g$layout$name == "guide-box")
  leg = g$grobs[[index]][[1]][[1]]
  
  # Get the legend title 
  title = leg$grobs[[4]]
  
  # Set up the heights: for the two margins and the original title
  heights <- unit.c(Tmargin, unit(1, "grobheight", title), Bmargin)
  
  # Set up a column of three viewports
  vp <- viewport(layout = grid.layout(3, 1,
                                      heights = heights), name = "vp1")
  
  # The middle row, where the title text will appear, is named as 'child_vp'.
  child_vp <- viewport(layout.pos.row = 2, clip = "off", name = "child_vp")
  
  # Put the title into a gTree containing one grob (the title) and the three viewports
  TitleText <- gTree(children = gList(title),
                     vp = vpTree(vp, vpList(child_vp)))
  
  # Back to the legend: Set height for row 2 of legend to new height of TitleText
  leg$heights[2] = sum(heights)
  
  # Add the new TitleText grob to row 2 of legend
  leg <- gtable_add_grob(leg, TitleText, 
                         t = 2, l = 2, r = 5, name = "TitleText")
  
  # Remove the original title
  leg$grobs <- leg$grobs[-4]
  leg$layout <- leg$layout[-4, ]
  
  # Put the legend back into the plot
  g$grobs[[index]][[1]][[1]] = leg
  
  class(g) =  c("TitleMargins", class(g))
  
  g
  
}

# A print method for the plot
print.TitleMargins <- function(x) {
  grid.newpage()
  grid.draw(x)
}


# Try it out 
# Set your legend title margins
Tmargin = unit(0, "mm")
Bmargin = unit(2.5, "cm")
# Apply the function
TitleMargins(phi_theoret, Tmargin, Bmargin)

