### Sigmoid
### December 2022
### adam.milton.morgan@gmail.com


my.sigmoid <- function(x = seq(-10,10,length.out = 1000),
                    right.asymptote = 1,
                    left.asymptote = 0,
                    x1 = 0,
                    y1 = .5,
                    x2 = 2,
                    y2 = .99,
                    plot.on = FALSE){
  
  ## Returns a symetrical sigmoid:
  ## - asymptotes at right.asymptote and left.asymptote
  ## - passes thru 2 points: (x0,y0) and (x1,y1) (defines rate of increase and horizontal shift)
  
  ## Solve for x.shift and rate:
  range <- range(c(right.asymptote, left.asymptote))
  range.diff <- range[2] - range[1]
  # Fist, scale the y values to range 0,1
  y1.scaled <- (y1 - range[1]) / range.diff
  y2.scaled <- (y2 - range[1]) / range.diff
  #
  rate <- (log((1/y1.scaled) - 1) - log((1/y2.scaled) - 1)) /  (x2 - x1)
  x.shift <- (log((1/y1.scaled) - 1) / rate) + x1
# x.shift <- (log((1/y2) - 1) / rate) + x2
  
  if(left.asymptote > right.asymptote){
    y1.scaled <- 1 - y1.scaled
    y2.scaled <- 1 - y2.scaled
  }
  
  out <- 1/(1+exp(-(x - x.shift) * rate))
  
  if(left.asymptote > right.asymptote){
    out <- 1 - out
  }
  
  # Scale
  out <- out * range.diff
  # Shift vertically
  out <- out + range[1]
  
  ## Plot?
  if(plot.on){
    x.range <- range(c(x1,x2))
    x.range.diff <- x.range[2] - x.range[1]
    plot(x = x,
         y = out,
         type = 'l')
    abline(h = c(y1,y2), lty = 2, col = c('blue','pink'))
    abline(v = c(x1,x2), lty = 2, col = c('blue','pink'))  
  }
  
  return(out)
}

# my.sigmoid(left.asymptote = -2,
#            right.asymptote = 1,
#            x1 = 0, y1 = .2,
#            x2 = 1, y2 = .9,
#            plot.on = TRUE)
# 
# my.sigmoid(left.asymptote = -5,
#            right.asymptote = -2,
#            y1 = -4.5,
#            y2 = -3.5,
#            plot.on = TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# z = seq(-10,10,length.out = 1000)
# up = 1
# down = -1
# .x1 = 0
# .y1 = mean(c(up, down))
# .x2 = .4
# .y2 = .4
# my.sigmoid(x = z,
#            right.asymptote = up,
#            left.asymptote = down,
#            x1 = .x1,
#            y1 = .y1,
#            x2 = .x2,
#            y2 = .y2,
#            plot.on = TRUE)
# 
# plot(x = z,
#      y = my.sigmoid(x = z,
#                     right.asymptote = up,
#                     left.asymptote = down,
#                     x1 = .x1,
#                     y1 = .y1,
#                     x2 = .x2,
#                     y2 = .y2),
#      type = 'l')
# abline(h = c(.y1,.y2), lty = 2, col = c('blue','pink'))
# abline(v = c(.x1,.x2), lty = 2, col = c('blue','pink'))  
# 
# 
# 
# 
# y = 1 / (1 + exp(-((x-m)*r)))
# 1 = y * (1 + exp(-((x-m)*r)))
# (1/y) - 1 = exp(-((x-m)*r))
# log((1/y) - 1) = -((x-m)*r)
# -log((1/y) - 1) / (x-m) = r
# -log((1/y) - 1) / r = x-m
# (log((1/y) - 1) / r) + x = m
# 
# # Plug in x1,y1 and x2,y2 and solve for m and r
# -log((1/y1) - 1) / (x1-m) = r
# -log((1/y2) - 1) / (x2-m) = r
# # Divide
# (-log((1/y1) - 1) / (x1-m)) / (-log((1/y2) - 1) / (x2-m)) = r/r = 1
# log((1/y1) - 1) / (x1-m) / log((1/y2) - 1) / (x2-m) = r/r = 1
# (log((1/y1) - 1) / log((1/y2) - 1)) * ((x2-m) / (x1-m)) = 1
# (log((1/y1) - 1) / log((1/y2) - 1)) = 1 / ((x2-m) / (x1-m))
# log((1/y1) - 1) / log((1/y2) - 1) = (x1-m) / (x2-m)
# log((1/y1) - 1) / log((1/y2) - 1) = (x1 / (x2-m)) - (m / (x2-m))
# # NO DICE!
# # Subtract?
# (-log((1/y1) - 1) / (x1-m)) - (-log((1/y2) - 1) / (x2-m)) = r - r = 0
# (-log((1/y1) - 1) / (x1-m)) = (-log((1/y2) - 1) / (x2-m))
# log((1/y1) - 1) / (x1-m) = log((1/y2) - 1) / (x2-m)
# log((1/y1) - 1) / log((1/y2) - 1) =  (x1-m)/ (x2-m)
# # STILL NO DICE!
# 
# # Try m formula:
# (log((1/y1) - 1) / r) + x1 = m
# (log((1/y2) - 1) / r) + x2 = m
# # Subtract!
# (log((1/y1) - 1) / r) + x1 - (log((1/y2) - 1) / r) + x2 = m - m = 0
# (log((1/y1) - 1) / r) + x1 = (log((1/y2) - 1) / r) + x2
# log((1/y1) - 1) + (x1 * r) = log((1/y2) - 1) + (x2 * r)
# log((1/y1) - 1) - log((1/y2) - 1) = (x2 * r) - (x1 * r)
# log((1/y1) - 1) - log((1/y2) - 1) = (x2 - x1) * r
# (log((1/y1) - 1) - log((1/y2) - 1)) /  (x2 - x1) = r
# # YES!
# # Now solve for m:
# (log((1/y1) - 1) / r) + x1 = m
# # (should give same as:)
# (log((1/y2) - 1) / r) + x2 = m
# 
# 
# 
