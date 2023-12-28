# Estimation Approach to Statistical Inference
## Color

### Color Transformations

.colorTransparent <- function(someColor, alpha = 100) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(x) {
    rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, maxColorValue = 255)
  })
}

.colorIntensity <- function(someColor, increase = .0) {
  newColor <- rgb2hsv(col2rgb(someColor))
  newColor[3, ] <- newColor[3, ] + increase
  newColor[newColor > 1] <- 1
  apply(newColor, 2, function(x) {
    hsv(h = x[1], s = x[2], v = x[3])
  })
}
