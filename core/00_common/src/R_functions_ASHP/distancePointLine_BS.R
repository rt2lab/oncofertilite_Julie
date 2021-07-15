distancePointLine <- function(x, y, slope, intercept) {
 ## x, y is the point to test.
 ## slope, intercept is the line to check distance.
 ##
 ## Returns distance from the line.
 ##
 ## Returns 9999 on 0 denominator conditions.
 x1 <- x-10
 x2 <- x+10
 y1 <- x1*slope+intercept
 y2 <- x2*slope+intercept
 dd <- distancePointSegment(x,y, x1,y1, x2,y2)
 return(dd)
}

distancePointSegment <- function(px, py, x1, y1, x2, y2) {
 ## px,py is the point to test.
 ## x1,y1,x2,y2 is the line to check distance.
 ##
 ## Returns distance from the line, or if the intersecting point on the line nearest
 ## the point tested is outside the endpoints of the line, the distance to the
 ## nearest endpoint.
 ##
 ## Returns 9999 on 0 denominator conditions.
 lineMagnitude <- function(x1, y1, x2, y2) sqrt((x2-x1)^2+(y2-y1)^2)
 ans <- NULL
 ix <- iy <- 0   # intersecting point
 lineMag <- lineMagnitude(x1, y1, x2, y2)
 if( lineMag < 0.00000001) {
   stop("Short segment")
 }
 u <- (((px - x1) * (x2 - x1)) + ((py - y1) * (y2 - y1)))
 u <- u / (lineMag * lineMag)
 if((u < 0.00001) || (u > 1)) {
   ## closest point does not fall within the line segment, take the shorter distance
   ## to an endpoint
   ix <- lineMagnitude(px, py, x1, y1)
   iy <- lineMagnitude(px, py, x2, y2)
   if(ix > iy)  ans <- iy
   else ans <- ix
 } else {
   ## Intersecting point is on the line, use the formula
   ix <- x1 + u * (x2 - x1)
   iy <- y1 + u * (y2 - y1)
   ans <- lineMagnitude(px, py, ix, iy)
 }
 return(ans)
}


