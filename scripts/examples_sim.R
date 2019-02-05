library(sf)
library(ggplot2)

set.seed(123)

#---- Generate polys set 1 ----

polys <- vector(mode = 'list', length = 100)

polys[[1]] <- st_polygon(list(rbind(c(0, 0), c(.1, 0), 
                                    c(.1, .1), c(0, .1), 
                                    c(0, 0))))

for(i in seq_along(polys)[-1]) {
  if(i <= 10) {
    polys[[i]] <- polys[[i - 1]] + c(.1, 0)
  } else {
    polys[[i]] <- polys[[i - 10]] + c(0, .1)
  }
}

x <- st_sfc(polys)
plot(x)

#----- Generate polys set 2 ----

polys2 <- vector(mode = 'list', length = 25)

polys2[[1]] <- st_polygon(list(rbind(c(0, 0), c(.2, 0), 
                                     c(.2, .2), c(0, .2), 
                                     c(0, 0))))

for(i in seq_along(polys2)[-1]) {
  if(i <= 5) {
    polys2[[i]] <- polys2[[i - 1]] + c(.2, 0)
  } else {
    polys2[[i]] <- polys2[[i - 5]] + c(0, .2)
  }
}

x2 <- st_sfc(polys2)
plot(x2, add = T, col = '#ff0000d3')

#---- Generate points set ----

n <- 50

points <- vector(mode = 'list', length = n)

for(i in seq_len(n)) {
  points[[i]] <- st_point(x = c(runif(1), runif(1)))
}

points <- st_sfc(points)
plot(points, add = T)

x <- st_sf(data.frame(geom = x))
x2 <- st_sf(data.frame(geom = x2))
points <- st_sf(data.frame(geom = points))

#---- plots ----

(sq1 <- ggplot(data = x) +
   geom_sf(color = 'black', fill = '#3844D5', 
           alpha = .8, size = .2) +
   theme_minimal()) 

(sq2 <- ggplot(data = x2) +
    geom_sf(color = 'black', fill = '#D53838', 
            alpha = .8, size = .2) +
    theme_minimal()) 

(pts1 <- ggplot(data = points) +
    geom_sf(color = 'black') +
    theme_minimal())

(sq12 <- ggplot() +
    geom_sf(data = x, color = 'black', 
            fill = '#3844D5', alpha = .8,
            size = .2) +
    geom_sf(data = x2, color = 'black', 
            fill = '#D53838', alpha = .8,
            size = .2) +
    theme_minimal())

(sq1pt <- ggplot() +
    geom_sf(data = x, color = 'black', 
            fill = '#3844D5', alpha = .8,
            size = .2) +
    geom_sf(data = points, color = 'black', 
            fill = 'black') +
    theme_minimal())

x1_2 <- x[st_intersects(x, points, sparse = F) %>% apply(1, any),]

(ptsq1 <- ggplot() +
    geom_sf(data = x, color = 'black', 
            fill = 'transparent', size = .2) +
    geom_sf(data = x1_2, color = 'black', 
            fill = '#3844D5', alpha = .8,
            size = .2) +
    geom_sf(data = points, color = 'black', 
            fill = 'black') +
    theme_minimal())

# vor

vor <- points$geometry %>% 
  st_union() %>% 
  st_voronoi() %>% 
  st_cast() %>% 
  {st_sf(data.frame(geom = .))} %>% 
  st_crop(., st_bbox(x))

###

x$`Proportion of Area Covered` <- NA

for(i in seq_along(x$geometry)) {
  aux <- st_intersection(x = x$geometry[i], y = vor$geometry[1])
  x$`Proportion of Area Covered`[i] <- ifelse(length(aux) == 0, 0, {st_area(aux)/st_area(vor$geometry)})  
}

x$id <- 1:nrow(x)

x3 <- x %>% dplyr::filter(`Proportion of Area Covered` > 0)
data_list <- split(x3, x3$id)

# labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +

ggplot(data = x) +
  geom_sf(data = vor, fill = 'gray', size = .2, col = 'black') +
  geom_sf(data = vor$geometry[1], fill = '#1A5180',
          size = .2, col = 'black') +
  geom_sf(data = points) +
  geom_sf(fill = 'transparent', col = 'white', linetype = 2) +
  theme_minimal()

img <- magick::image_graph(500, 500, res = 60)
# out <- parallel::mclapply(data_list, function(x) {
out <- lapply(data_list, function(y) {
  
  p <- ggplot(data = x) +
    geom_sf(data = vor, fill = 'gray', size = .2, col = 'black') +
    geom_sf(fill = 'transparent', col = 'white', linetype = 2,
            size = .2) +
    geom_sf(data = vor$geometry[1], fill = '#1A5180',
            size = .2, col = 'black') +
    geom_sf(data = y, fill = '#FFDD34', alpha = .3, color = 'transparent') +
    geom_sf(data = points) +
    theme_minimal() +
    ggtitle(label = paste('Proportion of Cell Covered:', 
                          round(y$`Proportion of Area Covered`, 4)))
  
  print(p)
})

dev.off()

animation <- magick::image_animate(img, fps = 1)
print(animation)
magick::image_write(animation, path = 'img/poly_inter.gif')
