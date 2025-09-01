# corners of pitch
top_left_corner <- c(0,0)
bot_left_corner <- c(0,80)
top_right_corner <- c(120,0)
bot_right_corner <- c(120, 80)

# endpoints of half-line
top_halfline <- c(60,0)
bot_halfline <- c(60, 80)

# corners of left penalty area
top_left_penl <- c(0, 18)
bot_left_penl <- c(0,62)
top_right_penl <- c(18,18)
bot_right_penl <- c(18,62)

# corners of left goalkeeper area
top_left_gkl <- c(0,30)
bot_left_gkl <- c(0,50)
top_right_gkl <- c(6,30)
bot_right_gkl <- c(6,50)

# left goal posts
top_postl <- c(0,36)
bot_postl <- c(0,44)

# corners of left net
top_left_netl <- c(-2,36)
bot_left_netl <- c(-2, 44)
top_right_netl <- c(0, 36)
bot_right_netl <- c(0,44)

# corners of right penalty area
top_left_penr <- c(102,18)
bot_left_penr <- c(102,62)
top_right_penr <- c(120,18)
bot_right_penr <- c(120,62)
# corners of right goal keeper's area
top_left_gkr <- c(114,30)
bot_left_gkr <- c(114,50)
top_right_gkr <- c(120,30)
bot_right_gkr <- c(120,50)
# right goal posts
top_postr <- c(120,36)
bot_postr <- c(120,44)
# corners of left net
top_left_netr <- c(120,36)
bot_left_netr <- c(120, 44)
top_right_netr <- c(122, 36)
bot_right_netr <- c(122,44)

# half-circles
left_halfcirc <-
  data.frame(x = 60 + 10*cos(seq(from = pi/2, to = 3*pi/2, length = 100)),
             y = 40 - 10 * sin(seq(from = pi/2, to = 3*pi/2, length = 100)))

right_halfcirc <-
  data.frame(x = 60 + 10*cos(seq(from =-pi/2, to = pi/2, length = 100)),
             y = 40 - 10 * sin(seq(from = -pi/2, to = pi/2, length = 100)))


par(mar = c(3,3,2,1), 
    mgp = c(1.8, 0.5, 0), 
    xpd = TRUE) # xpd allows plotting in margins
plot(1, type = "n",
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n", bty = "n",
     xlim = c(0,120), ylim = c(80,0))
# boundaries of pitch
rect(xleft = bot_left_corner[1], ybottom = bot_left_corner[2], 
     xright = top_right_corner[1], ytop = top_right_corner[2])
# left penalty area
rect(xleft = bot_left_penl[1], ybottom = bot_left_penl[2],
     xright = top_right_penl[1], ytop = top_right_penl[2])
# left goalkeeper's area
rect(xleft = bot_left_gkl[1], ybottom = bot_left_gkl[2],
     xright = top_right_gkl[1], ytop = top_right_gkl[2])
# left net
rect(xleft = bot_left_netl[1], ybottom = bot_left_netl[2],
     xright = top_right_netl[1], ytop = top_right_netl[2])

# right penalty area
rect(xleft = bot_left_penr[1], ybottom = bot_left_penr[2],
     xright = top_right_penr[1], ytop = top_right_penr[2])
# right goalkeeper's area
rect(xleft = bot_left_gkr[1], ybottom = bot_left_gkr[2],
     xright = top_right_gkr[1], ytop = top_right_gkr[2])
# right net
rect(xleft = bot_left_netr[1], ybottom = bot_left_netr[2],
     xright = top_right_netr[1], ytop = top_right_netr[2])
# half-line
lines(x = c(top_halfline[1], bot_halfline[1]),
      y = c(top_halfline[2], bot_halfline[2]))
# left half-circle 
lines(x = left_halfcirc$x, y = left_halfcirc$y)
# right half-circle

# Now plot Mead's location
oi_colors <- palette.colors(palette = "Okabe-Ito")

points(shot$location.x, shot$location.y, pch = 16, cex = 2,
       col = oi_colors[1])
# polygon for cone
polygon(x = c(shot$location.x, top_postr[1], bot_postr[1], shot$location.x),
        y = c(shot$location.y, top_postr[2], bot_postr[2], shot$location.y),
        col = adjustcolor(col = oi_colors[3], alpha.f = 0.5))
for(i in 1:nrow(ff)){
  points(x = ff$location[i][[1]][1], 
         y = ff$location[i][[1]][2],
         pch = 1.2,
         col = ifelse(ff$teammate[i], oi_colors[1], oi_colors[2]))
}


