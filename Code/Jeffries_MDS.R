# -------------------------------------------------------------------------------- # 
# Package Loading
# -------------------------------------------------------------------------------- # 
# cmdscale = Classical Multidimensional Scale
# this is one of many options of MDS packages within R
library(stats)

# MASS 
library(MASS)

# 2D plot
library(ggplot2)

# 3D plot
library(rgl)
citation()
# -------------------------------------------------------------------------------- # 
# Part 1
# 2D Solution
# -------------------------------------------------------------------------------- # 

# Data that is included in this analysis
mtcars[,c(1, 4)]

#Calculate Dist.
data.dist <- dist(mtcars[, c(1,4)])

data.dist

#Calculate MDS
# k = number of dimensions
data.mds_2D <- cmdscale(data.dist, k=2, eig = T)

data1 <- as.data.frame(data.mds_2D)

#2D MDS Distances 
data.mds_2D

data.mds2D[,1]
data.mds2D[,2]

#Create x,y refs
data.x_2D <- data.mds[,1]
data.y_2D <- data.mds[,2]

#Plot
ggplot(data = data1, aes(x = data.x_2D, y = data.y_2D)) +
  geom_point(size = 2) + 
  labs(title = "Classical MDS for mtcars Dataset",
       subtitle = "Using {cmdscale} Package",
       y = "Dimension 2",
       x = "Dimension 1") +
  theme_minimal(base_size = 16) +
  xlim(-100,200) +
  ylim(-10,10)

# -------------------------------------------------------------------------------- # 
# Part 2
# 3D Solution
# -------------------------------------------------------------------------------- # 

#Calculate MDS
data.mds_3D <- cmdscale(data.dist, k=3)

#Create x,y refs
data.x_3D <- data.mds[,1]
data.y_3D <- data.mds[,2]
data.z_3D <- data.mds[,3]

#Plot
plot3d(data.x, data.y, data.z)

plot3d( 
  x = data.x_3D, y = data.y_3D, z = data.z_3D, 
  #col = data$color, 
  type = "p", 
  size = 8,
  xlab = "Dimension 1", ylab = "Dimension 2", zlab = "Dimension 3")

#Plot ellipse of concentration -- the content "universe"!
rgl_init()
plot3d(x = data.x, y = data.y, z = data.z,
       size = 8,
       xlab = "Dimension 1", ylab = "Dimension 2", zlab = "Dimension 3")
ellips <- ellipse3d(cov(cbind(x = data.x, y = data.y, z = data.z)), 
                    centre=c(mean(data.x), mean(data.y), mean(data.z)), level = 0.95)
plot3d(ellips, col = "blue", alpha = 0.3, add = TRUE, box = FALSE) 

# -------------------------------------------------------------------------------- # 
# Part 3
# Finding mtcars Stress values via isoMDS from the {MASS} package
# -------------------------------------------------------------------------------- # 

# Same data from before
data.mds2D

Dim1 <- data.mds2D[,1]
Dim2 <- data.mds2D[,2]

# Plot the solution with car names (rather than points)

plot(Dim1, -Dim2, type = "n", xlab = "", ylab = "", main = "Using cmdscale(mtcars) on Motor Trend Car Road Tests Dataset")
text(Dim1, -Dim2, rownames(mtcars))

# Creating mtcars distances
dist_mtcars <- dist(mtcars)
dist_mtcars

# Individual dimension Stress output
mds1 <- isoMDS(dist_mtcars, k = 1)
mds2 <- isoMDS(dist_mtcars, k = 2)
mds3 <- isoMDS(dist_mtcars, k = 3)
mds4 <- isoMDS(dist_mtcars, k = 4)

# Scree plot for Stress values
stress = c(mds1$stress, mds2$stress, mds3$stress, mds4$stress)
dimensions = 1:4
plot(dimensions, stress, type = "b", xlab = "Number of Dimensions", ylab = "Stress",
     xaxt = "n")
axis(side=1, at=c(1, 2, 3, 4))
