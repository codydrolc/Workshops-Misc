#-------------------------------------------------------------------------------
#---- Singular value decomposition and biplot

# This example uses the discretionary budget of the federal government in the
# United States. Certain categories of the budget have been collapsed into 
# "Other Agencies" for simplicity. The data are hosted in a tab-delimited text
# file on my github. Entries in each cell are the percent of the discretionary
# budget going to each agency/area in a given year separted by columns.

# Install packages
install.packages(c("ggplot2", "ggrepel", "reshape2"))

# Load packages
library(ggplot2); library(ggrepel) 

# Read data
url <- "https://raw.githubusercontent.com/codydrolc/Workshops-Misc/master/Measurement_Theory/discretionary_budget.txt"
discretionary <- read.table(url, header = TRUE, sep = "\t", check.names = FALSE)

# A quick look at the data
# Reshape data from wide to long and group by year
disc.long <- reshape2::melt(discretionary, id.vars = "year")

# Bar graph of each category over time
ggplot(disc.long, aes(year, value)) + facet_wrap(~variable, ncol = 7) + 
  geom_bar(stat = "identity") + theme_bw() + theme(panel.grid.minor = element_blank())

# Drop the first column for now
disc <- discretionary[, 2:22]
disc <- as.data.frame(lapply(disc, as.numeric)) # treat all data as numeric

# Standardize the variables and return matrix
disc <- scale(disc) # What should the mean and variance be?

# Perform the singluar value decomposition
# Compute the first two left and right singular vectors
# Left singular vector maximum is the number of rows in our matrix
# Right singular vector maximum is the number of columns in our matrix
svd.disc <- svd(disc, nu = 2, nv = 2)

# Save the first two singular values 
svals <- svd.disc$d[1:2]

# Create matrices for variables and observations
# Singular vectors are weighted by the sqrt of the first two singular values
vars <- svd.disc$v[, 1:2] * (rep(1, length(svd.disc$v[, 1]))) %*% t(sqrt(svals))
obs <- svd.disc$u[, 1:2] * (rep(1, length(svd.disc$u[, 1]))) %*% t(sqrt(svals))

# Examine matrices
vars
obs

# Attach row names to matrices
row.names(vars) <- colnames(disc)
row.names(obs) <- (discretionary$year)

# Create data frames
vars.df <- as.data.frame(vars)
obs.df <- as.data.frame(obs)

# Add origin points
vars.df$originx <- rep(0, 21) # 21 for the number of spending categories
vars.df$originy <- rep(0, 21)

# Basic biplot
ggplot() + 
  geom_segment(vars.df, mapping = aes(x = originx, y = originx, xend = V1, yend = V2)) +
  theme_bw() + theme(panel.grid.minor = element_blank())

# Biplot with years as points and labels for segments
ggplot(obs.df, aes(V1, V2)) + 
  geom_text(label = rownames(obs.df), position = position_jitter(w = .3, h = .3)) +
  geom_segment(vars.df, mapping = aes(x = originx, y = originx, xend = V1, yend = V2)) +
  coord_fixed() +
  geom_text_repel(data = vars.df, aes(V1, V2, label = rownames(vars)), seed = 7,
          segment.color="blue", box.padding = unit(0.75, "lines"), force = 2) +
  scale_x_continuous(limits = c(-1.75, 1.75), breaks = seq(-1.5,1.5,.5), 
          sec.axis = dup_axis()) + 
  scale_y_continuous(limits = c(-1.75, 1.75), breaks = seq(-1.5,1.5,.5), 
          sec.axis = dup_axis()) +
  ylab("") + xlab("") + theme_bw() + theme(panel.grid.minor = element_blank())

# Variance explained by first two pairs of singular vectors
var.explained <- sum(svals^2)/sum(svd.disc$d^2)
var.explained # 0.5785381
