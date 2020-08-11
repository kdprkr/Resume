# set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/prof_apps/resume/")

# read in the apprpriate data.frames
df.seg <- read.table(file="segs.txt", header=TRUE)
df.pts <- read.table(file="pts.txt", header=TRUE)
#df.labs <- read.table(file="labs.txt", header=TRUE)

# create data.frame to plot year labels
lab.yr <- c("2012","2013","2014","2015","2016","2017","2018")
x.yr <- c(1.5,6,12,18,24,30,33.75)
y.yr <- 0
df.yr <- data.frame("year"=lab.yr, "x"=x.yr, "y"=y.yr)

# create a data.frame for the vertical segments separating months by year
x.vrt.yr <- c(2.75,8.75,14.75,20.75,26.75,32.75)
y.vrt.yr <- 0.5
yend.vrt.yr <- -0.5
df.vrt.yr <- data.frame("x"=x.vrt.yr, "xend"=x.vrt.yr, "y"=y.vrt.yr, "yend"=yend.vrt.yr)

# define colors for specific groups
hrz.cols <- c("#f1b6da","#f1b6da",
              "#5e3c99",
              "#d01c8b",
              "#4dac26","#4dac26",
              "#d01c8b","#d01c8b",
              "#5e3c99","#5e3c99",
              "#b8e186",
              "#4dac26",
              "#d01c8b",
              "#5e3c99",
              "#e66101",
              "#4dac26",
              "#d01c8b",
              "#252525",
              "#e66101")

# plot order is:
# segment - horizontal lines specifying length of "skill"
# segment - beginning vertical lines creating the "timeline effect"
# segment - ending vertical lines creating the "timeline effect" <- will error, that's okay, we want to remove values
# rect - shaded rectangle denoting first year of graduate school
# point - ending arrows creating the "timeline effect" <- will error, that's okay, we want to remove values
# point - point for the GTA panel
# hline - horizontal lines for year labels
# segment - vertical lines for year labels
# text - labels for year
# text - labels for horizontal segments

# plot
require(ggplot2)
p <- ggplot(data=df.seg) +
  geom_segment(stat="identity", lineend="square", size=1.0, 
               color=hrz.cols, aes(x=x, y=y, xend=x.end, yend=y)) +
  geom_segment(stat="identity", lineend="square", size=1.0, 
               color=hrz.cols, aes(x=x, y=y-0.12, xend=x, yend=y+0.12)) +
  geom_segment(stat="identity", lineend="square", size=1.0, 
               color=hrz.cols, aes(x=x.end.hrz.seg, y=y-0.12, xend=x.end.hrz.seg, yend=y+0.12)) + 
  geom_point(stroke=9, size=1, color=hrz.cols, shape=62, aes(x=x.end.arw-0.1, y=y)) +
  geom_point(inherit.aes=FALSE, data=df.pts, color="#e66101", shape=15, stroke=0, size=2.22,
             (aes(x=x, y=y))) +
  geom_rect(stat="identity", size=1.0, fill="gray", alpha= 0.025, 
            xmin=6.75, xmax=10.75, ymax=1, ymin=-1) +
  geom_hline(yintercept=c(0.5,-0.5), size=1.0, color="black") +
  geom_segment(data=df.vrt.yr, x=df.vrt.yr$x, xend=df.vrt.yr$x, y=df.vrt.yr$y, yend=df.vrt.yr$yend,
               stat="identity", lineend="square", size=1.0, color="black") +
  geom_text(data=df.yr, label=df.yr$year, x=df.yr$x, y=df.yr$y,
            color="black", size=6.6, fontface="bold") +
#  geom_text(inherit.aes=FALSE, data=df.labs, color="black", size=3.3,
 #           aes(label=label, x=x, y=y)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, size=2, color="black"),
        axis.ticks=element_blank(),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_text(size=20, color="black", angle=90, hjust=0.5, face="bold")) +
  scale_x_continuous(limits=c(0,34.75), expand=c(0,0)) +
  scale_y_continuous(limits=c(-9,15), expand=c(0,0), 
                     breaks=c(-4.5, 7.5), labels=c("Service","Teaching"))
p
  