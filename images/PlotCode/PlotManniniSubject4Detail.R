# source("C:/Stat/HMM/Writeups/Presentation/Plots/PlotsCode/PlotManniniSubject4Detail.R")

rm(list=ls())
library("R.matlab")
library("ggplot2")
library("grid")


#WristCorrected <- readMat("C:/Kinesiology/HMC Models/Data/Mannini/SD2010_wrist_12800_correctionON_90Hz.mat")
wristCorrected <- load("C:/Kinesiology/HMC Models/Data/Mannini/SD2010_wrist_12800_correctionON_90Hz.Rdata")

# raw.data is a list
# entry j is a vector of observations for window j, approximately 12.8 seconds of data at 90 Hz
raw.data <- WristCorrected$Data["raw.win",1,1][[1]]

# participant.labels is a factor vector (factors represented by integers)
# entry j is the subject ID for window j
participant.labels <- as.factor(as.vector(WristCorrected$Labels["Participant", , 1][[1]]))

# class.labels is a factor vector (factors represented by strings)
# The integer codes in the file are 2, 3, 4, 5 but are described as being 1, 2, 3, 4.  I assume I should just subtract 1
# entry j is the class label for window j
class.labels <- as.vector(WristCorrected$Labels["Lab5w", , 1][[1]]) - 1
#class.labels <- factor(c("ambulation", "cycling", "other", "sedentary")[class.labels],
#                          levels=c("sedentary", "ambulation", "cycling", "other"))
class.labels <- c("ambulation", "cycling", "other", "sedentary")[class.labels]





# utility functions for making the plots
vplayout <- function(myLayout) {
  grid.newpage()
  pushViewport(viewport(layout = myLayout))
}

subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


# 2 panels vertically arranged, with an extra line for a title
Layout <- grid.layout(nrow = 3, ncol = 2, widths = unit(c(15, 5), c("null", "null")), heights = unit(c(1.2, 10, 10), c("null", "null", "null")))
#grid.show.layout(Layout)

# each panel covers a half hour of time (90obs/sec * 60sec/min * 30min)
subplot.length <- 90*60*30

get.DO.Polys <- function(CD, min.y, max.y) {
  len <- nrow(CD)
  
  DO.change.points <- which(CD$class[1:(len - 1)] != CD$class[2:len])
  DO.startend.points <- rep(DO.change.points, each=2) + rep(c(0, 1), times=length(DO.change.points))
  DO.startend.points <- c(1, DO.startend.points, len)
  DO.Poly.positions <- data.frame(
    id = rep(seq(length(DO.change.points) + 1), each=4),
    x = rep(DO.startend.points, each=2),
    y = rep(c(min.y, max.y, max.y, min.y), length=2*length(DO.startend.points))
  )
  DO.Poly.values <- data.frame(
    id = seq(length(DO.change.points) + 1),
    value = CD[c(1, DO.change.points+1),"class"]
  )
  DO.Poly <- merge(DO.Poly.values, DO.Poly.positions, by=c("id"))
  
  return(DO.Poly)
}



font.size <- 16


subj <- 4

subj.inds <- which(participant.labels == subj)
  
  subj.obs <- c()
  subj.lab <- c()
  for(i in seq_along(subj.inds)) {
    subj.obs <- c(subj.obs, raw.data[[subj.inds[i]]])
    subj.lab <- c(subj.lab, rep(class.labels[subj.inds[i]], times=length(raw.data[[subj.inds[i]]])) )
  }
  
  full.data.subj <- data.frame(SMV = subj.obs, class=subj.lab)
  
  #subsampled.data.subj <- full.data.subj[seq(from=1, to=dim(full.data.subj)[1], by=90),]
  temp <- pretty(full.data.subj$SMV)
  min.y <- temp[1]
  max.y <- temp[length(temp)]
  
    DO.Poly1 <- get.DO.Polys(full.data.subj[1:subplot.length,], min.y, max.y)
    
    p1 <- ggplot() +
      geom_polygon(aes(x=x, y=y, fill=value, group=id), data=DO.Poly1) +
      geom_line(aes(x=seq_along(SMV), y=SMV), data=full.data.subj[1:subplot.length,]) +
      xlab("Time (minutes)") + ylab("VM (g)") +
#      scale_fill_discrete("Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE) +
      scale_fill_brewer("Activity Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE, type="div", palette="RdYlBu") +
      scale_x_continuous(limits=c(0,subplot.length), breaks=seq(from=0, len=7, by=90*60*5), labels=seq(from=0, len=7, by=5) ) +
      scale_y_continuous(limits=c(min.y,max.y), breaks=seq(from=min.y, to=max.y, by=1)) +
      theme_bw() +
      theme(legend.position = "none", legend.title = element_text(size=font.size), legend.text = element_text(size=font.size), axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size), axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size))
    
    
    DO.Poly2 <- get.DO.Polys(full.data.subj[(subplot.length+1):nrow(full.data.subj),], min.y, max.y)
    
    p2 <- ggplot() +
      geom_polygon(aes(x=x, y=y, fill=value, group=id), data=DO.Poly2) +
      geom_line(aes(x=seq_along(SMV), y=SMV), data=full.data.subj[(subplot.length+1):nrow(full.data.subj),]) +
      xlab("Time (minutes)") + ylab("VM (g)") +
#      scale_fill_discrete("Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE) +
      scale_fill_brewer("Activity Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE, type="div", palette="RdYlBu") +
      scale_x_continuous(limits=c(0,subplot.length), breaks=seq(from=0, len=7, by=90*60*5), labels=seq(from=30, len=7, by=5) ) +
      scale_y_continuous(limits=c(min.y,max.y), breaks=seq(from=min.y, to=max.y, by=1)) +
      theme_bw() +
      theme(legend.position = "none", legend.title = element_text(size=font.size), legend.text = element_text(size=font.size), axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size), axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size))
    
    p3.for.legend <- ggplot() +
      geom_polygon(aes(x=x, y=y, fill=value, group=id), data=DO.Poly2) +
      geom_line(aes(x=seq_along(SMV), y=SMV), data=full.data.subj[(subplot.length+1):nrow(full.data.subj),]) +
      xlab("Time (minutes)") + ylab("VM (g)") +
      #      scale_fill_discrete("Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE) +
      scale_fill_brewer("Activity Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE, type="div", palette="RdYlBu") +
      scale_x_continuous(limits=c(0,subplot.length), breaks=seq(from=0, len=7, by=90*60*5), labels=seq(from=30, len=7, by=5) ) +
      scale_y_continuous(limits=c(min.y,max.y), breaks=seq(from=min.y, to=max.y, by=1)) +
      theme_bw() +
      theme(legend.title = element_text(size=font.size), legend.text = element_text(size=font.size), axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size), axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size))

p3.for.legend
types.legend <- grid.get("guide-box.3-5-3-5")

p1notypes <- ggplot() +
  geom_line(aes(x=seq_along(SMV), y=SMV), data=full.data.subj[1:subplot.length,]) +
  xlab("Time (minutes)") + ylab("VM (g)") +
  #      scale_fill_discrete("Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE) +
  scale_fill_brewer("Activity Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE, type="div", palette="RdYlBu") +
  scale_x_continuous(limits=c(0,subplot.length), breaks=seq(from=0, len=7, by=90*60*5), labels=seq(from=0, len=7, by=5) ) +
  scale_y_continuous(limits=c(min.y,max.y), breaks=seq(from=min.y, to=max.y, by=1)) +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_text(size=font.size), legend.text = element_text(size=font.size), axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size), axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size))

p2notypes <- ggplot() +
  geom_line(aes(x=seq_along(SMV), y=SMV), data=full.data.subj[(subplot.length+1):nrow(full.data.subj),]) +
  xlab("Time (minutes)") + ylab("VM (g)") +
  #      scale_fill_discrete("Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE) +
  scale_fill_brewer("Activity Class", breaks=c("ambulation", "cycling", "other", "sedentary"), drop=FALSE, type="div", palette="RdYlBu") +
  scale_x_continuous(limits=c(0,subplot.length), breaks=seq(from=0, len=7, by=90*60*5), labels=seq(from=30, len=7, by=5) ) +
  scale_y_continuous(limits=c(min.y,max.y), breaks=seq(from=min.y, to=max.y, by=1)) +
  theme_bw() +
  theme(legend.position = "none", legend.title = element_text(size=font.size), legend.text = element_text(size=font.size), axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size), axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size))


#pdf(file="C:/Stat/HMM/Writeups/Proposal/Plots/ManniniSubject4AccelerationDetail.pdf", width=30, height=20)
png(file="C:/Stat/HMM/Writeups/DissertationDefense/images/ManniniSubject4AccelerationDetail.png", width=15, height=6, units="in", res=300)

vplayout(myLayout = Layout)
    
grid.text("Acceleration Signal Vector Magnitude (VM) vs. Time", just="center", gp=gpar(fontsize=1.2*font.size), vp=subplot(1,1))
print(p1, vp = subplot(2, 1))
print(p2, vp = subplot(3, 1))
pushViewport(viewport(layout.pos.row = c(1, 2, 3), layout.pos.col = 2))
grid.draw(types.legend)
#print(types.legend, vp = subplot(c(3, 4), 2))

dev.off()





png(file="C:/Stat/HMM/Writeups/DissertationDefense/images/ManniniSubject4AccelerationDetailnotype.png", width=10, height=5, units="in", res=300)

vplayout(myLayout = Layout)

grid.text("Acceleration Signal Vector Magnitude (VM) vs. Time", just="center", gp=gpar(fontsize=1.2*font.size), vp=subplot(1,1))
print(p1notypes, vp = subplot(2, 1))
print(p2notypes, vp = subplot(3, 1))

dev.off()
