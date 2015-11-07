require(rstan)
require(ggplot2)
require(reshape2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

Sys.setlocale("LC_TIME","C") # for as.Date
d <- read.csv("./data/HealthData.csv", header=T)

for (i in d) {
  d$hour[i] <- substr(d$Finish[i], 13, 17) # 時間 (hour)を抜き出す
}

d2 <- data.frame(
  finish=as.Date(d$Finish, format="%d-%b-%Y %H:%M"),
  hour=d$hour,
  steps=d$Steps..count.
)

d3 <- data.frame(
  hour=d$hour,
  weekdays=weekdays(d2$finish),
  steps = d2$steps
)

d3 <- transform(
  d3,
  weekdays=factor(
    weekdays, # factorの並び替え
    levels=c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")
  )
)

d4 <- data.frame(
  finish = d2[!duplicated(d2$finish),]$finish, # 重複削除
  steps = tapply(d2$steps, d2$finish, sum), # 同じ日でまとめる
  steps_count = as.integer(tapply(d2$steps, d2$finish, sum)) / 1000 # 同じ日でまとめる
)

d4 <- d4[-1 * length(d4$steps),] # 最終日の歩数 0 の行を除く

# line
p <- ggplot(
  d4,
  aes(
      x = finish,
      y = steps,
  )
)

p <- p + geom_line(size=0.5) +
    geom_vline(xintercept=as.integer(d4$finish[227]), colour="red", linetype="longdash") + # 引越しにより徒歩量が変化した日
    labs(title="Steps per day", x="Date", y="Steps")
plot(p)

# Histogram
p <- ggplot(
  d4,
  aes(
      x = steps_count,
      y = ..density..
  )
)
p <- p + geom_histogram(alpha=0.7, position="identity", size=1) +
      labs(title="Steps", x="Steps (x1000/day)", y="density") +
      geom_density(alpha=0, colour="magenta")
plot(p)

# Tile
g <- ggplot(
  d3,
  aes(
    x=hour,
    y=weekdays,
  )
)
g <- g + geom_tile(aes(fill=steps))
plot(g)

# Tile
qplot(hour, weekdays, fill=steps, data=d3, geom="tile") +
scale_fill_gradient(limits=c(0, 5000), low="white", high="black")

# Tile
qplot(hour, finish, fill=steps, data=d2, geom="tile") +
scale_fill_gradient(limits=c(0, 5000), low="white", high="black")
