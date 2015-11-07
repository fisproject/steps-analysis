require(dlm)
require(ggplot2)
require(reshape2)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

Sys.setlocale("LC_TIME","C") # for as.Date
d <- read.csv("./data/HealthData.csv", header=T)

df <- data.frame(finish=as.Date(d$Finish, format="%d-%b-%Y %H:%M"), steps=d$Steps..count.)

df.t <- data.frame(
          finish = df[!duplicated(df$finish),]$finish, # 重複削除
          steps = as.integer(tapply(df$steps, df$finish, sum)) / 1000  # 同じ日でまとめる
        )

df.t <- df.t[-1 * length(df.t$steps),] # 最終日の歩数 0 の行を除く

build.1 <- function(theta){
  dlmModPoly(order=1, dV=exp(theta[1]), dW=exp(theta[2]))
}

fit.1 <- dlmMLE(df.t$steps, parm=c(1, 1), build.1)

mod.step <- build.1(fit.1$par)
step.fit <- dlmFilter(df.t$steps, mod.step)
step.smooth <- dlmSmooth(step.fit) # スムージングする

plot(df.t, type="o", col=8, ylab="", main="Steps m")
lines(dropFirst(step.fit$m), col="blue", lwd=5)

plot(df.t, type="o", col=8, ylab="", main="Steps s")
lines(dropFirst(step.smooth$s), col="blue", lwd=5)

df.t2 <- data.frame(
          finish = df.t$finish,
          steps = df.t$steps,
          fit = step.fit$m[1:length(df.t$steps)],
          smooth = step.smooth$s[1:length(df.t$steps)]
        )

colnames(df.t2)[1] <- "time"
r <- melt(df.t2, id.var = c("time"))

# 歩数
p <- ggplot(
  r,
  aes(
      x = time,
      y = value,
      group = variable,
      colour = variable
  )
)

p <- p + geom_line(size=0.5) +
    labs(title="Steps", x="Date", y="Steps (x1000/day)")
plot(p)
