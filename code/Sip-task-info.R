#
# Sip-task-info.R,  4 Jan 19
# Data from:
# Stephen Cullum
#
# Example from:
# Evidence-based Software Engineering: based on the publicly available data
# Derek M. Jones
#
# TAG estimating effort company

# Ideas I tried out while analysing the SiP dataset

source("ESEUR_config.r")


library("bizdays")
library("car")
library("lubridate")
library("nlme")
library("plyr")
library("quantreg")


pal_col=rainbow(8)


Sip_all=read.csv(paste0(ESEUR_dir, "../articles/SiP/data/Sip-task-info.csv"), as.is=TRUE)
Sip_date=read.csv(paste0(ESEUR_dir, "../articles/SiP/data/est-act-dates.csv"), as.is=TRUE)
Sip_date$EstimateOn=as.Date(Sip_date$EstimateOn, format="%d-%b-%y")
Sip_date$StartedOn=as.Date(Sip_date$StartedOn, format="%d-%b-%y")
Sip_date$CompletedOn=as.Date(Sip_date$CompletedOn, format="%d-%b-%y")

Sip_date=Sip_date[order(Sip_date$TaskNumber), ]
Sip_date$TaskNumber=NULL
Sip_all=cbind(Sip_all, Sip_date)

# sanity checks
# range(Sip_date$EstimateOn)
# table(diff(Sip_all$EstimateOn))
# table(Sip_all$StartedOn-Sip_all$EstimateOn)
# tt=table(Sip_all$CompletedOn-Sip_all$StartedOn)

# Removed the 190 tasks that were cancelled before completion
Sip=subset(Sip_all, StatusCode != "CANCELLED")
# Single instance of this in the data
Sip=subset(Sip, StatusCode != "TEMPLATE")
Sip_stTN=subset(Sip, !duplicated(TaskNumber))
# Projects that were not completed at the time of the data snapshot
Sip=subset(Sip, StatusCode != "CHRONICLE")

Sip_uTN=subset(Sip, !duplicated(TaskNumber))

# P 1
str(Sip)

devs=count(Sip$DeveloperID)
plot(sort(devs$freq, decreasing=TRUE), type="b", log="y", col=point_col,
	xlab="Developer", ylab="Estimates\n")

proj=count(Sip_uTN$ProjectCode)
plot(sort(proj$freq, decreasing=TRUE), type="b", log="y", col=point_col,
	xlab="Project", ylab="Estimates\n")


# A PP plot
e_a=Sip_uTN$HoursEstimate/Sip_uTN$HoursActual
plot(ppoints(1:length(e_a)), sort(pnorm(e_a, mean=1)))
plot(ppoints(1:length(e_a)), sort(pnorm(log(e_a), mean=1)))

# P 2
plot_layout(2, 1)

plot(Sip_uTN$HoursEstimate, Sip_uTN$HoursActual, col=point_col)
plot(Sip_uTN$HoursEstimate, Sip_uTN$HoursActual, log="xy")


# P 3
# Very basic model that includes the two variables of interest

Sip_uTN$l_HoursEstimate=log(Sip_uTN$HoursEstimate)
Sip_uTN$l_HoursActual=log(Sip_uTN$HoursActual)

hours_mod=glm(log(HoursActual) ~ l_HoursEstimate, data=Sip_uTN)
summary(hours_mod)

hours_SC_mod=glm(log(HoursActual) ~ l_HoursEstimate:StatusCode, data=Sip_uTN)
summary(hours_SC_mod)

SC_effect=coef(hours_SC_mod)[-1]

pal_col=rainbow(length(SC_effect)) 

x_est=1:20
plot(x_est, x_est, type="l", log="xy", col="black",
        xlab="Estimate", ylab="Actual")

dummy=sapply(1:length(SC_effect),
		function(X) lines(x_est,
			exp(coef(hours_SC_mod)[1])*x_est^SC_effect[X], col=pal_col[X]))

hours_proj_mod=glm(log(HoursActual) ~ l_HoursEstimate+ProjectCode, data=Sip_uTN)
summary(hours_proj_mod)
# exp(coef(hours_proj_mod))

e1=subset(Sip_uTN, HoursEstimate == 1)
mean(e1$HoursActual)
exp(mean(log(e1$HoursActual)))

e100=subset(Sip_uTN, HoursEstimate == 100)
mean(e100$HoursActual)
exp(mean(log(e100$HoursActual)))

# hours_mod=glm(log(HoursActual) ~ l_HoursEstimate+I(l_HoursEstimate^2), data=Sip_uTN)
# summary(hours_mod)

# Plot shows estimate switching from under to over, as value increases
x_est=1:20
plot(x_est, exp(coef(hours_mod)[1])*x_est^coef(hours_mod)[2], type="l", log="xy", col="red",
	xlab="Estimate", ylab="Actual")
lines(x_est, x_est, col="green")

# Missing value or an infinity produced when evaluating the model
# hours_nmod=nls(l_HoursActual ~ a+b*l_HoursEstimate^c, trace=TRUE,
# 				start=list(a=0.3, b=0.03, c=0.8), data=Sip_uTN)

# P 4
# A model that separates out each developer and gives them some experience

dev_est_act=function(df)
{
if (nrow(df) < 100)
   return(NULL)

# Assume some initial experience
df$TN=100+(1:nrow(df))

ea_mod=glm(log(HoursActual) ~ log(HoursEstimate)+log(TN)+ProjectCode, data=df)
print(c("DeveloperID: ", df$DeveloperID[1]))
print(summary(ea_mod))

return(ea_mod)
}

d_ply(Sip, .(DeveloperID), dev_est_act)


# P 5

# Column names for each developer
udevID=unique(Sip$DeveloperID)
udevID_str=paste0("ID", udevID)
Sip$Mapped_ID=mapvalues(Sip$DeveloperID, udevID, 1:length(udevID))


team_summary=function(df)
{
# Who was on the team (as 0/1 for all developers)
devs=matrix(0, nrow=1, ncol=length(udevID))
colnames(devs)=udevID_str
devs[1, df$Mapped_ID]=1

# Team size and who was on the team
return(data.frame(size=nrow(df),
			devs,
			h_est=df$HoursEstimate[1], h_act=df$HoursActual[1]))
}


team_info=ddply(Sip, .(TaskNumber), team_summary)

# Team size vs. estimated hours
plot(team_info$h_est, team_info$size, log="x",
	xlab="Estimate", ylab="Team size\n")

# test_mod=glm(size ~ log(h_est), family=poisson(link="identity"), data=team_info)
# Even with weighting, a team size of less than two is predicted
# t_w=1-table(team_info$size)/sum(team_info$size)
# 
# test_mod=glm(size ~ log(h_est)+I(log(h_est)^2), data=team_info,
# 			weights=t_w[size])

# test_mod=glm(log(h_est) ~ size, data=team_info)
# summary(test_mod)

# x_vals=exp(seq(-2, 7, by=0.2))
# pred=predict(test_mod, newdata=data.frame(h_est=x_vals))
# lines(x_vals, exp(pred))

pal_col=rainbow(7)

size_den=function(t_size)
{
team=subset(team_info, size == t_size)
lines(density(team$h_est), col=pal_col[t_size])
}

plot(0, type="n", log="x",
	yaxs="i",
	xlim=c(1e-1, 2e2), ylim=c(0, 0.4),
	xlab="Estimate", ylab="Task density\n")
size_den(1)
size_den(2)
size_den(3)
size_den(4)
size_den(5)
size_den(6)
size_den(7)
legend(x="topright", legend=c("1 developer", paste0(2:7, " developers")), bty="n", fill=pal_col, cex=1.2)


# Average actual hours by team size
ddply(team_info, .(size), function(df) mean(df$h_act))

ts=count(team_info$size)
plot(ts, log="y", col=point_col,
	xlab="Team size", ylab="Tasks\n")


# P 6

# Some data reorganization

# Order entries by TaskNumber, for the developer
dev_task_ord=function(df)
{
df=df[order(df$TaskNumber), ]
# Assume some initial experience.
# Appear to have a very tiny impact on fitted models :-(
df$Estimate_Num=0+(1:nrow(df))

return(df)
}


# Calculate some values that are used later

dev_experience=ddply(Sip, .(Mapped_ID), dev_task_ord)

Sip_exp=merge(dev_experience, team_info, by="TaskNumber", all=TRUE)


# P 7

# Plot relative accuracy for developers who have made many estimates

dev_accuracy=function(D_ID)
{
dev=subset(Sip_exp, Mapped_ID == D_ID)

plot(dev$Estimate_Num, dev$HoursEstimate/dev$HoursActual, log="y",
	col=pal_col[dev$size],
	ylim=c(0.1, 10),
	xlab="Estimate", ylab="Estimated/Actual")
lines(loess.smooth(dev$Estimate_Num, dev$HoursEstimate/dev$HoursActual, span=0.3), col="black")

legend(x="topleft", legend=1:8, bty="n", fill=pal_col, cex=1.2)

text(nrow(dev)/2, 8, dev$DeveloperID[1], cex=2)
}

plot_layout(3, 2)

dev_accuracy(1)
dev_accuracy(2)
dev_accuracy(3)
dev_accuracy(5)
dev_accuracy(10)
dev_accuracy(12)


# P 8

# unq_sip=merge(Sip_uTN, team_info, by="TaskNumber", all=TRUE)
# 
# unq_sip$l_H_AE=log(unq_sip$HoursActual)-0.7*log(unq_sip$HoursEstimate)
# 
# plot(unq_sip$size, unq_sip$l_H_AE)
# lines(loess.smooth(unq_sip$size, unq_sip$l_H_AE, span=0.3), col=loess_col)
# 
# size_mod=glm(l_H_AE ~ I(size^0.3), data=unq_sip)
# size_mod=nls(l_H_AE ~ a+b*size^c, trace=TRUE, # fussy convergence
# 		start=list(a=-0.9, b=0.9, c=0.33), data=unq_sip)
# summary(size_mod)

# A compact way of getting all the developerIDs into a formula
ea_form=formula(paste("log(HoursActual) ~ log(HoursEstimate)+ProjectCode+",
			"I(size^0.5)+",
			# "I(Estimate_Num^-1.0)+",
			paste0(udevID_str, collapse="+")))

ea_mod=glm(ea_form, data=Sip_exp)
summary(ea_mod)


# Map the model coefficients to single values.
t=coef(ea_mod)
exp(t)

# t=glm(Priority ~ HoursActual:HoursEstimate, data=Sip_uTN)
# summary(t)


# P 9
# Investigate serial correlations

plot_layout(2, 1)

# Why should there be any correlation in an unknown quantity?
# Err, tasks are sequenced by size?
lag.plot(Sip_uTN$HoursActual, lag=4, log="xy", col=point_col)

# Does the estimate for the previous project anchor the next estimate?
# Any correlation may be a consequence of task size sequencing.
lag.plot(Sip_uTN$HoursEstimate, lag=4, log="xy", col=point_col)


# Does the accuracy of last estimate impact the accuracy of the next one?
lag.plot(Sip_uTN$HoursEstimate-Sip_uTN$HoursActual, lag=4, log="xy", col=point_col)

Sip_uTN$l_HoursEstimate=log(Sip_uTN$HoursEstimate)

hours_mod=gls(log(HoursActual) ~ l_HoursEstimate, data=Sip_uTN)

# This takes many hours.  So commented out...
# hm=update(hours_mod, correlation=corARMA(c(0.23, -0.9), p=1, q=1))


# How many distinct values cover over 50% of all estimates? 
# t=sort(table(Sip_all_uTN$HoursEstimate), decreasing=TRUE)
# sum(t[1:6])/sum(t)
# [1] 0.536

# P 10

# Use of round numbers

# Est_t=table(Sip_uTN$HoursEstimate)
# Act_t=table(Sip_uTN$HoursActual)
Est_t=count(Sip_uTN$HoursEstimate)
Act_t=count(Sip_uTN$HoursActual)


pal_col=rainbow(2)

plot(Est_t$x, Est_t$freq, log="x",
	xlim=c(0.4, 126),
	xlab="Estimated hours", ylab="Occurrences\n")

Est_m=subset(Est_t, freq > 5)
plot(Est_m$x*1.01, Est_m$freq, log="x", type="h", col=pal_col[1],
	yaxs="i",
	xlim=c(0.4, 126), ylim=c(5, 1700),
	xlab="Hours", ylab="Occurrences\n")

Act_m=subset(Act_t, freq > 5)
points(Act_m$x*0.99, Act_m$freq, type="h", col=pal_col[2])

legend(x="topright", legend=c("Estimate", "Actual"), bty="n", fill=pal_col, cex=1.2)


plot(Act_t$x, Act_t$freq, log="x",
	xlim=c(0.1, 30),
	xlab="Actual hours", ylab="Occurrences\n")


pal_col=rainbow(10)

hx_points=function(hours, col_str)
{
h2=subset(Sip_uTN, HoursEstimate == hours)
lines(100*(1:nrow(h2))/nrow(h2), sort(h2$HoursActual), col=pal_col[col_str])
text(60, hours, hours)
}


plot(0, type="n", log="y",
	xlim=c(1, 100), ylim=c(2e-1, 5e2),
	xlab="Index (normalised)", ylab="Actual\n")
hx_points(1, 1)
hx_points(2, 2)
hx_points(3, 3)
hx_points(7, 4)
hx_points(14, 5)
hx_points(21, 6)
hx_points(28, 7)
hx_points(35, 8)
hx_points(50, 9)
hx_points(100, 10)

# P 11
# Are estimates influenced by the units in which they are made?
# For instance, estimating in hours or days.

estimate_days=7*(1:7)
estimate_hrs=c(10, 15, 20, 25, 30, 40, 50)

days_est=subset(Sip_uTN, HoursEstimate %in% estimate_days)
hrs_est=subset(Sip_uTN, HoursEstimate %in% estimate_hrs)

pred_actual=function(df)
{
mod=glm(log(HoursActual) ~ log(HoursEstimate), data=df)
return(mod)
}

de_mod=pred_actual(days_est)
summary(de_mod)
he_mod=pred_actual(hrs_est)
summary(he_mod)

# P 12

# The HoursEstimated sequence has correlations out to one lag.
# Is this due to the small number of discrete values in the series?

# If the correlation is due to the repetition of a small number
# of distinct values, sampling will not change the series correlation.

plot_layout(1, 2)

Sip_all_uTN=subset(Sip_all, !duplicated(TaskNumber))

acf(Sip_all_uTN$HoursEstimate)
acf(diff(Sip_all_uTN$HoursEstimate))
pacf(Sip_all_uTN$HoursEstimate)
pacf(diff(Sip_all_uTN$HoursEstimate))

library("forecast")

arima(Sip_all_uTN$HoursEstimate)

sample_acf=function()
{
return(acf(sample(Sip_all_uTN$HoursEstimate), lag=3)$acf[, 1, 1])
}


boot_acf=replicate(500, sample_acf())
mean(boot_acf[2,]) # 0.0006537886
mean(boot_acf[3,]) # -0.000295102
sd(boot_acf[3,])   # 0.008559948

# P 13

# Common sequences of estimates

library(ngram)

est_ng=concatenate(as.character(Sip_all_uTN$HoursEstimate))

t=ngram(est_ng, n=2)
n2=head(get.phrasetable(t), n=20)


t=ngram(est_ng, n=3)
n3=head(get.phrasetable(t), n=20)

options(digits=3)
cbind(n2, n3)


t=ngram(est_ng, n=4)
head(get.phrasetable(t), n=10)


# What is the expected number of pairs?
t=sort(table(Sip_all_uTN$HoursEstimate), decreasing=TRUE)
(t[1:5])/sum(t)*((t[1:5]-1)/(sum(t)-1))*sum(t)
#         1         2         7       0.5         3 
# 226.41013  99.67189  96.73765  84.34778  41.73113 

# Triples
(t[1:5])/sum(t)*((t[1:5]-1)/(sum(t)-1))*((t[1:5]-2)/(sum(t)-2))*(sum(t)-1)

# P 14

proj=count(Sip_uTN$ProjectCode)
proj=subset(proj, freq >= 100)
sip_proj=subset(Sip_uTN, ProjectCode %in% proj$x) 

hours_proj_mod=glm(log(HoursActual) ~ l_HoursEstimate+ProjectCode, data=sip_proj)
# summary(hours_proj_mod)

# Project coefficient (need to strip off Intercept and HoursEstimate)
proj_effect=exp(coef(hours_proj_mod))[-c(1, 2)]

pal_col=rainbow(length(proj_effect)) 

x_est=1:20
plot(x_est, x_est, type="l", log="xy", col="black",
        xlab="Estimate", ylab="Actual")

dummy=sapply(1:length(proj_effect),
		function(X) lines(x_est,
			exp(coef(hours_proj_mod)[1])*x_est^coef(hours_proj_mod)[2]*proj_effect[X], col=pal_col[X]))

# P 15

# Range of Estimates for a given Actual

pal_col=rainbow(10)

ha_points=function(hours, col_str)
{
h2=subset(Sip_uTN, HoursActual == hours)
lines(100*(1:nrow(h2))/nrow(h2), sort(h2$HoursEstimate), col=pal_col[col_str])
text(60, hours, hours)
}


plot(0, type="n", log="y",
        xlim=c(1, 100), ylim=c(3e-1, 1e2),
        xlab="Index (normalised)", ylab="Estimate\n")
ha_points(0.75, 1)
ha_points(1, 2)
ha_points(2, 3)
ha_points(2.5, 4)
ha_points(3, 5)
ha_points(4, 6)
ha_points(5, 7)
ha_points(6, 8)
ha_points(7, 9)


# P 16

# Working days patterns

pal_col=rainbow(2)

bankhol=read.csv(paste0(ESEUR_dir, "../articles/SiP/data/ukbankholidays.csv"), as.is=TRUE)
bankhol$UK.BANK.HOLIDAYS=as.Date(bankhol$UK.BANK.HOLIDAYS, format="%d-%b-%Y")

create.calendar("UK", bankhol$UK.BANK.HOLIDAYS, weekdays=c("saturday", "sunday"))

days=exp(seq(0.0, 10, by =0.1))
# There is a bizdays in forecast!
task_dur=bizdays::bizdays(Sip_uTN$StartedOn, Sip_uTN$CompletedOn, "UK")
tab_td=count(task_dur)
tab_td=subset(tab_td, x > 0)

plot(tab_td$x, tab_td$freq, log="xy", col=pal_col[1],
	xlab="Working days", ylab="Tasks\n")
dur_mod=glm(log(freq) ~ log(x), data=tab_td)
summary(dur_mod)
pred=predict(dur_mod, newdata=data.frame(x=days))
lines(days, exp(pred), col=pal_col[1])
 

task_wait=bizdays::bizdays(Sip_uTN$EstimateOn, Sip_uTN$StartedOn, "UK")
tab_tw=count(task_wait)
tab_tw=subset(tab_tw, x > 0)

# plot(tab_tw$x, tab_tw$freq, log="xy", col=pal_col[2],
# 	xlab="Start waiting (working days)", ylab="Tasks\n")
points(tab_tw$x, tab_tw$freq, col=pal_col[2])

# There is a long tail that skews the majority fit, unless truncated
tab_tw=subset(tab_tw, x < 100) # as good a number as any
wait_mod=glm(log(freq) ~ log(x), data=tab_tw)
# wait_mod=glm(log(freq) ~ log(x)+I(log(x)^-1.5), data=tab_tw)
summary(wait_mod)
pred=predict(wait_mod, newdata=data.frame(x=days))
lines(days, exp(pred), col=pal_col[2])

legend(x="topright", legend=c("Start-complete", "Estimate-start"), bty="n", fill=pal_col, cex=1.2)

length(which(Sip_stTN$StartedOn == Sip_stTN$CompletedOn))/nrow(Sip_stTN)
# [1] 0.5141784
length(which(Sip_stTN$StartedOn == Sip_stTN$EstimateOn))/nrow(Sip_stTN)
# [1] 0.889239

# Not a good fit
# library("gnm")
# wait_mod=gnm(freq ~ instances(Mult(1, Exp(log(x))), 2)-1,
#                 data=tab_tw, verbose=TRUE, trace=FALSE,
#                 start=c(400.0, -1.0, 40.0, -0.10))

Sip_dday=subset(Sip_uTN, StartedOn < CompletedOn)
Sip_dday$elapsed=bizdays::bizdays(Sip_dday$StartedOn, Sip_dday$CompletedOn, "UK")
Sip_dday=subset(Sip_dday, elapsed > 0)

plot(Sip_dday$HoursActual, Sip_dday$elapsed, log="xy", col=pal_col[2],
		xlab="Actual hours", ylab="Elapsed workdays\n")

dday_mod=glm(log(elapsed) ~ log(HoursActual)+I(log(HoursActual)^2), data=Sip_dday)
summary(dday_mod)

x_hours=exp(seq(0, 8, by=0.2))
pred=predict(dday_mod, newdata=data.frame(HoursActual=x_hours))
lines(x_hours, exp(pred), col=pal_col[1])

# 7 hour day, a 1-day difference could at most be 2-days work
# Anything to right of line must be a multi-person task
lines(x_hours, x_hours/7-1, col=point_col)

# P 17

# Investigate the impact of day of the week

Sip_uTN$wday=wday(Sip_uTN$StartedOn, label=TRUE) # we want a factor
Sip_uTN$isFriday=(Sip_uTN$wday == "Wed") # The day with the largest significant impact

wday_mod=glm(log(HoursActual) ~ log(HoursEstimate)+
					isFriday+
					ProjectCode
					, data=Sip_uTN)
summary(wday_mod)


# Estimate & Start on the same day...

Sip_uTN$SameDayStart=(Sip_uTN$EstimateOn == Sip_uTN$StartedOn)

sday_mod=glm(log(HoursActual) ~ log(HoursEstimate)+
					SameDayStart+
					ProjectCode
					, data=Sip_uTN)
summary(sday_mod)


# 

# Include elapsed working days in Actual hours model

Sip_dday=subset(Sip_uTN, StartedOn < CompletedOn)
Sip_dday$elapsed=bizdays::bizdays(Sip_dday$StartedOn, Sip_dday$CompletedOn, "UK")
Sip_dday=subset(Sip_dday, elapsed >= 0)
Sip_dday=subset(Sip_dday, elapsed < 200)

hours_elap_mod=glm(log(HoursActual) ~ log(HoursEstimate)+log(elapsed+1e-4)+
					ProjectCode
					, data=Sip_dday)
summary(hours_elap_mod)



# P 18

# Modeling elapsed time

Sip_dday=subset(Sip_uTN, StartedOn < CompletedOn)
Sip_dday$wday=wday(Sip_dday$StartedOn, label=TRUE) # we want a factor
Sip_dday$elapsed=bizdays::bizdays(Sip_dday$StartedOn, Sip_dday$CompletedOn, "UK")
Sip_dday=subset(Sip_dday, elapsed >= 0)

#elap_mod=glm(elapsed ~ HoursEstimate+wday+
elap_mod=glm(log(elapsed+1e-4) ~ log(HoursEstimate)+wday+
					ProjectCode
					, data=Sip_dday)
summary(elap_mod)


# P 19

# Estimates, Starts, Completes per day

pal_col=rainbow(3)

day_mod=function(df, col_str)
{
df=subset(df, x <= 20) # Stop a few extremes skewing the fit
dest_mod=glm(log(freq) ~ x, data=df)
print(summary(dest_mod))
pred=predict(dest_mod)
lines(df$x, exp(pred), col=col_str)
}

day_est=count(count(Sip_uTN$EstimateOn)$freq)
day_starts=count(count(Sip_uTN$StartedOn)$freq)
day_comp=count(count(Sip_uTN$CompletedOn)$freq)

plot(day_est$x, day_est$freq, log="y", col=pal_col[1],
	xlim=c(1, 28), ylim=c(1, 635),
	xlab="Events per day", ylab="Occurrences\n")
points(day_starts$x, day_starts$freq, col=pal_col[2])
points(day_comp$x, day_comp$freq, col=pal_col[3])

day_mod(day_est, pal_col[1])
day_mod(day_starts, pal_col[2])
day_mod(day_comp, pal_col[3])

legend(x="topright", legend=c("Estimates", "Started", "Completed"), bty="n", fill=pal_col, cex=1.2)

# P 20

# Tasks per day, for each developer

# Not interested in tasks started/completed on same day
Sip_SC=subset(Sip, StartedOn < CompletedOn)

task_per_day=function(df)
{
if (nrow(df) < 200)
   return(NULL)
t=sapply(1:nrow(df), function(X)
			{
			t=seq(df$StartedOn[X], df$CompletedOn[X], "days")
			wd=weekdays(t, abbreviate=TRUE) # remove weekends
			return(t[wd != "Sat" & wd != "Sun"])
			})
day_est=count(count(unlist(t))$freq)
}


dev_tasks=ddply(Sip_SC, .(DeveloperID), task_per_day)

u_dev=unique(dev_tasks$DeveloperID)
dev_tasks$Mapped_ID=mapvalues(dev_tasks$DeveloperID, u_dev, 1:length(u_dev))


pal_col=rainbow(length(u_dev))

plot(0.1, type="n", log="y",
	xlim=c(1, 25), ylim=range(dev_tasks$freq),
	xlab="Tasks in progress", ylab=" Days\n")

d_ply(dev_tasks, .(DeveloperID), function(df) lines(df$x, df$freq, col=pal_col[df$Mapped_ID]))

# P 21

# Monthly tasks completed by unique developers

month_totals=function(X)
{
m_data=subset(Sip_uTN, (CompletedOn >= X) & (CompletedOn < X+days_in_month(X)))

devs=length(unique(m_data$DeveloperID))
completed=nrow(m_data)
arith_act=mean(m_data$HoursActual)
geom_act=exp(mean(exp(m_data$HoursActual)))

return(data.frame(devs, completed, arith_act, geom_act, date=X))
}


pal_col=rainbow(3)

# No tasks were completed in July
start_mon=as.Date("2004-08-01")
d_2009=as.Date("2009-01-01")
finish_mon=as.Date("2014-07-01")

mon_start=seq(start_mon, finish_mon, by="month")

mon_info=adply(as.array(mon_start), 1, month_totals)

# ccf(mon_info$devs, mon_info$completed)
# ccf(diff(mon_info$devs), diff(mon_info$completed))
# acf(mon_info$devs)
# pacf(mon_info$devs)
# pacf(mon_info$completed)

plot(start_mon, 1, type="n", log="y",
	xlim=range(mon_start), ylim=c(2, 450),
	xlab="Date", ylab="Occurrences\n")

points(mon_start, mon_info$completed, col=pal_col[2])
points(mon_start, mon_info$devs, col=pal_col[1])
points(mon_start, mon_info$arith_act, col=pal_col[3])

legend(x="topright", legend=c("Unique developers", "Completed tasks", "Mean actual hours"), bty="n", fill=pal_col, cex=1.2)

dc_mod=glm(log(completed) ~ log(devs)*(date < d_2009)+arith_act, data=mon_info)
summary(dc_mod)


# P 22

# Refit estimate/actual model for pre/post 2009

hours_mod=glm(log(HoursActual) ~ log(HoursEstimate)*(StartedOn < d_2009), data=Sip_uTN)
summary(hours_mod)

hours_proj_mod=glm(log(HoursActual) ~ l_HoursEstimate+ProjectCode, data=Sip_uTN)
summary(hours_proj_mod)


# P 23

# Model by regressing on the mean

# hr_mod=rq(log(HoursActual) ~ log(HoursEstimate):(StartedOn < d_2009), data=Sip_uTN)
hr_mod=rq(log(HoursActual) ~ log(HoursEstimate), tau=c(0.25, 0.5, 0.75), data=Sip_uTN)
# Both give errors
summary(hr_mod)
summary(hr_mod, method="iid")

qr_effect=coef(hr_mod)
pal_col=rainbow(ncol(qr_effect)) 

x_est=1:20
plot(x_est, x_est, type="l", log="xy", col="black",
        xlab="Estimate", ylab="Actual")

dummy=sapply(1:ncol(qr_effect),
		function(X) lines(x_est,
			exp(qr_effect[1, X])*x_est^qr_effect[2, X], col=pal_col[X]))

legend(x="bottomright", legend=c("25%", "50%", "75%"), bty="n", fill=rev(pal_col), cex=1.2)

# P 24

# Is there a trend and are there any changepoints?

library("trend")

mk.test(Sip_all_uTN$HoursEstimate)

ch_comp=pettitt.test(mon_info$completed)
mon_info$date[ch_comp$estimate]

ch_devs=pettitt.test(mon_info$devs)
mon_info$date[ch_devs$estimate]

ch_act=pettitt.test(mon_info$arith_act)
mon_info$date[ch_act$estimate]


# P 25

# Number of estimates correct/over/under per quarter/month for various developers

# For a quarter, sum estimates that are correct, over, under actual
dev_month_totals=function(X, df)
{
m_data=subset(df, (CompletedOn >= X) & (CompletedOn < X+days_in_month(X)))

correct=length(which(m_data$HoursEstimate == m_data$HoursActual))
over=length(which(m_data$HoursEstimate > m_data$HoursActual))
under=length(which(m_data$HoursEstimate < m_data$HoursActual))

return(data.frame(correct, over, under, date=X))
}

# Iterate over all quarter and plot result
dev_ok_over_under=function(df, dev_str=df$DeveloperID[1])
{
mon_info=adply(as.array(mon_start), 1, dev_month_totals, df)

plot(mon_info$date, mon_info$correct, type="l", col=pal_col[1], lwd=1.5,
	xaxs="i", yaxs="i",
        xlab="Date", ylab="Total\n")
lines(mon_info$date, mon_info$over, col=pal_col[2], lwd=1.5)
lines(mon_info$date, mon_info$under, col=pal_col[3], lwd=1.5)

legend(x="topright", legend=c("Correct", "Over", "Under"), bty="n", fill=pal_col, cex=1.3)

text(d_2009, max(mon_info$correct, na.rm=TRUE)*0.9,dev_str, cex=2)
}

plot_layout(2, 2)

pal_col=rainbow(3)

# No tasks were completed in July
start_mon=as.Date("2004-08-01")
d_2009=as.Date("2009-01-01")
finish_mon=as.Date("2014-07-01")

mon_start=seq(start_mon, finish_mon, by="quarter")

udevID=unique(Sip_uTN$DeveloperID)
udevID_str=paste0("ID", udevID)
Sip_uTN$Mapped_ID=mapvalues(Sip_uTN$DeveloperID, udevID, 1:length(udevID))


dev_ok_over_under(Sip_uTN, "All")
dev_ok_over_under(subset(Sip_uTN, Mapped_ID == 1))
dev_ok_over_under(subset(Sip_uTN, Mapped_ID == 2), "Stephen")
dev_ok_over_under(subset(Sip_uTN, Mapped_ID == 3))

# dev_ok_over_under(2)
# dev_ok_over_under(10)
# dev_ok_over_under(12)


# P26

# Does the number of estimates having a given duration, change over time?

# Number of estimates per week/month
plot_w_totals=function(est, col_str)
{
hr=subset(week_info, x == est)
points(week_start[as.numeric(hr$X1)], hr$frac, col=col_str)
}


week_totals=function(X)
{
m_data=subset(Sip_uTN, (CompletedOn >= X) & (CompletedOn < X+days_in_month(X)))

t=count(m_data$HoursEstimate)
t$frac=t$freq/nrow(m_data)
# t$date=X

return(subset(t, x %in% 1:10))
}

pal_col=rainbow(10)

# No tasks were completed in July
start_mon=as.Date("2004-08-01")
d_2009=as.Date("2009-01-01")
finish_mon=as.Date("2014-07-01")

week_start=seq(start_mon, finish_mon, by="month")


week_info=adply(as.array(week_start), 1, week_totals)

plot(start_mon, 1e-6, type="n", log="y",
	xaxs="i", yaxs="i",
	xlim=range(week_start), ylim=c(1e-3, 0.6),
        xlab="Date", ylab="Fraction\n")

plot_w_totals(1, pal_col[1])
plot_w_totals(2, pal_col[2])
plot_w_totals(3, pal_col[3])
plot_w_totals(5, pal_col[5])
plot_w_totals(7, pal_col[7])
plot_w_totals(10, pal_col[10])


