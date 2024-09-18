library(tidyverse)
library(KMsurv)
library(survival)
library(kableExtra)
library(survminer)

data(kidney, package = "KMsurv")
head(kidney) |> kbl() 

myTable <- as.data.frame(table(kidney$type))
colnames(myTable) <- c("Type", "Frequency")
myTable |> kbl() 

#change data format to character
kidney$type <- as.character(kidney$type)

#replace entries
kidney$type[kidney$type=="1"] <- "Group 1"
kidney$type[kidney$type=="2"] <- "Group 0"

myTable <- as.data.frame(table(kidney$type))
colnames(myTable) <- c("Group", "Frequency")
myTable |> kbl()

S.data <- Surv(kidney$time, kidney$delta)
km.fit <- survfit(S.data ~ 1,
                  data = kidney,
                  type = "kaplan-meier",
                  conf.type="plain",
                  subset = (type=="Group 1"))

ggsurvplot(km.fit,
           data = kidney,
           legend.labs = c("Group 0", "Group 1"),
           ggtheme = theme_bw(),
           conf.int = T,
           conf.int.style = "step")

kidneyKM <- kidney |>
  filter(type=="Group 1") |> 
  group_by(t_i = time) |>
  summarize(d_i = sum(delta),
            totalEvents = n()) |> 
  mutate(y_i = rev(cumsum(rev(totalEvents))),
         sHat = cumprod(1-(d_i/y_i)),
         #sTilde = cumprod(exp(-d_i/y_i)),
         standardErrorPlain = sqrt(sHat*(1-sHat)/y_i[1]))
