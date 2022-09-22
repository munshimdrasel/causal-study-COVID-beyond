rm(list = ls())

library(fst)
library(data.table)
library(tidyverse)
library(cowplot)

setwd ("/Volumes/GoogleDrive/My Drive/R/causal-study-COVID-beyond")

#nox regression

nox.facility <- as.data.table(read.fst( "data/all.facility.nox.2020.fst"))

nox.facility <- nox.facility %>% filter (week>8)

nox.facility<- setDT(nox.facility)[, .(TTT = sum(ATT, na.rm=TRUE),
                                       actual.nox.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.nox.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, NOx.Control.s.,NOx.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,NOx.Phase,NOx.Phase)]





#subtracting mean ATT from total treatment on treated 
nox.facility$ttt.mean <- nox.facility$TTT-mean(nox.facility$TTT)

#primary fuel
nox.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

nox.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
nox.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
nox.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#nox control
nox.facility[, Has.NOx.Control := 1]
nox.facility[NOx.Control.s. == "", Has.NOx.Control := 0]

#PM control
nox.facility[, Has.PM.Control := 1]
nox.facility[PM.Control.s. == "", Has.PM.Control := 0]

#Hg control
nox.facility[, Has.Hg.Control := 1]
nox.facility[Hg.Control.s. == "", Has.Hg.Control := 0]

# nox.facility$Fuel1 <- "Others"
# nox.facility$Fuel1 [ nox.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
# nox.facility$Fuel1 [ grepl("Natural Gas", nox.facility$Fuel.Type..Primary.)] <- "Natural Gas"


#Fuel control
# nox.facility$nox.control <- nox.facility$nox.Control.s.
# so2.facility$so2.control <- gsub("<br.*","",so2.facility$so2.control)
# so2.facility$so2.control.tec <- "Others"
# so2.facility$so2.control.tec [grepl("Lime", so2.facility$so2.control)] <- "Lime FGD"
# so2.facility$so2.control.tec [grepl("Limestone", so2.facility$so2.control)] <- "Limestone Treatment"




nox.facility$EPA.Region <- as.factor(nox.facility$EPA.Region)
# so2.facility$Fuel1.IsCoal <- as.factor(so2.facility$Fuel1.IsCoal)
# so2.facility$Fuel1.IsNatGas <- as.factor(so2.facility$Fuel1.IsNatGas)
# so2.facility$Fuel1.IsOthers <- as.factor(so2.facility$Fuel1.IsOthers)
# so2.facility$Fuel1 <- as.factor(so2.facility$Fuel1)
# so2.facility$so2.control.tec <- as.factor(so2.facility$so2.control.tec)

eqn <- ttt.mean ~ -1 +EPA.Region + Fuel1.IsCoal+Fuel1.IsNatGas+ Fuel1.IsOthers +Has.NOx.Control 
r.lm <- lm(eqn, data = nox.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

lm_df <- data.frame("Variable" = c( "EPA.Region1","EPA.Region2", "EPA.Region3","EPA.Region4","EPA.Region5","EPA.Region6","EPA.Region7","EPA.Region8","EPA.Region9","EPA.Region10",
                                    "Fuel1.IsCoal", "Fuel1.IsNatGas" , "Fuel1.IsOthers", "Has.NOx.Control"),
                    "estimate" = res$coefficients[,1], "ci0.025" = conf[,1], "ci0.975" = conf[,2])


# lm_df <- lm_df %>% mutate(Variable = recode(Variable, "Fuel1Natural Gas" = 'Natural Gas', "Fuel1Others" = "Others Fuel",
#                                   "so2.control.tecLimestone Treatment"= "Limestone Treatment", "so2.control.tecOthers"= "Others Control" ))

ggplot() +
  geom_pointrange(lm_df, mapping = aes(x = Variable, y = estimate, 
                                       ymin = ci0.025, ymax = ci0.975)) + coord_flip() +
  geom_hline(yintercept = 0)


#so2 regression



so2.facility <- as.data.table(read.fst( "data/all.facility.so2.2020.fst"))

so2.facility <- so2.facility %>% filter (week>8)

so2.facility<- setDT(so2.facility)[, .(TTT = sum(ATT, na.rm=TRUE),
                                       actual.so2.emis = sum(Y.tr.bar, na.rm=T),
                                       ct.so2.emis = sum(Y.ct.bar, na.rm=T)), 
                                   by = .(facility, STATE,Fuel.Type..Primary.,Facility.Latitude,Facility.Longitude,
                                          Source.Category, Unit.Type, SO2.Control.s.,SO2.Control.s.,
                                          PM.Control.s., Hg.Control.s., EPA.Region, NERC.Region,
                                          Associated.Stacks,Program.s.,SO2.Phase,SO2.Phase)]

#subtracting mean ATT from total treatment on treated 
so2.facility$ttt.mean <- so2.facility$TTT--mean(so2.facility$TTT)

#primary fuel
so2.facility[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

so2.facility[, Fuel1.IsNatGas := as.numeric(grepl("Natural Gas", Fuel.Type..Primary.))]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsNatGas  := NA]

other.fuel <- c("Other Gas", "Diesel Oil", "Wood", "Process Gas", "Residual Oil", "Petroleum Coke",
                "Other Oil") 
so2.facility[, Fuel1.IsOthers := as.numeric( Fuel.Type..Primary. %in% other.fuel)]
so2.facility[Fuel.Type..Primary. == "", Fuel1.IsOthers  := NA]

#so2 control
so2.facility[, Has.SO2.Control := 1]
so2.facility[SO2.Control.s. == "", Has.SO2.Control := 0]

#PM control
so2.facility[, Has.PM.Control := 1]
so2.facility[PM.Control.s. == "", Has.PM.Control := 0]

#Hg control
so2.facility[, Has.Hg.Control := 1]
so2.facility[Hg.Control.s. == "", Has.Hg.Control := 0]

# so2.facility$Fuel1 <- "Others"
# so2.facility$Fuel1 [ so2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
# so2.facility$Fuel1 [ grepl("Natural Gas", so2.facility$Fuel.Type..Primary.)] <- "Natural Gas"


#Fuel control
# so2.facility$so2.control <- so2.facility$SO2.Control.s.
# so2.facility$so2.control <- gsub("<br.*","",so2.facility$so2.control)
# so2.facility$so2.control.tec <- "Others"
# so2.facility$so2.control.tec [grepl("Lime", so2.facility$so2.control)] <- "Lime FGD"
# so2.facility$so2.control.tec [grepl("Limestone", so2.facility$so2.control)] <- "Limestone Treatment"




so2.facility$EPA.Region <- as.factor(so2.facility$EPA.Region)
# so2.facility$Fuel1.IsCoal <- as.factor(so2.facility$Fuel1.IsCoal)
# so2.facility$Fuel1.IsNatGas <- as.factor(so2.facility$Fuel1.IsNatGas)
# so2.facility$Fuel1.IsOthers <- as.factor(so2.facility$Fuel1.IsOthers)
# so2.facility$Fuel1 <- as.factor(so2.facility$Fuel1)
# so2.facility$so2.control.tec <- as.factor(so2.facility$so2.control.tec)

eqn <- ttt.mean ~ -1 +EPA.Region + Fuel1.IsCoal+Fuel1.IsNatGas+ Fuel1.IsOthers +Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1
#controls
r.lm <- lm(eqn, data = so2.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

lm_df <- data.frame("Variable" = c( "EPA.Region1","EPA.Region2", "EPA.Region3","EPA.Region4","EPA.Region5","EPA.Region6","EPA.Region7","EPA.Region8","EPA.Region9","EPA.Region10",
                                    "Fuel1.IsCoal", "Fuel1.IsNatGas" , "Fuel1.IsOthers", "Has.SO2.Control"),
                    "estimate" = res$coefficients[,1], "ci0.025" = conf[,1], "ci0.975" = conf[,2])

# lm_df <- lm_df %>% mutate(Variable = recode(Variable, "Fuel1Natural Gas" = 'Natural Gas', "Fuel1Others" = "Others Fuel",
#                                   "so2.control.tecLimestone Treatment"= "Limestone Treatment", "so2.control.tecOthers"= "Others Control" ))

ggplot() +
  geom_pointrange(lm_df, mapping = aes(x = Variable, y = estimate, 
                                       ymin = ci0.025, ymax = ci0.975)) + coord_flip() +
  geom_hline(yintercept = 0)



#==================================Regression test 2=======================

so2.facility$Fuel <- "Others"
so2.facility$Fuel [ so2.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
so2.facility$Fuel [ grepl("Natural Gas", so2.facility$Fuel.Type..Primary.)] <- "Natural Gas"

so2.facility$Fuel <- as.factor(so2.facility$Fuel)

#SO2
eqn <- TTT ~ -1 +EPA.Region * Fuel  *Has.SO2.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- TTT ~ -1 + EPA.Region : Fuel  : Has.SO2.Control
#controls
r.lm <- lm(eqn, data = so2.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

x <- as.data.frame(res$coefficients)
x <- setNames(cbind(rownames(x), x, row.names = NULL), 
         c("variable", "estimate", "Std. Error" ,"t value"  ,  "Pr(>|t|)"))

y <- as.data.frame(conf)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
              c("variable"  , "2.5 %" ,"97.5 %"))
y <- na.omit(y)

variable <- as.vector(x$variable)

lm_df <- data.frame("Variable" = variable,
                    "estimate" = x[,2], "ci0.025" = y[,2], "ci0.975" = y[,3])

so2_df <- lm_df

so2_df <- so2_df %>% 
  mutate(Variable = recode(Variable, "EPA.Region2:FuelCoal:Has.SO2.Control" = 'Region 2:Coal',
                           "EPA.Region3:FuelCoal:Has.SO2.Control" = 'Region 3:Coal',
                           "EPA.Region4:FuelCoal:Has.SO2.Control" = 'Region 4:Coal',
                           "EPA.Region5:FuelCoal:Has.SO2.Control" = 'Region 5:Coal',
                           "EPA.Region6:FuelCoal:Has.SO2.Control" = 'Region 6:Coal',
                           "EPA.Region7:FuelCoal:Has.SO2.Control" = 'Region 7:Coal',
                           "EPA.Region8:FuelCoal:Has.SO2.Control" = 'Region 8:Coal',
                           "EPA.Region9:FuelCoal:Has.SO2.Control" = 'Region 9:Coal',
                           "EPA.Region10:FuelCoal:Has.SO2.Control" = 'Region 10:Coal',
                           "EPA.Region2:FuelNatural Gas:Has.SO2.Control" = 'Region 2:Natural Gas',
                           "EPA.Region3:FuelNatural Gas:Has.SO2.Control" = 'Region 3:Natural Gas',
                           "EPA.Region4:FuelNatural Gas:Has.SO2.Control" = 'Region 4:Natural Gas',
                           "EPA.Region5:FuelNatural Gas:Has.SO2.Control" = 'Region 5:Natural Gas',
                           "EPA.Region6:FuelNatural Gas:Has.SO2.Control" = 'Region 6:Natural Gas',
                           "EPA.Region7:FuelNatural Gas:Has.SO2.Control" = 'Region 7:Natural Gas',
                           "EPA.Region8:FuelNatural Gas:Has.SO2.Control" = 'Region 8:Natural Gas',
                           "EPA.Region9:FuelNatural Gas:Has.SO2.Control" = 'Region 9:Natural Gas',
                           "EPA.Region1:FuelOthers:Has.SO2.Control" = 'Region 2:Others',
                           "EPA.Region2:FuelOthers:Has.SO2.Control" = 'Region 2:Others',
                           "EPA.Region3:FuelOthers:Has.SO2.Control" = 'Region 3:Others',
                           "EPA.Region4:FuelOthers:Has.SO2.Control" = 'Region 4:Others',
                           "EPA.Region5:FuelOthers:Has.SO2.Control" = 'Region 5:Others',
                           "EPA.Region6:FuelOthers:Has.SO2.Control" = 'Region 6:Others',
                           "EPA.Region7:FuelOthers:Has.SO2.Control" = 'Region 7:Others',
                           "EPA.Region8:FuelOthers:Has.SO2.Control" = 'Region 8:Others',
                           "EPA.Region9:FuelOthers:Has.SO2.Control" = 'Region 9:Others'))

so2_df$group[grepl("Coal", so2_df$Variable)] <- "Coal"
so2_df$group[grepl("Natural Gas", so2_df$Variable)] <- "Natural Gas"
so2_df$group[grepl("Others", so2_df$Variable)] <- "Others"
# gsub('SO2', expression(paste(SO["2"])), name)
# 
# str_replace_all( so2$Variable,"SO2", paste("SO"))
# 
# 
# so2_df$x[grepl("SO2", so2_df$Variable)] <- "Others"

# lm_df <- lm_df %>% mutate(Variable = recode(Variable, "Fuel1Natural Gas" = 'Natural Gas', "Fuel1Others" = "Others Fuel",
#                                   "so2.control.tecLimestone Treatment"= "Limestone Treatment", "so2.control.tecOthers"= "Others Control" ))

so2.regression <- so2_df %>%  group_by (group) %>%   ggplot(aes(color=group)) +
  geom_pointrange( mapping = aes(x = Variable, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975)) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.title = element_blank(),
                                      legend.position = "none",
                                      axis.title.y = element_blank()) +
  labs(y=expression( paste("coefficients: facilities having ",SO["2"], " control")))

# ggsave("regression_so2_2020.png", path = "./plots/")


#==========================NOx
nox.facility$Fuel <- "Others"
nox.facility$Fuel [ nox.facility$Fuel.Type..Primary.=="Coal"] <- "Coal"
nox.facility$Fuel [ grepl("Natural Gas", nox.facility$Fuel.Type..Primary.)] <- "Natural Gas"

nox.facility$Fuel <- as.factor(nox.facility$Fuel)

nox.facility$Has.NOx.Control[nox.facility$Has.NOx.Control==1] <- "Has NOx Control"
nox.facility$Has.NOx.Control[nox.facility$Has.NOx.Control==0] <- "Has No NOx Control"

nox.facility$Has.NOx.Control <- as.factor(nox.facility$Has.NOx.Control)

# eqn <- TTT ~ -1 +EPA.Region * Fuel  *Has.NOx.Control #+ Unit.Type +NOx.Phase + Source.Category #without intercept -1

eqn <- TTT ~ -1 + EPA.Region : Fuel  : Has.NOx.Control

# eqn <- TTT ~ -1 +EPA.Region : Fuel1.IsCoal : Fuel1.IsNatGas : Fuel1.IsOthers  : Has.SO2.Control
#controls
r.lm <- lm(eqn, data = nox.facility)

res <- summary(r.lm)
conf <- confint(r.lm)

x <- as.data.frame(res$coefficients)
x <- setNames(cbind(rownames(x), x, row.names = NULL), 
              c("variable", "estimate", "Std. Error" ,"t value"  ,  "Pr(>|t|)"))

y <- as.data.frame(conf)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
              c("variable"  , "2.5 %" ,"97.5 %"))
y <- na.omit(y)

variable <- as.vector(x$variable)

lm_df <- data.frame("Variable" = variable,
                    "estimate" = x[,2], "ci0.025" = y[,2], "ci0.975" = y[,3])

nox_df <- lm_df
# lm_df <- lm_df %>% mutate(Variable = recode(Variable, "Fuel1Natural Gas" = 'Natural Gas', "Fuel1Others" = "Others Fuel",
#                                   "so2.control.tecLimestone Treatment"= "Limestone Treatment", "so2.control.tecOthers"= "Others Control" ))
nox_df <- nox_df %>% 
  mutate(Variable = recode(Variable, 
                           "EPA.Region1:FuelCoal:Has.NOx.Control" = 'Region 1:Coal',
                           "EPA.Region2:FuelCoal:Has.NOx.Control" = 'Region 2:Coal',
                           "EPA.Region3:FuelCoal:Has.NOx.Control" = 'Region 3:Coal',
                           "EPA.Region4:FuelCoal:Has.NOx.Control" = 'Region 4:Coal',
                           "EPA.Region5:FuelCoal:Has.NOx.Control" = 'Region 5:Coal',
                           "EPA.Region6:FuelCoal:Has.NOx.Control" = 'Region 6:Coal',
                           "EPA.Region7:FuelCoal:Has.NOx.Control" = 'Region 7:Coal',
                           "EPA.Region8:FuelCoal:Has.NOx.Control" = 'Region 8:Coal',
                           "EPA.Region9:FuelCoal:Has.NOx.Control" = 'Region 9:Coal',
                           "EPA.Region10:FuelCoal:Has.NOx.Control" = 'Region 10:Coal',
                           "EPA.Region1:FuelNatural Gas:Has.NOx.Control" = 'Region 1:Natural Gas',
                           "EPA.Region2:FuelNatural Gas:Has.NOx.Control" = 'Region 2:Natural Gas',
                           "EPA.Region3:FuelNatural Gas:Has.NOx.Control" = 'Region 3:Natural Gas',
                           "EPA.Region4:FuelNatural Gas:Has.NOx.Control" = 'Region 4:Natural Gas',
                           "EPA.Region5:FuelNatural Gas:Has.NOx.Control" = 'Region 5:Natural Gas',
                           "EPA.Region6:FuelNatural Gas:Has.NOx.Control" = 'Region 6:Natural Gas',
                           "EPA.Region7:FuelNatural Gas:Has.NOx.Control" = 'Region 7:Natural Gas',
                           "EPA.Region8:FuelNatural Gas:Has.NOx.Control" = 'Region 8:Natural Gas',
                           "EPA.Region9:FuelNatural Gas:Has.NOx.Control" = 'Region 9:Natural Gas',
                           "EPA.Region10:FuelNatural Gas:Has.NOx.Control" = 'Region 10:Natural Gas',
                           "EPA.Region1:FuelOthers:Has.NOx.Control" = 'Region 2:Others',
                           "EPA.Region2:FuelOthers:Has.NOx.Control" = 'Region 2:Others',
                           "EPA.Region3:FuelOthers:Has.NOx.Control" = 'Region 3:Others',
                           "EPA.Region4:FuelOthers:Has.NOx.Control" = 'Region 4:Others',
                           "EPA.Region5:FuelOthers:Has.NOx.Control" = 'Region 5:Others',
                           "EPA.Region6:FuelOthers:Has.NOx.Control" = 'Region 6:Others',
                           "EPA.Region7:FuelOthers:Has.NOx.Control" = 'Region 7:Others',
                           "EPA.Region8:FuelOthers:Has.NOx.Control" = 'Region 8:Others',
                           "EPA.Region9:FuelOthers:Has.NOx.Control" = 'Region 9:Others',
                           "EPA.Region10:FuelOthers:Has.NOx.Control" = 'Region 10:Others'))

nox_df$group[grepl("Coal", nox_df$Variable)] <- "Coal"
nox_df$group[grepl("Natural Gas", nox_df$Variable)] <- "Natural Gas"
nox_df$group[grepl("Others", nox_df$Variable)] <- "Others"


common.variable <- intersect(so2_df$Variable,nox_df$Variable)


nox.regression <- nox_df %>% filter(Variable%in% common.variable) %>% 
  group_by (group) %>%   ggplot(aes(color=group)) +
  geom_pointrange( mapping = aes(x = Variable, y = estimate, 
                                 ymin = ci0.025, ymax = ci0.975)) + coord_flip() +
  geom_hline(yintercept = 0) + theme( legend.position = c(0.2,0.4),
                                      axis.title.y = element_blank(),
                                      axis.text.y=element_blank()) +
  labs(y=expression( paste("coefficients: facilities having ",NO[X], " control")), color="Fuel Types") 

plot_grid(so2.regression, 
          nox.regression, 
          labels = c('A', 'B'), 
          label_size = 12)

ggsave("regression_nox_so2_2020.png", path = "./plots/", width=8, height=4, units="in")


# ggsave("regression_nox_2020.png", path = "./plots/")
