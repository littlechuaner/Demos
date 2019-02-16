data <- read.csv(file.choose(),header = TRUE)
###Check data
summary(data)
###Adjust data type as numeric and character.
syn_control <- within(data, { UnemploymentRate <- as.numeric(as.character(UnemploymentRate)) 
                              GovernmentRevenue <- as.numeric(as.character(GovernmentRevenue))
                              CurrentAccountBalance <- as.numeric(as.character(CurrentAccountBalance))
                              RegionName <- as.character(RegionName)})
###Recheck adjusted data
summary(syn_control)
###load package
library(Synth)
###Prepare data before fitting.
dataprep.out <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 8,
  controls.identifier = c(1:7,9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###Fit the model
synth.out <- synth(dataprep.out)
###Prediction table of fitting.
synth.tables <- synth.tab(dataprep.res = dataprep.out,synth.res = synth.out)
synth.tables$tab.pred
###Plot the path before treatment
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "GDP Growth Rate", Xlab = "Year",
          tr.intake = 1997,
          Ylim = c(-20,20),
          Main = "Figure 1. Trends in GDP Growth Rate in the pre-treatment period",
          Legend = c("Hong Kong","Synthetic Hong Kong"), Legend.position = "bottomright",
          Z.plot = TRUE)
###Plot the path for the whole time period
path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "GDP Growth Rate", Xlab = "Year",
          tr.intake = 1997,
          Ylim = c(-20,20),
          Main = "Figure 2. Trends in GDP Growth Rate between 1980 and 2018",
          Legend = c("HongKong","synthetic HongKong"), Legend.position = "bottomright",
          Z.plot = FALSE)
###Get the gap and plot the gap for the whole period.
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
          Ylab = "Gap", 
          Xlab = "Year",
          tr.intake = 1997,
          Main = "Figure 3. GDP Growth Rate gap")
###Time Placebo at 1988,leaving out region no.2 and 4 because lack of data.
dataprep.out2 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1988,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 8,
  controls.identifier = c(1,5:7),
  time.optimize.ssr = 1980:1988,
  time.plot = 1980:2018)
###Fit the model
synth.out <- synth(dataprep.out2)
###Plot gap for time placebo
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out2,
          Ylab = "Gap", 
          Xlab = "Year",
          tr.intake = 1989,
          Main = "Figure 4. GDP Growth Rate gap")
###Region placebo, get all other regions' gap.
dataprep.out1 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 1,
  controls.identifier = c(2:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out1 <- synth(dataprep.out1)
###Region no.1's gap
gaps1 <- dataprep.out1$Y1plot - (dataprep.out1$Y0plot %*% synth.out1$solution.w)
###
dataprep.out2 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 2,
  controls.identifier = c(1,3:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out2 <- synth(dataprep.out2)
###Region no.2's gap
gaps2 <- dataprep.out2$Y1plot - (dataprep.out2$Y0plot %*% synth.out2$solution.w)
###
dataprep.out3 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 3,
  controls.identifier = c(1:2,4:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out3 <- synth(dataprep.out3)
###Region no.3's gap
gaps3 <- dataprep.out3$Y1plot - (dataprep.out3$Y0plot %*% synth.out3$solution.w)
###
dataprep.out4 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 4,
  controls.identifier = c(1:3,5:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out4 <- synth(dataprep.out4)
###Region no.4's gap
gaps4 <- dataprep.out4$Y1plot - (dataprep.out4$Y0plot %*% synth.out4$solution.w)
###
dataprep.out5 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 5,
  controls.identifier = c(1:4,6:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out5 <- synth(dataprep.out5)
###Region no.5's gap
gaps5 <- dataprep.out5$Y1plot - (dataprep.out5$Y0plot %*% synth.out5$solution.w)
###
dataprep.out6 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 6,
  controls.identifier = c(1:5,7:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out6 <- synth(dataprep.out6)
###Region no.6's gap
gaps6 <- dataprep.out6$Y1plot - (dataprep.out6$Y0plot %*% synth.out6$solution.w)
###
dataprep.out7 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 7,
  controls.identifier = c(1:6,8:9),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out7 <- synth(dataprep.out7)
###Region no.7's gap
gaps7 <- dataprep.out7$Y1plot - (dataprep.out7$Y0plot %*% synth.out7$solution.w)
###
dataprep.out9 <- dataprep(
  foo = syn_control,
  predictors = c("Total.Investment","GrossNationalSavings","Inflation","UnemploymentRate",
                 "GovernmentRevenue","CurrentAccountBalance"),
  predictors.op = "mean",
  time.predictors.prior = 1980:1996,
  dependent = "GDPGrowth",
  unit.variable = "RegionNo",
  unit.names.variable = "RegionName",
  time.variable = "Year",
  treatment.identifier = 9,
  controls.identifier = c(1:8),
  time.optimize.ssr = 1980:1996,
  time.plot = 1980:2018)
###
synth.out9 <- synth(dataprep.out9)
###Region no.9's gap
gaps9 <- dataprep.out9$Y1plot - (dataprep.out9$Y0plot %*% synth.out9$solution.w)
###Plot all the gaps together
par(mfrow=c(1,1))
time <- c(1980:2017,1)
plot(time,gaps,type='l',xlim = c(1980,2016),
     ylim = c(-20,20),
     lty = 1,
     main = "Figure.5 Region Placebo Gap Plot",
     xlab = "Year",
     ylab = "Gaps")
lines(time,gaps1,col = "grey",lty = 1)
lines(time,gaps2,col = "grey",lty = 1)
lines(time,gaps3,col = "grey",lty = 1)
lines(time,gaps4,col = "grey",lty = 1)
lines(time,gaps5,col = "grey",lty = 1)
lines(time,gaps6,col = "grey",lty = 1)
lines(time,gaps7,col = "grey",lty = 1)
lines(time,gaps9,col = "grey",lty = 1)
legend(2005,-10, cex = 0.6,c("Hong Kong","Other Regions"), col = c("black","grey"),lty=c(1,1))