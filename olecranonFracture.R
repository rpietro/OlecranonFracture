#######################################################################################
#olecranonFracture.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

#######################################
#Joao is testing RCurl - NAO PRECISA RODAR ISSO ANA!!!
#uploading data ------------------------------------------------------------------------
#Functions to pull the dara from the internet file 
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
#see http://goo.gl/mQwxO on how to get this link
#link below won't work until data is entered in the right format
data <- getURL("https://docs.google.com/spreadsheet/pub?hl=en&hl=en&key=0AoTReYGK49h_dFhMN3FRMUpTb01qbTVJcU9SMGktNXc&single=true&gid=0&output=csv")
test<-read.csv(textConnection(data))
attach(test)
str (test)
head(test)
names(test)
olecranon_day1 <- c(rt1speccoltd1, rt1specschatd1) #Ana, please add the variables that constitute all raters for day one
olecranon_day30 <- c(rt1speccoltd30, rt1specschatd30)
icc(olecranon_day1vsday30, model="twoway", type="agreement") #we will use the default unit=single, no need to add that to the code. see documentation for details. Computes single score or average score ICCs as an index of interrater reliability of quantitative data. Additionally, F-test and confidence interval are computed. Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11. McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation co  efficients. Psychological Methods, 1, 30-46. Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.
kappam.fleiss(olecranon_day1vsday30)
########################################

#setting environment -------------------------------------------------------------------
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#Ricardo, this command is not working, but the other one below does. I believe it has something to do with the install.package call, in the other you used library.
#lapply(c("ggplot2", "psych", "RCurl", "irr", "car","Hmisc", "gmodels", "DAAG", "ResearchMethods"), install.packages, character.only=T)

#TODO create mlibrary function to upload many packages and post as gist
lapply(c("ggplot2", "psych", "RCurl", "irr", "ResearchMethods"), library, character.only=T)

#below is link for Ricardo:
olecranon.data <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/OlecranonFracture/OlecranonFracture.csv", header=T)

#Ana, please replace path below with the location of this file on your computer
#olecranon.data <- read.csv("/Users/user/Google Drive/Projeto_Duke/Olecranon/OlecranonFracture.csv", header=T)

#checking data set as a whole
attach(olecranon.data)
str (olecranon.data)
head(olecranon.data)
names(olecranon.data)
#command below will allow you to view the data in a sheet format - just remove the comment (#) and run it when you would like to do that
#View(olecranon.data)

# start by creating a data.frame (table with all the data you will use for a given analysis)


#Ana, here is the way this works: (1)you create two vectors with the two sets of variables you want to compare, make sure you make the sequence completely parallel. below is an example with just two variables: 
olecranon_day1 <- c(rt1speccoltd1, rt1specschatd1) #Ana, please add the variables that constitute all raters for day one
olecranon_day30 <- c(rt1speccoltd30, rt1specschatd30) #now add the ones for day 30
#below all you are doing is to create a table putting all the variables together
olecranon_day1vsday30 <- data.frame(olecranon_day1, olecranon_day30)
#below you simply compare them in terms of frequency of agreement
agree(olecranon_day1vsday30, tolerance=0) #Ana, I would not change the tolerance since the data were not coded appropriately for this
bhapkar(olecranon_day1vsday30) # Original example used from Bhapkar (1966). The Bhapkar (1966) test is a more powerful alternative to the Stuart-Maxwell test. Both tests are asymptotically equivalent and will produce comparable chi-squared values when applied a large sample of rated objects. Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.
#Ana, 6 below stands by the number of categories in the data set, will have to be replaced by 
finn(olecranon_day1vsday30, 6, model="twoway") #Computes the Finn coefficient as an index of the interrater reliability of quantitative data. Addition- ally, F-test and confidence interval are computed. Finn, R.H. (1970). A note on estimating the reliability of categorical data. Educational and Psycho- logical Measurement, 30, 71-76.
icc(olecranon_day1vsday30, model="twoway", type="agreement") #we will use the default unit=single, no need to add that to the code. see documentation for details. Computes single score or average score ICCs as an index of interrater reliability of quantitative data. Additionally, F-test and confidence interval are computed. Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11. McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation co  efficients. Psychological Methods, 1, 30-46. Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.
kappam.fleiss(olecranon_day1vsday30) #Computes Fleiss’ Kappa as an index of interrater agreement between m raters on categorical data. Conger, A.J. (1980). Integration and generalisation of Kappas for multiple raters. Psychological Bulletin, 88, 322-328. Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bul- letin, 76, 378-382. Fleiss, J.L., Levin, B., & Paik, M.C. (2003). Statistical Methods for Rates and Proportions, 3rd Edition. New York: John Wiley & Sons.

#The function below is set for the example of olecranon_day1 Vs olecranon_day30. It will give the blandaltman plot.
bland_altman_plot <- function(x,y,xlab="Average testresult", ylab="Deviation of experimental test")
  {
 x <- olecranon_day1  #var1, Ana here you should place the first observation to be compared
 y <- olecranon_day30 #var2, Ana here you should place the second observation to be compared
 d <- ((olecranon_day1 + olecranon_day30)/2) # Here the functino is calculating the avareg value of the paired observations, so just change the formule according to this sequence (1o observation + 2o Observation)
 diff <- x - y  # This is calculating the difference between paired observations       
 #The plot command will give you the blanaltamn plot. 
 #The "ylim" cammand sets the limit for y axis, here is set to -6 to 6, but you can change if you want to ge a broader range.
 #The "xlim"  command sets the limit for x axis.
 plot(diff ~ d,pch=16,ylim=c(-6,6),xlim=c(1,6),xlab=xlab,ylab=ylab)
 abline(h=mean(diff)-c(-1,0,1)*sd(diff),lty=2)
 # The abline command will define the range for the agreement interval. Here it is set to 1 Standard Deviation + and 1 -, but you can change the range. See for genral information http://goo.gl/sv4kd and here http://goo.gl/IUbSJ for theoretical information about the plot. 
}
bland_altman_plot(d,diff,xlab="Avarage",ylab="Difference Day1 to Day30")

#Ana, no need to use this at this point
#BlandAltman(olecranon_day1, olecranon_day30,gui=TRUE, bandsOn=FALSE, biasOn=FALSE, regionOn=FALSE, smooth=FALSE, sig=2)

#######################################################################################
#olecranonFracture.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################