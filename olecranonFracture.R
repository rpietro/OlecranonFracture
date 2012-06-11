#######################################################################################
#olecranonFracture.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

#setting environment -------------------------------------------------------------------
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#TODO create mlibrary function to upload many packages and post as gist
lapply(c("ggplot2", "psych", "RCurl", "irr"), library, character.only=T)

#Ana, please replace path below with the location of this file on your computer
olecranon.data <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/OlecranonFracture/OlecranonFracture.csv")

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

#Joao, can you get below to work with the variables we have?
bland_altman_plot <- function(x,y,xlab="Average testresult", ylab="Deviation of experimental test")
  {
 x <- #var1, Ana here you should place the first observation
 y <- #var1, Ana here you should place the second observation
 d <- ((X + y)/2)
 diff <- x - y         

 plot(diff ~ d,pch=16,ylim=c(-6,6),xlab=xlab,ylab=ylab)
 abline(h=mean(diff)-c(-1,0,1)*sd(diff),lty=2)
}
bland_altman_plot(d,diff,xlab="Avarage #Name of the variables",ylab="Difference Before and After #Name of the variable")

#######################################################################################
#olecranonFracture.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################