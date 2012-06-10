#######################################################################################
#ERAS.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################

#setting environment -------------------------------------------------------------------
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#TODO create mlibrary function to upload many packages and post as gist
lapply(c("ggplot2", "psych", "RCurl", "irr"), library, character.only=T)

#uploading data ------------------------------------------------------------------------
#TODO create package for gsheethttps
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
#see http://goo.gl/mQwxO on how to get this link
#link below won't work until data is entered in the right format

olecranon.data <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVW3PoO2euydHVHcVU2S3BROWRYM29OYWhNTGlMUVE&single=true&gid=5&output=csv")
read.csv(textConnection(olecranon.data))
names(olecranon.data)

#checking data set as a whole
# attach(olecranon.data)
# str (olecranon.data)
# head(olecranon.data)


#notice below that each of the ratings has to be an integer:
str(video)
agree(video)

#additional analyses -- 
data(vision)
bhapkar(vision) # Original example used from Bhapkar (1966). The Bhapkar (1966) test is a more powerful alternative to the Stuart-Maxwell test. Both tests are asymptotically equivalent and will produce comparable chi-squared values when applied a large sample of rated objects. Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.

data(video)
#Ana, 6 below stands by the number of categories in the data set, will have to be replaced by 
finn(video, 6, model="twoway") #Computes the Finn coefficient as an index of the interrater reliability of quantitative data. Addition- ally, F-test and confidence interval are computed. Finn, R.H. (1970). A note on estimating the reliability of categorical data. Educational and Psycho- logical Measurement, 30, 71-76.

data(anxiety)
icc(anxiety, model="twoway", type="agreement") #we will use the default unit=single, no need to add that to the code. see documentation for details. Computes single score or average score ICCs as an index of interrater reliability of quantitative data. Additionally, F-test and confidence interval are computed. Bartko, J.J. (1966). The intraclass correlation coefficient as a measure of reliability. Psychological Reports, 19, 3-11. McGraw, K.O., & Wong, S.P. (1996), Forming inferences about some intraclass correlation co  efficients. Psychological Methods, 1, 30-46. Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.

data(diagnoses)
kappam.fleiss(diagnoses) #Computes Fleiss’ Kappa as an index of interrater agreement between m raters on categorical data. Conger, A.J. (1980). Integration and generalisation of Kappas for multiple raters. Psychological Bulletin, 88, 322-328. Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bul- letin, 76, 378-382. Fleiss, J.L., Levin, B., & Paik, M.C. (2003). Statistical Methods for Rates and Proportions, 3rd Edition. New York: John Wiley & Sons.

#Joao, what are you using to generate Bland-Altman plots?

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
#ERAS.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################

