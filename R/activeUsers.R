#install.packages(c("devtools","RGA","RGoogleAnalytics","RJSONIO"))
install.packages("tidyverse")
library(RGA)
#library(lubridate)
#library(RGoogleAnalytics)
install.packages("RGoogleAnalytics")
library(RGoogleAnalytics)
library(RJSONIO)
#library(stats)

getActiveUsers <- function() {
	profile.id <- "79525860" ## Google Analytics View ID (IP Filtered)
	profileUnfiltered.id <- "118039351" ## (Unfiltered)
	client.id <- "419497916126-3de9a34jhunanqtlnbcfm09uger88dqb.apps.googleusercontent.com"
	client.secret <- "L32vmz6aJ_76PfIUgg2RHuSf"

	retn_list <- list("profile id" = profile.id,
	                  "profile id unfiltered" = profileUnfiltered.id,
	                  "client id"= client.id,
	                  "client.secret" = client.secret)

	return(retn_list)
}


