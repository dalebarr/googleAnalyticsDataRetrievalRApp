install.packages((c("devtools","lubridate","RGoogleAnalytics","RJSONIO","stats")))
require(RGA)
require(lubridate)
require(RGoogleAnalytics)
require(devtools)
require(RJSONIO)
require(stats)

getActiveUsers <- function() {
	profile.id <- "79525860" ## Google Analytics View ID (IP Filtered)
	profileUnfiltered.id <- "118039351" ## (Unfiltered)
	client.id <- "419497916126-3de9a34jhunanqtlnbcfm09uger88dqb.apps.googleusercontent.com"
	client.secret <- "L32vmz6aJ_76PfIUgg2RHuSf"

	## Setup date period
	## Get current week
	weekRange <- seq(Sys.Date() - 10, Sys.Date(), by = 1)
	Sun <- weekRange[grepl("Sun", weekdays(weekRange))]
	wkNum <- length(Sun)
	wkDate <- Sun[wkNum]

	## Get current year
	dtNow <- as.Date(Sys.Date(), '%m/%d/%Y')
	yrNow <- as.numeric(format(dtNow,'%Y'))

	## Authorise token
	token <- Auth(client.id, client.secret)

	authorize(username = "cashflowfunding.co.nz@gmail.com",
			client.id,
			client.secret,
			reauth = FALSE,
			cache = TRUE,
			token)

	## load(file = "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/GoogleAnalyticsReport/Token")  ## Production
	#load(file = "E:/Projects/CFF Utilities/CFFDashboard/GoogleAnalyticsReport/Token") ## Dev
	#load(file = "C:/Users/dale.CFF/Documents/getcffpackage/googleAnalyticsDataRetrievalRApp/R/Token")
	#load(file = "/Users/dale.CFF/Documents/getcffpackage/googleAnalyticsDataRetrievalRApp/R/Token")
	##ValidateToken(token)

	# Build a list from query

	##StartOfWeekDay <- strftime(as.Date(Sys.Date() - 1, "%W"))
	##EndOfWeekDay <- strftime(as.Date(Sys.Date() + 5, "%W"))
	StartOfWeekDay <- as.character(wkDate)
	EndOfWeekDay <- as.character(strftime(wkDate + 6))
	#print(StartOfWeekDay)
	#print(EndOfWeekDay)

	##  weekly list
	query.list <- Init(start.date = StartOfWeekDay,
					end.date = EndOfWeekDay,
					#dimensions = "ga:week",
					dimension = "ga:nthWeek",
					metrics = "ga:pageviews,ga:users,ga:newUsers,ga:bounces,ga:hits",
					max.results = 10000,
					sort = "ga:nthweek",
					table.id = "ga:79525860")

	# Create Query Builder Object for parameter validation
	ga.query <- QueryBuilder(query.list)

	# Extract the data and store it in a data-frame
	ga.data <- GetReportData(ga.query, token)
	thisWeekUsers <- ga.data[, 4]
	thisWeekHits <- ga.data[, 6]

	## Console output:
	#print(ga.data)
	#print(thisWeekUsers)
	#print(thisWeekHits)

	## Daily list
	daily.list <- Init(start.date = StartOfWeekDay,
						end.date = EndOfWeekDay,
						dimensions = "ga:date",
						#metrics = "ga:pageviews",
						metrics = "ga:users",
						max.results = 10000,
						sort = "ga:date",
						table.id = "ga:79525860")

	dly.query <- QueryBuilder(daily.list)
	dly.data <- GetReportData(dly.query, token)
	#print(dly.data[, 2])

	## Monthly list
	StartMonthOfTheYear <- paste(as.character(yrNow), "-01-01", sep = "")
	EndMonthOfTheYear <- paste(as.character(yrNow), "-12-31", sep = "")
	monthly.list <- Init(start.date = StartMonthOfTheYear,
						end.date = EndMonthOfTheYear,
						dimensions = "ga:month",
						#metrics = "ga:pageviews",
						metrics = "ga:uniquePageviews",
						max.results = 10000,
						sort = "ga:month",
						table.id = "ga:79525860")

	mth.query <- QueryBuilder(monthly.list)
	mth.data <- GetReportData(mth.query, token)
	#print(mth.data[, 2])

	## Daily bounce rate list
	bounce.list <- Init(start.date = StartOfWeekDay,
						end.date = EndOfWeekDay,
						dimensions = "ga:date",
						metrics = "ga:bounces",
						max.results = 10000,
						sort = "ga:date",
						table.id = "ga:79525860")

	bounce.query <- QueryBuilder(bounce.list)
	bounce.data <- GetReportData(bounce.query, token)
	thisWeekBounce <- sum(bounce.data[, 2])
	#print(bounce.data[, 2])

	## Get active users (IP Filtered)
	#activeusers <- !is.null(list(get_realtime(profileId = profile.id)))
	activeusers <- list(get_realtime(profileId = profile.id))
	#print(activeusers)

	## Get active users (UnFiltered)
	UnfilteredActiveusers <- list(get_realtime(profileId = profileUnfiltered.id))
	#print(UnfilteredActiveusers)

	## Creates json file per query, respectively
	exportActiveUsersJson <- toJSON(activeusers)
	##write(exportActiveUsersJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/activeUsr.json")
	##write(exportActiveUsersJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/activeUsr.json")
	#print(exportActiveUsersJson)

	exportUnfilteredActiveUsersJson <- toJSON(UnfilteredActiveusers)
	##write(exportUnfilteredActiveUsersJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/unfilteredActiveUsr.json")
	#write(exportUnfilteredActiveUsersJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/unfilteredActiveUsr.json")
	#print(exportUnfilteredActiveUsersJson)

	exportNewUsersThisWeekJson <- toJSON(thisWeekUsers)
	##write(exportNewUsersThisWeekJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/usersThisWeek.json")
	#write(exportNewUsersThisWeekJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/usersThisWeek.json")
	#print(exportNewUsersThisWeekJson)

	exportPageBounceThisWeekJson <- toJSON(thisWeekBounce)
	##write(exportPageBounceThisWeekJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/pageBounceThisWeek.json")
	#write(exportPageBounceThisWeekJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/pageBounceThisWeek.json")
	#print(exportPageBounceThisWeekJson)

	exportPageHitsThisWeekJson <- toJSON(thisWeekHits)
	##write(exportPageHitsThisWeekJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/pageHitsThisWeek.json")
	#write(exportPageHitsThisWeekJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/pageHitsThisWeek.json")
	#print(exportPageHitsThisWeekJson)

	exportDailyUsersJson <- toJSON(dly.data[,2])
	##write(exportDailyUsersJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/dailyUsersThisWeek.json")
	#write(exportDailyUsersJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/dailyUsersThisWeek.json")
	#print(exportDailyUsersJson)

	exportMonthlyUsersJson <- toJSON(mth.data[, 2])
	##write(exportMonthlyUsersJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/monthlyUsers.json")
	#write(exportMonthlyUsersJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/monthlyUsers.json")
	#print(exportMonthlyUsersJson)

	exportBouncedDailyUsersJson <- toJSON(bounce.data[, 2])
	##write(exportBouncedDailyUsersJson, "/AnalyticsDashboardApp/RProject/GoogleAnalyticsReport/NodejsReportUsingGoogleAnalyticsWebApp/public/data/bouncedailyUsers.json")
	#write(exportBouncedDailyUsersJson, "E:/Projects/CFF Utilities/CFFDashboard/NodejsReportUsingGoogleAnalyticsWebApp/public/data/bouncedailyUsers.json")
	#print(exportBouncedDailyUsersJson)

	retn_list <- toJSON("filterActiveUsers" = exportActiveUsersJson,
					        "unfilterActiveUsers" = exportUnfilteredActiveUsersJson,
					        "newUsersThisWeek" = exportNewUsersThisWeekJson,
					        "pageBounceThisWeek" = exportPageBounceThisWeekJson,
					        "pageHitsThisWeek" = exportPageHitsThisWeekJson,
					        "dailyUsers" = exportDailyUsersJson,
					        "monthlyUsers" = exportMonthlyUsersJson,
					        "bouncedDailyUsers" = exportBouncedDailyUsersJson)

	print(retn_list)

	return(retn_list)
}


