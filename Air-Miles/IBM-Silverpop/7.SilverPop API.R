#Load libraries
library(httr)
library(dplyr)


#Copying required credentials from Silverpop
##Application Name:	Ali's app for API access
##Client ID:	2fb47a91-3e34-4b2f-b9b5-93cffbf31500
##Client Secret:	5b5aa1d0-9fcd-4740-b970-79b059c32a74
##Refresh Token: rDI5XuVASIqwIOOeJdRHYC71vrLvci64kE1JgDipG8BAS1


#The following from IBM website is incorrect, use the alternate provided
ibmUrl <- "https://api3.ibmmarketingcloud/oauth/token"
ibmUrl <- "https://api3.ibmmarketingcloud.com/oauth/token"
clientId <- "2fb47a91-3e34-4b2f-b9b5-93cffbf31500"
clientSecret <- "5b5aa1d0-9fcd-4740-b970-79b059c32a74"
refreshToken <- "rDI5XuVASIqwIOOeJdRHYC71vrLvci64kE1JgDipG8BAS1"



#Request authentication (access token)
reqAccess <- POST(url = ibmUrl, 
		body = list(grant_type = "refresh_token", client_id = clientId,
			    client_secret = clientSecret, refresh_token = refreshToken))
					   
warn_for_status(reqAccess)
access_token <- content(reqAccess)$access_token



#database/table id <- 3874161
#account_no <- 4241

<SelectRecipientData>
<LIST_ID>45654</LIST_ID>
<EMAIL>someone@adomain.com</EMAIL>
<COLUMN>
<NAME>Customer Id</NAME>
<VALUE>123-45-6789</VALUE>
</COLUMN>
</SelectRecipientData>
</Body>
</Envelope>





#Test query
r1 <- POST(url = ibmUrl, 
	body = list(VISIBILITY = 1, LIST_TYPE = 2, grant_type = "refresh_token", client_id = clientId, 
		    client_secret = clientSecret, refresh_token = refreshToken), verbose())

r2 <- POST(url = ibmUrl, 
	body = list(JOB_ID = "61693369", grant_type = "refresh_token", client_id = clientId, 
		    client_secret = clientSecret, refresh_token = refreshToken), verbose())


xmlBody <- "<Envelope><Body>
		<RawRecipientDataExport>
		<EVENT_DATE_START>01/01/2015 00:00:00</EVENT_DATE_START>
		<EVENT_DATE_END>01/01/2018 23:59:00</EVENT_DATE_END>
		<MOVE_TO_FTP/>
		<EXPORT_FORMAT>PIPE</EXPORT_FORMAT>
		<EMAIL>admin@yourorg.com</EMAIL>
		<ALL_EVENT_TYPES/>
		<EXCLUDE_DELETED/>
		<COLUMN>
		<NAME>CustomerID</NAME>
		</COLUMN>
		<COLUMN>
		<NAME>Address</NAME>
		</COLUMN>
		<RETURN_MAILING_NAME/>
		</RawRecipientDataExport>
		</Body></Envelope>"

r3 <- POST(url = ibmUrl,
		body = xmlBody, verbose())



#####Notes

https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
https://kb.silverpop.com/@api/deki/files/10231/XML_API_Guide_2015_Fall.pdf
https://kb.silverpop.com/kb/Engage/API/API_XML/OAuth
file:///C:/Users/kazami/Downloads/XML_API_Test_Harness_-_Winter_2015_-_Public/Silverpop%20XML%20APIs%20-%20Test%20Harness.html




Raw Recipient Data Export

<Envelope><Body>
<RawRecipientDataExport>
<EVENT_DATE_START>01/01/2015 00:00:00</EVENT_DATE_START>
<EVENT_DATE_END>01/01/2018 23:59:00</EVENT_DATE_END>
<MOVE_TO_FTP/>
<EXPORT_FORMAT>PIPE</EXPORT_FORMAT>
<EMAIL>admin@yourorg.com</EMAIL>
<ALL_EVENT_TYPES/>
<EXCLUDE_DELETED/>
<COLUMN>
<NAME>CustomerID</NAME>
</COLUMN>
<COLUMN>
<NAME>Address</NAME>
</COLUMN>
<RETURN_MAILING_NAME/>
</RawRecipientDataExport>
</Body></Envelope>




Join Table Remove

<Envelope>
<Body>
<JoinTable>
<TABLE_ID>4325237</TABLE_ID>
<LIST_ID>3874161</LIST_ID>
<REMOVE/>
</JoinTable>
</Body>
</Envelope>





ExportListXMLCommand - MemberInfo

<Envelope><Body>
<ExportList>
<LIST_ID>3874161</LIST_ID>
<EXPORT_COLUMNS>
<COLUMN>country</COLUMN>
<COLUMN>account_no</COLUMN>
<COLUMN>card_no</COLUMN>
<COLUMN>account_status</COLUMN>
<COLUMN>Email</COLUMN>
<COLUMN>emailable</COLUMN>
<COLUMN>mobile</COLUMN>
<COLUMN>smsable</COLUMN>
<COLUMN>Full_Name</COLUMN>
<COLUMN>application_date</COLUMN>
<COLUMN>balance</COLUMN>
<COLUMN>birth_date</COLUMN>
<COLUMN>card_type</COLUMN>
<COLUMN>last_transaction_date</COLUMN>
<COLUMN>pr_host_email</COLUMN>
<COLUMN>pr_host_sms</COLUMN>
<COLUMN>RECIPIENT_ID</COLUMN>
</EXPORT_COLUMNS>
<EXPORT_FORMAT>PIPE</EXPORT_FORMAT>
<ADD_TO_STORED_FILES/>
</ExportList>
</Body></Envelope>