dfc1.1 <- dfc1 %>%
		select(extractdate, pay_type_code, totalnewcustomers) %>%
		group_by(extractdate, pay_type_code) %>%
		summarise(totalnewcustomers = sum(totalnewcustomers)) %>%
		mutate(custNumTidy = tidyNum(totalnewcustomers)) %>%
		ungroup()  %>%
		arrange(pay_type_code, extractdate) %>%	
		group_by(pay_type_code) %>%
		mutate(percentDiff = 100 * ((totalnewcustomers - lag(totalnewcustomers))/lag(totalnewcustomers)),
		       percentTidy = tidyNum(percentDiff)) %>%
		arrange(extractdate)


dfc1.1.1 <- dfc1.1 %>%
		arrange(desc(extractdate)) %>%
		filter(pay_type_code == 'Prepaid') %>%
		mutate(status = ifelse(percentDiff >= 0, 'increased', 'decreased'))
		
dfc1.1.2 <- dfc1.1 %>%
		arrange(desc(extractdate)) %>%
		filter(pay_type_code == 'Postpaid') %>%
		mutate(status = ifelse(percentDiff >= 0, 'increased', 'decreased'))



g1 <- ggplot(data = dfc1.1, aes(x = extractdate, y = totalnewcustomers, fill = pay_type_code)) + ggtitle('New Customers Across Weeks') +
		xlab('Week Ending Date') + ylab('New Customers') + singleBarGeom + myTheme +
		facet_grid(pay_type_code ~ ., scales = 'free')


print(g1)
