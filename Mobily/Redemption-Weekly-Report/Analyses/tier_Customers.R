dfc1.2 <- dfc1 %>%
		filter(extractdate == max(extractdate)) %>%
		select(extractdate, pay_type_code, tier, totalcustomers) %>%
		group_by(pay_type_code) %>%
		mutate(relativeCust = round(100 * (totalcustomers/sum(totalcustomers))),
			custNumTidy = tidyNum(totalcustomers)) %>%
		arrange(tier, pay_type_code)

dfc1.2.1 <- dfc1.2 %>%
		filter(pay_type_code == 'Prepaid') %>%
		arrange(desc(relativeCust))

dfc1.2.1.1 <- tidyNum(sum(dfc1.2.1$totalcustomers))


dfc1.2.2 <- dfc1.2 %>%
		filter(pay_type_code == 'Postpaid') %>%
		arrange(desc(relativeCust))

dfc1.2.2.1 <- tidyNum(sum(dfc1.2.2$totalcustomers))
		

g1 <- ggplot(data = dfc1.2, aes(x = tier, y = relativeCust)) + ggtitle('Relative Percentage of Customers Across Tiers in Previous Week') +
		xlab('Tiers') + ylab('Percentage of Customers') + singleBarGeom + myTheme +
		facet_grid(pay_type_code ~ ., scales = 'free')

print(g1)
