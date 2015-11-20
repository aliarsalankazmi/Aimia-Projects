df7 <- df1 %>%
  	select(redemption_we, pay_type_code, package_name, msisdn, points_denomination) %>%
		group_by(redemption_we, pay_type_code, package_name, msisdn) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we, pay_type_code, package_name) %>%
		summarise(total_redemptions = sum(total_redemptions_perCustomer),
			  unique_customers = n_distinct(msisdn),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer))

df7.1 <- df7 %>% 
  	filter(pay_type_code == 'Prepaid', !is.na(package_name)) %>%
		arrange(redemption_we, total_redemptions)
df7.1$package_name <- factor(df7.1$package_name, levels = unique(df7.1$package_name), ordered = TRUE)
		

g1 <- df7.1 %>%
	ggplot(aes(x = total_redemptions, y = package_name)) + 
	geom_segment(aes(yend = package_name), xend = 0, colour = 'grey90') +
	geom_point(aes(size = total_points_redeemed), colour = 'steelblue') + 
	facet_grid(redemption_we ~ ., scales = 'free') + 
	myTheme + theme(panel.grid.major.x = element_blank()) +
  ggtitle("Prepaid Packages' Redemption Performance") +
  ylab('') + xlab('Number of Redemptions') + 
  scale_size_continuous('Points Redeemed')


print(g1)


df7.2 <- df7 %>% 
    filter(pay_type_code == 'Postpaid', !is.na(package_name)) %>%
  	arrange(redemption_we, total_redemptions)
df7.2$package_name <- factor(df7.2$package_name, levels = unique(df7.2$package_name), ordered = TRUE)


g2 <- df7.2 %>%
	ggplot(aes(x = total_redemptions, y = package_name)) + 
	geom_segment(aes(yend = package_name), xend = 0, colour = 'grey90') +
	geom_point(aes(size = total_points_redeemed), colour = 'steelblue') + 
	facet_grid(redemption_we ~ ., scales = 'free') + 
	myTheme + theme(panel.grid.major.x = element_blank()) +
  ggtitle("Postpaid Packages' Redemption Performance") +
  ylab('') + xlab('Number of Redemptions') +
  scale_size_continuous('Points Redeemed')


print(g2)



highestPrepRedemp <- df7.1 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Prepaid') %>%
                        filter(total_redemptions == max(total_redemptions)) %>%
                        select(package_name, total_redemptions) %>%
                        mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestPrepPoints <- df7.1 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Prepaid') %>%
                        filter(total_points_redeemed == max(total_points_redeemed)) %>%
                        select(package_name, total_points_redeemed) %>%
                        mutate(pts_redemp_format = format(total_points_redeemed, big.mark = ','))

lowestPrepRedemp <- df7.1 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Prepaid') %>%
                        filter(total_redemptions == min(total_redemptions)) %>%
                        select(package_name, total_redemptions) %>%
                        mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

lowestPrepPoints <- df7.1 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Prepaid') %>%
                        filter(total_points_redeemed == min(total_points_redeemed)) %>%
                        select(package_name, total_points_redeemed) %>%
                        mutate(pts_redemp_format = format(total_points_redeemed, big.mark = ','))


highestPostRedemp <- df7.2 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Postpaid') %>%
                        filter(total_redemptions == max(total_redemptions)) %>%
                        select(package_name, total_redemptions) %>%
                        mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestPostPoints <- df7.2 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Postpaid') %>%
                        filter(total_points_redeemed == max(total_points_redeemed)) %>%
                        select(package_name, total_points_redeemed) %>%
                        mutate(pts_redemp_format = format(total_points_redeemed, big.mark = ','))

lowestPostRedemp <- df7.2 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Postpaid') %>%
                        filter(total_redemptions == min(total_redemptions)) %>%
                        select(package_name, total_redemptions) %>%
                        mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

lowestPostPoints <- df7.2 %>%
                        ungroup() %>%
                        filter(redemption_we == max(redemption_we), 
                               pay_type_code == 'Prepaid') %>%
                        filter(total_points_redeemed == min(total_points_redeemed)) %>%
                        select(package_name, total_points_redeemed) %>%
                        mutate(pts_redemp_format = format(total_points_redeemed, big.mark = ','))

