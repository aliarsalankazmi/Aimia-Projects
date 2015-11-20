df7.3 <- df1 %>%
    filter(pay_type_code == 'Prepaid', !is.na(package_name)) %>%
    select(redemption_we, rdm_type, package_name, msisdn, points_denomination) %>%
    group_by(redemption_we, rdm_type, package_name, msisdn) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we, rdm_type, package_name) %>%
		summarise(total_redemptions = sum(total_redemptions_perCustomer),
			  unique_customers = n_distinct(msisdn),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer))

g1 <- df7.3 %>%
    ggplot(aes(x = redemption_we, y = package_name)) + 
    geom_tile(aes(fill = total_redemptions), colour = 'white') + 
    scale_fill_gradient(name = 'Total Redemptions', low = 'white', high = 'steelblue') + 
    geom_point(aes(size = total_points_redeemed), colour = 'black') +
    myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), 
                    panel.border = element_blank()) +
    facet_grid(rdm_type ~ ., scales = 'free') +
  ggtitle("Prepaid Packages' Redemption Performance") +
  ylab('') + xlab('Week Ending Date') + 
  scale_size_continuous('Points Redeemed')


print(g1)


df7.4 <- df1 %>%
    filter(pay_type_code == 'Postpaid', !is.na(package_name)) %>%
    select(redemption_we, rdm_type, package_name, msisdn, points_denomination) %>%
    group_by(redemption_we, rdm_type, package_name, msisdn) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we, rdm_type, package_name) %>%
		summarise(total_redemptions = sum(total_redemptions_perCustomer),
			  unique_customers = n_distinct(msisdn),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer))

g2 <- df7.4 %>%
    ggplot(aes(x = redemption_we, y = package_name)) + 
    geom_tile(aes(fill = total_redemptions), colour = 'white') + 
    scale_fill_gradient(name = 'Total Redemptions', low = 'white', high = 'steelblue') + 
    geom_point(aes(size = total_points_redeemed), colour = 'black') +
    myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), 
                    panel.border = element_blank()) +
    facet_grid(rdm_type ~ ., scales = 'free') +
  ggtitle("Postpaid Packages' Redemption Performance") +
  ylab('') + xlab('Week Ending Date') + 
  scale_size_continuous('Points Redeemed')
    


print(g2)



###For Prepaid
highestRedempIntPre <- df7.3 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == max(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestRedempPartPre <- df7.3 %>% ungroup() %>% 
          filter(rdm_type == 'Partner', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == max(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

lowestRedempIntPre <- df7.3 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == min(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestPointsIntPre <- df7.3 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_points_redeemed == max(total_points_redeemed)) %>%
          select(package_name, total_points_redeemed) %>%
          mutate(points_redemp_format = format(total_points_redeemed, big.mark = ','))

highestPointsPartPre <- df7.3 %>% ungroup() %>% 
          filter(rdm_type == 'Partner', redemption_we == max(redemption_we)) %>%
          filter(total_points_redeemed == max(total_points_redeemed)) %>%
          select(package_name, total_points_redeemed) %>%
          mutate(points_redemp_format = format(total_points_redeemed, big.mark = ','))

###For Postpaid
highestRedempIntPost <- df7.4 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == max(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestRedempPartPost <- df7.4 %>% ungroup() %>% 
          filter(rdm_type == 'Partner', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == max(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

lowestRedempIntPost <- df7.4 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_redemptions == min(total_redemptions)) %>%
          select(package_name, total_redemptions) %>%
          mutate(total_redemp_format = format(total_redemptions, big.mark = ','))

highestPointsIntPost <- df7.4 %>% ungroup() %>% 
          filter(rdm_type == 'Mobily', redemption_we == max(redemption_we)) %>%
          filter(total_points_redeemed == max(total_points_redeemed)) %>%
          select(package_name, total_points_redeemed) %>%
          mutate(points_redemp_format = format(total_points_redeemed, big.mark = ','))

highestPointsPartPost <- df7.4 %>% ungroup() %>% 
          filter(rdm_type == 'Partner', redemption_we == max(redemption_we)) %>%
          filter(total_points_redeemed == max(total_points_redeemed)) %>%
          select(package_name, total_points_redeemed) %>%
          mutate(points_redemp_format = format(total_points_redeemed, big.mark = ','))

