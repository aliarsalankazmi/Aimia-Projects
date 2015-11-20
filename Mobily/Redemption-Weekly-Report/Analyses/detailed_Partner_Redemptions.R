df6 <- df1 %>%
  filter(rdm_type == 'Partner') %>%
  select(redemption_we, msisdn, item_sub_category, pay_type_code, points_denomination) %>%
  group_by(redemption_we, pay_type_code, item_sub_category, msisdn) %>%
  summarise(total_redemptions_perCustomer = n(),
            points_redeemed_perCustomer = sum(points_denomination)) %>%
  group_by(redemption_we, pay_type_code, item_sub_category) %>%
  summarise(total_redemptions = sum(total_redemptions_perCustomer),
            unique_customers = n_distinct(msisdn),
            total_points_redeemed = sum(points_redeemed_perCustomer),
            avg_points_redeemed = mean(points_redeemed_perCustomer),
            max_points_redeemed = max(points_redeemed_perCustomer),
            min_points_redeemed = min(points_redeemed_perCustomer)) %>%
  mutate(percent_total_redemptions = (total_redemptions/sum(total_redemptions))*100,
         percent_points_redeemed = (total_points_redeemed/sum(total_points_redeemed))*100,
         redempNumTidy = tidyNum(total_redemptions),
         ptsNumTidy = tidyNum(total_points_redeemed),
         avgNumTidy = tidyNum(avg_points_redeemed)) %>%
  arrange(redemption_we, pay_type_code, item_sub_category)

longPartnerNames <- c('Abdul Latif Jameel', 'Tokyo Games - ', 'Al Musbah Telecom', 'AL BANDAR', 'Arabian Hala Group', 'Izone - Ali Reza')
shortPartnerNames <- c('ALJ', 'Tokyo Games', 'Al Musbah Telecom', 'Al Bandar', 'Avis', 'Izone')
longNamesIndex <- sapply(longPartnerNames, function(i) grep(pattern = i, x = levels(df6$item_sub_category)))
levels(df6$item_sub_category)[longNamesIndex] <- shortPartnerNames


g1 <- df6 %>% filter(pay_type_code == 'Prepaid') %>%
  ggplot(aes(x = redemption_we, y = item_sub_category)) +
  geom_tile(aes(fill = percent_total_redemptions), colour = 'white') + scale_fill_gradient(low = 'white', high = 'steelblue') +
  geom_text(aes(x = redemption_we, y = item_sub_category, label = paste0(round(percent_total_redemptions,0),'%')), alpha = .7, fontface = 'bold', family = 'Garamond', size = 3.5) + ggtitle('Percentage of Total Prepaid Redemptions at Partners') +  xlab('Week Ending Date') + ylab('') + myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), legend.position = 'none')

g2 <- df6 %>% filter(pay_type_code == 'Prepaid') %>%
  ggplot(aes(x = redemption_we, y = item_sub_category)) +
  geom_tile(aes(fill = percent_total_redemptions), colour = 'white') + scale_fill_gradient(low = 'white', high = 'steelblue') +
  geom_text(aes(x = redemption_we, y = item_sub_category, label = paste0(round(percent_points_redeemed,0),'%')), alpha = .7, fontface = 'bold', family = 'Garamond', size = 3.5) +
  ggtitle('Percentage of Total Prepaid Points Redeemed at Partners') +  xlab('Week Ending Date') + ylab('') + myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), legend.position = 'none')

g3 <- df6 %>% filter(pay_type_code == 'Postpaid') %>%
  ggplot(aes(x = redemption_we, y = item_sub_category)) +
  geom_tile(aes(fill = percent_total_redemptions), colour = 'white') + scale_fill_gradient(low = 'white', high = 'steelblue') +
  geom_text(aes(x = redemption_we, y = item_sub_category, label = paste0(round(percent_total_redemptions,0),'%')), alpha = .7, fontface = 'bold', family = 'Garamond', size = 3.5) +
  ggtitle('Percentage of Total Postpaid Redemptions at Partners') +  xlab('Week Ending Date') + ylab('') + myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), legend.position = 'none')

g4 <- df6 %>% filter(pay_type_code == 'Postpaid') %>%
  ggplot(aes(x = redemption_we, y = item_sub_category)) +
  geom_tile(aes(fill = percent_total_redemptions), colour = 'white') + scale_fill_gradient(low = 'white', high = 'steelblue') +
  geom_text(aes(x = redemption_we, y = item_sub_category, label = paste0(round(percent_points_redeemed,0),'%')), alpha = .7, fontface = 'bold', family = 'Garamond', size = 3.5) +
  ggtitle('Percentage of Total Postpaid Points Redeemed at Partners') +  xlab('Week Ending Date') + ylab('') + myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), legend.position = 'none')


print(g1)
print(g2)
print(g3)
print(g4)



df6.1 <- df6 %>%
  ungroup() %>%
  select(redemption_we, pay_type_code, item_sub_category, total_redemptions, unique_customers, 
         total_points_redeemed,avg_points_redeemed, contains('percent'), contains('tidy')) %>%
  arrange(pay_type_code, item_sub_category, redemption_we) %>%
  group_by(pay_type_code, item_sub_category) %>%
  mutate(percent_totRedemp_Change = round(percent_total_redemptions - lag(percent_total_redemptions)),
         percent_totPts_Change = round(percent_points_redeemed - lag(percent_points_redeemed))) %>%
  ungroup() %>%
  filter(redemption_we == max(redemption_we)) %>%
  mutate(totRedemp_Status = ifelse(percent_totRedemp_Change >= 0, 'increased', 'decreased'),
         totPts_Status = ifelse(percent_totPts_Change >= 0, 'increased', 'decreased')) %>%
  group_by(pay_type_code) %>%
  arrange(desc(percent_total_redemptions))

df6.1.Prepaid <- df6.1 %>%
  filter(pay_type_code == 'Prepaid')

df6.1.Postpaid <- df6.1 %>%
  filter(pay_type_code == 'Postpaid')

