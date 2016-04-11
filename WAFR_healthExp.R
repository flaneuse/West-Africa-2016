healthEcon = read.csv('~/Documents/USAID/West Africa Regional 2016/datain/HealthEconomics.csv')

he = healthEcon %>% 
  gather(var, rate, -Year, -YearCode, -Country)

# Health econ1: GDP + health exp + ext exp. -------------------------------
ggplot(healthEcon, aes(x = GDPGrowthRate, y = GovHlthExpendOfTotalExpend,
                       # colour = ExternalHlthExpendOfTotalHlthExpend, 
                       alpha = Year, group = Country)) +
  geom_path() +
  facet_wrap(~Country, scales = 'free') +
  theme_xygrid()


order = healthEcon %>% 
  filter(Year == 2013) %>% 
  ungroup() %>% 
  arrange(desc(GovHlthExpendOfTotalExpend))

he$Country = factor(he$Country, 
                    levels = order$Country)


ggplot(he %>% filter(var %in% c('GDPGrowthRate', 'GovHlthExpendOfTotalExpend', 'ExternalHlthExpendOfTotalHlthExpend')), aes(x = Year, y = rate,
                                                                                                                            colour = var, 
                                                                                                                            group = var)) +
  geom_line() +
  facet_wrap(~Country, scales = 'free_y') +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))

ggsave("~/GitHub/West Africa 2016/plots/2016-04-11_healthEcon1_freescales.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


ggplot(he %>% filter(var %in% c('GDPGrowthRate', 'GovHlthExpendOfTotalExpend', 'ExternalHlthExpendOfTotalHlthExpend')), aes(x = Year, y = rate,
                                                                                                                            colour = var, 
                                                                                                                            group = var)) +
  geom_line() +
  facet_wrap(~Country) +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))

ggsave("~/GitHub/West Africa 2016/plots/2016-04-11_healthEcon1.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

healthEcon = healthEcon %>% 
  group_by(Country)
arrange(Year)

# ggplot(healthEcon, aes(x = Year, y = OutOfPocketHlthExpend,
#                        alpha = Year, group = Country
#                        # colour = ExternalHlthExpendOfTotalHlthExpend, 
# )) +
#   geom_path(size = 1) +
#   scale_colour_gradientn(colours = brewer.pa) +
#   # geom_text(aes(label = Year)) +
#   facet_wrap(~Country, scales = 'free_y') +
#   theme_xygridlight()



ggplot(he %>% filter(var %in% c('OutOfPocketHlthExpend', 'GovHlthExpendOfTotalExpend')), 
       aes(x = Year, y = rate,
           colour = var, 
           group = var)) +
  geom_line() +
  facet_wrap(~Country, scales = 'free_y') +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))



ggsave("~/GitHub/West Africa 2016/plots/2016-04-11_healthEcon2_freescales.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(he %>% filter(var %in% c('OutOfPocketHlthExpend', 'GovHlthExpendOfTotalExpend')), 
       aes(x = Year, y = rate,
           colour = var, 
           group = var)) +
  geom_line() +
  facet_wrap(~Country) +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))

ggsave("~/GitHub/West Africa 2016/plots/2016-04-11_healthEcon2.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Not strong relationship -------------------------------------------------

ggplot(healthEcon %>% filter(Year == 2013), aes(x = OutOfPocketHlthExpend, y = GovHlthExpendOfTotalExpend)) +
  geom_point()

ggplot(healthEcon %>% filter(Year == 2013), aes(x = OutOfPocketHlthExpend, y = GovHlthExpendOfTotalExpend)) +
  geom_point()

