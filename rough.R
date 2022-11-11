tmp.map_name = 'Iraq'
somalia.edges <- unique(df[df$map_name==tmp.map_name, c('from', 'to')])
somalia.edges <- somalia.edges %>%
                    inner_join(df_nodes[, c('id', 'hq_city', 'hq_province',
                                            'hq_country')], by=c('from'='id'))
cnames <- colnames(somalia.edges)
cnames[cnames == 'hq_city'] <- 'from_city'
cnames[cnames == 'hq_province'] <- 'from_province'
cnames[cnames == 'hq_country'] <- 'from_country'
colnames(somalia.edges) <- cnames

somalia.edges <- somalia.edges %>% 
                    inner_join(df_nodes[, c('id', 'hq_city', 'hq_province',
                                            'hq_country')], by=c('to'='id'))
cnames <- colnames(somalia.edges)
cnames[cnames == 'hq_city'] <- 'to_city'
cnames[cnames == 'hq_province'] <- 'to_province'
cnames[cnames == 'hq_country'] <- 'to_country'
colnames(somalia.edges) <- cnames