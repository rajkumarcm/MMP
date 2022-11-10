original.df <- read.csv('relationshipsmmp.csv', header=T)
indices <- which(original.df$map_name=='North Caucasus' &
                 original.df$type=='Mergers' &
                 original.df$group1_name=='Caucasus Emirate')
browser()
caucasus_emirate <- original.df[indices, ]
caucasus_emirate <- data.frame(link_id=caucasus_emirate$link_id,
                               type=caucasus_emirate$status,
                               group1_id=caucasus_emirate$group2_id,
                               group2_id=caucasus_emirate$group1_id,
                               group1_name=caucasus_emirate$group2_name,
                               group2_name=caucasus_emirate$group1_name,
                               year=caucasus_emirate$year,
                               multiple=caucasus_emirate$multiple,
                               map_name=caucasus_emirate$map_name,
                               primary=caucasus_emirate$primary,
                               description=caucasus_emirate$description)

original.df <- original.df[-indices,]
new.df <- rbind(original.df, caucasus_emirate)
browser()
write.csv(file='relationship_caucasus_fixed.csv', x=new.df)