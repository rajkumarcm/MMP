library(stringr)

encode_address <- function(locations)
{
  cnames <- colnames(locations)
  city_colname <- cnames[str_detect(cnames, '\\S*city\\S*')]
  province_colname <- cnames[str_detect(cnames, '\\S*province\\S*')]
  country_colname <- cnames[str_detect(cnames, '\\S*country\\S*')]
  addresses <- NULL
  for(i in 1:nrow(locations))
  {
    location <- locations[i,]
    if((!is.na(location[, city_colname])) | 
       (!is.na(location[, province_colname])) | 
       (!is.na(location[, country_colname])))
    {
      tmp_addr <- ""
      
      if(nchar(location[, city_colname]) == 0)
      {
        tmp_addr <- ""
      }
      else
      {
        tmp_addr <- location[, city_colname]
      }
      
      if(nchar(location[, province_colname]) > 0)
      {
        if(nchar(tmp_addr) > 0 )
        {
          tmp_addr <- sprintf('%s, %s', tmp_addr, location[, province_colname])
        }
        else
        {
          tmp_addr <- location[, province_colname]
        }
      }
      
      if(nchar(location[, country_colname]) > 0)
      {
        if(nchar(tmp_addr) > 0)
        {
          tmp_addr <- sprintf('%s, %s', tmp_addr, location[, country_colname])
        }
        else
        {
          tmp_addr <- location[, country_colname]
        }
      }
      
      if(is.null(addresses))
      {
        addresses <- data.frame(addr=tmp_addr, hq_city=location[, city_colname],
                                hq_province=location[, province_colname],
                                hq_country=location[, country_colname])
      }
      else
        addresses <- rbind(addresses, 
                           data.frame(addr=tmp_addr, hq_city=location[, city_colname],
                                                 hq_province=location[, province_colname],
                                                 hq_country=location[, country_colname]))
      
    }
  }
  addresses <- addresses[addresses$addr != "",]
  addresses
}