library('leaflet')

RJ_Icon <- makeIcon(
  iconUrl = "https://tibaudotorg.files.wordpress.com/2016/12/location_orange_icon.png",
  iconWidth = 65*215/230, iconHeight = 65,
  iconAnchorX = 65*215/230/2, iconAnchorY = 65
)

#https://tibaudotorg.files.wordpress.com/2016/12/location_orange_icon.png
# https://tibaudotorg.files.wordpress.com/2016/12/sun-rays-small.png
# Christ the Redeemer (statue), Sugarloaf Mountain, Copacabana Beach and Maracana Stadium
RJ_Lat_Long <- data.frame(
  lat = c(-22.9519, -22.9493, -22.9711, -22.9121),
  lng = c(-43.2105, -43.1546, -43.1825, -43.2302)
)

RJ_Sites <- c(
  "<a href='https://en.wikipedia.org/wiki/Christ_the_Redeemer_(statue)'>Christ the Redeemer (statue)</a>",
  "<a href='https://en.wikipedia.org/wiki/Sugarloaf_Mountain'>Sugarloaf Mountain</a>",
  "<a href='https://en.wikipedia.org/wiki/Copacabana,_Rio_de_Janeiro#Copacabana_Beach'>Copacabana Beach</a>",
  "<a href='https://en.wikipedia.org/wiki/Maracanã_Stadium'>Maracana Stadium</a>"
)

RJ_Lat_Long %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = RJ_Icon, popup = RJ_Sites)