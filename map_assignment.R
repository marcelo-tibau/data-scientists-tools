library('leaflet')

RJ_Icon <- makeIcon(
  iconUrl = "https://tibaudotorg.files.wordpress.com/2016/12/location_orange_icon.png",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

# Christ the Redeemer (statue), Sugarloaf Mountain, Copacabana Beach and Maracana Stadium
RJ_Lat_Long <- data.frame(
  lat = c(-22.9519, -22.9493, -22.9711, -22.9121),
  lng = C(-43.2105, -43.1546, -43.1825, -43.2302)
)

RJ_Sites <- c(
  "<a href='https://en.wikipedia.org/wiki/Christ_the_Redeemer_(statue)'>Christ the Redeemer (statue)</a>",
  "<a href='https://en.wikipedia.org/wiki/Sugarloaf_Mountain'>Sugarloaf Mountain</a>",
  "<a href='https://en.wikipedia.org/wiki/Copacabana,_Rio_de_Janeiro#Copacabana_Beach'>Copacabana Beach</a>",
  "<a href='https://en.wikipedia.org/wiki/Maracan�_Stadium'>Maracana Stadium</a>"
)

RJ_Lat_Long %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = RJ_Icon, popup = RJ_Sites)