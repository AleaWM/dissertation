# PINs to drop

drop_parcels <- c(
  "1129312017", # Building on sherwin. Land is in Coastal SFHA for 2018 but building is not.
  "1129315024", # coastal building not in floodplain, on Jarvis!
  "1132400037", # building miiiight be in coastal floodplain but barely.
  "1405203011", # coastal building not in floodplain
  "1405203012", # coastal building not in floodplain
  "1405211015", # coastal building not in floodplain
  "1405211016", # coastal building not in floodplain
  "1405211021", # coastal building not in floodplain
  "1405211023", # coastal building not in floodplain
  "1405215017", # coastal building, partly in 1 in 500 year floodzone
  "1405403019", # coastal building not in floodplain
  "2130123021", # coastal building, not in floodplain according to redfin
  "0710101038",

  # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
  "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
  "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
  "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
  "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
  "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
  "2130124001", "2130124002", "2130124003", "2130124004",  # almost completely in the lake
  "2130999001", "2132213002", # actual water canal in calumet area
  "2608202004", "2608400034", "3017211033" # also water pins.
)
