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
  "2608202004", "2608400034", "3017211033", # also water pins.

  "1129110024", "2717109004",

  # flagged as added to City
  "1129110014", # coastal condo, FF = 1, building by the park in rogers park!
  "1129110023", # coastal condo, FF = 4, building by the tennis courts in rogers park!
  "1129308022", # coastal condo, FF = 1, rogers park
  "1129318015", # coastal condo, FF = 1, rogers park
  "1405207009", # coastal condo, FF = 1
  "1405211024", # coastal condo, FF = 1
  "1405211025", # almost coastal condo, FF = 3

  # new construction homes that had huge price increases

  "2411209095", "2411209097", "2411209098",
  "1336106098",

  "1729309068",  # nothing actually wrong with this PIN, sales seem normal, just missing eff_date info,
  # near the river but not on the river, appears to be the land around other pins?


  "0427302008", # old parcel, northfield / Glenview area.  not FZ
  "0427302009", # old parcel, northfield / Glenview area.  not FZ
  "0633108005", # new buildings in Bartlett, next to the SFHA but not in it
  "0633108006", # new buildings in Bartlett, next to the SFHA but not in it
  "0633108007", # new buildings in Bartlett, next to the SFHA but not in it
  "0633108008", # new buildings in Bartlett, next to the SFHA but not in it
  "0633108009",  # land around the new buildings
  "0633200015", # more new buildings in bartlett: Wood Lily Ct.
  "0633205019",
  "0633205020", # bartlett, Bluebell LN
  "1705320076", # no building here?
  "1710130027", # by chicago riverbut not floodzone
  "1710400054", # by chicago riverbut not floodzone, no buildinghere?
  "2226307004", # new homesin Lemont. The onesthat use to be a golf course! not FZ
  "2226307005", # new homesin Lemont. The onesthat use to be a golf course! not FZ
  "2226307010",
  "2226307011",
  "2226307012",
  "2433302053", # nobuildinghere
  "2730212019", # in A LOMR since 2018, Clover Drive, Orland Park. New Buildings. By floodzone but not in floodzone
  "2804400093",   # midlothian turnpike. by floodzone but not in floodzone

  "0520308066"  # the 1 building in winnetka flagged as having the bldg poly be mapped out of the 1% SFHA. It still is in 0.2% zone, BUT the property became tax exempt. 1205 Sunset Road
  # after further digging about this property, it was bought by the municipality so that stormwater improvement constructions could be done. Neat! Lookedat old FIRM and SFHA status did not change. false positive anyways.
)
