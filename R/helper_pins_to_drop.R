# PINs to drop



drop_parcels <- c(
  "0121205008",  # giant new $3.5 million house
  "0122400028",  # mansoin in south barington, false positive.
  "0124200001",  # Million + Dollar home in South Barington, not in Flood plain,

  "0311101036",  # built after first sale
  "0324102013",  # sold 179 times

  "0424101038",  # built after first sale
  "0427302008",  # old parcel, northfield / Glenview area.  not FZ
  "0427302009",  # old parcel, northfield / Glenview area.  not FZ

  "0508314014",  # Coastal home in glencoe
  "0508314030",  # false positive in glencoe
  "0508400001",  # pins in lake
  "0508400002",  # pins in lake
  "0508400003",  # pins in lake
  "0508400004",  # pins in lake

  "0516106013",  # land by lake,
  "0520308066",  # the 1 building in winnetka flagged as having the bldg poly be mapped out of the 1% SFHA. It still is in 0.2% zone, BUT the property became tax exempt. 1205 Sunset Road
  # after further digging about this property, it was bought by the municipality so that stormwater improvement constructions could be done. Neat! Lookedat old FIRM and SFHA status did not change. false positive anyways.
  "0521202007",  # coastal home in winnetka
  "0527111007",  # coastal home in kennilworth
  "0527200057",  # coastal condo in wilmette, false positive

  "0618301017",  # rebuilt everything
  "0619210020",  # coded as condo in Elgin,tons of PINs and buildings on one giant parcel. Not actually in flood zone though.
  "0627216009",  # another weird Elgin parcel with lots of pin sales
  "0628402019",  # new buildings in Bartlett (multiple buildings coded as condos) but no buildings in floodzone, just edge of large land parcel.
  "0633108005",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108006",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108007",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108008",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108009",  # land around the new buildings
  "0633200015",  # more new buildings in bartlett: Wood Lily Ct.
  "0633205019",
  "0633205020",  # bartlett, Bluebell LN

  "0710101038",
  "0714117007",  # condo in Schaumburg with lots of PINs as homes on one large parcel. not actually in flood zones
  "0718207039",  # large land parcel that touches floodplain but buildings are not in it at all

  "0831400074",  # sold 45 times"

  "0915412014",  # 10 x growth in price,

  "1036206042",  # sold 52 times

  "1120105009",  # coastal condo by sheridan graveyard & evanston
  "1129110014",  # coastal condo, FF = 1, building by the park in rogers park!
  "1129110023",  # coastal condo, FF = 4, building by the tennis courts in rogers park!
  "1129110024",
  "1129308017",
  "1129308022",  # coastal condo, FF = 1, rogers park
  "1129312017",  # Building on sherwin. Land is in Coastal SFHA for 2018 but building is not.
  "1129315024",  # coastal building not in floodplain, on Jarvis!
  "1129318015",  # coastal condo, FF = 1, rogers park
  "1132400037",  # building miiiight be in coastal floodplain but barely.

  "1336106098",

  "1405203011",  # coastal building not in floodplain
  "1405203012",  # coastal building not in floodplain
  "1405207009",  # coastal condo, FF = 1
  "1405211015",  # coastal building not in floodplain
  "1405211016",  # coastal building not in floodplain
  "1405211017",  # not residential parcels, some in water
  "1405211021",  # coastal building not in floodplain
  "1405211023",  # coastal building not in floodplain
  "1405211024",  # coastal condo, FF = 1
  "1405211025",  # almost coastal condo, FF = 3
  "1405215015",
  "1405215017",  # coastal building, partly in 1 in 500 year floodzone
  "1405403019",  # coastal building not in floodplain
  "1405403020",  # not residential parcels, some in water
  "1405403022",
  "1405403023",

  "1416999001",  # not residential parcels, some in water
  "1429212022",  # sold 146 times

  "1513300022",  # sold 47 times
  "1513300026",  # sold 126 times

  "1703203009",  # parcel level sold 246 times
  "1705217018",  # BDBC SPE LLC
  "1705320076",  # no building here?
  "1709418014",  # sold 46 times
  "1710130027",  # by chicago riverbut not floodzone
  "1710135038",  # trump tower,
  "1710222007",  # by chicago river and navy pier
  "1710400054",  # by chicago riverbut not floodzone, no building here?
  "1710403001",  # not residential parcels, some in water
  "1715113004",  # land and partially in water parcels
  "1716401034",   # by south branch of chicago river. not flood zone
  "1717102043",  # 1400 W MONROE OWNER LLC,
  "1729309068",  # nothing actually wrong with this PIN, sales seem normal, just missing eff_date info,
  # near the river but not on the river, appears to be the land around other pins?

  "2130108012",  # land and partially in water parcels
  "2130108018",  # land and partially in water parcels
  "2130108019",  # land and partially in water parcels
  "2130108028",  # land polygons along the lake, no residential buildings in them
  "2130108030",  # land polygons along the lake, no residential buildings in them
  "2130108031",  # land polygons along the lake, no residential buildings in them
  "2130108032",  # land polygons along the lake, no residential buildings in them
  "2130108033",  # land polygons along the lake, no buildings within them
  "2130114012",  # land polygons along the lake, no buildings within them
  "2130114013",  # land polygons along the lake, no buildings within them
  "2130114014",  # land polygons along the lake, no buildings within them
  "2130114015",  # land polygons along the lake, no buildings within them
  "2130114016",  # land polygons along the lake, no buildings within them
  "2130114029",  # very south side of chicago, coastal condo
  "2130123021",  # coastal building, not in floodplain according to redfin
  "2130124001",  # almost completely in the lake
  "2130124002",  # almost completely in the lake
  "2130124003",  # almost completely in the lake
  "2130124004",  # almost completely in the lake
  "2130999001",  # actual water canal in calumet area
  "2132213002",  # actual water canal in calumet area

  "2226307004",  # new homes in Lemont. The ones that use to be a golf course! not FZ
  "2226307005",  # new homes in Lemont. The ones that use to be a golf course! not FZ
  "2226307010",
  "2226307011",
  "2226307012",

  "2411209095",
  "2411209097",
  "2411209098",
  "2433302053",  # no building here

  "2608202004",  # also water pins.
  "2608400034",  # also water pins.

  "2717100003",  # became 2717100006? fire department in orland park? But doesn't exist in 2025 parcels so didn't match to polygons
  "2717100005",  # became 2717100006? fire department in orland park? But doesn't exist in 2025 parcels so didn't match to polygons
  "2717109004",

  "2730212019",  # in A LOMR since 2018, Clover Drive, Orland Park. New Buildings. By floodzone but not in floodzone
  "2804400093",  # midlothian turnpike. by floodzone but not in floodzone

  "3017211033"   # also water pins.
)


#
# drop_parcels <- c(
#   "1129312017", # Building on sherwin. Land is in Coastal SFHA for 2018 but building is not.
#   "1129315024", # coastal building not in floodplain, on Jarvis!
#   "1132400037", # building miiiight be in coastal floodplain but barely.
#   "1405203011", # coastal building not in floodplain
#   "1405203012", # coastal building not in floodplain
#   "1405211015", # coastal building not in floodplain
#   "1405211016", # coastal building not in floodplain
#   "1405211021", # coastal building not in floodplain
#   "1405211023", # coastal building not in floodplain
#   "1405215017", # coastal building, partly in 1 in 500 year floodzone
#   "1405403019", # coastal building not in floodplain
#   "2130123021", # coastal building, not in floodplain according to redfin
#   "0710101038",
#
#   # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
#   "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
#   "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
#   "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
#   "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
#   "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
#   "2130124001", "2130124002", "2130124003", "2130124004",  # almost completely in the lake
#   "2130999001", "2132213002", # actual water canal in calumet area
#   "2608202004", "2608400034", "3017211033", # also water pins.
#
#   "1129110024", "2717109004",
#
#   # flagged as added to City
#   "1129110014", # coastal condo, FF = 1, building by the park in rogers park!
#   "1129110023", # coastal condo, FF = 4, building by the tennis courts in rogers park!
#   "1129308022", # coastal condo, FF = 1, rogers park
#   "1129318015", # coastal condo, FF = 1, rogers park
#   "1405207009", # coastal condo, FF = 1
#   "1405211024", # coastal condo, FF = 1
#   "1405211025", # almost coastal condo, FF = 3
#
#   # new construction homes that had huge price increases
#   "2411209095", "2411209097", "2411209098",
#   "1336106098",
#
#   "1729309068",  # nothing actually wrong with this PIN, sales seem normal, just missing eff_date info,
#   # near the river but not on the river, appears to be the land around other pins?
#
#
#   "0427302008", # old parcel, northfield / Glenview area.  not FZ
#   "0427302009", # old parcel, northfield / Glenview area.  not FZ
#   "0633108005", # new buildings in Bartlett, next to the SFHA but not in it
#   "0633108006", # new buildings in Bartlett, next to the SFHA but not in it
#   "0633108007", # new buildings in Bartlett, next to the SFHA but not in it
#   "0633108008", # new buildings in Bartlett, next to the SFHA but not in it
#   "0633108009", # land around the new buildings
#   "0633200015", # more new buildings in bartlett: Wood Lily Ct.
#   "0633205019",
#   "0633205020", # bartlett, Bluebell LN
#   "1705320076", # no building here?
#   "1710130027", # by chicago riverbut not floodzone
#   "1710400054", # by chicago riverbut not floodzone, no building here?
#   "2226307004", # new homes in Lemont. The ones that use to be a golf course! not FZ
#   "2226307005", # new homes in Lemont. The ones that use to be a golf course! not FZ
#   "2226307010",
#   "2226307011",
#   "2226307012",
#   "2433302053", # no building here
#   "2730212019", # in A LOMR since 2018, Clover Drive, Orland Park. New Buildings. By floodzone but not in floodzone
#   "2804400093",   # midlothian turnpike. by floodzone but not in floodzone
#
#   "0520308066",  # the 1 building in winnetka flagged as having the bldg poly be mapped out of the 1% SFHA. It still is in 0.2% zone, BUT the property became tax exempt. 1205 Sunset Road
#   # after further digging about this property, it was bought by the municipality so that stormwater improvement constructions could be done. Neat! Lookedat old FIRM and SFHA status did not change. false positive anyways.
#   "0618301017", # rebuilt everything
#   "0121205008",  # giant new $3.5 million house
#   "0915412014", # 10 x growth in price,
#   "0311101036", # built after first sale
#   "0424101038", # built after first sale
#   "1710135038", # trump tower,
#   "0527200057", # coastal condo in wilmette, false positive
#   "1120105009", # coastal condo by sheridan graveyard & evanston
#   "0508314030", # false positive in glencoe
#   "0516106013", # land by lake,
#   "0718207039", # large land parcel that touches floodplain but buildings are not in it at all
#   "0628402019", # new buildings in Bartlett (multiple buildings coded as condos) but no buildings in floodzone, just edge of large land parcel.
#
#   # pin splits and new pins from condo buildings
#   "1717102043", # 1400 W MONROE OWNER LLC,
#   "1705217018", # BDBC SPE LLC
#   "1703203009", # parcel level sold 246 times
#   "0324102013", # sold 179 times
#   "1429212022", # sold 146 times
#   "1703223023", # sold 131 times
#   "1513300026", # sold 126 times
#   "1036206042",  # sold 52 times
#   "1513300022", # sold 47 times
#   "1709418014", # sold 46 times
#   "0831400074", # sold 45 times"
#   "0619210020", # coded as condo in Elgin,tons of PINs and buildings on one giant parcel. Not actually in flood zone though.
#   "0627216009", # another weird Elgin parcel with lots of pin sales
#   "0714117007", # condo in Schaumburg with lots of PINs as homes on one large parcel. not actually in flood zones
#   "0124200001",  # Million + Dollar home in South Barington, not in Flood plain,
#   "2717100003", "2717100005", # became 2717100006? fire department in orland park? But doesn't exist in 2025 parcels so didn't match to polygons
#   "1129110014", # coastal condo
#   "1129110023",
#   "1129110024",
#   "1129308017",
#   "1405203011",
#   "1405203012",
#   "1129308022",
#   "1405207009",
#   "1405211015",
#   "1405211016",
#   "1405211021",
#   "1405211023",
#   "1405211024",
#   "1405215015", # coastal condo,
#   "1405215017", # coastal condo,
#   "1405403019",
#   "1405403022",
#   "1405403023",
#   "1710222007", # by chicago river and navy pier
#   "2130114029", # very south side of chicago, coastal condo
#   "0508314014", # Coastal home in glencoe
#   "0527111007", # coastal home in kennilworth
#   "0521202007", # coastal home in winnetka
#   "0122400028"  # mansion in south barington, false positive.
# )
