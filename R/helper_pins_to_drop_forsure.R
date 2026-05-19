# PINs to drop



drop_parcels <- c(
  "0121205008",  # giant new $3.5 million house
  # "0122400028",  # mansion in south barington, false positive.
  # "0124200001",  # Million + Dollar home in South Barington, not in Flood plain,

  "0311101036",  # built after first sale
  "0315212076",  # built new after first sale
  # "0324102013",  # sold 179 times

  "0424101038",  # built after first sale
  "0427302008",  # old parcel, northfield / Glenview area.  not FZ, maybe not residential? not in data anymore
  "0427302009",  # old parcel, northfield / Glenview area.  not FZ, not in data anymore

  #  "0527200057",  # coastal condo in wilmette, false positive
  # "0508314014",  # Coastal home in glencoe
  # "0508314030",  # false positive in glencoe
  "0508400001",  # pins in lake
  "0508400002",  # pins in lake
  "0508400003",  # pins in lake
  "0508400004",  # pins in lake

  "0516106013",  # land by lake,
  "0520308066",  # the 1 building in winnetka flagged as having the bldg poly be mapped out of the 1% SFHA. It still is in 0.2% zone, BUT the property became tax exempt. 1205 Sunset Road
  # after further digging about this property, it was bought by the municipality so that stormwater improvement constructions could be done. Neat! Lookedat old FIRM and SFHA status did not change. false positive anyways.
  # "0521202007",  # coastal home in winnetka
  "0527111007",  # coastal home in kennilworth, changed property class, exclude
  #  "0527200057",  # coastal condo in wilmette, false positive

  "0618301017",  # rebuilt everything, not in data anymore so not residential twice
  # "0619210020",  # coded as condo in Elgin,tons of PINs and buildings on one giant parcel. Not actually in flood zone though.
  # "0627216009",  # another weird Elgin parcel with lots of pin sales
  "0628402019",  # new buildings in Bartlett (multiple buildings coded as condos) but no buildings in floodzone, just edge of large land parcel.
  "0633108005",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108006",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108007",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108008",  # new buildings in Bartlett, next to the SFHA but not in it
  "0633108009",  # land around the new buildings
  "0633200015",  # more new buildings in bartlett: Wood Lily Ct.
  "0633205019",  # not in data anymore
  "0633205020",  # sold twice days apart, same price; bartlett, Bluebell LN

  "0710101038",   # not in data anymore so probably not residential
  # "0714117007",  # condo in Schaumburg with lots of PINs as homes on one large parcel. not actually in flood zones
  # "0718207039",  # large land parcel that touches floodplain but buildings are not in it at all

  # "0831400074",  # sold 45 times"

  "0915412014",  # not residential; 10 x growth in price,

  # "1036206042",  # sold 52 times

  "1120105009",  # coastal condo by sheridan graveyard & evanston
  "1129110010",  # coastal building, false positive, weird split parcel where they own land by the water but the building isn't actually on the coast
  "1129110014",  # coastal condo, FF = 1, building by the park in rogers park
  # "1129110023",  # coastal condo, FF = 4, building by the tennis courts in rogers park
  #  "1129110024",  # Rogers park coastal
  #  "1129308017",  # rogers park coastal
  #  "1129308022",  # coastal condo, FF = 1, rogers park
  # "1129312017",  # Building on Sherwin. Land is in Coastal SFHA for 2018 but building is not.
  # "1129312018",   # coastal building on fargo, false positive
  # "1129315024",  # coastal building not in floodplain, on Jarvis!
  # "1129318015",  # coastal condo, FF = 1, rogers park
  # "1132400037",  # building miiiight be in coastal floodplain but barely.
  # "0113301013", # dropped in code to filter out first sale only! built in  2014, has a giant mansion on it.had large growth and was flagged as beingremoved from SFHA but it wasn't.
  # but sold twice afterwards so only drop the earliest sale before there was a home on there!
  "1336106098",  # new build, goes from 50K to 635K in 8 months

  # "1405203011",  # coastal building not in floodplain
  # "1405203012",  # coastal building not in floodplain
  # "1405207009",  # flagged as added; coastal condo, FF = 1
  # "1405211015",  # flagged as added; coastal building not in floodplain
  # "1405211016",   # flagged as added; coastal building not in floodplain
  "1405211017",   # not residential parcels, some in water
  # "1405211021",  # coastal building not in floodplain
  # "1405211023",  # coastal building not in floodplain
  # "1405211024",  # coastal condo, FF = 1
  # "1405211025",  # almost coastal condo, FF = 3
  "1405215015",
  # "1405215017",  # coastal building, partly in 1 in 500 year floodzone
  # "1405403019",  # coastal building not in floodplain
  "1405403020",  # not residential parcels, some in water
  # "1405403022", # flagged as removed, coastal building on sheridan, parcel also doesn't exist in 2025
  # "1405403023", # flagged as removed, coastal building on sheridan, parcel also doesn't exist in 2025

  "1416999001",  # not residential parcels, some in water
  # "1429212022",  # sold 146 times, parcel doesn't exist in 2025

  # "1513300022",  # condo building, sold 47 times, parcel didn't exist in 2025
  # "1513300026",  # condo building, sold 126 times, parcel didn't exist in 2025

  # "1703203009",  # condo building,parcel level sold 246 times. No longer exists. I think condos were bought out and turned into apartments? Could filter out sales after 2020 since that is when they were bought up? Essentially drop the last sale. parcel didn't exist in 2025
  "1705217018",  # BDBC SPE LLC
  "1705320076",  # no building here?
  "1709418014",  # sold 46 times
  "1710130027",  # by chicago river but not floodzone
  "1710135038",  # trump tower,
  "1710222007",  # by chicago river and navy pier
  "1710400054",  # by chicago river but not flood zone, no building here?
  "1710403001",  # not residential parcels, some in water
  "1715113004",  # land and partially in water parcels
  "1716401034",  # by south branch of chicago river. not flood zone
  "1717102043",  # 1400 W MONROE OWNER LLC,
  # "1729309068",  # nothing actually wrong with this PIN, weird shape, sales seem normal, just missing eff_date info,
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

  # "2130114029",  # very south side of chicago, coastal condo
  # "2130123021",  # coastal building, not in floodplain according to redfin

  "2130124001",  # almost completely in the lake
  "2130124002",  # almost completely in the lake
  "2130124003",  # almost completely in the lake
  "2130124004",  # almost completely in the lake
  "2130999001",  # actual water canal in calumet area
  "2132213002",  # actual water canal in calumet area

  # "2226307004",  # new homes in Lemont. The ones that use to be a golf course! not FZ
  # "2226307005",  # new homes in Lemont. The ones that use to be a golf course! not FZ
  # "2226307010",
  # "2226307011",
  # "2226307012",

  "2411209095", # went from $89K to $483K  in one day of sales
  "2411209097", # went from $89K to $533K  in one day of sales
  "2411209098", # went from $89K to $511K  in one day of sales
  "2433302053",  # no building here

  "2608202004",  # also water pins.
  "2608400034",  # also water pins.

  "2717100003",  # became 2717100006? fire department in orland park? But doesn't exist in 2025 parcels so didn't match to polygons
  "2717100005",  # became 2717100006? fire department in orland park? But doesn't exist in 2025 parcels so didn't match to polygons

  "2717109004",
  "2727103030",  # was owned by bank (was bought in 2006 for $220K by real humans and then foreclosed. Bank sold to LLC in 2014, then bought for cheap and sold as a rental for a lot of money later, unrealistic price changes for model)
  # "2730212019",  # in A LOMR since 2018, Clover Drive, Orland Park. New Buildings. By floodzone but not in floodzone
  # "2804400093",  # midlothian turnpike. by floodzone but not in floodzone

  "3017211033"   # also water pins.
)
