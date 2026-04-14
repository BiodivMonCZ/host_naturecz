paseky_dir <- "../host_data/VMB1_VMB0/"
print(list.files(paseky_dir), max = 10)
paseky_dir2 <- "../host_data/VMB1_VMB2/"
print(list.files(paseky_dir2), max = 10)
paseky_dir3 <- "../host_data/VMB2_VMB0/"
print(list.files(paseky_dir3), max = 10)


example <- sf::st_read(file.path(paseky_dir, "EVL_CZ0110040_L3.1_VMB1_VMB0.gpkg"))
example2 <- sf::st_read(file.path(paseky_dir2, "EVL_CZ0110040_L3.1_VMB1_VMB2.gpkg"))
example3 <- sf::st_read(file.path(paseky_dir3, "EVL_CZ0110040_L3.1_VMB2_VMB0.gpkg"))
#example
head(example)
head(example2)
head(example3)

# evl site:
example$SITECODE
# hab code
example$HABITAT
# okrsek:
example$REGION_ID.x
# datum
example$DATUM.x
example$DATUM.x.1

# evl site:
example2$SITECODE
# hab code
example2$HABITAT
# okrsek:
example2$REGION_ID.x
# datum
example2$DATUM.x
example2$DATUM.x.1

# evl site:
example3$SITECODE
# hab code
example3$HABITAT
# okrsek:
example3$REGION_ID.x
# datum
example3$DATUM.x
example3$DATUM.x.1


# tabulka předmětů ochrany v daných EVL
head(sites_habitats)
all(sites_habitats$sdf_code == sites_habitats$feature_code)
