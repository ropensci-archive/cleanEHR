find_library(pugixml required)
add_library(ccxml data_info.cc patient2json.cc records.cc)
target_link_libraries(pugixml)
