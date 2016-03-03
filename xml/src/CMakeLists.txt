find_library(pugixml required)
add_executable(xml load.cpp)
target_link_libraries(xml pugixml)
