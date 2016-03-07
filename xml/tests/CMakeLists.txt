include(CTest)
enable_testing()
add_executable(test_datainfo test_datainfo.cc)
target_link_libraries(test_datainfo pugixml ccxml)
