include(CTest)
enable_testing()

add_executable(test_patientdata test_patientdata.cc)
target_link_libraries(test_patientdata pugixml ccxml)

add_executable(test_datainfo test_datainfo.cc)
target_link_libraries(test_datainfo pugixml ccxml)


