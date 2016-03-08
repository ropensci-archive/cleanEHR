#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "ccxml.h"


TEST_CASE("Test patient data record", "[record]") {
  Records test_record = Records("../data/cc.xml", "../data/data_item.csv");

  SECTION("Check the first patient time-wise data.") {
    test_record.parse_xml(0);
    CHECK(test_record.npatient == 1);
    auto heart_rate = test_record.episodes[0].time_data["NIHR_HIC_ICU_0108"]["val"];
    for (auto i: heart_rate) {
      CHECK(std::stoi(i) >= 0);
      CHECK(std::stoi(i) < 250);
    }
  }

  SECTION("Check the id of first two patients.") {
    test_record.parse_xml(0, 2);
    CHECK(test_record.npatient == 3);
    CHECK(test_record.episodes[0].simple_data["NIHR_HIC_ICU_0073"] == 
        "57fb752c860311e4ae76005056b34847");
    CHECK(test_record.episodes[2].simple_data["NIHR_HIC_ICU_0073"] == 
        "5824fa28860311e4ae76005056b34847");
  }
}
