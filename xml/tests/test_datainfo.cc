#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "ccxml.h"

TEST_CASE("Code list") {
  // TODO automatically search path
  auto data_info = DataInfo("/Users/sinan/workspace/projects/UCLH/data-play/xml/data/data_item.csv");
  SECTION("check code list by giving some abitrary codes") {
    CHECK(data_info.nhic_code_list["NHICcode"][0] == "NIHR_HIC_ICU_0002");
    CHECK(data_info.nhic_code_list["NHICdtCode"][0] == "NULL");
    CHECK(data_info.nhic_code_list["NHICmetaCode"][0] == "NULL");
  }
}


TEST_CASE("Category list") {
  // TODO automatically search path
  auto data_info = DataInfo("/Users/sinan/workspace/projects/UCLH/data-play/xml/data/data_item.csv");
  SECTION("check category table by nhic code list data.") {
  for (auto i: data_info.nhic_code_list["NHICcode"])
    if ( i != "NULL")    
      CHECK((data_info.nhic_code_category[i] == ITEM_1D or 
            data_info.nhic_code_category[i] == ITEM_2D));
  
  for (auto i: data_info.nhic_code_list["NHICdtCode"])
    if ( i != "NULL")    
      CHECK(data_info.nhic_code_category[i] == TIME_CODE);
  
  for (auto i: data_info.nhic_code_list["NHICmetaCode"])
    if ( i != "NULL")    
      CHECK(data_info.nhic_code_category[i] == META_CODE);
  }


  SECTION("check category table by some random code.") {
    CHECK(data_info.nhic_code_category["NIHR_HIC_ICU_0002"] == ITEM_1D);
    CHECK(data_info.nhic_code_category["NIHR_HIC_ICU_0931"] == ITEM_2D);
    CHECK(data_info.nhic_code_category["NIHR_HIC_ICU_0941"] == TIME_CODE);
    CHECK(data_info.nhic_code_category["NIHR_HIC_ICU_0866"] == META_CODE);
  }
}
