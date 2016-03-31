#include "records.h"


Records::Records(std::string xml, std::string info) {
  Records::info_file.assign(info);
  Records::xml_file.assign(xml);
  Records::information = DataInfo(Records::info_file);
}

void Records::parse_xml(int id_start, int id_end){
  pugi::xml_document doc;
  pugi::xml_parse_result result = doc.load_file(Records::xml_file.c_str());
  if (!result) std::cout << result.description() << std::endl;


  pugi::xml_node root = doc.document_element();
  //! skip the meta data
  auto context_node = root.first_child().first_child();
  xml_node pd = context_node.next_sibling();
  if (id_end == -1) id_end = std::distance(pd.begin(), pd.end());
  
  int id = 0;
  for (auto pd_node = pd.begin(); pd_node != pd.end(); ++pd_node) {
    if (id >= id_start & id <= id_end) {
      PatientData pd = PatientData(*pd_node, Records::information, id);
      if (!pd.bad_data){
        Records::episodes.push_back(pd);
      }
    }
    id++;
  }
  Records::npatient = episodes.size();
}

void Records::parse_xml(int id) {
  Records::parse_xml(id, id);
}

void Records::parse_xml() {
  Records::parse_xml(0, -1);
}

void Records::push_back(PatientData&) {


}
