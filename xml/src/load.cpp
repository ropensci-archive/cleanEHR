#include "ccxml.h"
class MetaData {
  public:
    MetaData(xml_node node) {
      std::cout << "initialised\n";
    }; 
};

class PatientData : pugi::xml_node {
  public:
    PatientData(xml_node n): node(n){};
    pugi::xml_node node; 

    //! \brief traverse the whole xml tree of a patient and extract the NHIC
    //! labels and corresponding values.
    struct simple_walker: pugi::xml_tree_walker{
      t_vstring values;
      t_vstring labels;

      //! \brief update for_each() to feed xml_node::traverse.
      virtual bool for_each(pugi::xml_node& node){
        std::string node_name = node.name();
        if(node_name.find("NIHR_HIC_ICU_")!=std::string::npos) {
          labels.push_back(node.name());
          values.push_back(node.first_child().value());
        }
        return true;
      }
    };

    void getList(void) {
      simple_walker walker;
      node.traverse(walker);


      /*      for(int i = 0; i < walker.values.size(); ++i)
              std::cout << walker.labels[i] << " = " << walker.values[i] << std::endl;*/
    }
};




int main()
{
  /*  pugi::xml_document doc;
      pugi::xml_parse_result result = doc.load_file("/Users/sinan/workspace/projects/UCLH/data-play/xml/data/cc.xml");
      if (!result)
      std::cout << result.description() << std::endl;

      pugi::xml_node root = doc.document_element();
      auto context_node = root.first_child().first_child();
      xml_node patient1 = context_node.next_sibling().first_child();


      int i = 0;
      for(auto n = patient1; n; n = n.next_sibling()){
      i++;
      auto pd1 = PatientData(n);
      pd1.getList();
      //    std::cout << i <<std::endl;
      }
      */
  auto data_info = ccxml::DataInfo("/Users/sinan/workspace/projects/UCLH/data-play/xml/data/data_item.csv");
  data_info.readcsv();

  return 0;
}
