#ifndef CCXML_PATIENT_DATA_H
#define CCXML_PATIENT_DATA_H

#include "type.h"
#include "data_info.h"

#include <algorithm>
#include <map>
#include "pugixml.hpp"

using namespace pugi;

class MetaData {
  public:
    MetaData(xml_node node) {
      std::cout << "initialised\n";
    }; 
};

class PatientData : pugi::xml_node{
  public:
    pugi::xml_node node;
    t_vstring values;
    t_vstring labels;
    DataInfo info; 

    PatientData(xml_node n, DataInfo info_): node(n), info(info_){
      TreeWalker walker;
      node.traverse(walker);

      values = walker.values_;
      labels = walker.labels_;

      

    };

    //! \brief traverse the whole xml tree of a patient and extract the NHIC
    //! labels and corresponding values.
    struct TreeWalker: pugi::xml_tree_walker {
      t_vstring values_;
      t_vstring labels_;

      //! \brief update for_each() to feed xml_node::traverse.
      virtual bool for_each(pugi::xml_node& node){
        std::string node_name = node.name();
        if(node_name.find("NIHR_HIC_ICU_")!=std::string::npos) {
          labels_.push_back(node.name());
          values_.push_back(node.first_child().value());
        }
        return true;
      }
    };
};



#endif
