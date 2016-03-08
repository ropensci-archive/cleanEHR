#ifndef CCXML_PATIENT_DATA_H
#define CCXML_PATIENT_DATA_H

#include "type.h"
#include "data_info.h"

#include <algorithm>
#include <map>
#include <assert.h>
#include <regex>
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
    DataInfo info;

    t_vstring values;
    t_vstring labels;

    //! store simple 1D data with its NHIC code as keys.
    typedef std::map<std::string, std::string> t_simple_data;
    //! store time-wise data and meta data. Level 1 keys: NHIC code; Level 2 
    //! keys: (time, val, meta). e.g. a["NIHR_HIC_ICU_0001"]["val"][0].
    typedef std::map<std::string, std::map<std::string, t_vstring>> t_time_data;

    t_simple_data simple_data;
    t_time_data time_data;

    PatientData(xml_node n, DataInfo info_): node(n), info(info_){
      TreeWalker walker;
      node.traverse(walker);

      values = walker.values_;
      labels = walker.labels_;
      assert(values.size() == labels.size());
      GetSimpleData();
      GetTimeData();
    };


    t_vstring FindCode(t_vstring code_list, code_type type) {
      t_vstring select_code;
      for (std::string cl: code_list) {
        if (info.nhic_code_category[cl] == type)
          select_code.push_back(cl);
      }
      return select_code;
    };


    //! 
    void GetSimpleData() { 
      //! make sure values and labels have been initialised.
      assert(values.size() != 0);
      assert(labels.size() != 0);

      for (std::string &l: labels) {
        if (info.nhic_code_category[l] == ITEM_1D) {
          auto i = &l - &labels[0];
          simple_data[l] = values[i];
        }
      }
    };


    //! TODO meta data should be considered in the future. 
    void GetTimeData() {
      t_vstring labels_unique = labels;

      std::sort(labels_unique.begin(), labels_unique.end());
      auto it_uni = std::unique(labels_unique.begin(), labels_unique.end());
      labels_unique.erase(it_uni, labels_unique.end());

      t_vstring item_code_unique = FindCode(labels_unique, ITEM_2D);
      t_vstring time_code_unique = FindCode(labels_unique, TIME_CODE);

      assert(item_code_unique.size() == time_code_unique.size());

      for (auto item: item_code_unique) {
        for (int i = 0; i < labels.size(); ++i) {
          if (labels[i] == item)
            time_data[item]["val"].push_back(values[i]);
          if (labels[i] == info.item2time[item]) 
            time_data[item]["time"].push_back(values[i]);
        }
        assert(time_data[item]["time"].size() != 0);
        assert(time_data[item]["time"].size() == time_data[item]["val"].size());
      }
    };


    //! \brief traverse the whole xml tree of a patient and extract the NHIC
    //! labels and corresponding values.
    struct TreeWalker: pugi::xml_tree_walker {
      t_vstring values_;
      t_vstring labels_;

      //! \brief update for_each() to feed xml_node::traverse.
      virtual bool for_each(pugi::xml_node& node){
        std::string node_name = node.name();
        std::size_t pos = node_name.find("NIHR_HIC_ICU_");
        if(pos != std::string::npos) {
          labels_.push_back(node_name.substr(pos));
          values_.push_back(node.first_child().value());
        }
        return true;
      }
    };
};



#endif
