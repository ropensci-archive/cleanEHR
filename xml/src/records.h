#ifndef CCXML_RECORDS_H
#define CCXML_RECORDS_H

#include "type.h"
#include "patient_data.h"
#include "data_info.h"
#include <vector>
#include <string>
#include "pugixml.hpp"
class Records {
  public:
    //! number of patient in this record.
    int npatient = 0;
    //! xml file location.
    std::string xml_file;
    //! information table (csv) location.
    std::string info_file;
    //! information table.
    DataInfo information;
    //! records of multiple patients.
    std::vector<PatientData> episodes;
    //! \brief initialise the record by reading the xml file.
    Records(std::string xml, std::string info);
    //! \brief parse the entire xml file.
    void parse_xml();
    //! \brief parse a spesific episode, episode id is based appearing sequence
    //!  of the xml file. The first episode id is 0.
    void parse_xml(int id);
    //! \brief parse a selected range of episodes, episode id is 
    //!  based appearing sequence of the xml file. The first episode id is 0.
    void parse_xml(int id_start, int id_end);

    void push_back(PatientData&);
    void erase(int id);
};
#endif
