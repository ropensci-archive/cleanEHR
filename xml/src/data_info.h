#ifndef CCXML_DATA_INFO_H
#define CCXML_DATA_INFO_H

#include "ccxml.h"

namespace ccxml{
  class DataInfo {
    public:
      std::string filename;
      t_vstring id_2d_items;
      t_vstring id_2d_time;
      t_vstring id_1d_item;

      DataInfo(std::string file_name): filename(file_name){};
      t_vstring ReadOneLine(std::ifstream &file);
      void readcsv(void);

    private:
      enum{NHICcode, NHICdtCode, NHICmetaCode};
  };



  t_vstring DataInfo::ReadOneLine(std::ifstream &file) {
    t_vstring csv_line;
    std::string line_buffer, cell_buffer;
    std::getline(file, line_buffer);

    std::stringstream line_stream(line_buffer);

    while (std::getline(line_stream, cell_buffer, ',')) {
      csv_line.push_back(cell_buffer);
    }
    return csv_line;
  };

  //!
  void DataInfo::readcsv(void) {
    std::ifstream csv_file (filename);
    if (csv_file.is_open()) {
      //! read header
      auto this_line = ReadOneLine(csv_file);

        for(int i = 0; i < this_line.size(); ++i)
          std::cout << this_line[i] << ",";
        std::cout << "\n";

      while (! csv_file.eof()) {
        this_line = ReadOneLine(csv_file);
      }
    }
    csv_file.close();
  };
}
#endif
