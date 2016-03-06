#include "data_info.h"

t_vstring DataInfo::ReadOneLine(std::ifstream &file) {
  t_vstring csv_line;
  std::string line_buffer, cell_buffer;
  std::getline(file, line_buffer);

  std::stringstream line_stream(line_buffer);

  while (std::getline(line_stream, cell_buffer, ',')) {
    csv_line.push_back(cell_buffer);
  }
  return csv_line;
}


//!
void DataInfo::GetCodeList(std::map<std::string, t_vstring>& code_list) {
  std::vector<std::string>::iterator match_col;
  std::map<std::string, int> col_index;
  t_vstring this_line;
  std::string error_message;
  int col_num; // col number obtained from the header.

  std::ifstream csv_file (filename);
  if (csv_file.is_open()) {
    t_vstring header = ReadOneLine(csv_file); // read header.
    col_num = header.size(); // assign column number.

    for (auto item : check_list){ //get a map items in the check_list. 
      match_col = find(header.begin(), header.end(), item);
      if (match_col == header.end()) {
        error_message.assign("cannot find the searching column: ");
        error_message.append(item).append(" from ").append(filename);
        throw std::out_of_range(error_message);
      }
      col_index[item] = match_col - header.begin();
    }

    // read lines from the csv file. 
    while (! csv_file.eof()) {
      this_line = ReadOneLine(csv_file);
      if (this_line.size() == 0 ) break;
      else if(this_line.size() != col_num) 
        throw std::out_of_range("csv does not have the same column number.");
      for (auto item : check_list)
        code_list[item].push_back(this_line[col_index[item]]);
    }
  }
  csv_file.close();
}

void DataInfo::GetCategoryList(std::map<std::string, code_type>& category_table) {
  GetCodeList(nhic_code_list);
  for (auto id: nhic_code_list["NHICcode"])
    if (id != "NULL") category_table[id] = ITEM_2D;
  for (auto id: nhic_code_list["NHICdtCode"])
    if (id != "NULL") category_table[id] = TIME_CODE;
  for (auto id: nhic_code_list["NHICmetaCode"])
    if (id != "NULL") category_table[id] = META_CODE;
}




  /*  for (auto category: check_list) {
      for (auto id: nhic_code_list[category]) {
      if (id != "NULL") {
      if (id == "NHICcode") { // differentiate 1d and 2d data.
      int this_index = &id - &nhic_code_list[category][0]; 
      if (nhic_code_list["NHICdtCode"][this_index] == "NULL")
      category_table[id] = ITEM_1D; 
      else 
      category_table[id] = "1";
      }
      else if (id == "NHICdtCode")
      category_table[id] = TIME_CODE; 
      else if ("NHICmetaCode")
      category_table[id] = META_CODE;
      else
      throw std::out_of_range("category label cannot be found.");
      }
      }
      }
      }*/

