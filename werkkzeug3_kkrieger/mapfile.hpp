// This file is distributed under a BSD license. See LICENSE.txt for details.

#ifndef __MAPFILE_HPP_
#define __MAPFILE_HPP_

#include "debuginfo.hpp"

/****************************************************************************/

class MAPFileReader : public DebugInfoReader
{
  struct Section;
  sArray<Section> Sections;

  sInt ScanString(const sChar *&string, DebugInfo &to);
  static bool IsHexString(const sChar *str, sInt count);

  Section *GetSection(sInt num,sU32 offs);

public:
  bool ReadDebugInfo(sChar *fileName, DebugInfo &to);
};

/****************************************************************************/

#endif
