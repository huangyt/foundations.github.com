// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/srsync/SyncFileOperation.cpp,v 1.8 2005/02/05 10:45:30 kommer Exp $

#include "SyncFileOperation.h"
#include <acdk/util/Arrays.h>

namespace acdk {
namespace net {
namespace srsync {

using namespace acdk::net::srfsys;

bool SyncFileOperation::NeverOverwriteFiles = false;
bool SyncFileOperation::IgnoreFileTime = false;


RSyncFileOperationArray
SyncFileOperation::merge(IN(RString) lpath, IN(RFileInfoArray) locals, IN(RString) rpath, IN(RFileInfoArray) remotes)
{
  RSyncFileOperationArray ra = new SyncFileOperationArray(0);
  SynFileComparator comparator(lpath, rpath);
  RSynFileComparator sfc(&comparator);
#ifdef _MSC_VER
  // GPF
  ::acdk::util::ArraysImpl::sort(RObjectArray(locals));
  ::acdk::util::ArraysImpl::sort(RObjectArray(remotes));
#else
  ::acdk::util::Arrays::sort(locals, sfc);
  ::acdk::util::Arrays::sort(remotes, sfc);
#endif
  
  int i;
  for (i = 0; i < locals->length(); ++i)
  {
    RFileInfo cl = locals[i];
    //int t = ::acdk::util::Arrays::sequenceSearch(remotes, cl, &comparator);
    

    //int t = acdk::util::Arrays::binarySearch<RFileInfo, RSynFileComparator>(remotes, cl, sfc);
    int t = acdk::util::Arrays::binarySearch(remotes, cl, sfc);
    //int t = ::acdk::util::ArraysImpl::binarySearch(RObjectArray(remotes), RObject(cl), &comparator);
    if (t == -1)
    {
      RFileInfo remotefi = new FileInfo(cl);
      remotefi->dir = rpath + File::separator() + cl->dir->substr(lpath->length());
      //remotefi->name = cl->name;
      //remotefi->flags = cl->flags;
      remotefi->exists(false);
      RSyncFileOperation fop = new SyncFileOperation(CopyToRemote, cl, remotefi);
      ra->append(fop);
    } else {
      
      RFileInfo cr = remotes[t];
      cr->isChecked(true);
      if (IgnoreFileTime == false && cl->modified > cr->modified)
      {
        cr->modified = cl->modified;
        cr->created = cl->created;
        cr->flags = cl->flags;
        ra->append(new SyncFileOperation(CopyToRemote, cl, cr));
      } else  if (IgnoreFileTime == false && cl->modified < cr->modified) {
        cl->modified = cr->modified;
        cl->created = cr->created;
        cl->flags = cr->flags;
        ra->append(new SyncFileOperation(CopyToLocal, cl, cr));
      } else {
        ra->append(new SyncFileOperation(InSync, cl, cr));
      }

    }
  }
  for (i = 0; i < remotes->length(); ++i)
  {
    RFileInfo cr = remotes[i];
    if (cr->isChecked() == false)
    {
      RFileInfo localfi = new FileInfo();
      localfi->dir = lpath + File::separator() + cr->dir->substr(rpath->length());
      localfi->name = cr->name;
      localfi->exists(false);
      RSyncFileOperation fop = new SyncFileOperation(CopyToLocal, localfi, cr);
      ra->append(fop);
    }
  }
  return ra;
  /*
newmove:
  for (i = 0; i < ra->length(); ++i)
  {
    RSyncFileOperation rai = ra[i];
    if (rai->fop == CopyToLocal)
    {
      for (int j = 0; j < ra->length(); ++j)
      {
        RSyncFileOperation raj = ra[j];
        if (raj->fop == CopyToRemote)
        {
          if (raj->localFileInfo->digest == rai->remoteFileInfo->digest)
          {
            RSyncFileOperation fop = new SyncFileOperation(MoveLocal, raj->localFileInfo, rai->remoteFileInfo);
            ra->append(fop);
            if (j < i)
            {
              ra = ::acdk::util::Arrays::removeElement(ra, j);
              ra = ::acdk::util::Arrays::removeElement(ra, i);
            } else {
              ra = ::acdk::util::Arrays::removeElement(ra, i);
              ra = ::acdk::util::Arrays::removeElement(ra, j);
            }
            goto newmove;
          }

        }
      }
    }
  }
  return ra;
  */
}



} //namespace srsync
} //namespace acdk 
} // namespace acdk 



