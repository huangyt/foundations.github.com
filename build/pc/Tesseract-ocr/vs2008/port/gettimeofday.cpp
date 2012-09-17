///////////////////////////////////////////////////////////////////////
// File:        gettimeofday.cpp
// Description: Implementation of gettimeofday based on leptonica
// Author:      tomp2010, zdenop
// Created:     Tue Feb 21 21:38:00 CET 2012
//
// (C) Copyright 2012, Google Inc.
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
///////////////////////////////////////////////////////////////////////

#include <allheaders.h>
#include "gettimeofday.h"

#if 0
void l_getCurrentTime(l_int32  *sec, l_int32  *usec)
{
	ULARGE_INTEGER  utime, birthunix;
	FILETIME        systemtime;
	LONGLONG        birthunixhnsec = 116444736000000000;  /*in units of 100 ns */
	LONGLONG        usecs;

	GetSystemTimeAsFileTime(&systemtime);
	utime.LowPart  = systemtime.dwLowDateTime;
	utime.HighPart = systemtime.dwHighDateTime;

	birthunix.LowPart = (DWORD) birthunixhnsec;
	birthunix.HighPart = birthunixhnsec >> 32;

	usecs = (LONGLONG) ((utime.QuadPart - birthunix.QuadPart) / 10);

	if (sec) *sec = (l_int32) (usecs /   1000000);
	if (usec) *usec = (l_int32) (usecs % 1000000);
	return;
}
#endif // #ifdef _WIN32

int gettimeofday(struct timeval *tp, struct timezone *tzp) {
  l_int32 sec, usec;
  if (tp == NULL)
    return -1;

  l_getCurrentTime(&sec, &usec);
  tp->tv_sec = sec;
  tp->tv_usec = usec;
  return 0;
}
