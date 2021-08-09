// This file is distributed under a BSD license. See LICENSE.txt for details.

#include "_types.hpp"
#include "_startconsole.hpp"
#ifdef __linux__
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <crtdbg.h>
#endif
#include <malloc.h>

/****************************************************************************/
/***                                                                      ***/
/***   System Initialisation                                              ***/
/***                                                                      ***/
/****************************************************************************/

sSystem_ *sSystem;
class sBroker_ *sBroker;

/****************************************************************************/
/****************************************************************************/

size_t MemoryUsedCount;
sInt sFatality = 0;

#if !sINTRO
#undef new
#ifdef __linux__
void * operator new(size_t size, char const *file, int line)
{
  return malloc(size);
}

void * operator new[](size_t size, char const *file, int line)
{
  return malloc(size);
}

void operator delete(void *p)
{
    free(p);
}
#else
void * __cdecl operator new(unsigned int size,const char *file,int line)
{
  void *p;
  p = _malloc_dbg(size,_NORMAL_BLOCK,file,line);
  MemoryUsedCount+=_msize(p); 
  return p;
}

#ifdef __MINGW32__
void * __cdecl operator new[](unsigned int size,const char *file,int line)
{
  void *p;
  p = _malloc_dbg(size,_NORMAL_BLOCK,file,line);
  MemoryUsedCount+=_msize(p);
  return p;
}
#endif

void operator delete(void *p)
{
	if(p)
	{
		MemoryUsedCount-=_msize(p); 
		_free_dbg(p,_NORMAL_BLOCK);
	}
}
#endif

#define new new(__FILE__,__LINE__)
#endif

#if sINTRO
#if !_DEBUG
void * __cdecl malloc(unsigned int size)
{
	return HeapAlloc(GetProcessHeap(),HEAP_NO_SERIALIZE,size);
}

void __cdecl free(void *ptr)
{
	HeapFree(GetProcessHeap(),HEAP_NO_SERIALIZE,ptr);
}
#endif

void * __cdecl operator new(unsigned int size)
{
	return malloc(size);
}

void * __cdecl operator new[](unsigned int size)
{
	return malloc(size);
}

void __cdecl operator delete(void *ptr)
{
	if(ptr)
		free(ptr);
}

void __cdecl operator delete[](void *ptr)
{
	if(ptr)
		free(ptr);
}

int __cdecl _purecall()
{
	return 0;
}

#if !_DEBUG
extern "C" int _fltused;
int _fltused;
#endif

#endif

/****************************************************************************/

sInt main(sInt argc,sChar **argv)
{
  sInt ret;

  sBroker = new sBroker_;
  sSystem = new sSystem_;
  ret = sAppMain(argc,argv);
  delete sSystem;
  delete sBroker;

  return ret;
}

/****************************************************************************/
/***                                                                      ***/
/***   Init/Exit/Debug                                                    ***/
/***                                                                      ***/
/****************************************************************************/

void sSystem_::Log(sChar *s)
{
#ifdef __linux__
  puts(s);
#else
  OutputDebugString(s);
#endif
}

/****************************************************************************/

sNORETURN void sSystem_::Abort(sChar *msg)
{
  _CrtSetDbgFlag(_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG)&~(_CRTDBG_LEAK_CHECK_DF|_CRTDBG_ALLOC_MEM_DF));
  if(msg)
    PrintF("\afatal error: %s\n",msg);
  ExitProcess(0);
}

/****************************************************************************/

void sSystem_::Tag()
{
}

/****************************************************************************/
/***                                                                      ***/
/***   Console IO                                                         ***/
/***                                                                      ***/
/****************************************************************************/

void sSystem_::PrintF(const sChar *format,...)
{
  sChar buffer[2048];

  sFormatString(buffer,2048,format,&format);
#ifdef __linux__
  puts(buffer);
#else
  DWORD written;
  WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),buffer,
    sGetStringLen(buffer),&written,0);
#endif
}

/****************************************************************************/
/***                                                                      ***/
/***   File                                                               ***/
/***                                                                      ***/
/****************************************************************************/

sU8 *sSystem_::LoadFile(const sChar *name,sInt &size)
{
  sInt result;
  sU8 *mem;

  mem = 0;
  result = sFALSE;
#ifdef __linux__
  int fd;
  ssize_t test;

  fd = open(name, O_CREAT | O_RDONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if(fd != -1)
  {
    size = lseek(fd, 0, SEEK_END);
    if(size != -1)
    {
      mem = new sU8[size];
      lseek(fd, 0, SEEK_SET);
      if((test = read(fd, mem, size)))
        result = sTRUE;
      if(size != (sInt)test)
        result = sFALSE;
    }
    close(fd);
  }
#else
  HANDLE handle;
  DWORD test;

  handle = CreateFile(name,GENERIC_READ,FILE_SHARE_READ,0,OPEN_EXISTING,0,0);
  if(handle != INVALID_HANDLE_VALUE)
  {
    size = GetFileSize(handle,&test);
    if(test==0)
    {
      mem = new sU8[size];
      if(ReadFile(handle,mem,size,&test,0))
        result = sTRUE;
      if(size!=(sInt)test)
        result = sFALSE;
    }
    CloseHandle(handle);
  }
#endif

  if(!result)
  {
    if(mem)
      delete[] mem;
    mem = 0;
  }

  return mem;
}

/****************************************************************************/

sU8 *sSystem_::LoadFile(const sChar *name)
{
  sInt dummy;
  return LoadFile(name,dummy);
}

/****************************************************************************/

sChar *sSystem_::LoadText(const sChar *name)
{
  sInt result;
  sU8 *mem;

  mem = 0;
  result = sFALSE;
#ifdef __linux__
  int fd;
  ssize_t size;
  off_t test;

  fd = open(name, O_CREAT | O_RDONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (fd != -1) {
    size = lseek(fd, 0, SEEK_END);
    if (size != -1) {
      mem = new sU8[size+1];
      lseek(fd, 0, SEEK_SET);
      if ((test = read(fd, mem, size)))
        result = sTRUE;
      if (size != (sInt)test)
        result = sFALSE;
      mem[size] = 0;
    }
    close(fd);
  }
#else
  HANDLE handle;
  sInt size;
  DWORD test;

  handle = CreateFile(name,GENERIC_READ,FILE_SHARE_READ,0,OPEN_EXISTING,0,0);
  if(handle != INVALID_HANDLE_VALUE)
  {
    size = GetFileSize(handle,&test);
    if(test==0)
    {
      mem = new sU8[size+1];
      if(ReadFile(handle,mem,size,&test,0))
        result = sTRUE;
      if(size!=(sInt)test)
        result = sFALSE;
      mem[size]=0;
    }
    CloseHandle(handle);
  }
#endif

  if(!result)
  {
    delete[] mem;
    mem = 0;
  }

  return (sChar *)mem;}

/****************************************************************************/

sBool sSystem_::SaveFile(const sChar *name,const sU8 *data,sInt size)
{
  sInt result;

  result = sFALSE;

#ifdef __linux__
  int fd;
  ssize_t test;

  fd = open(name, O_CREAT | O_WRONLY | O_EXCL, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (fd == -1)
      fd = open(name, O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (fd != -1) {
      if ((test = write(fd, data, size)))
          result = sTRUE;
      if (size != (sInt)test)
          result = sFALSE;
      close(fd);
  }
#else
  HANDLE handle;
  DWORD test;

  handle = CreateFile(name,GENERIC_WRITE,FILE_SHARE_WRITE,0,CREATE_NEW,0,0);  
  if(handle == INVALID_HANDLE_VALUE)
    handle = CreateFile(name,GENERIC_WRITE,FILE_SHARE_WRITE,0,TRUNCATE_EXISTING,0,0);  
  if(handle != INVALID_HANDLE_VALUE)
  {
    if(WriteFile(handle,data,size,&test,0))
      result = sTRUE;
    if(size!=(sInt)test)
      result = sFALSE;
    CloseHandle(handle);
  }
#endif

  return result;
}

/****************************************************************************/

sInt sSystem_::GetTime()
{
  return GetTickCount();
}

/****************************************************************************/
