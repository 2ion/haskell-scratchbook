#ifndef GINSU_CWSTRING_HSC_H
#define GINSU_CWSTRING_HSC_H
#include <HsFFI.h>
#if __NHC__
#undef HsChar
#define HsChar int
#endif
#define CF_CHARSET_SUPPORT 1
#define CF_WCHAR_SUPPORT 1
#define HAVE_WADDNWSTR 1
#define HAVE_RESIZETERM 1
#define GHC64 1
#line 13 "CWString.hsc"
#ifdef CF_WCHAR_SUPPORT
#line 24 "CWString.hsc"
#endif 
#line 37 "CWString.hsc"
#if __GLASGOW_HASKELL__ >= 603
#line 44 "CWString.hsc"
#else 
#line 47 "CWString.hsc"
#endif 
#line 56 "CWString.hsc"
#if !defined(__STDC_ISO_10646__)
#line 57 "CWString.hsc"
#undef CF_WCHAR_SUPPORT
#line 58 "CWString.hsc"
#endif 
#line 61 "CWString.hsc"
#ifdef CF_WCHAR_SUPPORT
#line 67 "CWString.hsc"
#include <wchar.h>
#line 68 "CWString.hsc"
#include <limits.h>
#line 69 "CWString.hsc"
#include <stdlib.h>
#line 91 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 101 "CWString.hsc"
#endif 
#line 113 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 115 "CWString.hsc"
#else 
#line 123 "CWString.hsc"
#endif 
#line 126 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 128 "CWString.hsc"
#else 
#line 136 "CWString.hsc"
#endif 
#line 139 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 141 "CWString.hsc"
#else 
#line 149 "CWString.hsc"
#endif 
#line 152 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 155 "CWString.hsc"
#else 
#line 165 "CWString.hsc"
#endif 
#line 168 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 170 "CWString.hsc"
#else 
#line 179 "CWString.hsc"
#endif 
#line 182 "CWString.hsc"
#ifndef __GLASGOW_HASKELL__
#line 184 "CWString.hsc"
#else 
#line 195 "CWString.hsc"
#endif 
#line 198 "CWString.hsc"
#else 
#line 201 "CWString.hsc"
#endif 
#line 207 "CWString.hsc"
#if defined(CF_WCHAR_SUPPORT)
#line 234 "CWString.hsc"
#ifdef __GNUC__
extern
#endif
inline HsInt hs_get_mb_cur_max () { return MB_CUR_MAX; }
#line 289 "CWString.hsc"
#else 
#line 302 "CWString.hsc"
#endif 
#endif
