#include "CWString_hsc.h"
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
#line 58 "CWString.hsc"
#endif 
#line 61 "CWString.hsc"
#ifdef CF_WCHAR_SUPPORT
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
#ifndef __GNUC__
extern inline
#endif
HsInt hs_get_mb_cur_max () 
#ifndef __GNUC__
;
#else
{ return MB_CUR_MAX; }
#endif
#line 289 "CWString.hsc"
#else 
#line 302 "CWString.hsc"
#endif 
