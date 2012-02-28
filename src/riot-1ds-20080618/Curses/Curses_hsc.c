#include "Curses_hsc.h"
#line 232 "Curses.hsc"
#if 1
#line 242 "Curses.hsc"
#else 
#line 252 "Curses.hsc"
#endif 
#line 403 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_color_pair (HsInt pair) 
#ifndef __GNUC__
;
#else
{return COLOR_PAIR (pair);}
#endif
#line 539 "Curses.hsc"
#if defined(CF_WCHAR_SUPPORT) && defined(HAVE_WADDNWSTR)
#line 564 "Curses.hsc"
#else 
#line 586 "Curses.hsc"
#endif 
#line 872 "Curses.hsc"
#ifdef KEY_RESIZE
#line 874 "Curses.hsc"
#endif 
#line 875 "Curses.hsc"
#ifdef KEY_MOUSE
#line 877 "Curses.hsc"
#endif 
#line 908 "Curses.hsc"
#ifdef HAVE_RESIZETERM
#line 914 "Curses.hsc"
#else 
#line 918 "Curses.hsc"
#endif 
#line 923 "Curses.hsc"
#ifdef SIGWINCH
#line 927 "Curses.hsc"
#else 
#line 931 "Curses.hsc"
#endif 
#line 976 "Curses.hsc"
#ifdef KEY_MOUSE
#line 1006 "Curses.hsc"
#else 
#line 1009 "Curses.hsc"
#endif 
#line 1056 "Curses.hsc"
#if 1
#line 1086 "Curses.hsc"
#ifdef ACS_S3
#line 1094 "Curses.hsc"
#endif 
#line 1122 "Curses.hsc"
#ifdef ACS_S3
#line 1130 "Curses.hsc"
#endif 
#line 1132 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_ulcorner (void) 
#ifndef __GNUC__
;
#else
{return ACS_ULCORNER;}
#endif
#line 1133 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_llcorner (void) 
#ifndef __GNUC__
;
#else
{return ACS_LLCORNER;}
#endif
#line 1134 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_urcorner (void) 
#ifndef __GNUC__
;
#else
{return ACS_URCORNER;}
#endif
#line 1135 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_lrcorner (void) 
#ifndef __GNUC__
;
#else
{return ACS_LRCORNER;}
#endif
#line 1136 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_rtee     (void) 
#ifndef __GNUC__
;
#else
{return ACS_RTEE;}
#endif
#line 1137 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_ltee     (void) 
#ifndef __GNUC__
;
#else
{return ACS_LTEE;}
#endif
#line 1138 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_btee     (void) 
#ifndef __GNUC__
;
#else
{return ACS_BTEE;}
#endif
#line 1139 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_ttee     (void) 
#ifndef __GNUC__
;
#else
{return ACS_TTEE;}
#endif
#line 1140 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_hline    (void) 
#ifndef __GNUC__
;
#else
{return ACS_HLINE;}
#endif
#line 1141 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_vline    (void) 
#ifndef __GNUC__
;
#else
{return ACS_VLINE;}
#endif
#line 1142 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_plus     (void) 
#ifndef __GNUC__
;
#else
{return ACS_PLUS;}
#endif
#line 1143 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_s1       (void) 
#ifndef __GNUC__
;
#else
{return ACS_S1;}
#endif
#line 1144 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_s9       (void) 
#ifndef __GNUC__
;
#else
{return ACS_S9;}
#endif
#line 1145 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_diamond  (void) 
#ifndef __GNUC__
;
#else
{return ACS_DIAMOND;}
#endif
#line 1146 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_ckboard  (void) 
#ifndef __GNUC__
;
#else
{return ACS_CKBOARD;}
#endif
#line 1147 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_degree   (void) 
#ifndef __GNUC__
;
#else
{return ACS_DEGREE;}
#endif
#line 1148 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_plminus  (void) 
#ifndef __GNUC__
;
#else
{return ACS_PLMINUS;}
#endif
#line 1149 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_bullet   (void) 
#ifndef __GNUC__
;
#else
{return ACS_BULLET;}
#endif
#line 1150 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_larrow   (void) 
#ifndef __GNUC__
;
#else
{return ACS_LARROW;}
#endif
#line 1151 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_rarrow   (void) 
#ifndef __GNUC__
;
#else
{return ACS_RARROW;}
#endif
#line 1152 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_darrow   (void) 
#ifndef __GNUC__
;
#else
{return ACS_DARROW;}
#endif
#line 1153 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_uarrow   (void) 
#ifndef __GNUC__
;
#else
{return ACS_UARROW;}
#endif
#line 1154 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_board    (void) 
#ifndef __GNUC__
;
#else
{return ACS_BOARD;}
#endif
#line 1155 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_lantern  (void) 
#ifndef __GNUC__
;
#else
{return ACS_LANTERN;}
#endif
#line 1156 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_block    (void) 
#ifndef __GNUC__
;
#else
{return ACS_BLOCK;}
#endif
#line 1157 "Curses.hsc"
#ifdef ACS_S3
#line 1158 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_s3       (void) 
#ifndef __GNUC__
;
#else
{return ACS_S3;}
#endif
#line 1159 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_s7       (void) 
#ifndef __GNUC__
;
#else
{return ACS_S7;}
#endif
#line 1160 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_lequal   (void) 
#ifndef __GNUC__
;
#else
{return ACS_LEQUAL;}
#endif
#line 1161 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_gequal   (void) 
#ifndef __GNUC__
;
#else
{return ACS_GEQUAL;}
#endif
#line 1162 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_pi       (void) 
#ifndef __GNUC__
;
#else
{return ACS_PI;}
#endif
#line 1163 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_nequal   (void) 
#ifndef __GNUC__
;
#else
{return ACS_NEQUAL;}
#endif
#line 1164 "Curses.hsc"
#ifndef __GNUC__
extern inline
#endif
chtype hs_curses_acs_sterling (void) 
#ifndef __GNUC__
;
#else
{return ACS_STERLING;}
#endif
#line 1165 "Curses.hsc"
#endif 
#line 1167 "Curses.hsc"
#endif 
#line 1176 "Curses.hsc"
#if 0
#line 1215 "Curses.hsc"
#endif 
