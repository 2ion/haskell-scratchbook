#ifndef CURSES_CURSES_HSC_H
#define CURSES_CURSES_HSC_H
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
#line 150 "Curses.hsc"
#include <my_curses.h>
#line 232 "Curses.hsc"
#if 1
#line 242 "Curses.hsc"
#else 
#line 252 "Curses.hsc"
#endif 
#line 403 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_color_pair (HsInt pair) {return COLOR_PAIR (pair);}
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
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_ulcorner (void) {return ACS_ULCORNER;}
#line 1133 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_llcorner (void) {return ACS_LLCORNER;}
#line 1134 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_urcorner (void) {return ACS_URCORNER;}
#line 1135 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_lrcorner (void) {return ACS_LRCORNER;}
#line 1136 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_rtee     (void) {return ACS_RTEE;}
#line 1137 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_ltee     (void) {return ACS_LTEE;}
#line 1138 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_btee     (void) {return ACS_BTEE;}
#line 1139 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_ttee     (void) {return ACS_TTEE;}
#line 1140 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_hline    (void) {return ACS_HLINE;}
#line 1141 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_vline    (void) {return ACS_VLINE;}
#line 1142 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_plus     (void) {return ACS_PLUS;}
#line 1143 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_s1       (void) {return ACS_S1;}
#line 1144 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_s9       (void) {return ACS_S9;}
#line 1145 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_diamond  (void) {return ACS_DIAMOND;}
#line 1146 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_ckboard  (void) {return ACS_CKBOARD;}
#line 1147 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_degree   (void) {return ACS_DEGREE;}
#line 1148 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_plminus  (void) {return ACS_PLMINUS;}
#line 1149 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_bullet   (void) {return ACS_BULLET;}
#line 1150 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_larrow   (void) {return ACS_LARROW;}
#line 1151 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_rarrow   (void) {return ACS_RARROW;}
#line 1152 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_darrow   (void) {return ACS_DARROW;}
#line 1153 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_uarrow   (void) {return ACS_UARROW;}
#line 1154 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_board    (void) {return ACS_BOARD;}
#line 1155 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_lantern  (void) {return ACS_LANTERN;}
#line 1156 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_block    (void) {return ACS_BLOCK;}
#line 1157 "Curses.hsc"
#ifdef ACS_S3
#line 1158 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_s3       (void) {return ACS_S3;}
#line 1159 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_s7       (void) {return ACS_S7;}
#line 1160 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_lequal   (void) {return ACS_LEQUAL;}
#line 1161 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_gequal   (void) {return ACS_GEQUAL;}
#line 1162 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_pi       (void) {return ACS_PI;}
#line 1163 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_nequal   (void) {return ACS_NEQUAL;}
#line 1164 "Curses.hsc"
#ifdef __GNUC__
extern
#endif
inline chtype hs_curses_acs_sterling (void) {return ACS_STERLING;}
#line 1165 "Curses.hsc"
#endif 
#line 1167 "Curses.hsc"
#endif 
#line 1176 "Curses.hsc"
#if 0
#line 1215 "Curses.hsc"
#endif 
#endif
