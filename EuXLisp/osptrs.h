// osptrs.h - table entries for machine specific functions 
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford 

#ifndef OSPTRS_H
#define OSPTRS_H

#ifdef MACINTOSH
{
"hidepen", xhidepen}

,
{
"showpen", xshowpen}

,
{
"getpen", xgetpen}

,
{
"pensize", xpensize}

,
{
"penmode", xpenmode}

,
{
"penpat", xpenpat}

,
{
"pennormal", xpennormal}

,
{
"moveto", xmoveto}

,
{
"move", xmove}

,
{
"lineto", xlineto}

,
{
"line", xline}

,
{
"show-graphics", xshowgraphics}

,
{
"hide-graphics", xhidegraphics}

,
{
"clear-graphics", xcleargraphics}

,
#endif
#ifdef MSDOS
{
"system", xsystem}

,
{
"get-key", xgetkey}

,
{
"difftime", xdifftime}

,
#ifdef NOTDEF
{
"int86", xint86}

,
{
"inbyte", xinbyte}

,
{
"outbyte", xoutbyte}

,
#endif
#endif
#ifdef UNIX
{
"system", xsystem}

,
#endif

#endif
