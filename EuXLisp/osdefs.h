// osdefs.h - extern declarations for machine specific functions 
/*      Copyright (c) 1988, by David Michael Betz
        All Rights Reserved */
// Euscheme code Copyright (c) 1994 Russell Bradford 

#ifndef OSDEFS_H
#define OSDEFS_H

#ifdef MACINTOSH
extern LVAL xhidepen(), xshowpen(), xgetpen(), xpensize(), xpenmode();
extern LVAL xpenpat(), xpennormal(), xmoveto(), xmove(), xlineto(), xline();
extern LVAL xshowgraphics(), xhidegraphics(), xcleargraphics();
#endif

#ifdef MSDOS
extern LVAL xsystem(), xgetkey(), xtime(), xdifftime();
#ifdef NOTDEF
extern LVAL xint86(), xinbyte(), xoutbyte();
#endif
#endif

#ifdef UNIX
extern LVAL xsystem();
#endif

#endif
