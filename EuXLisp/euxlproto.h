/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: Function prototypes
///  Description:
//    Automatically generated function prototypes by cproto
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------
#ifndef EUXLPROTO_H
#define EUXLPROTO_H

// osspecific.c
extern void osinit(char *banner);
extern void osfinish();
extern void oserror(char *msg);
extern int osrand(int n);
extern FILE *osaopen(char *name, char *mode);
extern FILE *osbopen(char *name, char *mode);
extern int osclose(FILE *fp);
extern long ostell(FILE *fp);
extern int osunlink(char *path);
extern int osseek(FILE *fp, long offset, int whence);
extern int osagetc(FILE *fp);
extern int osaputc(int ch, FILE *fp);
extern int osbgetc(FILE *fp);
extern int osbputc(int ch, FILE *fp);
extern int ostgetc();
extern int osselect(FILE *fp);
extern int ospeekchar(FILE *fp);
extern void ostputc(int ch);
extern void ostputs(char *str);
extern void osflush();
extern void oscheck();
extern void oscheck_int();
extern void set_ticks_per_second();
extern euxlValue x_cpu_time();
extern void check_if_disabled(char *name);
extern euxlValue xsystem();
extern euxlValue xtmpfile();
extern euxlValue xgetenv();
extern euxlValue xputenv();

// euxlisp.c
extern void xlmain(int argc, char **argv);
extern void xlload();
extern void xlcontinue();
extern void xlcleanup();
extern void xltoplevel();
extern void xltoplevel_int();
extern void xlfail(char *msg, euxlValue err);
extern void xlerror(char *msg, euxlValue arg);
extern void set_xlframe(int n);
extern void do_xlerror(char *msg, euxlValue arg, euxlValue errname, int cc);
extern void xlinterror(char *msg, euxlValue arg, euxlValue errname);
extern void xlcerror(char *msg, euxlValue arg, euxlValue errname);
extern void callerrorhandler();
extern void xlabort(char *msg);
extern void xlfatal(char *msg);
extern void xlwrapup(int n);
extern void do_backtrace(euxlValue *from);
extern euxlValue xbacktrace();
extern euxlValue xframe_up();
extern euxlValue xframe_down();
extern euxlValue xframe_env();
extern euxlValue xframe_fun();

// xscom.c
extern euxlValue xlcompile(euxlValue expr, euxlValue ctenv);
extern euxlValue xlfunction(euxlValue fun, euxlValue fargs, euxlValue body, euxlValue ctenv);
extern int list_size(euxlValue list);
extern euxlValue findsym(euxlValue symbol, euxlValue list);
extern euxlValue find_or_load_module(euxlValue sym);
extern euxlValue xreintern();
extern euxlValue xreintern_syntax();
extern euxlValue xmodule_directives();
extern euxlValue append(euxlValue a, euxlValue b);
extern euxlValue xsyntax_error();
extern void decode_procedure(euxlValue fptr, euxlValue fun);
extern int decode_instruction(euxlValue fptr, euxlValue code, int lc, euxlValue env);

// xsdmem.c
extern euxlValue cons(euxlValue x, euxlValue y);
extern euxlValue newframe(euxlValue parent, int size);
extern euxlValue cvstring(char *str);
extern euxlValue cvstring2(char *str, int len);
extern euxlValue cvsymbol(char *pname);
extern euxlValue cvmodule(char *name);
extern euxlValue cvfixnum(long n);
extern euxlValue cvflonum(double n);
extern euxlValue cvchar(int ch);
extern euxlValue cvclosure(euxlValue code, euxlValue env);
extern euxlValue cvpromise(euxlValue code, euxlValue env);
extern euxlValue cvsubr(int type, euxlValue (*fcn)(), int offset);
extern euxlValue cvstream(FILE *fp, int flags);
extern euxlValue cvtable(euxlValue comp, euxlValue fill);
extern euxlValue newvector(int size);
extern euxlValue newstring(int size);
extern euxlValue newcode(int nlits);
extern euxlValue newcontinuation(int size);
extern euxlValue newobject(euxlValue cls, int size);
extern euxlValue newgeneric();
extern euxlValue newmethod();
extern euxlValue newslot(euxlValue name);
extern int nexpand(int size);
extern int checkvmemory(int size);
extern int makevmemory(int size);
extern int vexpand(int size);
extern NSEGMENT *newnsegment(unsigned int n);
extern VSEGMENT *newvsegment(unsigned int n);
extern void pstack();
extern void gc(int reason);
extern void xlminit(unsigned int ssize);

// xsftab.c
extern euxlValue xstdin();
extern euxlValue xstdout();
extern int eq(euxlValue arg1, euxlValue arg2);
extern int eqv(euxlValue arg1, euxlValue arg2);
extern int equal(euxlValue arg1, euxlValue arg2);
extern int equals(euxlValue arg1, euxlValue arg2);
extern int vectorequal(euxlValue v1, euxlValue v2);
extern euxlValue xltoofew(char *cfn_name);
extern void xltoofew_int();
extern void xltoomany(char *cfn_name);
extern void xltoomany_int();
extern euxlValue xlbadtype(euxlValue val, char *name, char *fn);

// xsfun1.c
extern euxlValue xcons();
extern euxlValue xcar();
extern euxlValue xicar();
extern euxlValue xcdr();
extern euxlValue xicdr();
extern euxlValue xcaar();
extern euxlValue xcadr();
extern euxlValue xcdar();
extern euxlValue xcddr();
extern euxlValue xcaaar();
extern euxlValue xcaadr();
extern euxlValue xcadar();
extern euxlValue xcaddr();
extern euxlValue xcdaar();
extern euxlValue xcdadr();
extern euxlValue xcddar();
extern euxlValue xcdddr();
extern euxlValue xcaaaar();
extern euxlValue xcaaadr();
extern euxlValue xcaadar();
extern euxlValue xcaaddr();
extern euxlValue xcadaar();
extern euxlValue xcadadr();
extern euxlValue xcaddar();
extern euxlValue xcadddr();
extern euxlValue xcdaaar();
extern euxlValue xcdaadr();
extern euxlValue xcdadar();
extern euxlValue xcdaddr();
extern euxlValue xcddaar();
extern euxlValue xcddadr();
extern euxlValue xcdddar();
extern euxlValue xcddddr();
extern euxlValue xsetcar();
extern euxlValue xisetcar();
extern euxlValue xsetcdr();
extern euxlValue xisetcdr();
extern euxlValue xlist();
extern euxlValue xliststar();
extern euxlValue xappend();
extern euxlValue xlreverse(euxlValue next);
extern euxlValue xreverse();
extern euxlValue xlastpair();
extern euxlValue xsize();
extern euxlValue xmember();
extern euxlValue xmemv();
extern euxlValue xmemq();
extern euxlValue xlmember(euxlValue x, euxlValue list, int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue member(int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue xassoc();
extern euxlValue xassv();
extern euxlValue xassq();
extern euxlValue assoc(int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue xlistref();
extern euxlValue xlisttail();
extern euxlValue xboundp();
extern euxlValue xsymvalue();
extern euxlValue xsetsymvalue();
extern euxlValue xsymplist();
extern euxlValue xsetsymplist();
extern euxlValue xget();
extern euxlValue xput();
extern euxlValue xgetsyntax();
extern euxlValue xputsyntax();
extern euxlValue xtheenvironment();
extern euxlValue xprocenvironment();
extern euxlValue xenvp();
extern euxlValue xenvbindings();
extern euxlValue xenvparent();
extern euxlValue xvector();
extern euxlValue xmakevector();
extern euxlValue xvsize();
extern euxlValue xivsize();
extern euxlValue xvref();
extern euxlValue xivref();
extern euxlValue xvset();
extern euxlValue xivset();
extern euxlValue xvectlist();
extern euxlValue xlistvect();
extern euxlValue xnullp();
extern euxlValue xatomp();
extern euxlValue xlistp();
extern euxlValue xnumberp();
extern euxlValue xbooleanp();
extern euxlValue xconsp();
extern euxlValue xsymbolp();
extern euxlValue xkeywordp();
extern euxlValue xintegerp();
extern euxlValue xfloatp();
extern euxlValue xcharp();
extern euxlValue xstringp();
extern euxlValue xvectorp();
extern euxlValue xfunctionp();
extern euxlValue xobjectp();
extern euxlValue xdefaultobjectp();
extern euxlValue xeq();
extern euxlValue xeqv();
extern euxlValue xequal();
extern euxlValue xgensym();
extern euxlValue xsprintf();

// xsfun2.c
extern void xapply();
extern void xvalues();
extern void xcallcc();
extern void xmap();
extern void xmap1();
extern void xforeach();
extern void xforeach1();
extern void xcallwi();
extern void xcallwo();
extern void xwithfile1();
extern void xload();
extern void xloadnoisily();
extern void xload1();
extern void xforce();
extern void xforce1();
extern euxlValue xsymstr();
extern euxlValue xstrsym();
extern euxlValue xread();
extern euxlValue xrdchar();
extern euxlValue xrdbyte();
extern euxlValue xrdshort();
extern euxlValue xrdlong();
extern euxlValue xpeek_char();
extern euxlValue xchar_readyp();
extern euxlValue xeofobjectp();
extern euxlValue xwrite();
extern euxlValue xprintnl();
extern euxlValue xwrchar();
extern euxlValue xwrbyte();
extern euxlValue xwrshort();
extern euxlValue xwrlong();
extern euxlValue xprint();
extern euxlValue xsflush();
extern euxlValue xflush();
extern euxlValue xprbreadth();
extern euxlValue xprdepth();
extern euxlValue xopeni();
extern euxlValue xopeno();
extern euxlValue xopena();
extern euxlValue xopenu();
extern euxlValue xclose();
extern euxlValue xclosei();
extern euxlValue xcloseo();
extern euxlValue xgetfposition();
extern euxlValue xsetfposition();
extern euxlValue xunlink();
extern euxlValue xstreamp();
extern euxlValue xinputstreamp();
extern euxlValue xoutputstreamp();
extern euxlValue xtranson();
extern euxlValue xtransoff();
extern euxlValue xmakestring();
extern euxlValue xstrlen();
extern euxlValue xstrnullp();
extern euxlValue xstrappend();
extern euxlValue xstrref();
extern euxlValue xstrset();
extern euxlValue xsubstring();
extern euxlValue xstrlist();
extern euxlValue xliststring();
extern euxlValue xstrlss();
extern euxlValue xstrleq();
extern euxlValue xstreql();
extern euxlValue xstrgeq();
extern euxlValue xstrgtr();
extern euxlValue xstrilss();
extern euxlValue xstrileq();
extern euxlValue xstrieql();
extern euxlValue xstrigeq();
extern euxlValue xstrigtr();
extern euxlValue xcharint();
extern euxlValue xintchar();
extern euxlValue xstringnum();
extern euxlValue xnumstring();
extern euxlValue xchrlss();
extern euxlValue xchrleq();
extern euxlValue xchreql();
extern euxlValue xchrgeq();
extern euxlValue xchrgtr();
extern euxlValue xchrilss();
extern euxlValue xchrileq();
extern euxlValue xchrieql();
extern euxlValue xchrigeq();
extern euxlValue xchrigtr();
extern euxlValue xcompile();
extern euxlValue xdecompile();
extern euxlValue xsave();
extern euxlValue xrestore();
extern euxlValue xgc();
extern euxlValue xerror();
extern euxlValue default_handler();
extern euxlValue xreset();
extern euxlValue xgetarg();
extern euxlValue xexit();

// xsimage.c
extern int xlisave(char *fname);
extern FILE *path_open(char *fname, char *env_var_name, char **builtin_path, char *found);
extern int xlirestore(char *fname);
extern OFFTYPE cvoptr(euxlValue p);

// xsinit.c
extern void xlinitws(unsigned int ssize);
extern void xlsymbols();

// xsint.c
extern euxlValue xtraceon();
extern euxlValue xtraceoff();
extern void xlexecute(euxlValue fun);
extern void xlapply();
extern void xlreturn();
extern euxlValue current_continuation(int cc);
extern void gc_protect(void (*protected_fcn)(void));
extern void xlstkover();

// xsio.c
extern int xlgetc(euxlValue fptr);
extern void xlungetc(euxlValue fptr, int ch);
extern int xlpeekchar(euxlValue fptr);
extern void xlputc(euxlValue fptr, int ch);
extern void xlflush();
extern void stdputstr(char *str);
extern void errprint(euxlValue expr);
extern void errprin(euxlValue expr);
extern void errputstr(char *str);

// xsmath.c
extern euxlValue xexactp();
extern euxlValue xinexactp();
extern euxlValue xatan();
extern euxlValue xfloor();
extern euxlValue xceiling();
extern euxlValue xround();
extern euxlValue xtruncate();
extern euxlValue xadd();
extern euxlValue xmul();
extern euxlValue xsub();
extern euxlValue xdiv();
extern euxlValue xquo();
extern euxlValue xrem();
extern euxlValue xmin();
extern euxlValue xmax();
extern euxlValue xexpt();
extern euxlValue xlogand();
extern euxlValue xlogior();
extern euxlValue xlogxor();
extern euxlValue xlognot();
extern euxlValue xabs();
extern euxlValue xadd1();
extern euxlValue xsub1();
extern euxlValue xsin();
extern euxlValue xcos();
extern euxlValue xtan();
extern euxlValue xasin();
extern euxlValue xacos();
extern euxlValue xxexp();
extern euxlValue xsqrt();
extern euxlValue xxlog();
extern euxlValue xrandom();
extern euxlValue xgcd();
extern euxlValue xnegativep();
extern euxlValue xzerop();
extern euxlValue xpositivep();
extern euxlValue xevenp();
extern euxlValue xoddp();
extern euxlValue xlss();
extern euxlValue xleq();
extern euxlValue xeql();
extern euxlValue xgeq();
extern euxlValue xgtr();

// xsmodule.c
extern void init_root_module();
extern void init_root_exports();
extern euxlValue module_symbols();
extern euxlValue module_exports();
extern euxlValue symbol_module();
extern euxlValue current_mod();
extern euxlValue mod_list();
extern euxlValue unintern();
extern euxlValue xkeyword_array();
extern euxlValue xset_module();
extern euxlValue find_module(euxlValue sym);
extern euxlValue xfind_module();
extern euxlValue get_module(char *name);

// xsobj.c
extern euxlValue class_of(euxlValue obj);
extern euxlValue xclassof();
extern void xldescribe(euxlValue obj);
extern euxlValue xdescribe();
extern euxlValue xallocate();
extern euxlValue find_key(euxlValue key, euxlValue inits, euxlValue deefault);
extern euxlValue xfind_key();
extern euxlValue xtelos_error();
extern void xinitloop1();
extern void xinitialize_object();
extern euxlValue xinitialize_class();
extern euxlValue xclass_name();
extern euxlValue xclass_superclasses();
extern euxlValue xclass_subclasses();
extern euxlValue xclass_slots();
extern euxlValue xclass_keywords();
extern euxlValue xset_class_keywords();
extern euxlValue xclass_instsize();
extern euxlValue xclass_abstractp();
extern euxlValue xclass_cpl();
extern euxlValue xclassp();
extern int xlsubclassp(euxlValue cl1, euxlValue cl2);
extern euxlValue xsubclassp();
extern euxlValue xsetivar();
extern euxlValue xgetivar();
extern euxlValue xgf_name();
extern euxlValue xgf_args();
extern euxlValue xgf_setargs();
extern euxlValue xgf_optargs();
extern euxlValue xgf_methods();
extern euxlValue xgf_cache1();
extern euxlValue xgf_cache2();
extern euxlValue xmethod_gf();
extern euxlValue xmethod_fun();
extern euxlValue xmethod_domain();
extern euxlValue xslot_name();
extern euxlValue xslot_keyword();
extern euxlValue xslot_default();
extern euxlValue xset_slot_default();
extern euxlValue xslot_requiredp();
extern euxlValue xset_slot_requiredp();
extern euxlValue xfind_slot_index();
extern void xloinit();
extern euxlValue find_and_cache_methods(euxlValue gf, euxlValue arglist);
extern euxlValue xmake_and_add_method();
extern euxlValue xmake_method();
extern euxlValue xadd_method();
extern euxlValue xmake_generic();
extern void telos_bad_ref_error(euxlValue object, euxlValue wanted, int interp);
extern euxlValue xcheck_ref();

// xsprint.c
extern void xlprin1(euxlValue expr, euxlValue file);
extern void xlprint(euxlValue expr, euxlValue file);
extern void xlterpri(euxlValue fptr);
extern void xlputstr(euxlValue fptr, char *str);

// xsread.c
extern int xlread(euxlValue fptr, euxlValue *pval);

// xssym.c
extern void xlsubr(char *sname, int type, euxlValue (*fcn)(), int offset);
extern euxlValue xlenter_keyword(char *name);
extern euxlValue xlenter_module(char *name, euxlValue module);
extern euxlValue xlgetprop(euxlValue sym, euxlValue prp);
extern void xlputprop(euxlValue sym, euxlValue val, euxlValue prp);
extern euxlValue xlgetsyntax(euxlValue sym, euxlValue prp);
extern void xlputsyntax(euxlValue sym, euxlValue val, euxlValue prp);
extern int hash(char *str, int len);

// xstable.c
extern euxlValue xmake_table();
extern euxlValue xtable_ref();
extern euxlValue xtable_set();
extern euxlValue xtable_comparator();
extern euxlValue xtable_delete();
extern euxlValue xtable_size();
extern euxlValue xtable_keys();
extern euxlValue xtable_values();
extern euxlValue xtable_fill();
extern euxlValue xtable_setfill();
extern euxlValue xtable_clear();

// xsocket.c
#ifdef SOCK
extern void xlsockerror(char *msg, euxlValue val);
extern euxlValue socket_socket();
extern euxlValue socket_connect();
extern euxlValue socket_bind();
extern euxlValue socket_listen();
extern euxlValue socket_accept();
extern euxlValue socket_block();
extern euxlValue socket_nonblock();
extern euxlValue socket_reuse();
extern euxlValue socket_noreuse();
extern euxlValue socket_close();
extern euxlValue socket_shutdown();
extern euxlValue socket_peeraddr();
extern euxlValue socket_peerstream();
extern euxlValue socket_sockaddr();
extern euxlValue socket_sockstream();
extern euxlValue socket_host_to_ip();
extern euxlValue socket_ip_to_host();
extern euxlValue socket_convert_to_stream();
extern euxlValue stream_fd();
extern euxlValue stream_unbuffered();
extern euxlValue stream_block_buffered();
extern euxlValue stream_line_buffered();
extern euxlValue socket_fd_zero_read();
extern euxlValue socket_fd_set_read();
extern euxlValue socket_fd_isset_read();
extern euxlValue socket_select_read();
extern euxlValue socket_fd_zero_write();
extern euxlValue socket_fd_set_write();
extern euxlValue socket_fd_isset_write();
extern euxlValue socket_select_write();
extern int xdr_send_int(FILE *handle, int i);
extern int xdr_recv_int(FILE *handle, int *ip);
extern int xdr_send_double(FILE *handle, double i);
extern int xdr_recv_double(FILE *handle, double *ip);
extern int xdr_send_string(FILE *handle, char *i, int len);
extern int xdr_recv_string(FILE *handle, char **ip, int len);
extern euxlValue stream_xdr_send_int();
extern euxlValue stream_xdr_recv_int();
extern euxlValue stream_xdr_send_float();
extern euxlValue stream_xdr_recv_float();
extern euxlValue stream_xdr_send_string();
extern euxlValue stream_xdr_recv_string();
#endif

///-----------------------------------------------------------------------------
#endif // EUXLPROTO_H
///-----------------------------------------------------------------------------
