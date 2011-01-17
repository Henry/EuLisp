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

///-----------------------------------------------------------------------------
/// euxlOS.c
///-----------------------------------------------------------------------------
extern void euxcOSInit(const char *banner);
extern void euxcOSFinish();
extern void euxcOSError(const char *msg);
extern int euxcOSRand(int n);
extern FILE *euxcOSAOpen(const char *name, const char *mode);
extern FILE *euxcOSBOpen(const char *name, const char *mode);
extern int euxcOSClose(FILE *fp);
extern long euxcOSTell(FILE *fp);
extern int euxcOSUnlink(const char *path);
extern int euxcOSSeek(FILE *fp, long offset, int whence);
extern int euxcOSAGetc(FILE *fp);
extern int euxcOSAPutc(int ch, FILE *fp);
extern int euxcOSBGetc(FILE *fp);
extern int euxcOSBPutc(int ch, FILE *fp);
extern int euxcOSTGetc();
extern int euxcOSSelect(FILE *fp);
extern int euxcOSPeekChar(FILE *fp);
extern void euxcOSTPutc(int ch);
extern void euxcOSTPuts(const char *str);
extern void euxcOSFlush();
extern void euxcOSCheck();
extern void euxcOSCheckInt();
extern void euxcSetTicksPerSecond();
extern euxlValue euxlCpuTime();
extern euxlValue euxlSystem();
extern euxlValue euxlTmpFile();
extern euxlValue euxlGetenv();
extern euxlValue euxlPutenv();

///-----------------------------------------------------------------------------
/// euxlisp.c
///-----------------------------------------------------------------------------
extern void euxcTopLevel();
extern void euxcTopLevelInt();
extern void euxcFail(const char *msg, euxlValue err);
extern void euxcError(const char *msg, euxlValue arg);
extern void euxcSetFrame(int n);
extern void euxcDoError(const char *msg, euxlValue arg, euxlValue errname, int cc);
extern void euxcIntError(const char *msg, euxlValue arg, euxlValue errname);
extern void euxcCerror(const char *msg, euxlValue arg, euxlValue errname);
extern void euxcCallErrorHandler();
extern void euxcAbort(const char *msg);
extern void euxcFatal(const char *msg);
extern void euxcWrapup(int n);
extern void euxcDoBacktrace(euxlValue *from);
extern euxlValue euxlBacktrace();
extern euxlValue euxlFrameUp();
extern euxlValue euxlFrameDown();
extern euxlValue euxlFrameEnv();
extern euxlValue euxlFrameFun();

///-----------------------------------------------------------------------------
// euxlBCodeCompiler.c
///-----------------------------------------------------------------------------
extern euxlValue euxcCompile(euxlValue expr, euxlValue ctenv);
extern euxlValue euxcCompileFunction(euxlValue fun, euxlValue fargs, euxlValue body, euxlValue ctenv);
extern int euxcListSize(euxlValue list);
extern euxlValue euxcFindSym(euxlValue symbol, euxlValue list);
extern euxlValue euxcFindOrLoadModule(euxlValue sym);
extern euxlValue euxlReintern();
extern euxlValue euxlReinternSyntax();
extern euxlValue euxlModuleDirectives();
extern euxlValue euxcAppend(euxlValue a, euxlValue b);
extern euxlValue euxlSyntaxError();
extern void euxcDecodeProcedure(euxlValue fptr, euxlValue fun);
extern int euxcDecodeInstruction(euxlValue fptr, euxlValue code, int lc, euxlValue env);

///-----------------------------------------------------------------------------
/// euxlAlloc.c
///-----------------------------------------------------------------------------
extern euxlValue euxcCons(euxlValue x, euxlValue y);
extern euxlValue euxcNewFrame(euxlValue parent, int size);
extern euxlValue euxcMakeString(const char *str);
extern euxlValue euxcMakeString2(const char *str, int len);
extern euxlValue euxcMakeSymbol(const char *pname);
extern euxlValue euxcMakeModule(const char *name);
extern euxlValue euxcMakeFPI(long n);
extern euxlValue euxcMakeDoubleFloat(double n);
extern euxlValue euxcMakeChar(int ch);
extern euxlValue euxcMakeClosure(euxlValue code, euxlValue env);
extern euxlValue euxcMakePromise(euxlValue code, euxlValue env);
extern euxlValue euxcMakeXFun(int type, euxcXFunType fcn, int offset);
extern euxlValue euxcMakeFun(int type, euxcFunType fcn, int offset);
extern euxlValue euxcMakeStream(FILE *fp, int flags);
extern euxlValue euxcMakeTable(euxlValue comp, euxlValue fill);
extern euxlValue euxcNewVector(int size);
extern euxlValue euxcNewString(int size);
extern euxlValue euxcNewCode(int nlits);
extern euxlValue euxcNewContinuation(int size);
extern euxlValue euxcNewObject(euxlValue cls, int size);
extern euxlValue euxcNewGeneric();
extern euxlValue euxcNewMethod();
extern euxlValue euxcNewSlot(euxlValue name);
extern int euxcNExpand(int size);
extern int euxcCheckVmemory(int size);
extern int euxcMakeVmemory(int size);
extern int euxcVexpand(int size);
extern euxcNodeSegment *euxcNewnsegment(unsigned int n);
extern euxcVectorSegment *euxcNewvsegment(unsigned int n);
extern void euxcPstack();
extern void gc(int reason);
extern void euxcAllocInit(unsigned int ssize);

///-----------------------------------------------------------------------------
/// euxlFunTab.c
///-----------------------------------------------------------------------------
extern euxlValue euxlStdin();
extern euxlValue euxlStdout();
extern int euxcEq(euxlValue arg1, euxlValue arg2);
extern int euxcEqv(euxlValue arg1, euxlValue arg2);
extern int euxcEqual(euxlValue arg1, euxlValue arg2);
extern int euxcEquals(euxlValue arg1, euxlValue arg2);
extern int euxcVectorEqual(euxlValue v1, euxlValue v2);
extern euxlValue euxcTooFew(const char *functionName);
extern void euxcTooFewInt();
extern void euxcTooMany(const char *functionName);
extern void euxcTooManyInt();
extern euxlValue euxcBadType(euxlValue val, const char *name, const char *fn);

///-----------------------------------------------------------------------------
/// euxlFun1.c
///-----------------------------------------------------------------------------
extern euxlValue euxlCons();
extern euxlValue euxlCar();
extern euxlValue euxlICar();
extern euxlValue euxlCdr();
extern euxlValue euxlICdr();
extern euxlValue euxlCaar();
extern euxlValue euxlCadr();
extern euxlValue euxlCdar();
extern euxlValue euxlCddr();
extern euxlValue euxlCaaar();
extern euxlValue euxlCaadr();
extern euxlValue euxlCadar();
extern euxlValue euxlCaddr();
extern euxlValue euxlCdaar();
extern euxlValue euxlCdadr();
extern euxlValue euxlCddar();
extern euxlValue euxlCdddr();
extern euxlValue euxlCaaaar();
extern euxlValue euxlCaaadr();
extern euxlValue euxlCaadar();
extern euxlValue euxlCaaddr();
extern euxlValue euxlCadaar();
extern euxlValue euxlCadadr();
extern euxlValue euxlCaddar();
extern euxlValue euxlCadddr();
extern euxlValue euxlCdaaar();
extern euxlValue euxlCdaadr();
extern euxlValue euxlCdadar();
extern euxlValue euxlCdaddr();
extern euxlValue euxlCddaar();
extern euxlValue euxlCddadr();
extern euxlValue euxlCdddar();
extern euxlValue euxlCddddr();
extern euxlValue euxlSetCar();
extern euxlValue euxlIsetCar();
extern euxlValue euxlSetCdr();
extern euxlValue euxlIsetCdr();
extern euxlValue euxlList();
extern euxlValue euxlListStar();
extern euxlValue euxlAppend();
extern euxlValue euxcReverseList(euxlValue next);
extern euxlValue euxlReverseList();
extern euxlValue euxlLastPair();
extern euxlValue euxlSize();
extern euxlValue euxlMember();
extern euxlValue euxlMemv();
extern euxlValue euxlMemq();
extern euxlValue euxcMember(euxlValue x, euxlValue list, int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue euxlGeneralMember(int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue euxlAssoc();
extern euxlValue euxlAssv();
extern euxlValue euxlAssq();
extern euxlValue euxcAssoc(int (*fcn)(euxlValue a, euxlValue b));
extern euxlValue euxlListRef();
extern euxlValue euxlListTail();
extern euxlValue euxlBoundp();
extern euxlValue euxlSymValue();
extern euxlValue euxlSetSymValue();
extern euxlValue euxlSymPlist();
extern euxlValue euxlSetSymPlist();
extern euxlValue euxlGet();
extern euxlValue euxlPut();
extern euxlValue euxlGetSyntax();
extern euxlValue euxlPutSyntax();
extern euxlValue euxlTheEnvironment();
extern euxlValue euxlProcEnvironment();
extern euxlValue euxlEnvp();
extern euxlValue euxlEnvBindings();
extern euxlValue euxlEnvParent();
extern euxlValue euxlVector();
extern euxlValue euxlMakeVector();
extern euxlValue euxlVectorSize();
extern euxlValue euxlIVectorSize();
extern euxlValue euxlVectorRef();
extern euxlValue euxlIVectorRef();
extern euxlValue euxlVectorSet();
extern euxlValue euxlIVectorSet();
extern euxlValue euxlVectorToList();
extern euxlValue euxlListToVector();
extern euxlValue euxlNullp();
extern euxlValue euxlAtomp();
extern euxlValue euxlListp();
extern euxlValue euxlNumberp();
extern euxlValue euxlBooleanp();
extern euxlValue euxlConsp();
extern euxlValue euxlSymbolp();
extern euxlValue euxlKeywordp();
extern euxlValue euxlIntegerp();
extern euxlValue euxlDoubleFloatp();
extern euxlValue euxlCharp();
extern euxlValue euxlStringp();
extern euxlValue euxlVectorp();
extern euxlValue euxlFunctionp();
extern euxlValue euxlObjectp();
extern euxlValue euxlDefaultObjectp();
extern euxlValue euxlEq();
extern euxlValue euxlEqv();
extern euxlValue euxlEqual();
extern euxlValue euxlGensym();
extern euxlValue euxlSprintf();

///-----------------------------------------------------------------------------
/// euxlFun2.c
///-----------------------------------------------------------------------------
extern void euxlApply();
extern void euxlValues();
extern void euxlCallCC();
extern void euxlMapList();
extern void euxlMapListCont();
extern void euxlDoList();
extern void euxlDoListCont();
extern void euxlCallWithInput();
extern void euxlCallWithOutput();
extern void euxlWithFileCont();
extern void euxlLoad();
extern void euxlLoadNoisily();
extern void euxlLoadCont();
extern void euxlForce();
extern void euxlForceCont();
extern euxlValue euxlSymbolToString();
extern euxlValue euxlStringToSymbol();
extern euxlValue euxlRead();
extern euxlValue euxlReadChar();
extern euxlValue euxlReadByte();
extern euxlValue euxlReadShort();
extern euxlValue euxlReadLong();
extern euxlValue euxlPeekChar();
extern euxlValue euxlCharReadyp();
extern euxlValue euxlEOFObjectp();
extern euxlValue euxlWrite();
extern euxlValue euxlPrintnl();
extern euxlValue euxlWriteChar();
extern euxlValue euxlWriteByte();
extern euxlValue euxlWriteShort();
extern euxlValue euxlWriteLong();
extern euxlValue euxlPrint();
extern euxlValue euxlSFlush();
extern euxlValue euxlFlush();
extern euxlValue euxlPrintBreadth();
extern euxlValue euxlPrintDepth();
extern euxlValue euxlOpenInput();
extern euxlValue euxlOpenOutput();
extern euxlValue euxlOpenAppend();
extern euxlValue euxlOpenUpdate();
extern euxlValue euxlClose();
extern euxlValue euxlCloseInput();
extern euxlValue euxlCloseOutput();
extern euxlValue euxlGetFilePosition();
extern euxlValue euxlsetFilePosition();
extern euxlValue euxlUnlink();
extern euxlValue euxlStreamp();
extern euxlValue euxlInputStreamp();
extern euxlValue euxlOutputStreamp();
extern euxlValue euxlTransciptOn();
extern euxlValue euxlTranscriptOff();
extern euxlValue euxlMakeString();
extern euxlValue euxlStringLength();
extern euxlValue euxlStringNullp();
extern euxlValue euxlStringAppend();
extern euxlValue euxlStringRef();
extern euxlValue euxlStringSet();
extern euxlValue euxlSubString();
extern euxlValue euxlStringToList();
extern euxlValue euxlListToString();
extern euxlValue euxlStringLt();
extern euxlValue euxlStringLtEq();
extern euxlValue euxlStringEql();
extern euxlValue euxlStringGtEq();
extern euxlValue euxlStringGt();
extern euxlValue euxlstringCaInLt();
extern euxlValue euxlstringCaInLtEq();
extern euxlValue euxlstringCaInEql();
extern euxlValue euxlstringCaInGtEq();
extern euxlValue euxlstringCaInGt();
extern euxlValue euxlCharToInt();
extern euxlValue euxlTntToChar();
extern euxlValue euxlStringToNum();
extern euxlValue euxlNumToString();
extern euxlValue euxlCharLt();
extern euxlValue euxlCharLtEq();
extern euxlValue euxlCharEql();
extern euxlValue euxlCharGtEq();
extern euxlValue euxlCharGt();
extern euxlValue euxlCharCaInLt();
extern euxlValue euxlCharCaInLteq();
extern euxlValue euxlCharCaInEql();
extern euxlValue euxlCharCaInGtEq();
extern euxlValue euxlCharCaInGt();
extern euxlValue euxlCompile();
extern void euxlEvalCm();
extern euxlValue euxlDecompile();
extern euxlValue euxlSave();
extern euxlValue euxlRestore();
extern euxlValue euxlGc();
extern euxlValue euxlError();
extern euxlValue euxcDefaultHandler();
extern euxlValue euxlReset();
extern euxlValue euxlGetArg();
extern euxlValue euxlExit();

///-----------------------------------------------------------------------------
/// euxlImage.c
///-----------------------------------------------------------------------------
extern int euxlSaveImage(const char *fname);
extern FILE *euxcPathOpen(const char *fname, const char *env_var_name, const char **builtin_path, char *found);
extern int euxlRestoreImage(const char *fname);
extern euxmOffType euxcMakePtr(euxlValue p);

///-----------------------------------------------------------------------------
/// euxlInit.c
///-----------------------------------------------------------------------------
extern void euxcInitWorkspace(unsigned int ssize);
extern void euxcInitSymbols();

///-----------------------------------------------------------------------------
/// euxlVM.c
///-----------------------------------------------------------------------------
extern euxlValue euxlTraceOn();
extern euxlValue euxlTraceOff();
extern void euxcExecute(euxlValue fun);
extern void euxcApply();
extern void euxcReturn();
extern euxlValue euxcCurrentContinuation(int cc);
extern void euxcGcProtect(void (*protected_fcn)(void));
extern void euxcStackOverflow();

///-----------------------------------------------------------------------------
/// euxlIO.c
///-----------------------------------------------------------------------------
extern int euxcGetc(euxlValue fptr);
extern void euxcUngetc(euxlValue fptr, int ch);
extern int euxcPeekChar(euxlValue fptr);
extern void euxcPutc(euxlValue fptr, int ch);
extern void euxcFlush();
extern void euxcStdPutString(const char *str);
extern void euxcErrorPrint(euxlValue expr);
extern void euxcErrorPrin(euxlValue expr);
extern void euxcErrorPutString(const char *str);

///-----------------------------------------------------------------------------
/// euxlMath.c
///-----------------------------------------------------------------------------
extern euxlValue euxlExactp();
extern euxlValue euxlInexactp();
extern euxlValue euxlAtan();
extern euxlValue euxlFloor();
extern euxlValue euxlCeiling();
extern euxlValue euxlRound();
extern euxlValue euxlTruncate();
extern euxlValue euxlAdd();
extern euxlValue euxlMul();
extern euxlValue euxlSub();
extern euxlValue euxlDiv();
extern euxlValue euxlQuo();
extern euxlValue euxlRem();
extern euxlValue euxlMin();
extern euxlValue euxlMax();
extern euxlValue euxlExpt();
extern euxlValue euxlLogand();
extern euxlValue euxlLogior();
extern euxlValue euxlLogxor();
extern euxlValue euxlLognot();
extern euxlValue euxlAbs();
extern euxlValue euxlAdd1();
extern euxlValue euxlSub1();
extern euxlValue euxlSin();
extern euxlValue euxlCos();
extern euxlValue euxlTan();
extern euxlValue euxlAsin();
extern euxlValue euxlAcos();
extern euxlValue euxlXexp();
extern euxlValue euxlSqrt();
extern euxlValue euxlXlog();
extern euxlValue euxlRandom();
extern euxlValue euxlGcd();
extern euxlValue euxlNegativep();
extern euxlValue euxlZerop();
extern euxlValue euxlPositivep();
extern euxlValue euxlEvenp();
extern euxlValue euxlOddp();
extern euxlValue euxlLt();
extern euxlValue euxlLtEq();
extern euxlValue euxlEql();
extern euxlValue euxlGtEq();
extern euxlValue euxlGt();

///-----------------------------------------------------------------------------
/// euxlModule.c
///-----------------------------------------------------------------------------
extern void euxcInitRootModule();
extern void euxcInitRootExports();
extern euxlValue euxcModuleSymbols();
extern euxlValue euxcModuleExports();
extern euxlValue euxcSymbolModule();
extern euxlValue euxcCurrentMod();
extern euxlValue euxcModList();
extern euxlValue euxcUnintern();
extern euxlValue euxlKeywordArray();
extern euxlValue euxlSetModule();
extern euxlValue euxcFindModule(euxlValue sym);
extern euxlValue euxlFindModule();
extern euxlValue euxcGetModule(const char *name);

///-----------------------------------------------------------------------------
/// euxlTelos.c
///-----------------------------------------------------------------------------
extern euxlValue euxcClassOf(euxlValue obj);
extern euxlValue euxlClassOf();
extern void euxcDescribe(euxlValue obj);
extern euxlValue euxlDescribe();
extern euxlValue euxlAllocate();
extern euxlValue euxcFindKey(euxlValue key, euxlValue inits, euxlValue deefault);
extern euxlValue euxlFindKey();
extern euxlValue euxlTelosError();
extern void euxlInitLoopCont();
extern void euxlInitializeObject();
extern euxlValue euxlInitializeClass();
extern euxlValue euxlClassName();
extern euxlValue euxlClassSuperclasses();
extern euxlValue euxlClassSubclasses();
extern euxlValue euxlClassSlots();
extern euxlValue euxlClassKeywords();
extern euxlValue euxlSetClassKeywords();
extern euxlValue euxlClassInstsize();
extern euxlValue euxlClassAbstractp();
extern euxlValue euxlClassCpl();
extern euxlValue euxlClassp();
extern int euxcSubClassp(euxlValue cl1, euxlValue cl2);
extern euxlValue euxlSubClassp();
extern euxlValue euxlSetIVar();
extern euxlValue euxlGetIVar();
extern euxlValue euxlGfName();
extern euxlValue euxlGfArgs();
extern euxlValue euxlGfSetargs();
extern euxlValue euxlGfOptargs();
extern euxlValue euxlGfMethods();
extern euxlValue euxlGfCache1();
extern euxlValue euxlGfCache2();
extern euxlValue euxlMethodGf();
extern euxlValue euxlMethodFun();
extern euxlValue euxlMethodDomain();
extern euxlValue euxlSlotName();
extern euxlValue euxlSlotKeyword();
extern euxlValue euxlSlotDefault();
extern euxlValue euxlSetSlotDefault();
extern euxlValue euxlSlotRequiredp();
extern euxlValue euxlSetSlotRequiredp();
extern euxlValue euxlFindSlotIndex();
extern void euxcInitTelos();
extern euxlValue euxcFindAndCacheMethods(euxlValue gf, euxlValue arglist);
extern euxlValue euxlMakeAndAddMethod();
extern euxlValue euxlMakeMethod();
extern euxlValue euxlAddMethod();
extern euxlValue euxlMakeGeneric();
extern void euxcTelosBadRefError(euxlValue object, euxlValue wanted, int interp);
extern euxlValue euxlCheckRef();

///-----------------------------------------------------------------------------
/// euxlPrint.c
///-----------------------------------------------------------------------------
extern void euxcPrin1(euxlValue expr, euxlValue file);
extern void euxcPrint(euxlValue expr, euxlValue file);
extern void euxcTerpri(euxlValue fptr);
extern void euxcPutString(euxlValue fptr, const char *str);

///-----------------------------------------------------------------------------
/// euxlRead.c
///-----------------------------------------------------------------------------
extern int euxcRead(euxlValue fptr, euxlValue *pval);

///-----------------------------------------------------------------------------
/// euxlSymbol.c
///-----------------------------------------------------------------------------
extern void euxcXFun(const char *sname, int type, euxcXFunType fcn, int offset);
extern void euxcFun(const char *sname, int type, euxcFunType fcn, int offset);
extern euxlValue euxcEnterKeyword(const char *name);
extern euxlValue euxcEnterModule(const const char *name, euxlValue module);
extern euxlValue euxcGetProp(euxlValue sym, euxlValue prp);
extern void euxcPutProp(euxlValue sym, euxlValue val, euxlValue prp);
extern euxlValue euxcGetSyntax(euxlValue sym, euxlValue prp);
extern void euxcPutSyntax(euxlValue sym, euxlValue val, euxlValue prp);
extern int euxcHash(const char *str, int len);

///-----------------------------------------------------------------------------
/// euxlTable.c
///-----------------------------------------------------------------------------
extern euxlValue euxlMakeTable();
extern euxlValue euxlTableRef();
extern euxlValue euxlTableSet();
extern euxlValue euxlTableComparator();
extern euxlValue euxlTableDelete();
extern euxlValue euxlTableSize();
extern euxlValue euxlTableKeys();
extern euxlValue euxlTableValues();
extern euxlValue euxlTableFill();
extern euxlValue euxlTableSetFill();
extern euxlValue euxlTableClear();

///-----------------------------------------------------------------------------
/// euxlSocket.c
///-----------------------------------------------------------------------------
#ifdef SOCK
extern void euxlSocketError(const char *msg, euxlValue val);
extern euxlValue euxcSocketSocket();
extern euxlValue euxcSocketConnect();
extern euxlValue euxcSocketBind();
extern euxlValue euxcSocketListen();
extern euxlValue euxcSocketAccept();
extern euxlValue euxcSocketBlock();
extern euxlValue euxcSocketNonBlock();
extern euxlValue euxcSocketReuse();
extern euxlValue euxcSocketNoReuse();
extern euxlValue euxcSocketClose();
extern euxlValue euxcSocketShutdown();
extern euxlValue euxcSocketPeerAddr();
extern euxlValue euxcSocketPeerStream();
extern euxlValue euxcSocketSockAddr();
extern euxlValue euxcSocketSockStream();
extern euxlValue euxcSocketHostToIP();
extern euxlValue euxcSocketIPToHost();
extern euxlValue euxcSocketToStream();
extern euxlValue euxcStreamFd();
extern euxlValue euxcStreamUnbuffered();
extern euxlValue euxcStreamBlockBuffered();
extern euxlValue euxcStreamLineBuffered();
extern euxlValue euxcSocketFdZeroRead();
extern euxlValue euxcSocketFdSetRead();
extern euxlValue euxcSocketFdIssetRead();
extern euxlValue euxcSocketSelectRead();
extern euxlValue euxcSocketFdZeroWrite();
extern euxlValue euxcSocketFdSetWrite();
extern euxlValue euxcSocketFdIssetWrite();
extern euxlValue euxcSocketSelectWrite();
extern int euxcXdrSendInt(FILE *handle, int i);
extern int euxcXdrRecvInt(FILE *handle, int *ip);
extern int euxcXdrSendDouble(FILE *handle, double i);
extern int euxcXdrRecvDouble(FILE *handle, double *ip);
extern int euxcXdrSendString(FILE *handle, const char *i, int len);
extern int euxcXdrRecvString(FILE *handle, char **ip, int len);
extern euxlValue euxcStreamXdrSendInt();
extern euxlValue euxcStreamXdrRecvInt();
extern euxlValue euxcStreamXdrSendDoubleFloat();
extern euxlValue euxcStreamXdrRecvDoubleFloat();
extern euxlValue euxcStreamXdrSendString();
extern euxlValue euxcStreamXdrRecvString();
#endif

///-----------------------------------------------------------------------------
#endif // EUXLPROTO_H
///-----------------------------------------------------------------------------
