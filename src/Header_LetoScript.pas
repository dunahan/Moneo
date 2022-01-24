(*------------------------------------------------------------------------------
Header_LetoScript

Definitions that need to be fully abstract from any one unit, so that any unit
can refer to them in its interface. This prevents pesky circular uses problems.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Header_LetoScript;

{$I LetoScript.inc}

interface

uses
  Classes,
  Header_Leto;

const

  LS_Version		= 'UNICORN';
  MyVersion		= '.27rc3';
  VersionDate		= 'Sep 30 2006';

type

  TLetoScriptOp		= (
    opUnknown, opInvalid, opNull, opComment,
    opStop,
    opLParen, opRParen, opLBrace, opRBrace, opLBrack, opRBrack,
    opConst, opIConst, opSymbol,
    opBlock, opExpr, opSet,
    opSub, opFunc,
    opDeref, opAttrib,
    opAuto,
    opBinExp,
    opLogNot, opUnaMinus, opBitNot, opUnaPlus, opRef,
    opBind,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus, opAddSp, {opRemSp,}
    opShl, opShr,
    opUnaryOp,
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp,
    opBitAnd,
    opBitOr, opBitXor,
    opLogAnd,
    opLogOr,
    opRange,
    opCondIf, opCondThen,
    opAssign, opLAssign,
    opKey, opComma, { note different precedence }
    opListOp,
    opLitNot,
    opLitAnd,
    opLitOr, opLitXor,
    opNamedOp
  );
  TLetoScriptOpSet	= set of TLetoScriptOp;

  TLetoScriptOpKind	= (
    okNone,
    { Comment }
    comCPP, comPascal, comDoc,
    { Const }
    numInt, numReal, numSci,
    numBin, numHex, numOct,
    numCurrency, numDate,
    quSingle, quDouble, quPrintn, quCmd,
    quQ, quQQ, quQW, quQX, quQXU,
    idBare, idSafe, idLabel, idAttrib,
    { Symbols }
    symScalar, symList, symField, symFile,
    { Autocrement }
    acPostInc, acPostDec, acPreInc, acPreDec,
    { Assign }
    eqExp, eqPlus, eqMinus,
    eqMult, eqDiv, eqMod,
    eqBitAnd, eqBitOr, eqBitXor,
    eqShl, eqShr, eqLogAnd, eqLogOr,
    { Named operators }
    noIf, noElsif, noElse, noUnless,
    noDo, noUntil, noWhile, noFor, noContinue,
    noSwitch
  );
  TLetoScriptOpKindSet	= set of TLetoScriptOpKind;


  TLetoScriptValFlag	= (
    vfAnon, vfDefinedCheck, vfEvalEach,
    vfVivify, vfMyVivify,
    vfConditional,
    vfKey, vfKeyValue
  );
  TLetoScriptValFlags	= set of TLetoScriptValFlag;


  TLetoScriptOpCon	= (
    ocVoid, ocString,
    ocScalar, ocList, ocBool,
    ocSymList, ocParams
  );


  TLetoScriptFnSys	= (
    fnNil, fnNamed,
    { System }
    fnCaller, fnChomp, fnChop, fnChr, fnClose,
    fnDefined, fnDelete, fnDie, fnEach, fnExists, fnExit,
    fnGoto, fnGreedy, fnGrep, fnHex, fnIndex, fnIsNum, fnJoin, fnKeys,
    fnLast, fnLc, fnLength, fnList, fnLocal, fnLocaltime,
    fnMap, fnMy, fnNext, fnNo, fnOct, fnOrd,
    fnPack, fnPop, fnPrint, fnPrintF, fnPrintList, fnPrintN, fnPush,
    fnRand, fnRedo, fnReset, fnReturn, fnReverse, fnRindex,
    fnS, fnScalar, fnShift, fnSleep, fnSort, fnSplice, fnSplit,
    fnSprintf, fnSub, fnSubst, fnSubstr, fnSystem, fnSystemQ,
    fnTime, fnUc, fnUndef, fnUnpack, fnUnshift, fnUse,
    fnValues, fnVar, fnWantArray, fnWarn
  );
  TLetoScriptFnSysSet	= set of TLetoScriptFnSys;

  TLetoScriptFnMath	= (
    { Arithmetic }
    fnMathAbs, fnMathCeil, fnMathDivMod, fnMathExp,
    fnMathFloor, fnMathFrac, fnMathFrexp,
    fnMathInt, fnMathIsZero,
    fnMathLdexp, fnMathLn, fnMathLnXP1, fnMathLog2, fnMathLog10, fnMathLogN,
    fnMathMax, fnMathMin, fnMathMulDiv,
    fnMathPoly, fnMathRound, fnMathRoundTo,
    fnMathSameValue, fnMathSign, fnMathSimpleRoundTo, fnMathSqr, fnMathSqrt,
    fnMathTrunc,
    { infinity, isinfinite, isnan, nan, neginfinity, pi }
    { Trig }
    fnMathACos, fnMathACosH, fnMathACot, fnMathACotH,
    fnMathACsc, fnMathACscH, fnMathASec, fnMathASecH,
    fnMathASin, fnMathASinH, fnMathATan, fnMathATan2, fnMathATanH,
    fnMathCos, fnMathCosin, fnMathCosH,
    fnMathCot, fnMathCotan, fnMathCotH,
    fnMathCosecant, fnMathCsc, fnMathCscH,
    fnMathHypot,
    fnMathSec, fnMathSecant, fnMathSecH, fnMathSin, fnMathSinCos, fnMathSinH,
    fnMathTan, fnMathTanH
    { Geom }
  );
  TLetoScriptFnMathSet	= set of TLetoScriptFnMath;

  TLetoScriptFnGff	= (
    fnGffAdd, fnGffClear, fnGffDelete, fnGffFind, fnGffReplace
  );
  TLetoScriptFnGffSet	= set of TLetoScriptFnGff;

  TLetoScriptFnErf	= (
    fnErfImport, fnErfExport, fnErfList, fnErfRemove
  );
  TLetoScriptFnErfSet	= set of TLetoScriptFnErf;

  TLetoScriptFnBic	= (
    fnBicFindBicByTag, fnBicFindNewestBic, fnBicVault
  );
  TLetoScriptFnBicSet	= set of TLetoScriptFnBic;

  TLetoScriptFnTlk	= (
    fnTlkDialog, fnTlkAddStrref, fnTlkFindStrref,
    fnTlkGetStrref, fnTlkGetStrrefFlags,
    fnTlkSetStrref, fnTlkSetStrrefFlags
  );
  TLetoScriptFnTlkSet	= set of TLetoScriptFnTlk;

  TLetoScriptFn2da	= (
    fn2daLocate, fn2daLookup, fn2daMeta
  );
  TLetoScriptFn2daSet	= set of TLetoScriptFn2da;

  TLetoScriptFnFpt	= (
    fnFptExtract, fnFptInject
  );
  TLetoScriptFnFptSet	= set of TLetoScriptFnFpt;

  TLetoScriptFnFileSys	= (
    fnFSFileCopy, fnFSFileDelete, fnFSDirExists, fnFSFileExists, fnFSFileInfo,
    fnFSFileMove, fnFSFileRename, fnFSForceDirs
  );
  TLetoScriptFnFileSysSet = set of TLetoScriptFnFileSys;

  {$IFDEF LETOSCRIPT_SQL}
  TLetoScriptFnSql	= (
    fnSqlBindBlob, fnSqlBof, fnSqlConnect, fnSqlDisconnect, fnSqlEof,
    fnSqlExec, fnSqlField, fnSqlFirst, fnSqlFieldNames, fnSqlFields,
    fnSqlInsertId, fnSqlLast, fnSqlNext, fnSqlParams, fnSqlPrior, fnSqlQuery,
    fnSqlRecordCount, fnSqlRetrieve, fnSqlRowsAffected, fnSqlStore
  );
  TLetoScriptFnSqlSet	= set of TLetoScriptFnSql;
  {$ENDIF}


type

  TFuncCode		= record
    Name		: String;
    Fn			: Integer;
    Code		: String;
  end;

const

  { These are the modules or methods that are DISABLED by default.
    Must include a leading and trailing comma for dirty Pos() hacks to work. }
  DefaultACL		= ',filesys,system.system,';

  // TODO 3: ADD: common sigs? like 'GFF V3.2'
  LS_Functions_Gff	: array[0..21] of TFuncCode = (
    (Name: 'gffinvalid';	Fn: -1;
     Code: '$0000'),
    (Name: 'gffbyte';		Fn: -1;
     Code: '$0001'),
    (Name: 'gffchar';		Fn: -1;
     Code: '$0002'),
    (Name: 'gffword';		Fn: -1;
     Code: '$0004'),
    (Name: 'gffshort';		Fn: -1;
     Code: '$0008'),
    (Name: 'gffdword';		Fn: -1;
     Code: '$0010'),
    (Name: 'gffint';		Fn: -1;
     Code: '$0020'),
    (Name: 'gffdword64';	Fn: -1;
     Code: '$0040'),
    (Name: 'gffint64';		Fn: -1;
     Code: '$0080'),
    (Name: 'gfffloat';		Fn: -1;
     Code: '$0100'),
    (Name: 'gffdouble';		Fn: -1;
     Code: '$0200'),
    (Name: 'gffstring';		Fn: -1;
     Code: '$0400'),
    (Name: 'gffresref';		Fn: -1;
     Code: '$0800'),
    (Name: 'gfflocstring';	Fn: -1;
     Code: '$1000'),
    (Name: 'gffvoid';		Fn: -1;
     Code: '$2000'),
    (Name: 'gffstruct';		Fn: -1;
     Code: '$4000'),
    (Name: 'gfflist';		Fn: -1;
     Code: '$8000'),
    (Name: 'add';		Fn: Word(fnGffAdd);
     Code: '$name!, $value=0, $type=; /copyfrom, $lang=0, $setifexists=0'),
    (Name: 'clear';		Fn: Word(fnGffClear);
     Code: '/field='),
    (Name: 'delete';		Fn: Word(fnGffDelete);
     Code: '/field, $lang='),
    (Name: 'find';		Fn: Word(fnGffFind);
     Code: '$name, $value=*, /root=; ' +
     '$matchcase=0, $type, $lang=-1, $strref, ' +
     '$depth=-1, $target=-1, $haschildren, ' +
     '&match!'),
    (Name: 'replace';		Fn: Word(fnGffReplace);
     Code: '$name, $value=*, $setvalue=, /root=; ' +
     '$matchcase=0, $type, $lang=-1, $strref, ' +
     '$depth=-1, $target=-1, $haschildren, ' +
     '$settype=0, $setlang=-1, $setstrref, $delete=0, $deleteparent=0, ' +
     '&match!')
  );

  LS_Functions_Erf	: array[0..3] of TFuncCode = (
     // DONE 1: ADD: [27r3] SaveAs param after a greedy list
     // TODO 4: CHANGE: Eliminate need for [L] as 'hint'?
    (Name: 'export';		Fn: Word(fnErfExport);
     Code: '@resmask=; $saveas [L]'),
    (Name: 'import';		Fn: Word(fnErfImport);
     Code: '$filemask; $saveas [L]'),
    (Name: 'list';		Fn: Word(fnErfList);
     Code: '@resmask= [L]'),
    (Name: 'remove';		Fn: Word(fnErfRemove);
     Code: '@resmask= [L]')
  );

  LS_Functions_Bic	: array[0..2] of TFuncCode = (
    (Name: 'findbicbytag';	Fn: Word(fnBicFindBicByTag);
     Code: '$path, $tag; $filemask=*.bic'),
    (Name: 'findnewestbic';	Fn: Word(fnBicFindNewestBic);
     Code: '$path; $filemask=*.bic'),
    // DONE 1: CHANGE: [27r3] vault -FullPaths is now -NoFullPaths (notes)
    { => True is redundant
      The idiom throughout LetoScript is that an option is always False until
      set True. Because specifying the option without a value has the effect
      of setting it True, this has an added benefit of easy-to-read syntax:
      vault '/path', -NoFullPaths, -Recursive
    }
    (Name: 'vault';		Fn: Word(fnBicVault);
     Code: '$path=$_, $filemask=*.bic; ' +
     '$readonly=0, $hidden=0, $dirsonly=0, $nofullpaths=0, $recursive=0')
  );

  LS_Functions_Tlk	: array[0..13] of TFuncCode = (
    (Name: 'no_strref';		Fn: -1;
    Code: '$FFFFFFFF'),
    (Name: 'tlk_valid_range';	Fn: -1;
    Code: '$00FFFFFF'),
    (Name: 'tlk_flag_custom';	Fn: -1;
    Code: '$01000000'),
    (Name: 'tlk_flag_text';	Fn: -1;
    Code: '$0001'),
    (Name: 'tlk_flag_snd';	Fn: -1;
    Code: '$0002'),
    (Name: 'tlk_flag_sndlen';	Fn: -1;
    Code: '$0004'),
    (Name: 'tlk_mask_text_off';	Fn: -1;
    Code: '$000E'),
    (Name: 'dialog';		Fn: Word(fnTlkDialog);
     Code: '; $tlk, $tlkf, $custom, $customf, ' +
     '$usingtlk, $usingtlkf, $usingcustom, $usingcustomf [L]'),
    (Name: 'addstrref';		Fn: Word(fnTlkAddStrref);
     Code: '$value'),
    (Name: 'findstrref';	Fn: Word(fnTlkFindStrref);
     Code: '$stringmask=$_; $ignorecase=0'),
    (Name: 'getstrref';		Fn: Word(fnTlkGetStrref);
     Code: '$index=$_'),
    (Name: 'getstrrefflags';	Fn: Word(fnTlkGetStrrefFlags);
     Code: '$index=$_'),
    (Name: 'setstrref';		Fn: Word(fnTlkSetStrref);
     Code: '$index, $value=$_'),
    (Name: 'setstrrefflags';	Fn: Word(fnTlkSetStrrefFlags);
     Code: '$index, $flags')
  );

  LS_Functions_2da	: array[0..2] of TFuncCode = (
    (Name: 'locate';		Fn: Word(fn2daLocate);
    Code: '$array, $column=0, $value=$_; &match! [L]'),
    (Name: 'lookup';		Fn: Word(fn2daLookup);
    Code: '$array, $column=0, $row=$_ [L]'),
    (Name: 'meta';		Fn: Word(fn2daMeta);
    Code: '; $dir, $caching, $buildcache [L]')
  );

  LS_Functions_Fpt	: array[0..1] of TFuncCode = (
    (Name: 'extract';		Fn: Word(fnFptExtract);
    Code: '$filename, $varname, %handle!'),
    (Name: 'inject';		Fn: Word(fnFptInject);
    Code: '$filename, $varname, %handle=%_')
  );

  LS_Functions_FileSys	: array[0..7] of TFuncCode = (
    (Name: 'filecopy';		Fn: Word(fnFSFileCopy);
    Code: '$from, $to; $overwrite=0'),
    (Name: 'filedelete';	Fn: Word(fnFSFileDelete);
    Code: '$filename=$_'),
    (Name: 'directoryexists';	Fn: Word(fnFSDirExists);
    Code: '$dirname=$_'),
    (Name: 'fileexists';	Fn: Word(fnFSFileExists);
    Code: '$filename=$_'),
    (Name: 'fileinfo';		Fn: Word(fnFSFileInfo);
    Code: '$filename=$_'),
    (Name: 'filemove';		Fn: Word(fnFSFileMove);
    Code: '$from, $to; $overwrite=0'),
    (Name: 'filerename';	Fn: Word(fnFSFileRename);
    Code: '$from, $to; $overwrite=0'),
    // DONE 1: ADD: [27r3] ForceDirectories
    (Name: 'forcedirectories';	Fn: Word(fnFSForceDirs);
     Code: '$path')
  );

  LS_Functions_System	: array[0..44] of TFuncCode = (
    (Name: 'true';		Fn: -1;
     Code: '1'),
    (Name: 'false';		Fn: -1;
     Code: '0'),
    (Name: 'ftunknown';		Fn: -1;
     Code: '$0000'),
    (Name: 'fttxt';		Fn: -1;
     Code: '$0001'),
    (Name: 'ftgff';		Fn: -1;
     Code: '$0002'),
    (Name: 'fterf';		Fn: -1;
     Code: '$0004'),
    (Name: 'ftbif';		Fn: -1;
     Code: '$0008'),
    (Name: 'ft2da';		Fn: -1;
     Code: '$0010'),
    (Name: 'ftxml';		Fn: -1;
     Code: '$0020'),
    (Name: 'ftfpt';		Fn: -1;
     Code: '$0040'),
    (Name: 'caller';		Fn: Word(fnCaller);
     Code: ''),
    (Name: 'chr';		Fn: Word(fnChr);
     Code: '$_ [F]'),
    (Name: 'close';		Fn: Word(fnClose);
     Code: '%_'),
    (Name: 'defined';		Fn: Word(fnDefined);
     Code: '$_!'),
    (Name: 'delete';		Fn: Word(fnDelete);
     Code: '$_!'),
    (Name: 'die';		Fn: Word(fnDie);
     Code: '@_'),
    (Name: 'exit';		Fn: Word(fnExit);
     Code: '$status=0'),
    (Name: 'greedy';		Fn: Word(fnGreedy);
     Code: '@param^! [P]'),
    (Name: 'isnum';		Fn: Word(fnIsNum);
     Code: '$_ [F]'),
    (Name: 'keys';		Fn: Word(fnKeys);
     Code: '@_'),
    (Name: 'lc';		Fn: Word(fnLc);
     Code: '$_ [F]'),
    (Name: 'length';		Fn: Word(fnLength);
     Code: '$_'),
    (Name: 'my';		Fn: Word(fnMy);
     Code: '@vars!'),
    (Name: 'ord';		Fn: Word(fnOrd);
     Code: '$_ [F]'),
    (Name: 'pl';		Fn: Word(fnPrintList);
     Code: '@list=$_ [L]'),
    (Name: 'pop';		Fn: Word(fnPop);
     Code: '@array^=@_'),
    (Name: 'print';		Fn: Word(fnPrint);
     Code: '@list=$_ [L]'),
    (Name: 'printf';		Fn: Word(fnPrintF);
     Code: '$format, @items='),
    (Name: 'printlist';		Fn: Word(fnPrintList);
     Code: '@list=$_ [L]'),
    (Name: 'push';		Fn: Word(fnPush);
     Code: '@array^, @_'),
    (Name: 'rand';		Fn: Word(fnRand);
     Code: '$x=2; $seed [U]'),
    (Name: 'return';		Fn: Word(fnReturn);
     Code: '@list='),
    (Name: 's';			Fn: Word(fnSubst);
     Code: '$expr, $replacement=$_, $var^=$_; $replaceall, $ignorecase'),
    (Name: 'shift';		Fn: Word(fnShift);
     Code: '@array^=@_'),
    (Name: 'sleep';		Fn: Word(fnSleep);
     Code: '$_'),
    (Name: 'subst';		Fn: Word(fnSubst);
     Code: '$expr, $replacement=$_, $var^=$_; $replaceall, $ignorecase'),
    (Name: 'substr';		Fn: Word(fnSubstr);
     Code: '$expr, $offset, $length=0, $replacement='),
    (Name: 'system';		Fn: Word(fnSystem);
     Code: '@list=@_; $nowait=0; $noquotes=0'),
    (Name: 'time';		Fn: Word(fnTime);
     Code: ''),
    (Name: 'uc';		Fn: Word(fnUc);
     Code: '$_ [F]'),
    (Name: 'undef';		Fn: Word(fnUndef);
     Code: '$object!='),
    (Name: 'unshift';		Fn: Word(fnUnshift);
     Code: '@array^, @_'),
    (Name: 'values';		Fn: Word(fnValues);
     Code: '@_'),
    (Name: 'var';		Fn: Word(fnVar);
     Code: '$param^! [P]'),
    (Name: 'warn';		Fn: Word(fnWarn);
     Code: '@_')
  );

  // TODO 5: BUG: methods in math not alphabetical (better for prec?)
  LS_Functions_Math	: array[0..57] of TFuncCode = (
    (Name: 'pi';		Fn: -1;
     Code: '3.14159265358979'),
    (Name: 'abs';		Fn: Word(fnMathAbs);
     Code: '$_ [F]'),
    (Name: 'ceil';		Fn: Word(fnMathCeil);
     Code: '$_ [F]'),
    (Name: 'divmod';		Fn: Word(fnMathDivMod);
     Code: '$dividend, $divisor [F]'),
    (Name: 'exp';		Fn: Word(fnMathExp);
     Code: '$_ [F]'),
    (Name: 'floor';		Fn: Word(fnMathFloor);
     Code: '$_ [F]'),
    (Name: 'frac';		Fn: Word(fnMathFrac);
     Code: '$_ [F]'),
    (Name: 'frexp';		Fn: Word(fnMathFrexp);
     Code: '$_ [F]'),
    (Name: 'int';		Fn: Word(fnMathInt);
     Code: '$_ [F]'),
    (Name: 'iszero';		Fn: Word(fnMathIsZero);
     Code: '$x, $epsilon=0 [FU]'),
    (Name: 'ldexp';		Fn: Word(fnMathLdexp);
     Code: '$x, $p [F]'),
    (Name: 'ln';		Fn: Word(fnMathLn);
     Code: '$_ [F]'),
    (Name: 'lnxp1';		Fn: Word(fnMathLnXP1);
     Code: '$_ [F]'),
    (Name: 'log2';		Fn: Word(fnMathLog2);
     Code: '$_ [F]'),
    (Name: 'log10';		Fn: Word(fnMathLog10);
     Code: '$_ [F]'),
    (Name: 'logn';		Fn: Word(fnMathLogN);
     Code: '$base, $n [F]'),
    (Name: 'max';		Fn: Word(fnMathMax);
     Code: '$a, $b [F]'),
    (Name: 'min';		Fn: Word(fnMathMin);
     Code: '$a, $b [F]'),
    (Name: 'muldiv';		Fn: Word(fnMathMulDiv);
     Code: '$n, $numerator, $denominator [F]'),
    (Name: 'poly';		Fn: Word(fnMathPoly);
     Code: '@_=0 [F]'),
    (Name: 'round';		Fn: Word(fnMathRound);
     Code: '$_ [F]'),
    (Name: 'roundto';		Fn: Word(fnMathRoundTo);
     Code: '$x, $digit [F]'),
    (Name: 'samevalue';		Fn: Word(fnMathSameValue);
     Code: '$a, $b, $epsilon=0 [F]'),
    (Name: 'sign';		Fn: Word(fnMathSign);
     Code: '$_ [F]'),
    (Name: 'simpleroundto';	Fn: Word(fnMathSimpleRoundTo);
     Code: '$x, $digit=-2 [F]'),
    (Name: 'sqr';		Fn: Word(fnMathSqr);
     Code: '$_ [F]'),
    (Name: 'sqrt';		Fn: Word(fnMathSqrt);
     Code: '$_ [F]'),
    (Name: 'acos';		Fn: Word(fnMathACos);
     Code: '$_ [F]'),
    (Name: 'acosh';		Fn: Word(fnMathACosH);
     Code: '$_ [F]'),
    (Name: 'acot';		Fn: Word(fnMathACot);
     Code: '$_ [F]'),
    (Name: 'acoth';		Fn: Word(fnMathACotH);
     Code: '$_ [F]'),
    (Name: 'acsc';		Fn: Word(fnMathACsc);
     Code: '$_ [F]'),
    (Name: 'acsch';		Fn: Word(fnMathACscH);
     Code: '$_ [F]'),
    (Name: 'asec';		Fn: Word(fnMathASec);
     Code: '$_ [F]'),
    (Name: 'asech';		Fn: Word(fnMathASecH);
     Code: '$_ [F]'),
    (Name: 'asin';		Fn: Word(fnMathASin);
     Code: '$_ [F]'),
    (Name: 'asinh';		Fn: Word(fnMathASinH);
     Code: '$_ [F]'),
    (Name: 'atan';		Fn: Word(fnMathATan);
     Code: '$_ [F]'),
    (Name: 'atan2';		Fn: Word(fnMathATan2);
     Code: '$y, $x [F]'),
    (Name: 'atanh';		Fn: Word(fnMathATanH);
     Code: '$_ [F]'),
    (Name: 'cos';		Fn: Word(fnMathCosin);
     Code: '$_ [F]'),
    (Name: 'cosin';		Fn: Word(fnMathCosin);
     Code: '$_ [F]'),
    (Name: 'cosh';		Fn: Word(fnMathCosH);
     Code: '$_ [F]'),
    (Name: 'cot';		Fn: Word(fnMathCotan);
     Code: '$_ [F]'),
    (Name: 'cotan';		Fn: Word(fnMathCotan);
     Code: '$_ [F]'),
    (Name: 'coth';		Fn: Word(fnMathCotH);
     Code: '$_ [F]'),
    (Name: 'cosecant';		Fn: Word(fnMathCosecant);
     Code: '$_ [F]'),
    (Name: 'csc';		Fn: Word(fnMathCosecant);
     Code: '$_ [F]'),
    (Name: 'csch';		Fn: Word(fnMathCscH);
     Code: '$_ [F]'),
    (Name: 'hypot';		Fn: Word(fnMathHypot);
     Code: '$x, $y [F]'),
    (Name: 'sec';		Fn: Word(fnMathSecant);
     Code: '$_ [F]'),
    (Name: 'secant';		Fn: Word(fnMathSecant);
     Code: '$_ [F]'),
    (Name: 'sech';		Fn: Word(fnMathSecH);
     Code: '$_ [F]'),
    (Name: 'sin';		Fn: Word(fnMathSin);
     Code: '$_ [F]'),
    (Name: 'sincos';		Fn: Word(fnMathSinCos);
     Code: '$_ [F]'),
    (Name: 'sinh';		Fn: Word(fnMathSinH);
     Code: '$_ [F]'),
    (Name: 'tan';		Fn: Word(fnMathTan);
     Code: '$_ [F]'),
    (Name: 'tanh';		Fn: Word(fnMathTanH);
     Code: '$_ [F]')
  );

  {$IFDEF LETOSCRIPT_SQL}
  LS_Functions_Sql	: array[0..19] of TFuncCode = (
    (Name: 'bindblob';		Fn: Word(fnSqlBindBlob);
     Code: '')
    (Name: 'bof';		Fn: Word(fnSqlBof);
     Code: '')
    (Name: 'connect';		Fn: Word(fnSqlConnect);
     Code: '')
    (Name: 'disconnect';	Fn: Word(fnSqlDisconnect);
     Code: '')
    (Name: 'eof';		Fn: Word(fnSqlEof);
     Code: '')
    (Name: 'exec';		Fn: Word(fnSqlExec);
     Code: '')
    (Name: 'field';		Fn: Word(fnSqlField);
     Code: '')
    (Name: 'fieldnames';	Fn: Word(fnSqlFieldNames);
     Code: '')
    (Name: 'fields';		Fn: Word(fnSqlFields);
     Code: '')
    (Name: 'first';		Fn: Word(fnSqlFirst);
     Code: '')
    (Name: 'insertid';		Fn: Word(fnSqlInsertId);
     Code: '')
    (Name: 'last';		Fn: Word(fnSqlLast);
     Code: '')
    (Name: 'next';		Fn: Word(fnSqlNext);
     Code: '')
    (Name: 'params';		Fn: Word(fnSqlParams);
     Code: '')
    (Name: 'prior';		Fn: Word(fnSqlPrior);
     Code: '')
    (Name: 'query';		Fn: Word(fnSqlQuery);
     Code: '')
    (Name: 'recordcount';	Fn: Word(fnSqlRecordCount);
     Code: '')
    (Name: 'retrieve';		Fn: Word(fnSqlRetrieve);
     Code: '')
    (Name: 'rowsaffected';	Fn: Word(fnSqlRowsAffected);
     Code: '')
    (Name: 'store';		Fn: Word(fnSqlStore);
     Code: '')
  );
  {$ENDIF}
(*
  {$IFDEF LETOSCRIPT_SQL}
  SqlListOps		: TLetoScriptFnSqlSet = [
    fnSqlBindBlob, fnSqlConnect, fnSqlFields, fnSqlParams, fnSqlQuery,
    fnSqlRetrieve, fnSqlStore
  ];
  {$ENDIF}
*)

var
  LS_Functions		: array of TFuncCode;


type

  TPrecedence		= record
    Ops			: TLetoScriptOpSet;
  end;

const

  Terms			: TLetoScriptOpSet = [
    opConst, opIConst, opSymbol,
    opBlock, opExpr,
    opFunc
  ];

  AssocLeft		: TLetoScriptOpSet = [
    opSymbol, opBlock, opExpr, opFunc,
    opDeref,
    opBind,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus, opAddSp, {opRemSp,}
    opShl, opShr,
    opBitAnd,
    opBitOr, opBitXor,
    opLogAnd,
    opLogOr,
    opComma, opKey,
    opLitAnd,
    opLitOr, opLitXor,
    OpStop
  ];

  AssocRight		: TLetoScriptOpSet = [
    opConst, opIConst,
    opBinExp,
    opLogNot, opUnaMinus, opBitNot, opUnaPlus, opRef,
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrNE, opStrCmp,
    opCondIf,
    opAssign,
    opLitNot,
    opNamedOp
  ];

  Precedence		: array[0..26] of TPrecedence = (
  { Highest }
    {26 L} (Ops: [opConst, opIConst, opSymbol, opBlock, opExpr, opSet, opFunc]),
    {25 L} (Ops: [opDeref, opAttrib]),
    {24 N} (Ops: [opAuto]),
    {23 R} (Ops: [opBinExp]),
    {22 R} (Ops: [opLogNot, opUnaMinus, opBitNot, opUnaPlus, opRef]),
    {21 L} (Ops: [opBind]),
    {20 L} (Ops: [opBinMult, opBinDiv, opBinMod, opBinRep]),
    {19 L} (Ops: [opBinPlus, opBinMinus, opLitPlus, opLitMinus, opAddSp {, opRemSp}]),
    {18 L} (Ops: [opShl, opShr]),
    {17 N} (Ops: [opUnaryOp]),
    {16 R} (Ops: [opNumLT, opNumGT, opNumLE, opNumGE,
                  opStrLT, opStrGT, opStrLE, opStrGE]),
    {15 R} (Ops: [opNumEq, opNumNE, opNumCmp,
                  opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp]),
    {14 L} (Ops: [opBitAnd]),
    {13 L} (Ops: [opBitOr, opBitXor]),
    {12 L} (Ops: [opLogAnd]),
    {11 L} (Ops: [opLogOr]),
    {10 N} (Ops: [opRange]),
    {09 R} (Ops: [opCondIf, opCondThen]),
    {08 R} (Ops: [opAssign]),
    {07 L} (Ops: [opKey]),
    {06 L} (Ops: [opComma]),
    {05 N} (Ops: [opListOp]),
    {04 R} (Ops: [opLitNot]),
    {03 L} (Ops: [opLitAnd]),
    {02 L} (Ops: [opLitOr, opLitXor]),
    {01 R} (Ops: [opNamedOp]),
    {00 L} (Ops: [opStop])
  { Lowest }
  );

  OpPrecLeftRight	: TLetoScriptOpSet = [
    opLogNot, opUnaMinus, opBitNot, opUnaPlus, opRef,
    opUnaryOp, opListOp
  ];

  OpHasLeft		: TLetoScriptOpSet = [
    opDeref, opAttrib,
    opBinExp,
    opBind,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus,
    opShl, opShr,
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp,
    opBitAnd,
    opBitOr, opBitXor,
    opLogAnd,
    opLogOr,
    opRange,
    opCondIf, opCondThen,
    opAssign,
    opLitAnd,
    opLitOr, opLitXor
  ];
  KindHasLeft		: TLetoScriptOpKindSet = [
    acPostInc, acPostDec
  ];

  OpHasRight		: TLetoScriptOpSet = [
    opDeref, opAttrib,
    opBinExp,
    opLogNot, opUnaMinus, opBitNot, opUnaPlus, opRef,
    opBind,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus,
    opShl, opShr,
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp,
    opBitAnd,
    opBitOr, opBitXor,
    opLogAnd,
    opLogOr,
    opRange,
    opCondIf, opCondThen,
    opAssign,
    opLitNot,
    opLitAnd,
    opLitOr, opLitXor
  ];
  KindHasRight		: TLetoScriptOpKindSet = [
    acPreInc, acPreDec
  ];

  OpHasLeftOrRight	: TLetoScriptOpSet = [
    opStop, opBlock, opExpr, opSet,
    opAddSp, {opRemSp,}
    opUnaryOp,
    opComma, opKey,
    opListOp
  ];
  KindHasLeftOrRight	: TLetoScriptOpKindSet = [
    { noIf, noUnless, noFor, noSub, noWhile -> polyamorous }
  ];


  { Used to determine if an Op is foldable. }
  Foldable		: TLetoScriptOpSet = [
    opBinExp,
    opUnaMinus, opBitNot, opUnaPlus,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus, opAddSp, {opRemSp,}
    opShl, opShr,
    {
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp,
    }
    opBitAnd,
    opBitOr, opBitXor,
    opRange
  ];

  { An Op is foldable if both its sides are NonInterpolative. }
  NonInterpolative	: TLetoScriptOpKindSet = [
    numInt, numReal, numSci,
    numBin, numHex, numOct,
    quSingle,
    idBare
  ];

  { String types that have interpolative content. }
  Interpolative		: TLetoScriptOpKindSet = [
    quDouble, quPrintn, quCmd
  ];


type

  TOpName		= record
    Op			: TLetoScriptOp;
    Name		: String;
  end;

  TKindName		= record
    Kind		: TLetoScriptOpKind;
    Name		: String;
  end;

const

  OpNames		: array[1..67] of TOpName = (
    (Op: opLParen;	Name: '('),		(Op: opRParen;	Name: ')'),
    (Op: opLBrace;	Name: '{'),		(Op: opRBrace;	Name: '}'),
    (Op: opLBrack;	Name: '['),		(Op: opRBrack;	Name: ']'),
    (Op: opConst;	Name: 'Constant'),	(Op: opIConst;	Name: 'Constant'),
    (Op: opSymbol;	Name: 'Symbol'),
    (Op: opBlock;	Name: 'Block'),		(Op: opExpr;	Name: 'Expression'),
    (Op: opSet;		Name: 'Set'),		(Op: opSub;	Name: 'Subroutine'),
    (Op: opFunc;	Name: 'Function'),
    (Op: opDeref;	Name: 'Dereference'),	(Op: opRef;	Name: 'Reference'),
    (Op: opBind;	Name: 'Bound /Field'),
    (Op: opAttrib;	Name: 'Attribute'),
    (Op: opAuto;	Name: 'Autocrement'),
    (Op: opBinExp;	Name: 'Exponentiation'),
    (Op: opUnaMinus;	Name: 'Unary -'),	(Op: opUnaPlus;	Name: 'Unary +'),
    (Op: opBinMult;	Name: 'Multiplication (*)'),
    (Op: opBinDiv;	Name: 'Division (/)'),
    (Op: opBinMod;	Name: 'Modulus (%)'),	(Op: opBinRep;	Name: 'Repeat (x)'),
    (Op: opBinPlus;	Name: 'Addition (+)'),	(Op: opBinMinus; Name: 'Subtraction (-)'),
    (Op: opLitPlus;	Name: 'Concatenation'),	(Op: opLitMinus; Name: 'Omission'),
    (Op: opLogNot;	Name: '!'),
    (Op: opLogAnd;	Name: '&&'),		(Op: opLogOr;	Name: '||'),
    (Op: opBitNot;	Name: '~'),		(Op: opBitAnd;	Name: 'Bitwise and (&)'),
    (Op: opBitOr;	Name: 'Bitwise or (|)'),
    (Op: opBitXor;	Name: 'Bitwise xor (^)'),
    (Op: opShl;		Name: 'Left bitshift (<<)'),
    (Op: opShr;		Name: 'Right bitshift (>>)'),
//    (Op: opAddSp;	Name: 'Space addition (+^)'),
//    (Op: opRemSp;	Name: 'Space removal (-^)'),
    (Op: opUnaryOp;	Name: 'Unary operator'),
    (Op: opListOp;	Name: 'List operator'),
    (Op: opNumLT;	Name: 'Numeric lt (<)'),
    (Op: opNumGT;	Name: 'Numeric gt (>)'),
    (Op: opNumLE;	Name: 'Numeric le (<=)'),
    (Op: opNumGE;	Name: 'Numeric ge (>=)'),
    (Op: opNumEq;	Name: 'Numeric eq (==)'),
    (Op: opNumNe;	Name: 'Numeric ne (!=)'),
    (Op: opNumCmp;	Name: 'Numeric cmp (<=>)'),
    (Op: opStrLT;	Name: 'String lt'),	(Op: opStrGT;	Name: 'String gt'),
    (Op: opStrLE;	Name: 'String le'),	(Op: opStrGE;	Name: 'String ge'),
    (Op: opStrEq;	Name: 'String eq'),	(Op: opStrNe;	Name: 'String ne'),
    (Op: opStrCmp;	Name: 'String cmp'),
    (Op: opRange;	Name: 'Range operator'),
    (Op: opCondIf;	Name: 'Ternary ?'),	(Op: opCondThen; Name: 'Ternary :'),
    (Op: opAssign;	Name: 'Scalar Assignment'),
    (Op: opLAssign;	Name: 'List Assignment'),
    (Op: opComma;	Name: 'Comma'),		(Op: opKey;	Name: 'Keyhole'),
    (Op: opLitNot;	Name: '"not"'),		(Op: opLitAnd;	Name: '"and"'),
    (Op: opLitOr;	Name: '"or"'),		(Op: opLitXor;	Name: '"xor"'),
    (Op: opNamedOp;	Name: 'Named Operator')
  );

  OpKindNames		: array[1..54] of TKindName = (
    (Kind: comCPP;	Name: 'Comment'),	(Kind: comPascal; Name: 'Comment'),
    (Kind: comDoc;	Name: 'Comment'),
    (Kind: numInt;	Name: 'Integer'),	(Kind: numReal;	Name: 'Real'),
    (Kind: numSci;	Name: 'Number'),	(Kind: numBin;	Name: 'Binary'),
    (Kind: numHex;	Name: 'Hex'),		(Kind: numOct;	Name: 'Octal'),
    (Kind: numCurrency;	Name: 'Currency'),	(Kind: numOct;	Name: 'Date'),
    (Kind: quSingle;	Name: '''string'''),	(Kind: quDouble; Name: '"string"'),
    (Kind: quPrintn;	Name: '<string>'),	(Kind: quCmd;	Name: '`string`'),
    (Kind: quQ;		Name: '''string'''),	(Kind: quQQ;	Name: '"string"'),
    (Kind: quQW;	Name: 'Quote-Words'),	(Kind: quQX;	Name: '`string`'),
    (Kind: idBare;	Name: 'Bareword'),	(Kind: idSafe;	Name: 'Safeword'),
    (Kind: idLabel;	Name: 'Label'),		(Kind: idAttrib; Name: 'Attribute'),
    (Kind: symScalar;	Name: 'Scalar var'),	(Kind: symList;	Name: 'List var'),
    (Kind: symField;	Name: '/Field'),	(Kind: symFile;	Name: 'Handle'),
    (Kind: acPostInc;	Name: 'Postincrement (++)'),
    (Kind: acPostDec;	Name: 'Postdecrement (--)'),
    (Kind: acPreInc;	Name: 'Preincrement (++)'),
    (Kind: acPreDec;	Name: 'Predecrement (--)'),
    (Kind: eqExp;	Name: 'Assignment (**=)'),
    (Kind: eqPlus;	Name: 'Assignment (+=)'),
    (Kind: eqMinus;	Name: 'Assignment (-=)'),
    (Kind: eqMult;	Name: 'Assignment (*=)'),
    (Kind: eqDiv;	Name: 'Assignment (/=)'),
    (Kind: eqMod;	Name: 'Assignment (%=)'),
    (Kind: eqBitAnd;	Name: 'Assignment (&=)'),
    (Kind: eqBitOr;	Name: 'Assignment (|=)'),
    (Kind: eqBitXor;	Name: 'Assignment (^=)'),
    (Kind: eqShl;	Name: 'Assignment (<<=)'),
    (Kind: eqShr;	Name: 'Assignment (>>=)'),
    (Kind: eqLogAnd;	Name: 'Assignment (&&=)'),
    (Kind: eqLogOr;	Name: 'Assignment (||=)'),
    (Kind: noIf;	Name: 'If'),		(Kind: noElsif; Name: 'Elsif'),
    (Kind: noElse;	Name: 'Else'),		(Kind: noUnless; Name: 'Unless'),
    (Kind: noDo;	Name: 'Do'),		(Kind: noUntil; Name: 'Until'),
    (Kind: noWhile;	Name: 'While'),		(Kind: noFor;	Name: 'For'),
    (Kind: noContinue;	Name: 'Continue'),	(Kind: noSwitch; Name: 'Switch')
  );


type

  TLetoScriptError	= (
    { Internal codes }
    LS_Success, Err_LS_IO,
    { Moneo errors }
    Err_Moneo_BadScript, Err_Moneo_BadLog, Err_Moneo_BadHelp, Err_Moneo_BadSettings,
    Err_Moneo_NYI, Err_Moneo_NoSuchFile, Err_Moneo_Locked,
    { LetoScript errors }
    Err_LS_NYI,
    Err_LS_Syntax, Err_LS_UnknownOp, Err_LS_Unterminated,
    Err_LS_NoLib, Err_LS_NotInLib, Err_LS_LibDisabled,
    Err_LS_BadSub, Err_LS_UndefSub, Err_LS_SubNeedsPredecl,
    Err_LS_SubRedef, Err_LS_SubHasTooManyParms, Err_LS_SubIdRedecl,
    Err_LS_SubBadShort, Err_LS_SubBadOption,
    Err_LS_SubBadParam, Err_LS_SubBadDefault, Err_LS_SubBadAttrib,
    Err_LS_NoComDocHere, Err_LS_BadDeref,
    Err_LS_InvalidNum, Err_LS_InvalidName,
    Err_LS_InvalidAssoc, Err_LS_InvalidSide,
    Err_LS_BadArgType, Err_LS_MissingArg,
    Err_LS_CantModify, Err_LS_ReadOnlyValue, Err_LS_BadIndex,
    Err_LS_BadOverload, Err_LS_NoHandle, Err_LS_LiveHandle, Err_LS_NotInFile,
    Err_LS_InvalidReal, Err_LS_InvalidDeclare,
    Err_LS_BadSqlType,
    Err_LS_Die, Err_LS_Exit,
    {	Warnings }
    Err_LS_Warn, Err_LS_DefaultWarn, Err_LS_Caught, Err_LS_Deprecated, Err_LS_Embedded,
    Err_LS_Useless, Err_LS_Bareword, Err_LS_Uninit, Err_LS_InvalidArg,
    Err_LS_SubAmbiguous, Err_LS_SubAmbiguousParam,
    Err_LS_AssInCond, Err_LS_NotNumeric,
    Err_LS_InvalidField, Err_LS_InvalidFile, Err_LS_InvalidFilename,
    Err_LS_NoFile, Err_LS_NotGffFile, Err_LS_NotErfFile,
    Err_LS_NotFptFile, Err_LS_NotFptVarName,
    Err_LS_ImpliedClose, Err_LS_NoTlk,
    Err_LS_BadAttrib, Err_LS_NotLocString, Err_LS_ResRefOverflow,
    Err_LS_Outside,
    Err_LS_AtLineNear,
    {	Hints }
    Err_LS_Hint,
    {	Notices }
    Err_LS_Notice
  );
  TLSErrorSet		= set of TLetoScriptError;

  TLSErrToStr		= record
    Err			: TLetoScriptError;
    S			: String;
  end;

const

  LetoScriptErrors	: array[0..72] of TLSErrToStr = (

     { Moneo Errors }
    // TODO 3: CHANGE: Consolidate wording of errors across apps.
    (Err: Err_Moneo_BadScript;
     S: 'Cannot open Moneo script'),
    (Err: Err_Moneo_BadLog;
     S: 'Cannot open log'),
    (Err: Err_Moneo_BadHelp;
     S: 'Cannot open helpfile'),
    (Err: Err_Moneo_BadSettings;
     S: 'Cannot open settingsfile'),
    (Err: Err_Moneo_NYI;
     S: 'Function not yet implemented.'),
    (Err: Err_Moneo_NoSuchFile;
     S: 'no such file or directory.'),
    (Err: Err_Moneo_Locked;
     S: 'the file is in use and locked.'),

     { LetoScript Errors }
    (Err: Err_LS_IO;
     S: 'File error (unspecified)'),
    (Err: Err_LS_NYI;
     S: 'Function not yet implemented'),
    (Err: Err_LS_Syntax;
     S: 'Syntax error'),
    (Err: Err_LS_UnknownOp;
     S: 'Unknown operator'),
    (Err: Err_LS_Unterminated;
     S: 'Unterminated %s'),
    (Err: Err_LS_NoLib;
     S: 'Module "%s" is not defined'),
    (Err: Err_LS_NotInLib;
     S: 'Function "%s" not found in library "%s"'),
    (Err: Err_LS_LibDisabled;
     S: 'Execution denied. "%s.%s" has been administratively disabled'),
    (Err: Err_LS_BadSub;
     S: 'Illegal declaration of subroutine &%s'),
    (Err: Err_LS_UndefSub;
     S: 'Undefined subroutine &%s called'),
    (Err: Err_LS_SubNeedsPredecl;
     S: 'Unexpected term (do you need to predeclare %s?)'),
    (Err: Err_LS_SubRedef;
     S: 'Subroutine &%s redefined'),
    (Err: Err_LS_SubHasTooManyParms;
     S: 'Subroutine &%s has too many parameters (maximum 256)'),
    (Err: Err_LS_SubIdRedecl;
     S: 'Identifier %s redeclared in definition of &%s'),
    (Err: Err_LS_SubBadShort;
     S: 'Shortcut %s used illegally (definition of &%s)'),
    (Err: Err_LS_SubBadOption;
     S: 'Shortcut %s not allowed as Option (definition of &%s)'),
    (Err: Err_LS_SubBadParam;
     S: 'Invalid parameter in subroutine &%s'),
    (Err: Err_LS_SubBadDefault;
     S: 'Invalid default value in subroutine &%s'),
    (Err: Err_LS_SubBadAttrib;
     S: 'Invalid attribute in subroutine &%s'),
    (Err: Err_LS_NoComDocHere;
     S: 'Document comment not allowed here'),
    (Err: Err_LS_BadDeref;
     S: 'Invalid dereference of "%s"'),
    (Err: Err_LS_InvalidNum;
     S: 'Illegal %s digit "%c"'),
    (Err: Err_LS_InvalidName;
     S: 'Invalid %s name'),
    (Err: Err_LS_InvalidAssoc;
     S: 'Invalid associate'),
    (Err: Err_LS_InvalidSide;
     S: 'Unexpected term'),
    (Err: Err_LS_BadArgType;
     S: 'Argument "%s" has invalid type (expected %s)'),
    (Err: Err_LS_MissingArg;
     S: 'Missing argument %s to function %s'),
    (Err: Err_LS_CantModify;
     S: 'Can''t modify %s in %s'),
    (Err: Err_LS_ReadOnlyValue;
     S: 'Modification of read-only value attempted'),
    (Err: Err_LS_BadIndex;
     S: 'Modification of non-creatable array value attempted, subscript %s'),
    (Err: Err_LS_BadOverload;
     S: 'Unsupported overloaded operator'),
    (Err: Err_LS_NoHandle;
     S: 'No such file handle'),
    (Err: Err_LS_LiveHandle;
     S: 'File %s is already open'),
    (Err: Err_LS_NotInFile;
     S: 'Operator ''in'' requires a file argument'),
    (Err: Err_LS_InvalidReal;
     S: 'Illegal real operation on %s'),
    (Err: Err_LS_InvalidDeclare;
     S: 'Can''t declare %s in "%s"'),
    (Err: Err_LS_BadSqlType;
     S: 'Unrecognized SQL protocol %s'),
    (Err: Err_LS_Die;
     S: '%s'),
    (Err: Err_LS_Exit;
     S: 'Halting with exit code %s'),

    (Err: Err_LS_Warn;
     S: '%s'),
    (Err: Err_LS_Caught;
     S: '...caught'),
    (Err: Err_LS_Deprecated;
     S: 'Deprecated use of %s; use instead %s'),
    (Err: Err_LS_Embedded;
     S: 'Operation not allowed in embedded environment'),
    (Err: Err_LS_Useless;
     S: 'Useless use of %s in void context'),
    (Err: Err_LS_Bareword;
     S: 'Bareword "%s" interpreted as string'),
    (Err: Err_LS_Uninit;
     S: 'Use of undefined value in %s'),
    (Err: Err_LS_InvalidArg;
     S: 'Discarded unrecognized argument %s to function %s'),
    (Err: Err_LS_SubAmbiguous;
     S: 'Subroutine name clashes with %s'),
    (Err: Err_LS_SubAmbiguousParam;
     S: 'Parameter name clashes with %s (definition of &%s)'),
    (Err: Err_LS_AssInCond;
     S: 'Found = in conditional, should be == (or eq)'),
    (Err: Err_LS_NotNumeric;
     S: 'Argument isn''t numeric'),
    (Err: Err_LS_InvalidField;
     S: 'Invalid Field %s'),
    (Err: Err_LS_InvalidFile;
     S: 'Invalid file %s'),
    (Err: Err_LS_InvalidFilename;
     S: 'Invalid filename %s'),
    (Err: Err_LS_NoFile;
     S: 'No file open'),
    (Err: Err_LS_NotGffFile;
     S: 'File %s is not a GFF'),
    (Err: Err_LS_NotErfFile;
     S: 'File %s is not an ERF'),
    (Err: Err_LS_NotFptFile;
     S: 'File %s is not an FPT'),
    (Err: Err_LS_NotFptVarName;
     S: 'VarName does not exist in this FPT'),
    (Err: Err_LS_ImpliedClose;
     S: 'File %s closed implicitly'),
    (Err: Err_LS_NoTlk;
     S: 'No TLK open or associated'),
    (Err: Err_LS_BadAttrib;
     S: 'Invalid attribute "%s"'),
    (Err: Err_LS_NotLocString;
     S: 'Field %s is not a LocString'),
    (Err: Err_LS_ResRefOverflow;
     S: 'ResRef assignment overflow. Truncated to 16 characters.'),
    (Err: Err_LS_Outside;
     S: '%s outside of string'),

    (Err: Err_LS_AtLineNear;
     //S: ' at %s line %d pos %d, near "%s".')
     S: ' at %s line %d pos %d.')

  );

  procedure BuildFuncArray;

  function TryGetLSFunction(
    const Lib, Name	: String;
    var Func		: Integer
  ): Boolean;

  function GetLSLibMembers(const Lib, MemberType: String): TStringList;

  function GetLSMatchingMethods(const Lib, Name: String): TStringList;

  function GetLSError(const Err: TLetoScriptError): String;

  function FormatLSError(
    const Err		: TLetoScriptError;
    const Symptoms	: array of const
  ): String;

  function GetWarning: String;

implementation

uses
  SysUtils;

(*------------------------------------------------------------------------------
BuildFuncArray

Dividing the functions into separate arrays is a convenience for the author.
Combining them back into a single array at runtime is a convenience for
the program. Unfortunately, Delphi is a bit strict about certain typechecks,
so there's some redundant code here, which is most inconvenient.

------------------------------------------------------------------------------*)
procedure BuildFuncArray;
var
  I, L			: Integer;
  procedure _Copy_(const AFunc: TFuncCode; const Lib: String);
  begin
    LS_Functions[L].Name	:= Lib + AFunc.Name;
    LS_Functions[L].Fn		:= AFunc.Fn;
    LS_Functions[L].Code	:= AFunc.Code;
    Inc(L);
  end;
begin
  SetLength(LS_Functions,
    Length(LS_Functions_Gff) +
    Length(LS_Functions_Erf) +
    Length(LS_Functions_Bic) +
    Length(LS_Functions_Tlk) +
    Length(LS_Functions_2da) +
    Length(LS_Functions_Fpt) +
    Length(LS_Functions_FileSys) +
    Length(LS_Functions_System) +
    Length(LS_Functions_Math)
  );

  L			:= 0;

  for I := 0 to High(LS_Functions_Gff) do
    _Copy_(LS_Functions_Gff[I], 'gff.');

  for I := 0 to High(LS_Functions_Erf) do
    _Copy_(LS_Functions_Erf[I], 'erf.');

  for I := 0 to High(LS_Functions_Bic) do
    _Copy_(LS_Functions_Bic[I], 'bic.');

  for I := 0 to High(LS_Functions_Tlk) do
    _Copy_(LS_Functions_Tlk[I], 'tlk.');

  for I := 0 to High(LS_Functions_2da) do
    _Copy_(LS_Functions_2da[I], 'meta.');

  for I := 0 to High(LS_Functions_Fpt) do
    _Copy_(LS_Functions_Fpt[I], 'fpt.');

  for I := 0 to High(LS_Functions_Filesys) do
    _Copy_(LS_Functions_Filesys[I], 'filesys.');

  for I := 0 to High(LS_Functions_System) do
    _Copy_(LS_Functions_System[I], 'system.');

  for I := 0 to High(LS_Functions_Math) do
    _Copy_(LS_Functions_Math[I], 'math.');

end;

(*------------------------------------------------------------------------------
TryGetLSFunction

------------------------------------------------------------------------------*)
function TryGetLSFunction(
  const Lib, Name	: String;
  var Func		: Integer
): Boolean;
var
  I			: Integer;
  N, S			: String;
  Seeking		: Boolean;
begin
  Result		:= True;
  Func			:= -1;
  N			:= Lib + '.' + Name;
  Seeking		:= True;

  for I := 0 to High(LS_Functions) do begin
    S			:= LS_Functions[I].Name;
    if Lib = '*' then
    else if Copy(S, 1, Length(Lib)) = Lib then begin
      if Seeking then
        Seeking		:= False;
    end
    else if not Seeking then
      Break
    else
      Continue;
    if StringMatch(N, LS_Functions[I].Name) then begin
      Func		:= I;
      Exit;
    end;
  end;

  Result		:= False;

end;

(*------------------------------------------------------------------------------
GetLSLibMembers

------------------------------------------------------------------------------*)
function GetLSLibMembers(const Lib, MemberType: String): TStringList;
var
  I, L			: Integer;
  S			: String;
begin
  Result		:= TStringList.Create;
  L			:= Length(Lib);

  for I := 0 to High(LS_Functions) do begin
    S			:= LS_Functions[I].Name;
    if Copy(S, 1, L) <> Lib then
    else if
      (MemberType = 'all') or (MemberType = '*') or
      ((MemberType = 'methods') and (LS_Functions[I].Fn > -1)) or
      ((MemberType = 'constants') and (LS_Functions[I].Fn = -1))
    then
      Result.Add( Copy(S, L+2, Length(S)) );
  end;

end;

(*------------------------------------------------------------------------------
GetLSMatchingMethods

------------------------------------------------------------------------------*)
function GetLSMatchingMethods(const Lib, Name: String): TStringList;
var
  I			: Integer;
  N, S			: String;
begin
  Result		:= TStringList.Create;
  N			:= Lib + '.' + Name;

  for I := 0 to High(LS_Functions) do begin
    S			:= LS_Functions[I].Name;
    if StringMatch(N, LS_Functions[I].Name) then
      Result.Add(IntToStr(I));
  end;

end;

(*------------------------------------------------------------------------------
GetLSError

------------------------------------------------------------------------------*)
function GetLSError(const Err: TLetoScriptError): String;
var
  I			: Cardinal;
begin

  if Err = Err_LS_DefaultWarn then begin
    Result		:= GetWarning;
    Exit;
  end;

  for I := 0 to High(LetoScriptErrors) do
    if LetoScriptErrors[I].Err = Err then begin
      Result		:= LetoScriptErrors[I].S;
      Exit;
    end;

  Result		:= 'An unknown error occurred.';

end;

(*------------------------------------------------------------------------------
FormatLSError

------------------------------------------------------------------------------*)
function FormatLSError(
  const Err		: TLetoScriptError;
  const Symptoms	: array of const
): String;
var
  I			: Integer;

  procedure _Format_(const Id, Str: String);
  begin
    Result		:= StringReplace(Result, Id, Str, []);
  end;

begin
  Result		:= GetLSError(Err);

  for I := 0 to High(Symptoms) do
    with Symptoms[I] do
      case VType of
        vtInteger:	_Format_('%d', IntToStr(VInteger));
        vtAnsiString:	_Format_('%s', String(VAnsiString));
        vtChar:		_Format_('%c', VChar);
        vtVariant:	_Format_('%s', String(VAnsiString)); // watch out
        {
        vtBoolean:	S := S + BoolToStr(VBoolean);
        vtExtended:	S := S + FloatToStr(VExtended^);
        vtString:	S := S + VString^;
        vtPChar:	S := S + VPChar;
        vtObject:	S := S + VObject.ClassName;
        vtClass:	S := S + VClass.ClassName;
        vtCurrency:	S := S + CurrToStr(VCurrency^);
        vtVariant:	S := S + String(VVariant^);
        vtInt64:	S := S + IntToStr(VInt64^);
        }
      end;

end;

(*------------------------------------------------------------------------------
GetWarning

The default warning (when none other is supplied or intended) in LetoScript -
indeed, abroad throughout Leto - should, logically, be identical or nearly
identical to the default Perl warning: "Warning: Something's wrong"

But where's the fun in that? Leto is, generically, an application designed
for a game (Neverwinter Nights), and besides, the subtle theme is Frank
Herbert's "Dune".

This does open up a plethora of opportunities. Given this, and the author's
penchant for some degree of non-determinism, the default warning is instead
a *collection* of quotes, references, and aphorisms.

This returns one of them.

------------------------------------------------------------------------------*)
function GetWarning: String;
var
  R			: Byte;
begin
  R			:= Random(100);
  //R			:= 98;

  // TODO 4: ADD: { no Advice }
  // TODO 3: ADD: Quote #101.
  // TODO 2: ADD: More quotes.
  // TODO 2: ADD: Sources.
  case R of

    01: Result := 'Never toss a dwarf.';
    02: Result :=
      'It''s all wrong. By rights we shouldn''t even be here. But we are. ';
      {
      'It''s like in the great stories, Mr. Frodo. The ones that really mattered. ' +
      'Full of darkness and danger, they were. ' +
      'And sometimes you didn''t want to know the end. ' +
      'Because how could the end be happy? ' +
      'How could the world go back to the way it was when so much bad had happened?';
      }
    03: Result := 'I Aear cân ven na mar.';
    04: Result :=
      'I assure you that the ability to view our futures can become a bore. ';
      {
      'Even to be thought of as a god, as I certainly was, ' +
      'can become ultimately boring. ';
      'It has occurred to me more than once that holy boredom is good and ' +
      'sufficient reason for the invention of free will.';
      }
    06: Result := 'Short-term expediency always fails in the long-term.';
    22: Result :=
      'Though you have taken me for a soft stranger-lad and easy prey, ' +
      'let me warn you: I am not, I am a halfling, hard, bold and wicked!';
    26: Result := '"Make no heroes," my father said.';
    28: Result := 'All things are known because we want to believe in them.';
    34: Result :=
      'Knowledge, the Duncans believe, resides only in particulars. ' +
      'I try to tell them that all words are plastic.';
    { A bit too heavy.
    36: Result :=
      'Most civilization is based on cowardice. ' +
      'It''s so easy to civilize by teaching cowardice. ' +
      'You water down the standards which would lead to bravery. ' +
      'You restrain the will. You regulate the appetites. ' +
      'You fence in the horizons. You make a law for every moment. ' +
      'You deny the existence of chaos. ' +
      'You teach even the children to breathe slowly. You tame.';
    }
    97: Result :=
      'I show you the false happiness and the shadow-catastrophe called Leto, ' +
      'the God Emperor. Now, will you learn the real happiness?';
    98: Result := 'If you must label the absolute, use its proper name: Temporary.';
    99: Result := 'Something''s not right.';

  { But, of course, the logical choice is the weighted default. }
  else
    Result		:= 'Something''s wrong.';
  end;

end;

end.
