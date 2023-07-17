" Vim syntax file
" Language: Boa
" Maintainer: Robert Dyer <rdyer@unl.edu>

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  " we define it here so that included files can test for it
  let main_syntax='boa'
  syn region boaFold start="{" end="}" transparent fold
endif

" don't use standard HiLink, it will not work with included syntax files
if version < 508
  command! -nargs=+ BoaHiLink hi link <args>
else
  command! -nargs=+ BoaHiLink hi def link <args>
endif

" some characters that cannot be in a Boa program (outside a string)
syn match boaError "[\\@`]"
syn match boaError "<<<\|\.\.\|=>\|<>\|||=\|&&=\|\*\/"
syn match boaError "#\|=<"
syn match boaOK "\.\.\."


" keyword definitions
syn keyword boaConditional if else switch case default before after _
syn keyword boaRepeat      while for do foreach exists ifall visit function traverse
syn keyword boaBoolean     true false
syn keyword boaConstant    PI Inf inf NaN nan SECOND SEC MINUTE MIN HOUR HR
syn keyword boaType        bool byte int float string time fingerprint visitor traversal fixp
syn keyword boaType        Project CodeRepository Revision ChangedFile ASTRoot Namespace Declaration Type Method Statement Expression Variable Modifier Person CFG CDG DDG PDG CFGNode CDGNode DDGNode PDGNode CFGEdge CDGEdge DDGEdge PDGEdge
syn keyword boaType        sum collection top bottom set minimum maximum mean unique quantile histogram text
syn match   boaType        "\(ForgeKind\|RepositoryKind\|FileKind\|ChangeKind\|TypeKind\|StatementKind\|ExpressionKind\|Visibility\|ModifierKind\|Scope\|TraversalKind\|TraversalDirection\|NodeType\|EdgeType\|EdgeLabel\)\(\.[A-Z_0-9]*\)*"
syn match   boaFuncs       "\(hash\|clear\|remove\|clone\|add\|poll\|offer\|peek\|push\|pop\|abs\|def\|new\|regex\|split\|splitn\|haskey\|contains\|containsall\|values\|keys\|len\|lookup\|max\|min\|sort\|sortx\|convert\|addday\|addmonth\|addweek\|addyear\|dayofmonth\|dayofweek\|dayofyear\|hourof\|minuteof\|monthof\|secondof\|yearof\|trunctoday\|trunctohour\|trunctominute\|trunctomonth\|trunctosecond\|trunctoyear\|now\|formattime\|lowercase\|uppercase\|strfind\|strrfind\|substring\|strreplace\|match\|matchposns\|matchstrs\|format\|highbit\|rand\|nrand\|log\|log10\|exp\|sqrt\|pow\|sin\|cos\|tan\|asin\|acos\|atan\|atan2\|cosh\|sinh\|tanh\|acosh\|asinh\|atanh\|ceil\|floor\|round\|trunc\|isnan\|isinf\|isfinite\|isnormal\|difference\|union\|intersect\|symdiff\|gettotalnodes\|gettotalcontrolnodes\|gettotaledges\|getpdtree\|getcfgslice\|getpdgslice\|parse\|parseexpression\|dot\|getvalue\|getcfg\|getcdg\|getddg\|getpdg\|getinedge\|getoutedge\|getast\|getastcount\|ast_len\|getsnapshot\|hasfiletype\|isfixingrevision\|iskind\|isliteral\|getnoargsvariables\|converttosymbolicname\|assignlatestvalue\|normalize\|reduce\|nnf\|simplify\|cnf\|dnf\|prettyprint\|has_visibility\|has_modifier\|has_modifier_final\|has_modifier_namespace\|has_modifier_private\|has_modifier_protected\|has_modifier_public\|has_modifier_static\|has_modifier_synchronized\|has_annotation\|get_annotation\|getrevisionscount\|getrevision\)\s*\((\)\@="
syn keyword boaStatement   return stop visit traverse current type
" to differentiate the keyword class from MyClass.class we use a match here
syn keyword boaBranch      break continue skipwhite
syn match   boaVarArg      "\.\.\."
syn keyword boaFuncDef     of stack map set output array input weight queue enum

if exists("boa_space_errors")
  if !exists("boa_no_trail_space_error")
    syn match boaSpaceError "\s\+$"
  endif
  if !exists("boa_no_tab_space_error")
    syn match boaSpaceError " \+\t"me=e-1
  endif
endif

" Comments
syn match   boaLineComment      "#.*" contains=@Spell

" Strings and constants
syn match   boaSpecialError     contained "\\."
syn match   boaSpecialCharError contained "[^']"
syn match   boaSpecialChar      contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\x\{4\}\)"
syn region  boaString           start=+"+ end=+"+ end=+$+ contains=boaSpecialChar,boaSpecialError,@Spell
syn region  boaString           start=+"""+ end=+"""+ contains=boaSpecialChar,boaSpecialError,@Spell
syn region  boaString           start=+`+ end=+`+ end=+$+ contains=@Spell
syn match   boaCharacter        "'[^']*'" contains=boaSpecialChar,boaSpecialCharError
syn match   boaCharacter        "'\\''" contains=boaSpecialChar
syn match   boaCharacter        "'[^\\]'"
syn match   boaNumber           "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   boaNumber           "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match   boaNumber           "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match   boaNumber           "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"

" unicode characters
syn match   boaSpecial          "\\u\d\{4\}"

" The following cluster contains all boa groups except the contained ones
syn cluster boaTop add=boaError,boaError,boaBranch,boaConditional,boaRepeat,boaFuncs,boaBoolean,boaConstant,boaOperator,boaType,boaType,boaStatement,boaStorageClass,boaAssert,boaExceptions,boaMethodDecl,boaError,boaLangObject,boaVarArg,boaLineComment,boaString,boaCharacter,boaNumber,boaSpecial,boaStringError


if exists("boa_highlight_functions")
  if boa_highlight_functions == "indent"
    syn match  boaFuncDef "^\(\t\| \{8\}\)[_$a-zA-Z][_$a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=boaType,boaStorageClass,@boaClasses
    syn region boaFuncDef start=+^\(\t\| \{8\}\)[$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=boaType,boaStorageClass,@boaClasses
    syn match  boaFuncDef "^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=boaType,boaStorageClass,@boaClasses
    syn region boaFuncDef start=+^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=boaType,boaStorageClass,@boaClasses
  else
    " This line catches method declarations at any indentation>0, but it assumes
    " two things:
    "   1. class names are always capitalized (ie: Button)
    "   2. method names are never capitalized (except constructors, of course)
    syn region boaFuncDef start=+^\s\+\(\(public\|protected\|private\|static\|abstract\|final\|native\|synchronized\)\s\+\)*\(\(void\|boolean\|char\|byte\|short\|int\|long\|float\|double\|\([A-Za-z_][A-Za-z0-9_$]*\.\)*[A-Z][A-Za-z0-9_$]*\)\(<[^>]*>\)\=\(\[\]\)*\s\+[a-z][A-Za-z0-9_$]*\|[A-Z][A-Za-z0-9_$]*\)\s*([^0-9]+ end=+)+ contains=boaType,boaStorageClass,boaLineComment,@boaClasses
  endif
  syn match  boaBraces  "[{}]"
  syn cluster boaTop add=boaFuncDef,boaBraces
endif

if exists("boa_mark_braces_in_parens_as_errors")
  syn match boaInParen contained "[{}]"
  BoaHiLink boaInParen boaError
  syn cluster boaTop add=boaInParen
endif

" catch errors caused by wrong parenthesis
syn region  boaParenT  transparent matchgroup=boaParen  start="(" end=")" contains=@boaTop,boaParenT1
syn region  boaParenT1 transparent matchgroup=boaParen1 start="(" end=")" contains=@boaTop,boaParenT2 contained
syn region  boaParenT2 transparent matchgroup=boaParen2 start="(" end=")" contains=@boaTop,boaParenT  contained
syn match   boaParenError ")"
" catch errors caused by wrong square parenthesis
syn region  boaParenT  transparent matchgroup=boaParen  start="\[" end="\]" contains=@boaTop,boaParenT1
syn region  boaParenT1 transparent matchgroup=boaParen1 start="\[" end="\]" contains=@boaTop,boaParenT2 contained
syn region  boaParenT2 transparent matchgroup=boaParen2 start="\[" end="\]" contains=@boaTop,boaParenT  contained
syn match   boaParenError "\]"

BoaHiLink boaParenError boaError

" The default highlighting.
if version >= 508 || !exists("did_boa_syn_inits")
  if version < 508
    let did_boa_syn_inits = 1
  endif
  BoaHiLink boaFuncDef          Function
  BoaHiLink boaVarArg           Function
  BoaHiLink boaBraces           Function
  BoaHiLink boaBranch           Conditional
  BoaHiLink boaConditional      Conditional
  BoaHiLink boaRepeat           Repeat
  BoaHiLink boaFuncs            Repeat
  BoaHiLink boaExceptions       Exception
  BoaHiLink boaAssert           Statement
  BoaHiLink boaStorageClass     StorageClass
  BoaHiLink boaMethodDecl       boaStorageClass
  BoaHiLink boaBoolean          Boolean
  BoaHiLink boaSpecial          Special
  BoaHiLink boaSpecialError     Error
  BoaHiLink boaSpecialCharError Error
  BoaHiLink boaString           String
  BoaHiLink boaCharacter        Character
  BoaHiLink boaSpecialChar      SpecialChar
  BoaHiLink boaNumber           Number
  BoaHiLink boaError            Error
  BoaHiLink boaStringError      Error
  BoaHiLink boaStatement        Statement
  BoaHiLink boaOperator         Operator
  BoaHiLink boaLineComment      Comment
  BoaHiLink boaConstant         Constant

  BoaHiLink boaType             Type

  BoaHiLink boaSpaceError       Error
endif

delcommand BoaHiLink

let b:current_syntax = "boa"

if main_syntax == 'boa'
  unlet main_syntax
endif

let b:spell_options="contained"

" vim: ts=8
