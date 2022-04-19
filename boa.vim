" Vim syntax file
" Language:	Boa
" Maintainer:	Robert Dyer <rdyer@unl.edu>

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

" some characters that cannot be in a boa program (outside a string)
syn match boaError "[\\@`]"
syn match boaError "<<<\|\.\.\|=>\|<>\|||=\|&&=\|\*\/"
syn match boaError "#\|=<"
syn match boaOK "\.\.\."


" keyword definitions
syn keyword boaError		goto const
syn keyword boaConditional	if else switch before after case default
syn keyword boaRepeat		while for do foreach exists ifall visit function traversal
syn keyword boaBoolean		true false
syn keyword boaType		bool byte int float string time fingerprint visitor
syn keyword boaType		Project CodeRepository Person Revision ChangedFile ASTRoot Namespace Declaration Type Method Variable Statement Expression Modifier CFG CFGNode
syn keyword boaType		sum collection top bottom set minimum maximum mean
syn match boaType		"\(RepositoryKind\|FileKind\|ChangeKind\|TypeKind\|StatementKind\|ExpressionKind\|Visibility\|ModifierKind\|CommentKind\|TraversalKind\|TraversalDirection\|CFGNodeType\|CFGEdgeLabel\)\(\.[A-Z_0-9]*\)*"
syn keyword boaStatement	return stop visit traverse
" to differentiate the keyword class from MyClass.class we use a match here
syn keyword boaBranch		break continue skipwhite
syn match   boaVarArg		"\.\.\."
syn keyword boaFuncDef     of stack map set output array input weight queue

if exists("boa_space_errors")
  if !exists("boa_no_trail_space_error")
    syn match	boaSpaceError	"\s\+$"
  endif
  if !exists("boa_no_tab_space_error")
    syn match	boaSpaceError	" \+\t"me=e-1
  endif
endif

" The following cluster contains all boa groups except the contained ones
syn cluster boaTop add=boaExternal,boaError,boaError,boaBranch,boaLabelRegion,boaLabel,boaConditional,boaRepeat,boaBoolean,boaConstant,boaTypedef,boaOperator,boaType,boaType,boaStatement,boaStorageClass,boaAssert,boaExceptions,boaMethodDecl,boaClassDecl,boaClassDecl,boaClassDecl,boaScopeDecl,boaError,boaError2,boaUserLabel,boaLangObject,boaVarArg


" Comments
syn match   boaLineComment	 "#.*" contains=@boaCommentSpecial2,@Spell

syn cluster boaTop add=boaLineComment

" Strings and constants
syn match   boaSpecialError	 contained "\\."
syn match   boaSpecialCharError contained "[^']"
syn match   boaSpecialChar	 contained "\\\([4-9]\d\|[0-3]\d\d\|[\"\\'ntbrf]\|u\x\{4\}\)"
syn region  boaString		start=+"+ end=+"+ end=+$+ contains=boaSpecialChar,boaSpecialError,@Spell
syn region  boaString		start=+"""+ end=+"""+ contains=boaSpecialChar,boaSpecialError,@Spell
syn region  boaString		start=+`+ end=+`+ end=+$+ contains=@Spell
" next line disabled, it can cause a crash for a long line
"syn match   boaStringError	  +"\([^"\\]\|\\.\)*$+
syn match   boaCharacter	 "'[^']*'" contains=boaSpecialChar,boaSpecialCharError
syn match   boaCharacter	 "'\\''" contains=boaSpecialChar
syn match   boaCharacter	 "'[^\\]'"
syn match   boaNumber		 "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match   boaNumber		 "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match   boaNumber		 "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match   boaNumber		 "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"

" unicode characters
syn match   boaSpecial "\\u\d\{4\}"

syn cluster boaTop add=boaString,boaCharacter,boaNumber,boaSpecial,boaStringError

if exists("boa_highlight_functions")
  if boa_highlight_functions == "indent"
    syn match  boaFuncDef "^\(\t\| \{8\}\)[_$a-zA-Z][_$a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=boaScopeDecl,boaType,boaStorageClass,@boaClasses
    syn region boaFuncDef start=+^\(\t\| \{8\}\)[$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=boaScopeDecl,boaType,boaStorageClass,@boaClasses
    syn match  boaFuncDef "^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*)" contains=boaScopeDecl,boaType,boaStorageClass,@boaClasses
    syn region boaFuncDef start=+^  [$_a-zA-Z][$_a-zA-Z0-9_. \[\]]*([^-+*/()]*,\s*+ end=+)+ contains=boaScopeDecl,boaType,boaStorageClass,@boaClasses
  else
    " This line catches method declarations at any indentation>0, but it assumes
    " two things:
    "	1. class names are always capitalized (ie: Button)
    "	2. method names are never capitalized (except constructors, of course)
    syn region boaFuncDef start=+^\s\+\(\(public\|protected\|private\|static\|abstract\|final\|native\|synchronized\)\s\+\)*\(\(void\|boolean\|char\|byte\|short\|int\|long\|float\|double\|\([A-Za-z_][A-Za-z0-9_$]*\.\)*[A-Z][A-Za-z0-9_$]*\)\(<[^>]*>\)\=\(\[\]\)*\s\+[a-z][A-Za-z0-9_$]*\|[A-Z][A-Za-z0-9_$]*\)\s*([^0-9]+ end=+)+ contains=boaScopeDecl,boaType,boaStorageClass,boaComment,boaLineComment,@boaClasses
  endif
  syn match  boaBraces  "[{}]"
  syn cluster boaTop add=boaFuncDef,boaBraces
endif

if exists("boa_mark_braces_in_parens_as_errors")
  syn match boaInParen		 contained "[{}]"
  BoaHiLink boaInParen	boaError
  syn cluster boaTop add=boaInParen
endif

" catch errors caused by wrong parenthesis
syn region  boaParenT	transparent matchgroup=boaParen  start="("  end=")" contains=@boaTop,boaParenT1
syn region  boaParenT1 transparent matchgroup=boaParen1 start="(" end=")" contains=@boaTop,boaParenT2 contained
syn region  boaParenT2 transparent matchgroup=boaParen2 start="(" end=")" contains=@boaTop,boaParenT  contained
syn match   boaParenError	 ")"
" catch errors caused by wrong square parenthesis
syn region  boaParenT	transparent matchgroup=boaParen  start="\["  end="\]" contains=@boaTop,boaParenT1
syn region  boaParenT1 transparent matchgroup=boaParen1 start="\[" end="\]" contains=@boaTop,boaParenT2 contained
syn region  boaParenT2 transparent matchgroup=boaParen2 start="\[" end="\]" contains=@boaTop,boaParenT  contained
syn match   boaParenError	 "\]"

BoaHiLink boaParenError	boaError

if !exists("boa_minlines")
  let boa_minlines = 10
endif
exec "syn sync ccomment boaComment minlines=" . boa_minlines

" The default highlighting.
if version >= 508 || !exists("did_boa_syn_inits")
  if version < 508
    let did_boa_syn_inits = 1
  endif
  BoaHiLink boaFuncDef		Function
  BoaHiLink boaVarArg			Function
  BoaHiLink boaBraces			Function
  BoaHiLink boaBranch			Conditional
  BoaHiLink boaUserLabelRef		boaUserLabel
  BoaHiLink boaLabel			Label
  BoaHiLink boaUserLabel		Label
  BoaHiLink boaConditional		Conditional
  BoaHiLink boaRepeat			Repeat
  BoaHiLink boaExceptions		Exception
  BoaHiLink boaAssert			Statement
  BoaHiLink boaStorageClass		StorageClass
  BoaHiLink boaMethodDecl		boaStorageClass
  BoaHiLink boaClassDecl		boaStorageClass
  BoaHiLink boaScopeDecl		boaStorageClass
  BoaHiLink boaBoolean		Boolean
  BoaHiLink boaSpecial		Special
  BoaHiLink boaSpecialError		Error
  BoaHiLink boaSpecialCharError	Error
  BoaHiLink boaString			String
  BoaHiLink boaCharacter		Character
  BoaHiLink boaSpecialChar		SpecialChar
  BoaHiLink boaNumber			Number
  BoaHiLink boaError			Error
  BoaHiLink boaStringError		Error
  BoaHiLink boaStatement		Statement
  BoaHiLink boaOperator		Operator
  BoaHiLink boaComment		Comment
  BoaHiLink boaLineComment		Comment
  BoaHiLink boaConstant		Constant
  BoaHiLink boaTypedef		Typedef

  BoaHiLink boaCommentTitle		SpecialComment

  BoaHiLink boaType			Type
  BoaHiLink boaExternal		Include

  BoaHiLink htmlComment		Special
  BoaHiLink htmlCommentPart		Special
  BoaHiLink boaSpaceError		Error
endif

delcommand BoaHiLink

let b:current_syntax = "boa"

if main_syntax == 'boa'
  unlet main_syntax
endif

let b:spell_options="contained"

" vim: ts=8
