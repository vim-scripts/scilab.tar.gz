" Vim syntax file
" Language:	Scilab
" Maintainer:	Vaclav Mocek <vamo@seznam.cz>
" Last Change:	Tue Oct 10 17:00:06 2003
"

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" KEYWORD SECTION

" SCILAB Function

syn keyword scilabStatement		return
syn keyword scilabLabel			select
syn keyword scilabConditional		else elseif end if case then
syn keyword scilabRepeat		for while
syn keyword scilabTodo			contained TODO

" Basic function
syn keyword scilabBasic			abort ans apropos bool2s break call
syn keyword scilabBasic			clear clearglobal debug empty errcatch
syn keyword scilabBasic			errclear error evstr exec execstr
syn keyword scilabBasic			exists exit feval find format fort
syn keyword scilabBasic			funptr getenv getfield getpid
syn keyword scilabBasic			getversion global gstacksize host
syn keyword scilabBasic			hypermat iconvert ieee intppty inttype
syn keyword scilabBasic			inv_coef iserror isglobal lasterror
syn keyword scilabBasic			list lsslist lstcat matrix mlist mode
syn keyword scilabBasic			mtlb_mode names null pause poly predef
syn keyword scilabBasic			pwd quit resume return rlist sciargs
syn keyword scilabBasic			setfield stacksize testmatrix tlist
syn keyword scilabBasic			type typename user varn what where
syn keyword scilabBasic			whereami whereis who whos


" Graphics
syn keyword scilabGraphics		Matplot Matplot1 Sfgrayplot Sgrayplot
syn keyword scilabGraphics		addcolor black bode champ champ1 chart
syn keyword scilabGraphics		colormap contour contour2d contour2di
syn keyword scilabGraphics		contourf dragrect drawaxis driver
syn keyword scilabGraphics		edit_curv errbar eval3d eval3dp evans
syn keyword scilabGraphics		fac3d fchamp fcontour fcontour2d fec
syn keyword scilabGraphics		fgrayplot fplot2d fplot3d fplot3d1
syn keyword scilabGraphics		gainplot genfac3d geom3d getcolor getfont
syn keyword scilabGraphics		getlinestyle getmark getsymbol gr_menu
syn keyword scilabGraphics		graduate graycolormap grayplot
syn keyword scilabGraphics		graypolarplot hist3d histplot hotcolormap
syn keyword scilabGraphics		isoview legends locate m_circle milk_drop
syn keyword scilabGraphics		nf3d nyquist param3d param3d1 paramfplot2d
syn keyword scilabGraphics		plot plot2d plot2d1 plot2d2 plot2d3
syn keyword scilabGraphics		plot2d4 plot3d plot3d1 plot3d2 plot3d3
syn keyword scilabGraphics		plotframe plzr polarplot replot rotate
syn keyword scilabGraphics		scaling sd2sci secto3d sgrid square
syn keyword scilabGraphics		subplot titlepage winsid xarc xarcs
syn keyword scilabGraphics		xarrows xaxis xbasc xbasimp xbasr
syn keyword scilabGraphics		xchange xclea xclear xclick xclip xdel
syn keyword scilabGraphics		xend xfarc xfarcs xfpoly xfpolys xfrect
syn keyword scilabGraphics		xget xgetex xgetmouse xgraduate xgrid
syn keyword scilabGraphics		xinfo xinit xlfont xload xname xnumb
syn keyword scilabGraphics		xpause xpoly xpolys xrect xrects xrpoly
syn keyword scilabGraphics		xs2fig xsave xsegs xselect xset xsetex
syn keyword scilabGraphics		xsetm xstring xstringb xstringl xtape
syn keyword scilabGraphics		xtitle zgrid


" Utilities and elementary function
syn keyword scilabElement		abs acos acosh acoshm acosm addf adj2sp
syn keyword scilabElement		amell and asin asinh asinhm asinm atan
syn keyword scilabElement		atanh atanhm besseli besselj besselk
syn keyword scilabElement		bessely binomial bloc2exp bloc2ss calerf
syn keyword scilabElement		ceil conj cos cosh coshm cosm cotg coth
syn keyword scilabElement		cothm cumprod cumsum delip diag dlgamma
syn keyword scilabElement		double int16 int32 uint8 uint16 uint32
syn keyword scilabElement		erf erfc erfcx eval eye fix floor
syn keyword scilabElement		frexp full gamma gammaln gsort imag int
syn keyword scilabElement		int8 integrate interp interpln intersect
syn keyword scilabElement		intsplin inttrap isdef isinf isnan isreal
syn keyword scilabElement		kron ldivf lex_sort linspace log log10 log2
syn keyword scilabElement		logm logspace max maxi mean median min
syn keyword scilabElement		mini minus modulo mps2linpro mtlb_sparce
syn keyword scilabElement		mulf nnz norm not ones or pen2ea pertrans
syn keyword scilabElement		prod rand rat rdivf real round 	sign
syn keyword scilabElement		signm sin sinh sinhm sinm size smooth
syn keyword scilabElement		solve sort sp2adj sparce spcompack speye
syn keyword scilabElement		spget splin spones sprand spzeros sqrt
syn keyword scilabElement		sgrtm squarewave ssprint ssrand
syn keyword scilabElement		st_deviation subf sum sys conf sysdiag
syn keyword scilabElement		syslin tan tanh tanhm tanm toeplitz
syn keyword scilabElement		trfmod trianfml tril trisolve triu
syn keyword scilabElement		typeof union unique zeros

" Input and output function
syn keyword scilabInpOut		diary disp dispfile file fileinfo
syn keyword scilabInpOut		fprintf fprintfMat fscanf fscanfMat
syn keyword scilabInpOut		getio input lines load manedit mclearerr
syn keyword scilabInpOut		mclose meof mscanf mget mgetl mgetstr
syn keyword scilabInpOut		mopen mfprintf mput mputl mputstr
syn keyword scilabInpOut		mfscanf mseek mtell newest oldload oldsave
syn keyword scilabInpOut		print printf printf_conversion read read4b
syn keyword scilabInpOut		readb readc readmps save scanf sprintf
syn keyword scilabInpOut		sscanf warning writb write write4b xgetfile

" Function
syn keyword scilabFunction		addinter argn clearfun comp deff delbpt
syn keyword scilabFunction		dispbpt edit funcprot function genlib
syn keyword scilabFunction		get_function_path getd getf lib macr2lst
syn keyword scilabFunction		macrovar newfun plotprofile profile setbpt
syn keyword scilabFunction		showprofile varargin varargout endfunction

" String manipulation
syn keyword scilabStrMan		code2str covstr emptystr grep length part
syn keyword scilabStrMan		str2code strcat strindex string stripblanks
syn keyword scilabStrMan		strsubst

" Dialog function
syn keyword scilabDialog		addmenu delmenu getvalue halt havewindow
syn keyword scilabDialog		keyboard setmenu unsetmenu x_choices
syn keyword scilabDialog		x_choose x_dialog x_matrix x_mdialog
syn keyword scilabDialog		x_message x_message_modeless

" Utilities
syn keyword scilabUtil			G_make c_link chdir dec2hex demos help 
syn keyword scilabUtil			hex2dec ilib_build ilib_compile ilib_for_link
syn keyword scilabUtil			ilib_gen_make ilib_gen_gateway ilib_gen_loader
syn keyword scilabUtil			link man sci2exp sci2map ulink unix unix_g
syn keyword scilabUtil			unix_s unix_w unix_x

" Time and date
syn keyword scilabTime			date getdate timer

" General System and Control macros
 syn keyword scilabGeneral		abcd abinv arhnk arl2 balreal bilin cainv
 syn keyword scilabGeneral		calfrq canon cls2dls colregul cont_frm
 syn keyword scilabGeneral		cont_mat contr contrss csim ctr_gram dbphi
 syn keyword scilabGeneral		ddp des2tf dscr dsimul d_ility equil1
 syn keyword scilabGeneral		feedback flts frep2tf freq freson g_margin
 syn keyword scilabGeneral		gfrancis imrep2ss invsyslin kpure krac2 lin
 syn keyword scilabGeneral		lqe lqg lqg2stan lqr ltitr markp2ss minreal
 syn keyword scilabGeneral		minss obs_gram obscont observer obsv_mat
 syn keyword scilabGeneral		obsvss p_margin pfss phasemag ppol projsl
 syn keyword scilabGeneral		repfreq ricc rowregul rtitr sm2des sm2ss
 syn keyword scilabGeneral		specfact ss2des ss2ss ss2tf st_ility
 syn keyword scilabGeneral		stabil svplot sysfact syssize tf2ss
 syn keyword scilabGeneral		time_id trzeros ui_observer unobs zeropen

" Robust Control Toolbox
 syn keyword scilabControl		augment bstap ccontrg colinout copfac dcf
 syn keyword scilabControl		des2ss dhnorm dtsi fourplan fspecg fstabst
 syn keyword scilabControl		gamitg gcare gfare gtild h2norm h_cl h_inf
 syn keyword scilabControl		h_inf_st h_norm hankelsv lcf leqr lft linf
 syn keyword scilabControl		linfn lqg_ltr macglov nehari parrot ric_dest
 syn keyword scilabControl		riccati rowinout sensi tf2des

" Tools for Dynamical Systems
 syn keyword scilabDynamic		arma arma2p armac armax armax1 arsimul 
 syn keyword scilabDynamic		narsimul noisegen odedi reglin

" Non-linear Tools
 syn keyword scilabNonlin		bvode colnew dasrt dassl datafit derivative
 syn keyword scilabNonlin		fit_dat fsolve impl int2d int3d intc intg
 syn keyword scilabNonlin		intl karmarkar leastsq linpro lmisolver
 syn keyword scilabNonlin		lmitool ode ode_discrete ode_root odedc
 syn keyword scilabNonlin		odeoptions optim quapro semidef

" Signal processing toolbox
 syn keyword scilabSignal		%asn %k %sn analpf buttmag casc cepstrum
 syn keyword scilabSignal		cheb1mag cheb2mag chepol convol corr
 syn keyword scilabSignal		cspect czt dft ell1mag eqfir egiir faurre
 syn keyword scilabSignal		ffilt fft filter find_freq findm frfit
 syn keyword scilabSignal		frmag fsfirlin group hank hilb iir irrgroup
 syn keyword scilabSignal		irrlp intdec jmat kalm lattn lattp lev levin
 syn keyword scilabSignal		lgfft lindguist mese mfft mrfit phc pspect
 syn keyword scilabSignal		remez remezb rpem sinc sincd srfaur srkf
 syn keyword scilabSignal		sskf system trans wfir wiener wigner window
 syn keyword scilabSignal		yulewalk zpbutt zpch1 zpch2 zpell

" Polynomial Calculation
 syn keyword scilabPolcal		bezout clean cmndred coeff coffg colcompr
 syn keyword scilabPolcal		degree denom derivat determ detr diophant
 syn keyword scilabPolcal		factors gcd hermit horner hrmt htrianr invr
 syn keyword scilabPolcal		lcm lcmdiag ldiv numer pdiv pol2des pol2str
 syn keyword scilabPolcal		polfact residu roots routh_t rowcompr sfact
 syn keyword scilabPolcal		simp simp_mode sylm systmat

" Linear Algebra
 syn keyword scilabLinalg		aff2ab balanc bdiag chfact chol chsolve
 syn keyword scilabLinalg		classmarkov coff colcomp companion cond
 syn keyword scilabLinalg		det eigenmarkov ereduc exp expm fstair
 syn keyword scilabLinalg		fullrf fullrfk genmarkov givens glever
 syn keyword scilabLinalg		gschur gspec hess householder im_inv
 syn keyword scilabLinalg		inv kernel kroneck linsolve lu ludel lufact
 syn keyword scilabLinalg		luget lusolve lyap nlev orth pbig pencan
 syn keyword scilabLinalg		penlaur pinv polar proj projspec psmall
 syn keyword scilabLinalg		qr quaskro randpencil range rank rcond
 syn keyword scilabLinalg		rowcomp rowshuff rref schur spaninter
 syn keyword scilabLinalg		spanplus spantwo spchol spec sqroot sva
 syn keyword scilabLinalg		svd sylv trace

" Metanet
 syn keyword scilabMeta			add_edge add_none adj_lists arc_graph
 syn keyword scilabMeta			arc_number articul bandwr best_match
 syn keyword scilabMeta			chain_struct check_graph circuit con_nodes
 syn keyword scilabMeta			connex contract_edge convex_hull cycle_basis
 syn keyword scilabMeta			delete_arcs delete_nodes edge_number gen_net
 syn keyword scilabMeta			girth glist graph_2_mat graph_center
 syn keyword scilabMeta			graph_complement graph_diameter graph_power
 syn keyword scilabMeta			graph_simp graph_sum graph_union hamilton
 syn keyword scilabMeta			is_connex knapsack line_graph load_graph
 syn keyword scilabMeta			make_graph mat_2_graph max_cap_path
 syn keyword scilabMeta			max_clique max_flow mesh2d metanet
 syn keyword scilabMeta			metanet_sync min_lcost_cflow min_lcost_flow1
 syn keyword scilabMeta			min_lcost_flow2 min_qcost_flow min_weight_tree
 syn keyword scilabMeta			neighbors netclose netwindow node_number
 syn keyword scilabMeta			node_2_path nodes_degrees path_2_nodes
 syn keyword scilabMeta			perfect_match pipe_network plot_graph
 syn keyword scilabMeta			predecessors qassign salesman save_graph
 syn keyword scilabMeta			shortest_path show_arcs show_graph
 syn keyword scilabMeta			show_nodes split_edge strong_con_nodes
 syn keyword scilabMeta			strong_connex subgraph successors supernode
 syn keyword scilabMeta			trans_closure

" Scicos
 syn keyword scilabScicos		scicos standard_define standard_draw
 syn keyword scilabScicos		standard_input standart_origin standard_output
 syn keyword scilabScicos		scicosim curblock getblocklabel getscicosvars
 syn keyword scilabScicos		setscicosvars 

" Sound
 syn keyword scilabSound		analyze auread auwrite lin2mu loadwave mapsound
 syn keyword scilabSound		mu2lin playsnd sound wavread wavwrite

" Cumulative Distribution Function, Iverses, Random Variables
 syn keyword scilabCumul		cdfbet cdfbin cdfchi cdfchn cdff cdffnc cdfgam
 syn keyword scilabCumul		cdfnbn cdfnor cdfpoi cdft grand

" Language data and translation tools
 syn keyword scilabTrans		ascii excel2sci formatman fun2string mfile2sci
 syn keyword scilabTrans		mtlb_load mtlb_save pol2tex sci2for texprint
 syn keyword scilabTrans		translatepaths


 " MATCH SECTION

" Operator
syn match scilabArithmeticOperator	"[-+]"
syn match scilabArithmeticOperator	"\.\=[*/\\^]"
syn match scilabRelationalOperator	"[=~]="
syn match scilabRelationalOperator	"[<>]=\="
syn match scilabLogicalOperator		"[&|~]"
"syn match scilabLineContinuation	"\.\{3}"
"syn match scilabIdentifier		"\<\a\w*\>"

"String
"syn match scilabTab			"\t"

"Numbers
"Standard numbers
syn match scilabNumber		"\<\d\+\>"
"floating point number, with dot, optional exponent
syn match scilabFloat		"\<\d\+\(\.\d*\)\=\([edED][-+]\=\d\+\)\=\>"
"floating point number, starting with a dot, optional exponent
syn match scilabFloat		"\.\d\+\([edED][-+]\=\d\+\)\=\>"

"Transpose character and delimiters: Either use just [...] or (...) aswell
syn match scilabDelimiter		"[][]"
"syn match scilabDelimiter		"[][()]"
"syn match scilabTransposeOperator	"[])a-zA-Z0-9.]'"lc=1
syn match scilabSemicolon		";"
syn match scilabComment			"//.*$"	contains=scilabTodo,scilabTab
syn match scilabError	"-\=\<\d\+\.\d\+\.[^*/\\^]"
"syn match scilabError	"-\=\<\d\+\.\d\+[eEdD][-+]\=\d\+\.\([^*/\\^]\)"

" REGION SECTION
syn region scilabString			start=+'+ end=+'+	oneline
syn region scilabString			start=+"+ end=+"+	oneline


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet

if version >= 508 || !exists("did_scilab_syntax_inits")
  if version < 508
    let did_scilab_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink scilabCol_I			Define
 
"Keyword
  HiLink scilabStatement		Statement
  HiLink scilabLabel			Label
  HiLink scilabConditional		Conditional
  HiLink scilabRepeat			Repeat
  HiLink scilabTodo			Todo

  HiLink scilabBasic			scilabCol_I
  HiLink scilabGraphics			scilabCol_I
  HiLink scilabElement			scilabCol_I
  HiLink scilabInpOut			scilabCol_I
  HiLink scilabFunction			Function
  HiLink scilabStrMan			scilabCol_I
  HiLink scilabDialog			scilabCol_I
  HiLink scilabUtil			scilabCol_I
  HiLink scilabTime			scilabCol_I
  HiLink scilabGeneral			scilabCol_I
  HiLink scilabControl			scilabCol_I
  HiLink scilabDynamic			scilabCol_I
  HiLink scilabNonlin 			scilabCol_I
  HiLink scilabSignal  			scilabCol_I
  HiLink scilabPolcal			scilabCol_I
  HiLink scilabLinalg 			scilabCol_I
  HiLink scilabMeta			scilabCol_I
  HiLink scilabScicos			scilabCol_I
  HiLink scilabSound			scilabCol_I
  HiLink scilabCumul			scilabCol_I
  HiLink scilabTrans			scilabCol_I
  
  
  "Match
  HiLink scilabArithmeticOperator	Operator
  HiLink scilabRelationalOperator	Operator
  HiLink scilabLogicalOperator		Operator
  HiLink scilabLineContinuation		SpecialChar
  HiLink scilabIdentifier		Identifier
  HiLink scilabTab			SpecialChar
  HiLink scilabNumber			Number
  HiLink scilabFloat			Float
  HiLink scilabDelimiter		Delimiter
  HiLink scilabTransposeOperator	Operator
  HiLink scilabSemicolon		SpecialChar
  HiLink scilabComment			Comment
  HiLink scilabError			Error
  
  "Region
  HiLink scilabString			String

  delcommand HiLink
endif
let b:current_syntax = "scilab"

" vim:sw=2 
