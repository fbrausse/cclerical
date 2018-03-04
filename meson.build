project('cclerical', 'c'
       , license : 'BSD-3'
       , version : '0.1'
       , meson_version: '>=0.43'
       )
ccllexer = custom_target( 'cclerical.lex'
                        , output : [ 'cclerical.lex.c'
                                   , 'cclerical.lex.h'
                                   ]
                        , input  : 'cclerical.l'
                        , command: [ find_program('lex')
                                   , '--batch'
                                   , '-o','@OUTPUT0@'
                                   , '--header-file=@OUTPUT1@'
                                   , '@INPUT@'
                                   ]
                        )
cclparser = custom_target( 'cclerical.tab'
                         , output : [ 'cclerical.tab.c'
                                    , 'cclerical.tab.h'
                                    , 'cclerical.output'
                                    ]
                         , input  : 'cclerical.y'
                         , command: [ find_program('yacc')
                                    , '-v'
                                    , '-b','cclerical'
                                    , '@INPUT@'
                                    ]
                         )
cclheaders = declare_dependency(sources : [ ccllexer[1], cclparser[1] ])

ccl_srcs = [ 'ccl.c'
           , ccllexer[0]
           , cclparser[0]
           , 'cclerical.c'
           , 'ccl-ssa.c'
           , 'ccl-ast-irram.c'
           ]

cclexe = executable('ccl', ccl_srcs
                   , dependencies : cclheaders
                   , c_args       : [ '-D_POSIX_C_SOURCE=2'
                                    , '-DCCL_VERSION_STR="'+meson.project_version()+'"'
                                    , '-std=c11'
                                    ]
                   )

ccl = generator( cclexe
               , arguments : [ '-b', 'iRRAM', '-o', '@OUTPUT@', '@INPUT@' ]
               , output    : '@BASENAME@.cc'
               )

subdir('examples')
