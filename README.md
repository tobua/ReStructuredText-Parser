ReStructuredText-Parser
=======================

A parser written in Haskell that converts ReST to LaTeX using the Parsec Library

Prerequisites:

    * Haskell 2013.2.0.0+
    * Parsec 3+

### Run a conversion:

Go to the directory that includes all the files

    cd ..
    
Start the Glasgow Haskell Compiler

    ghci
    
Load the parser
    
    :l a.hs
    
Read b.rest and write c.tex
    
    main
    
Done! The LaTeX source can be found in c.tex. To make a PDF simply type

    pdflatex c.tex

### Important files

The parser

    a.hs
    
The ReST source file

    b.rest
    
The LaTeX destination file

    c.tex
