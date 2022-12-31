                           The BONES Manual
                           ================



/Release 8/

Table of Contents
=================
1 Introduction 
2 Portability 
3 Requirements 
4 Installation 
5 Reporting Bugs and Upgrading 
6 Usage 
    6.1 Compiling Scheme code 
    6.2 Assembling and linking Executables 
    6.3 Command-line Options 
    6.4 Configuration Macros for Generated Assembly Code 
    6.5 Configurations 
7 Language Description 
    7.1 Program Specifications 
    7.2 Deviations from R5RS 
    7.3 Extensions to R5RS 
    7.4 Additional Library Code 
    7.5 The Evaluator 
8 Compiler Details 
    8.1 Compilation Strategy 
    8.2 Compiler Stages 
    8.3 Register Usage 
    8.4 Optimizations Performed 
    8.5 Performance 
    8.6 Hacking and Extending the Compiler 
9 Data Representation 
10 Garbage Collection 
11 Interfacing to Foreign Code 
12 Embedding 
13 Debugging 
14 Bugs and Limitations 
15 Porting Guide 
16 Suggestions for Projects 
17 Terms of Use 
18 Further Reading 
19 Contact 


1 Introduction 
~~~~~~~~~~~~~~~

  This is BONES, a compiler for R5RS Scheme that generates x86_64
  assembly code. BONES is designed to be simple and easy to
  understand, both to reduce the effort to learn and extend the system
  and to keep the complexity of the compiler at a minimum.

  BONES is a batch-compiler, it takes a Scheme source file and
  produces an assembler file to be subsequently translated into object
  code. It is also a whole-program compiler, which means it does not
  support separate compilation of multiple modules. The runtime system
  is by default added to your program before it is compiled, so there
  are no external libraries, with the exception of a few bits from the
  C library ("libc") (this is optional).

  BONES is mostly R5RS-compliant, but intentionally cuts some corners
  to reduce code-size, increase performance and simplify the compiler
  and runtime system. Type-checks are generally omitted, for example.
  Very little error checking is done and arithmetic overflow of small
  integers ("fixnums") is not detected. Some R7RS procedures are
  available in addition to the primitives required for R5RS.

  Since BONES produces assembly-code, build-times are quite short. The
  produced code is CPU- and OS-specific and currently supports x86_64
  on Linux, *BSD, Mac or Windows. Porting the system to other
  architectures and operating systems should be (relatively)
  straightforward, as the platform-specific parts of the compiler are
  small and the runtime-system is just a single file of about two
  thousand lines of assembly code.

  The compiler is self-compiling and uses just a few functions from
  the C-library. Alternatively, programs can be compiled on Linux
  without using a C library, by just invoking raw system calls, but
  support for this is currently incomplete (mostly related to
  number<->string conversion).  Code compiled with BONES can be easily
  embedded into programs written in other languages as long as they
  allow calling C functions.

  There are very little debugging facilities. The compiler expects
  correct code and no attempt is made to provide more than the most
  basic error messages. It is recommended to develop and test code
  first in an interactive Scheme implementation and use BONES only for
  generating executables when the code can be assumed to work.

  The development files for BONES are hosted at [bitbucket.org], where
  you will find example code (mostly tests). The compiler itself is
  developed with a currently unreleased Scheme system, but as BONES
  compiles itself, no additional implementation is needed to make
  changes and extend the system.


  [bitbucket.org]: https://bitbucket.org/bunny351/bones

2 Portability 
~~~~~~~~~~~~~~

  To run programs compiled with BONES (and to run the compiler
  itself), an x86_64 system is required, with support for SSE2 and
  SSE3 instructions. Currently the compiler runs under the Linux,
  *BSD, Mac OS X and Windows operating systems.

  BONES has been successfully tested on the following systems:

    OS                                                                                 nasm   C compiler       libc         
   -------------------------------------------------------------------------------+---------+----------------+-------------
    Linux Mint 13 Maya Ubuntu/Linaro 4.6.3-1ubuntu4 Linux 3.2.0-23-generic x86_64   2.09.10   gcc 4.6.3        EGLIBC 2.15  
                                                                                                               musl 1.1.0   
    openSUSE 13.1 (Bottle) Linux 3.15.1-2.g3289da4-default x86_64                   2.09.10   gcc 4.8.1        glibc 2.18   
    Windows 7 Professional                                                                                                  
    Mac OS 10.8.5                                                                   2.11.05   clang-503.0.38                
    OpenBSD 5.5 (GENERIC.MP) #315                                                   2.10.09   gcc 4.2.1                     

3 Requirements 
~~~~~~~~~~~~~~~

  The [NASM] assembler is required on all supported platforms. To link
  the assembled code you will typically need a C development
  environment. 

  On Linux [GCC] is the recommended compiler, but you will only need it
  for linking, and for providing the basic C runtime code (CRT).  A
  subset of the functionality provided by this system can be compiled
  to native code that does not need C runtime support - in this case
  all you need is `ld', the GNU linker.

  On Windows, [Microsoft Visual C] and [MINGW] are supported.

  On Mac OS, [Xcode] should be installed, with `gcc' or `clang' binaries in the `PATH', or another C compiler that is able to link macho64 binaries.


  [NASM]: http://www.nasm.us
  [GCC]: http://gcc.gnu.org
  [Microsoft Visual C]: http://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx
  [MINGW]: http://www.mingw.org
  [Xcode]: https://developer.apple.com/xcode/

4 Installation 
~~~~~~~~~~~~~~~

  BONES is written in Scheme and translates Scheme to x86_64
  assembler for the syntax accepted by NASM, which will
  have to be installed on your system. To link the assembled code you
  will need a linker and C runtime support files, including the C
  library, so a C compiler should be installed as well.

  To build the system, you need the pre-compiled assembly code for
  the `bones' executable, which you can obtain by downloading
  a distribution tarball at this location:

    [http://www.call-with-current-continuation.org/bones/bones.tar.gz]

    or 

    [http://www.call-with-current-continuation.org/bones/bones.zip]

  After downloading, unpack the file using the tar(1) command and 
  assemble and link the appropriate assembler file for the compiler:

  - For Linux:

  tar xfz bones.tar.gz
  cd bones-<date>
  nasm -f elf64 bones-x86_64-linux.s -o bones.o
  gcc bones.o -o bones -lrt


  - For Free/Net/OpenBSD:

  tar xfz bones.tar.gz
  cd bones-<date>
  nasm -f elf64 bones-x86_64-bsd.s -o bones.o
  gcc bones.o -o bones


    At least on OpenBSD, you may need to invoke `gcc' with the
    `-static' and `-nopie' options to generate a working
    executable. Alternatively pass `-feature pic' to bones when
    compiling the Scheme code.

  - For Windows:

    Unzip the archive and enter the following commands in a command
    shell window that has access to the command-line MSVC development
    tools:

  cd bones-<date>
  nasm -f win64 bones-x86_64-windows.s -o bones.obj
  link bones.obj libcmt.lib /out:bones.exe


    If you are using MINGW, linking is done with the following
    command:

  gcc bones.obj -o bones.exe


  - For Mac OS:

  tar xfz bones.tar.gz
  cd bones-<date>
  nasm -f macho64 bones-x86_64-mac.s -o bones.o
  gcc bones.o -o bones


    You may get a linker warning complaining that the object file
    produced by NASM does not have "PIE" enabled. I'm not completely
    sure why this happens and how this warning can be prevented. The
    linked executables run fine as far as I can tell (it may be caused
    by a bug in NASM or the Apple linker, but I'm not sure about it.)
    You can disable this warning by adding the option `-Wl,-no_pie'
    when linking an executable.

  Now you have a compiler, which you can test by entering 

  ./bones


  which should give you some usage information.

  The `bones' executable can be moved anywhere you desire, but assumes
  that some supporting files are in the directory where it is invoked.
  You can use the `-L' command-line option (described below) to set
  the directory where the supporting files are located. The default
  search path contains the following directories:

  - The current directory (".")

  - The value of `BONES_LIBRARY_PATH', if set. Multiple directories
    can be given, when separated by ";" (semicolon) on Windows or ":"
    (colon) on other systems.

  - "/usr/share/bones"

  - "/usr/local/share/bones"

  Additionally, when assembling generated code produced by BONES, you
  will have to pass `-I<INSTALLDIR>/' to the `NASM' invocation, as the
  code contains `%include' directives that refer to the
  assembler-parts of the runtime system. Note that trailing slash - this
  is required for `NASM'.

5 Reporting Bugs and Upgrading 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you find any bugs in BONES (you probably will), please send them
  to the author (see below under "Contact") so that the bug can be
  fixed in future releases. You should also first check the [git(1)]
  development repository at [http://bitbucket.org/bunny351/bones/] -
  perhaps the bug has already been fixed. The `master' branch should
  always be in a usable state, just clone the repository and use the
  contained sources to build a new `bones':

  git clone https://bitbucket.org/bunny351/bones.git
  cd bones
  bones bones.scm -o mynewbones.s


  Consult the `NEWS' file for obtaining information about the latest
  changes.


  [git(1)]: http://www.git-scm.org

6 Usage 
~~~~~~~~

6.1 Compiling Scheme code 
==========================

   Compiling code is straightforward. The `bones' program will compile
   a single Scheme source file given on the command-line and write the
   generated assembly code to stdout. You will have to assemble and
   link the generated code by hand, subprocesses to do this are not
   invoked automatically. The compiler accepts a few command-line
   options which are described below.

   Optionally, the file given to `bones' may be a /program
   specification/, an extended variant of the configuration language
   described in [SRFI-7]. The compiler treats code that consists of a
   single toplevel expression in the form `(program ...)' as a program
   specification. If the source file does not have this form, it is
   treated as if the code where embedded into the following
   specification:

  (program
    (include "base.scm")
    (code <...your code...>))


   See below for a description of the configuration language and the
   default configuration.


   [SRFI-7]: http://srfi.schemers.org/srfi-7

6.2 Assembling and linking Executables 
=======================================

   Once the compiler has produced an assembler file, it has to be
   assembled and linked. Using NASM, this is done like this:

  nasm -f <FORMAT> <FILENAME> -o <OBJECTFILENAME>


   `<FORMAT>' specifies the output format suitable for the platform on
   which the object file should be linked. For Linux and *BSD this is
   `elf64', for Windows `win64' and for Mac OS `macho64'.

   You can optionally add debug-information to have source-level
   access when debugging the generated executable with (say)
   `gdb'. This is done by adding `-g -F <DEBUGFORMAT>' to the `nasm'
   command line, where `<DEBUGFORMAT>' should be the format suitable
   for your particular platform, on Linux `dwarf' seems to work for
   me.

   Linking is done by invoking the appropriate linker for the target
   platform. On Linux, *BSD or Mac OS this is done by the "gcc"
   compiler driver, which also adds the necessary C runtime libraries
   needed:

  gcc <OBJECTFILENAME> -o <PROGRAMFILENAME>


   Note that on Linux you'll need to add `-lrt' to the linker
   invocation.

   On Windows the `link' program from Microsoft Viusal Studio can be
   used:

  link <OBJECTFILENAME> libcmt.lib /out:<PROGRAMFILENAME>


   Alternatively, use the MINGW compiler driver:

  gcc <OBJECTFILENAME> -o <PROGRAMFILENAME>


6.3 Command-line Options 
=========================

   The compiler accepts the following options in any order.

   `-case-insensitive': Compile source code in case-insensitive
       mode.  The default is to be case-sensitive.

   `-comment': Emit source-code forms as comments in generated
       assembly code.

   `-dump-features': Dump the set of enabled features after any
       program specification has been completely parsed and stop.

   `-dump-unused': Dump unused global variables, including library
       definitions and stop.

   `-expand': Dump source code to stdout after syntax-expansion and
       stop.

   `-feature FEATURE': Define `FEATURE', for use in
       program-specifications or `cond-expand' clauses.

   `-L LIBRARYPATH': Tells the compiler where to look for files to
       include directly or indirectly from program
       specifications. This option may be given multiple times where
       every occurence of a library-path is prepended to the search
       path.

   `-nostdlib': Do not wrap the source code into a default program
       specification as described above. This is mainly useful when
       you have pre-expanded code that was earlier produced by using
       `-expand'.

   `-o FILENAME': Write generated code to `FILENAME' instead of
       stdout.

   `-v': Print the compiler version and exit.

   `-verbose': Print currently executing compilation phases and
       some optimzation statistics to stderr.

6.4 Configuration Macros for Generated Assembly Code 
=====================================================

  `ENABLE_WRITE_BARRIER': When mutating data-structures that are
      not stored in the heap, the assigned value may be lost for
      tracing during garbage collection, leading to errors that are
      very hard to detect. When enabling this macro, the program
      aborts in such cases with an error message.

  `ENABLE_GC_LOGGING': Write some informational output to stderr
      every time a garbage collection is triggered.

  `PREFIX': Defines a custom entry-point prefix for embedding. By
      passing `-DPREFIX=my' to NASM, the Scheme toplevel can be run by
      calling `my_bones'.

  `TOTAL_HEAP_SIZE': Total size of the heap. Half of this space can be
      in use at any time. Defaults to 100MB.

6.5 Configurations 
===================

   /Configurations/ are used for the composition of programs,
   depending on source files and /features/ that are used to perform
   more fine-grained control over the characteristics of the compiled
   source and generated assembly code.

   The compiler will compile normal Scheme code with a default
   configuration, mentioned above, that includes further configuration
   options from the file `base.scm', selecting the default target and
   the primitives available, that is, most R5RS standard procedures
   and some non-standard extensions. To exercise more control, one can
   use the `program' form to specify source-files and enable or
   disable code to be included in the program that will be compiled.

   If you use your own `program' form, make sure to add a clause
   including the base components in `base.scm', as it provides some
   intrinsic forms that are necessary for correct execution of the
   standard procedures. Study `base.scm' for more information about
   this.

   Default features defined by `base.scm':

     Feature               Meaning                                                           
    ---------------------+------------------------------------------------------------------
     x86_64                Architecture identifier                                           
     ieee754               Floating point format used                                        
     linux                 Operating system                                                  
     bsd                   Operating system                                                  
     windows               Operating system                                                  
     mac                   Operating system                                                  
     lp64                  64-bit data model (Linux, *BSD + Mac)                             
     llp64                 64-bit data model (Windows)                                       
     file-ports            Primitives operating on file-ports are available                  
     file-system           Primitives operating on file-system entitities are available      
     process-environment   Access to the process-environment and subprocesses are available  
     time                  Access to the system time is available                            
     jiffy-clock           Access to the internal real-time clock is available               
     srfi-0                Specify availability of SRFI functionality                        
     srfi-6                                                                                  
     srfi-7                                                                                  
     srfi-16                                                                                 
     srfi-46                                                                                 

   The compiler option `-dump-features' can be used to show the
   features that are by default available for a given configuration.

   The following features are by default available to enable language
   extensions or target-specific compilation modes:

     Feature    Meaning                                                                                            
    ----------+---------------------------------------------------------------------------------------------------
     check      Insert low-level type- and limit-checks into the generated code                                    
     embedded   Generate code that can be embedded into an application                                             
     nolibc     Generate code that does not require linking with the C runtime library                             
     pic        Generate position-independent code, always enabled on Windows and Mac, optional on *BSD and Linux  

7 Language Description 
~~~~~~~~~~~~~~~~~~~~~~~

7.1 Program Specifications 
===========================

7.1.1 Syntax 
-------------

    Program specifications follow mostly the format as defined in the
     [SRFI-7 specification], with the following extensions:

    `(cond-expand ...)': Can be used interchangebly with
        `feature-cond'.

    `(error STRING)': Report error and abort compilation.

    `(include FILENAME ...)': Includes more program-specification
        clauses from `FILENAME'.

    `(provide FEATURE ...)': Adds additional features to the set
        of currently active features visible by `cond-expand' and
        `feature-cond' clauses. `FEATURE' should be a symbol.


        [SRFI-7 specification]: http://srfi.schemers.org/srfi-7/srfi-7.html

7.1.2 Builtin Features 
-----------------------

    Features available by default are: `bones', `srfi-0', `srfi-7',
    `srfi-16', `srfi-46'.

7.2 Deviations from R5RS 
=========================

7.2.1 Section 1.3.2 
--------------------

    Argument types to primitive procedures are not checked. The only
    detected errors are errors related to port- and file-system
    operations, when the heap is exhausted, or explicit calls to the
    `error' procedure.

7.2.2 Section 2 
----------------

    BONES is by default case-sensitive, both at compile-time and at
    run-time.

7.2.3 Section 3.4 
------------------

    Literal constants are immutable and destructively modifying such a
    constant will have an undefined effect.

7.2.4 Section 4.2.1 
--------------------

    Vectors are self-evaluating, as in R7RS.

7.2.5 Section 6 
----------------

    Built-in procedures may be redefined (using `define' or
    `define-syntax'), but it is undefined whether other standard
    procedures will still work as expected.

7.2.6 Section 6.1 
------------------

    `eqv?' performs /structural/ comparison, which means it compares
    the contents of its two arguments, in case they are of equal
    type. That means it will will return `#t' if both arguments have
    the same type and identical contents, even if the arguments are
    not numbers or characters.

    `eq?' will not necessarily return the expected result when
    comparing primitive procedures. Many of these will be compiled
    in-line using /identifier macros/, a feature of the
    syntax-expander used, which means that for example the two
    occurrences of `zero?' below will refer to two different
    occurrences of the code for that primitive:

  (eq? zero? zero?)  ==> #f


7.2.7 Section 6.2.1 
--------------------

    Only exact signed integers with 63 bits of magnitude (/fixnums/)
    and IEEE-754 extended precision floating point numbers (/flonums/)
    are supported. Numerical overflow is not detected, so arithmetic
    operations on fixnums will silently wrap on overflow.

7.2.8 Section 6.2.3 
--------------------

    `/' always returns an inexact number. This is not a violation
    of R5RS, but should be kept in mind.

7.2.9 Section 6.2.4 
--------------------
    
    The `#' character is not allowed inside inexact numeric constants.

7.2.10 Section 6.2.5 
---------------------

    `complex?' is not available.

    `max' and `min' return the maximal or minimal argument unchanged,
    without coercing between types in the case of arguments of mixes
    types.

    `numerator', `denominator' and `rationalize' are not available.

    `make-rectangular', `make-polar', `real-part', `imag-part',
    `magnitude' and `angle' are not available.

    The behaviour of the transcendental built-in procedures when given
    one of the special IEEE-754 numbers like /nan/ and /infinity/ is
    undefined.

7.2.11 Section 6.2.6 
---------------------

    `string->number' does not allow radix or exactness prefixes inside
    the given string argument, nor is the `#' character in inexact
    numeric strings supported.

    `string->number' and `number->string' only support the bases 8, 10
    and 16. Any other base is ignored and the conversion will be done
    using base 10.

7.2.12 Section 6.4 
-------------------

    `map' and `foreach' will terminate once any argument list ends.

    `force', when given a argument that is not a promise will return
    that argument unchanged.

7.2.13 Section 6.5 
-------------------

    `eval', `scheme-report-environment', `null-environment' and
    `interaction-environment' are not avaialable.

7.2.14 Section 6.6.1 
---------------------

    `close-input-port' and `close-output-port' will signal an error
    when the port has already been closed.

7.2.15 Section 6.6.2 
---------------------

    `char-ready?' is not available.

7.2.16 Section 6.6.4 
---------------------

    `load', `transcript-on' and `transcript-off' are not available.

7.3 Extensions to R5RS 
=======================

7.3.1 Section 2.1 
------------------

    The special notation `|...|' can be used to enter symbols
    containing otherwise illegal identifiers, for example `|x y|'
    would designate the symbol containing the characters `#\x?',
    `#\space' and `#\y'. `write' will print such symbols using this
    notation. The `|' (vertical bar) character itself can be escaped
    using `\' (backslash).

7.3.2 Section 2.2 
------------------

    Expression-comments of the form `#;' are supported as in R7RS.

    The character-sequence `#!' is treated like `;' and ignores the
    rest of the line.

7.3.3 Section 4.2.2 
--------------------

    BONES supports the R7RS binding form `letrec*'.

7.3.4 Section 4.3.1 
--------------------

    BONES uses Al Petrofsky's "alexpander", which provides several
    enhancements. Among these, /transformer/ expressions in
    `let-syntax' and `letrec-syntax' forms are extended to allow
    arbitrary expressions.

    The core generalization in this system is that both the
    transformer-specification and the expression in operator
    position of another expression may be any type of expression or
    syntax.  The four forms of syntax allowed are: a transformer (as
    allowed in the transformer-spec position in R5RS), a keyword (as
    allowed in the operator position in R5RS), a macro use that
    expands into a syntax, and a macro block (`let-syntax' or
    `letrec-syntax') whose body is a syntax.

    Some examples:

  ;; a macro with a local macro
  (let-syntax ((foo (let-syntax ((bar (syntax-rules () ((bar x) (- x)))))
                        (syntax-rules () ((foo) (bar 2))))))
      (foo))
  => -2
  
  ;; an anonymous let transformer, used directly in a macro call.
  ((syntax-rules ()
       ((_ ((var init) ...) . body)
        ((lambda (var ...) . body) init ...)))
     ((x 1) (y 2))
     (+ x y))
  => 3
  
  ;; a keyword used to initialize a keyword
  (let-syntax ((q quote)) (q x)) => x
  
  ;; Binding a keyword to an expression (which could also be thought
  ;; of as creating a macro that is called without arguments).
  (let ((n 0))
      (let-syntax ((x (set! n (+ n 1))))
        (begin x x x n)))
  => 3
  
  (let-syntax ((x append)) ((x x))) => ()


    Top-level macro blocks.

    At top level, if a macro block (a `let-syntax' or `letrec-syntax'
    form) has only one body element, that element need not be an
    expression (as would be required in R5RS).  Instead, it may be
    anything allowed at top level: an expression, a definition, a
    begin sequence of top-level forms, or another macro block
    containing a top-level form.

  (let-syntax ((- quote))
      (define x (- 1)))
  (list x (- 1)) => (1 -1)


    Note that, unlike the similar extension in Chez scheme 6.0, this is
    still R5RS-compatible, because we only treat definitions within the
    last body element as top-level definitions (and R5RS does not allow
    internal definitions within a body's last element, even if it is a
    `begin' form):

  (begin
      (define x 1)
      (let-syntax ()
        (define x 2)
        'blah)
      x)
  => 1, in R5RS and alexpander, but 2 in Chez scheme
  
  (begin
      (define x 1)
      (let-syntax ()
        (begin (define x 2)
               'blah))
      x)
  => 2, in alexpander and in Chez scheme, but an error in R5RS.


7.3.5 Section 4.3.2 
--------------------

    "Alexpander" fully supports [SRFI-46], including tail-patterns and
    user-selectable ellipses.


    [SRFI-46]: http://srfi.schemrs.org/srfi-46

7.3.6 Section 5.2.2 
--------------------

    Internal definitions expand into `letrec*' forms, as in R7RS.

    A definition of the form `(define <expression>)' causes the
    expression to be evaluated at the conclusion of any enclosing set
    of internal definitons.  That is, at top level, `(define
    <expression>)' is equivalent to just plain `<expression>'.  As for
    internal definitions, the following are equivalent:

  (let ()
      (define v1 <init1>)
      (define <expr1>)
      (define <expr2>)
      (define v2 <init2>)
      (define <expr3>)
      (begin
        <expr4>
        <expr5>))
  
  (let ()
      (define v1 <init1>)
      (define v2 <init2>)
      (begin
        <expr1>
        <expr2>
        <expr3>
        <expr4>
        <expr5>))


7.3.7 Section 5.3 
------------------

    The /transformer spec/ of a `define-syntax' form may be any
    expression.

7.3.8 Section 6.3.5 
--------------------

    The third argument to `substring' is optional and defaults to the
    length of the argument string.

7.3.9 Section 6.6.1 
--------------------

    `current-input-port' and `current-output-port' accept a single
    argument, a /port/, which changes the current default input- and
    output port.

7.3.10 Syntax Extensions 
-------------------------

    The following non-standard syntax is available:

    `(assert EXP [MSG ARGUMENT ...])': Signals an error if `EXP'
        evaluates to `#f', passing `MSG' and `ARGUMENTS ...' to
        `error' in that case.

    `(begin0 EXP1 EXP ...)': Evaluates the expressions and returns the
        result value(s) of the first expression.

    `(cond-expand CLAUSE ...)': The [SRFI-0] expansion-time conditional.

    `(cut EXP ...)': Notation for specializing parameters without
        currying - see [SRFI-26] for more information.

    `(define-inline (NAME ARGUMENTS ...) BODY ...)': Defines a
        procedure that should be in-lined at the call-site. This is
        implemented by defining an /identifier-macro/ for `NAME',
        which the syntax-expander will replace with a `lambda' form
        containing the procedure body , when used in the
        operator-position of a procedure-call. Any use of an inline
        procedure must appear textually after its definition.

    `(define-syntax-rule (NAME ARGUMENTS ...) EXPRESSION)': A more
        concise way of defining single-rule syntax.

    `(define-values (VAR ...) EXP)': Evaluates `EXP' and binds `VAR' ...
        to the values returned by it.

    `(fluid-let ((VAR EXP) ...) BODY ...)': Syntax for dynamic scoping,
        see [SRFI-15] for more information.

    `(handle-exceptions VAR HANDLE BODY ...)': Binds
        `current-exception-handler' temporarily during exection of
        `BODY ...' and evaluates the expression `HANDLE' if the body
        raises an exception. While evaluating `HANDLE', `VAR' is bound
        to the exception object and the next outer exception handler
        is active.

    `(let-optionals VAR1 ((VAR EXP) ... [RVAR]) BODY ...)': Destructures 
        the list in `VAR1', binding the optional
        arguments `VAR' ... with defaults taken from `EXP' ... Any
        remaining list elements can be bound in the optional final
        variable `RVAR'.

    `(letrec* ((VAR VAL) ...) BODY ...)': Binds `VAR' ... to the
        results of evaluating `VAL' ... and evaluates `BODY', with the
        variables being in scope during the evaluation of their
        values. Mostly equivalent to `letrec' but binds the variables
        sequentially.

    `(when EXP BODY ...)': Equivalent to `(if EXP (begin BODY ...))'.

    `(unless EXP BODY ...)': Equivalent to `(if (not EXP) (begin BODY ...))'.


    [SRFI-0]: http://srfi.schemers.org/srfi-0/
    [SRFI-26]: http://srfi.schemers.org/srfi-26/
    [SRFI-15]: http://srfi.schemers.org/srfi-15/

7.3.11 Additional procedures 
-----------------------------

* Fixnum Arithmetic 
  
  `(arithmetic-shift N M)': Shifts `N' `M' bits to the left, if
      `M' is positive, or to the right, if `M' is negative.
  
  `(bitwise-and N M)': Binary AND.
  
  `(bitwise-ior N M)': Binary inclusive OR.
  
  `(bitwise-xor N M)': Binary exclusive OR.
  
  `(bitwise-not N)': Binary inverse.
  
* Input/Output 
  
  `(case-sensitive [BOOL])': If set to true, `read' will
      preserve case when reading symbols (this is the default).
   
  `(current-error-port [PORT])': Returns or sets the port where
      error-messages are written to.
  
  `(eof-object)': Returns the /end-of-file/ object.
  
  `(get-output-string PORT)': Returns the currently accumulated
      output from the string-port `PORT'.
  
  `(open-append-output-file STRING)': Opens the file `STRING' in
      /append/ mode, and returns an output port.
  
  `(open-input-string STRING)': Returns an input-port on
      `STRING' from which data can be read.
  
  `(open-output-string)': Returns an output-port accumulating
      all output written to into a string.
  
  `(port? OBJECT)': Returns `#t' if `OBJECT' is an input- or
      output-port or `#f' otherwise.
  
  `(print OBJECT ...)': Writes the arguments to the current
      output port (using `display') followed by a newline and
      returns an unspecified value.
  
  `(read-string COUNT [PORT])': Reads `COUNT' or less characters
      from `PORT' and returns a string. If no input is available
      from `PORT', the /end-of-file/ object is returned. `PORT'
      defaults to the value of `(current-input-port)'.
  
  `(with-output-to-string STRING THUNK)': Calls the
      zero-argument procedure `THUNK' with the current output port
      temporarily bound to a string output port on `STRING' and
      returns the accumulated output from that port as a string.
  
  `(with-input-from-string STRING THUNK)': Calls the
      zero-argument procedure `THUNK' with the current input port
      temporarily bound to a string input port on `STRING' and
      returns the result.
  
  `(write-string STRING [PORT])': Writes the characters in `STRING' to `PORT',
      which defaults to the current output port.
  
  On Windows, all files are opened in binary mode, so no
  line-terminator translations take place.
  
* Process Environment 
  
  `(command-line)': Returns the name of the running program and
      all arguments given on the command-line as a list of
      strings. When the program is running in /embedded/ mode, then
      this procedure returns the empty list.
  
  `(current-process-id)': Returns the /ID/ of the current
      process as an exact integer.
  
  `(get-environment-variable STRING)': Returns the value of the
      /environment variable/ named by `STRING' as a string, or `#f'
      if such an environment variable is not defined.
  
  `(system STRING)': Executes `STRING' as a shell-command to be
      executed and returns the exit-status of the invoked command as
      an exact integer.
  
* File System 
  
  `(current-directory [STRING])': When called without arguments,
      this procedure returns the name of the current working
      directory.  When given an argument, the current working
      directory is changed to `STRING'.
  
  `(delete-file STRING)': Deletes the file designated by `STRING'.
  
  `(file-exists? STRING)': Returns `STRING' if the file
      designated by `STRING' exists or `#f' otherwise.
  
* Time 
  
  `(current-jiffy)': Returns the time since process startup, in
      some internal unit, as an exact integer. Use
      `jiffies-per-second' to obtain the number of internal time
      units per second.
  
  `(current-second)': Returns the current time, in seconds since
      the start of the /epoch/.
  
  `(jiffies-per-second)': Returns the number of internal time
      units per second as an exact integer.
  
* Control Flow 
  
  `(call/cc PROC)': Identical to `call-with-current-continuation'.
  
  `(emergency-exit [CODE])': Terminates the current program
      immediately.  Still pending `dynamic-wind' thunks are not
      invoked. `CODE' is the exit code of the process and defaults
      to 0. Invokes the `_exit(2)' library/system call.
  
  `(exit [CODE])': Terminates the process with the process
     status `CODE' (or 0, if not given). Pending `dynamic-wind'
     "after" thunks will be executed. Invokes the `exit(3)'
     library/system call.
  
  `(return-to-host RESULT)': Suspends the currently running
      program and returns `RESULT' to the caller. This procedure is
      only available when the `embedded' feature is defined.
  
* Error-handling and Exceptions 
  
  `(current-exception-handler [PROCEDURE])': Sets the current
      /exception handler/, a procedure to be called when an error is
      signalled. The procedure will be called with an object that
      was used as an argument to `raise' or with an
      /error-object/. The default exception handler writes the
      exception object to the port that is the current value of
      `current-error-port' and performs an immediate exit (as with
      `emergency-exit'). In embedded mode, the default exception
      handler invokes `return-to-host' with the exception object as
      argument.  `current-exception-handler' is a /parameter/.
  
  `(error [LOCATION] [MESSAGE] ARGUMENTS ...)': Creates an
      error-object from `LOCATION', `MESSAGE' and `ARGUMENTS' and
      invokes the current exception handler. `LOCATION' can be used
      to mark where the error occurred and should be a symbol.
  
  `(error-object? X)': Returns true if `X' is an error-object or
      false otherwise.
  
  `(error-object-irritants ERROROBJECT)': Returns the
      error-arguments stored in `ERROROBJECT'.
  
  `(error-object-location ERROROBJECT)': Returns the location
      stored in an error object or `#f' if no location is available.
  
  `(error-object-message ERROROBJECT)': Returns the
      error-message stored in `ERROROBJECT'.
  
  `(file-error? OBJECT)': Returns true if `OBJECT' is an error
      object raised by an invalid file-operation or false otherwise.
  
  `(raise OBJECT)': Raises an exception by calling the current
      exception handler with `OBJECT'.
  
  `(read-error? OBJECT)': Returns true if `OBJECT' is an error
      object raised by encountering invalid syntax in an invocation
      of `read' or false otherwise.
  
* Interrupts 
  
  /Interrupts/ are exceptions triggered by POSIX signals like
  `SIGINT'. Note that checking for pending signals is only done in
  certain situations: after a garbage collection has taken place, when
  a I/O operation was performed or when explicitly tested by calling
  `(check-interrupts)'. 
  
  The interpreter (`si') inserts interrupt checks at the start of
  every user-visible `lambda' form, so no explicit checks are
  necessary in interpreted code.
  
  On Windows, `SIGINT' can not be caught, due to limitations of its
  signal handling facilities.
  
  `(catch-interrupt [NUMBER [MODE]])': Establishes a handler for
       the signal with the given number (which defaults to the value
       for `SIGINT', i.e. user interrupts). If the signal is
       subsequently triggered, an `interrupt' error-object will be
       raised as an exception. If `MODE' is given, then it should be
       a boolean indicating whether the signal should be ignored
       (`#f') or the default action should take place (`#t').  Using
       this procedure is only needed in long-running computations
       that do little allocation and no I/O operations, when you
       quickly want to be informed about pending interrupts.
  
  `(check-interrupts)': Explicitly checks for pending interrupts
       triggered by signals.
  
  `(interrupt? OBJECT)': Returns true if `OBJECT' is an error
      object raised by a signal (e.g. a user-interrupt triggered by
      pressing Ctrl-C) or false otherwise.
  
  `(interrupt-number ERROROBJECT)': Returns the number of the
       signal that caused the interrupt designated by `ERROROBJECT'.
  
* Numeric Operations 
  
  `(finite? NUMBER)': Returns `#t' if `NUMBER' is not positive
      or negative /infinity/.
  
  `(nan? NUMBER)': Returns `#t' if `NUMBER' is the IEEE754 /NaN/
      object or `#f' otherwise.
  
* Parameters 
  
  `(make-parameter VALUE [GUARD])': Returns a /parameter/
      procedure that accepts a single optional argument. If that
      procedure is called without arguments, it returns `VALUE'. If
      it is called with an argument, the current value of the
      parameter is changed to this argument and subsequent
      0-argument invocations if the procedure will return the new
      value. If `GUARD' is given, then it should be a procedure of
      one argument that checks or converts any values given to the
      parameter procedure.
  
  `(parameterize ((PARAM VALUE) ...) BODY ...)': Dynamically
      binds parameters to the given values and executes `BODY'
      ... Note that the `PARAM' arguments are evaluated. After
      `BODY' ... is executed, the parameters are restored to their
      old values, with any guard-procedures temporarily disabled.
  

7.3.12 Bytevectors 
-------------------

    `(bytevector NUM ...)': Creates a new bytevector with the
        initial elements `NUM, ...'.

    `(bytevector? X)': Returns true, if `X' is a bytevector or
        false otherwise.

    `(bytevector-copy! TO AT FROM [START END])': Copies the
        elements of the bytevector `FROM', between `START' and `END'
        into the bytevector `TO', at position `AT'. `START' defaults
        to zero, `END' defaults to `(bytevector-length FROM)'.

    `(bytevector-length BYTEVECTOR)': Returns the length of
        `BYTEVECTOR' as an exact integer.

    `(bytevector-u8-ref BYTEVECTOR INDEX)': Returns the element of
        `BYTEVECTOR' at the given index.

    `(bytevector-u8-set! BYTEVECTOR INDEX N)': Modifies the
        element of `BYTEVECTOR' at the given index to contain `N'.

    `(make-bytevector SIZE [INIT])': Creates a noew bytevector,
        optionally filling it with `INIT'. If `INIT' is not given, the
        contents of the bytevector are unspecified.

    The reader accepts bytevector literals in the form `#u8(...)',
    `display' and `write' output bytevectors in this form as well.

    Bytevector-literals are self-evaluating and need not be quoted.

7.3.13 Miscellaneous 
---------------------

    `(free)': Returns the number of currently available unused
        bytes in the heap as an exact integer.

    `(make-disjoint-type [NAME])': Creates a new record type T and
        returns three values: a constructor procedure that takes a
        data object and returns a new record of type T, a predicate
        that returns true if given a record of type T and an accessor
        procedure receiving a record and returning the associated data
        object. `NAME' should be a symbol and is used for printing
        record instances.

    `(reclaim)': Reclaims unused memory in the heap and returns an
        undefined value. Garbage collection is done automatically, but
        in certain cases, for example if time-critical code is to be
        executed, it may be useful to delay the next garbage
        collection as much as possible.

    `(void ARGUMENT ...)': Ignores its arguments and returns an
        undefined result.

7.4 Additional Library Code 
============================

    Some library files are available with additional procedures and
    syntactic extensions. Use these with the `files' program
    specification clause:

    `match.scm': Alex Shinn's pattern matching facility, also
        available at [http://synthcode.com/scheme/match.scm]. This file
        can be used with the `files' program-specification form.

    `megalet.scm': The [SRFI-71] reference implementation, extending
        `let' for convenient binding of multiple values. This also
        adds "curried" `define' syntax for toplevel definitions and
        `let/letrec'.

    `records.scm': A record-definition-facility compatible to
        [SRFI-9].

    The following library files are `include' files, i.e.  they are
    intended to by used by adding

  (include "...FILENAME...")


    to a program-specification:

    `eval.scm': An fast evaluator for Scheme expressions (see below).

    `fastmath.scm': Provides numeric operations, specific for
        fixnums (exact integers) and floating-point numbers.

    `sort.scm': An implementation of mergesort.

    `support.scm': This is a collection of random utility
        procedures and syntax, used mainly in the compiler, but also
        useful for user code.


        [SRFI-71]: http://srfi.schemers.org/srfi-71/
        [SRFI-9]: http://srfi.schemers.org/srfi-9/

7.5 The Evaluator 
==================

   The library file `eval.scm' contains an evaluator for Scheme
   expressions, that may be useful for adding an interactive
   read-eval-print loop (REPL) to compiled code.  It has definitions
   for all standard- and non-standard syntax and primitives available
   in BONES.

   Evaluated code does not have access to the global variables of the
   containing compiled program, but can be extended to do so, for 
   example by adding

  (eval `(define hello ',(lambda () (print "hello!"))))


   (Quoted literals may be of any data type.)

   `eval.scm' provides the following procedures:

   `(eval FORM)': Evaluates `FORM' and returns its result(s).

   `(eval-trace [FLAG])': Parameter that enables maintaining a
       "call-trace", which can be printed in an error
       situation. Defaults to `#f'.

   `(load FILENAME [EVALPROC])': Reads expressions from the file
       given by `FILENAME' (a string) and evaluates them. If
       `EVALPROC' is given then each expression is passed to this
       procedure instead of `eval'. If `FILENAME' starts with "~"
       (tilde), then the remaining part will be appended to the value
       of the environment-variable `HOME'.

   `(load-verbose FILENAME [EVALPROC])': Like `load', but each
       expression is printed to the current output port before it is
       evaluated.

   `(oblist)': Returns an association list containing all global
       variables currently defined in the evaluator where each element
       of the list is of the form `(VARIABLENAME . VALUE)'.

   `(quit [RESULT])': Exit the currently running read-eval-print loop
       and return `RESULT', which defaults to an unspecified value.

   `(repl)': Start an interactive read-eval-print loop until EOF
       is read from the current input-port. Returns the value given to
       `quit'.

   All these procedures are also available in interpreted code.

   Note that error-checking is still minimal, even for evaluated code.

   The evaluator is also available as a standalone interpreter, in the
   file `si.scm', ("Scheme interpreter") which can be compiled and
   linked like any other Scheme program. When invoked without
   arguments, `si' will start a REPL, otherwise it will try to load
   and evaluate the file given in the first command-line argument and
   exit. See the source-code for `si' for more information about using
   and customizing the interpreter.

8 Compiler Details 
~~~~~~~~~~~~~~~~~~~

8.1 Compilation Strategy 
=========================

   The compiler is based on /Continuation Passing Style/ and does a
   straightforward translation of the CPS-transformed source code into
   x86_64 assembly language. The compiler is deliberately simple in
   its design to be easy to understand and modify, so very few
   optimizations are done.

   Procedures never return: an implicitly passed /continuation/
   argument holds a closure that is to be called with the result(s) of
   the current procedure, and every call is a tail-call, and can be
   implemented via a direct jump.

   /Closure conversion/ adds another implicit argument, the current
   closure. This object contains a pointer to the actual code of a
   procedure plus the values of any /free variables/, or, in the case
   of free variables that are assigned, /cells/ holding the
   value. These cells can be shared between different closures that
   refer to the same free variable which ensures that assignments by
   one closure are seen in the other.

   Finally code is generated, currently in NASM assembler syntax.
   The code includes one or more support libraries that provide core
   runtime routines and the garbage collector.

   The compiler uses /flat/ closures. Combined with CPS conversion,
   the generated code is /safe for space complexity/: unused data
   (garbage) is retained for a minimal duration of time, usually until
   the next CPS-procedure call is done. This means that data can be
   released very early, reducing memory pressure.

8.2 Compiler Stages 
====================

   The compiler performs the following passes over the source code:

   1. The source program is read into memory and the `program' form
      of the configuration language is expanded, if it exists.
   
   2. Macros are expanded resulting in code free of any derived
      syntactic forms.

   3. The code is /canonicalized/, which means that some superficial
      simplifications are performed. Additional certain special forms
      are converted into simpler internal forms. Variables are
      consistently renamed (a transformation called /alpha
      conversion/), to that every variable is uniquely named.

   4. The program is CPS-transformed.

   5. /Copy-propagation/ (replacement of known variables with their
      values) is done. This pass also detects variables that are known
      to be bound to distinct procedures.

   6. Next, unused local variables are identified and marked. A later
      pass will remove these variables if they are bound to
      expressions that are free of side-effects.

   7. The program is /closure converted/, that is, closure-allocations
      are made explicit.

   8. Assembly code is generated from the results of the previous
      passes.

8.3 Register Usage 
===================

   The machine registers are used in the following manner:

     Register name                          Usage                                 
    --------------------------------------+--------------------------------------
     rax, r11, r15                          Temporary registers                   
     rcx, rdx, rsi, rdi, r8, r9, r10, r12   Argument registers                    
     rbx                                    Holds currently executing closure     
     rbp                                    Allocation-pointer                    
     r13                                    Allocation-limit                      
     r14                                    Contains the internal value for "#f"  

   Since CPS-transformed procedures never return, registers need not
   be saved when a procedure call is made. In every procedure-call the
   registers `rbx', `rcx', ..., `r12' hold the actual arguments,
   including the implicit arguments introduced by the CPS- and
   closure-conversion passes: the current continuation (`rcx') and the
   current closure (`rbx'), respectively. Local variables (as created
   by `let', `letrec' or `letrec*') are also put into the remaining
   argument registers as long as registers are still available.

   `rax', `r11' and `r15' are used throughout compiled code and the
   runtime-system as temporaries.

   `rbp' holds the /allocation pointer/, a pointer to the next
   available space in the /heap/. `r13' holds the heap-limit and once
   the allocation pointer is equal or higher than the limit, a garbage
   collection will be triggered on the next procedure call.

   `r14' holds the value of the `#f' (false) constant, to speed up
   boolean tests.

   The XMM registers `xmm0' to `xmm2' are used throughout the runtime
   system, but can be freely changed by user code.

8.4 Optimizations Performed 
============================

   As already stated, not many optimizations are performed. Currently
   these are:

   During the canonicalization phase `lambda' or `case-lambda'
   expressions in the operator-position of a procedure-call are expanded
   in-line so that

  ((lambda (VAR ...) BODY ...) ARGUMENTS ...)


   is transformed into

  (let ((VAR ARGUMENT) ...) BODY ...)


   Unused variables are removed, and bound or assigned expressions
   that have no side-effects will also be eliminated. Unused local
   variables will not use up an argument register, regardless of
   whether the initialization expression has effects or not.

   Some effort is made to reduce redundant saves of intermediate
   values when translating the arguments for a procedure-call, as
   target registers for call-arguments may conflict with the use of
   local variables during the call set-up.

   Finally some simple optimizations are done, like the elimination of
   moves between identical registers, or using conditional moves
   instead of conditional jumps.

8.5 Performance 
================

   BONES should deliver reasonably good execution times. Only very
   little error checking is done and machine-registers are utilized as
   much as possibly, due to the CPS-nature of the compiled code. Also
   due to CPS, continuation-closures are heap-allocated which means
   that even during normal execution garbage will be accumulated and
   thus needs to be reclaimed. Allocation is done inline for small
   blocks of memory and the garbage collector is very efficient, with
   very short collection times, if you take care to keep your datasets
   small.

   Numerically intensive code may perform suboptimal - floating-point
   operations are "boxed", and thus require frequent allocating of
   inexact number objects.

   The generated code is relatively compact, and contains no
   dependencies besides the C runtime library, and even that can be
   removed, if not all functionality is required. Startup times should
   be excellent, as no complex initialization is needed for the
   runtime system and no memory is allocated dynamically.  Finally,
   continuations are very efficient and induce very little or no
   performance penalty, regardless how heavily they are used, because
   CPS makes continuations "explicit" and an inherent part of normal
   execution.

   Still BONES is very simple and will be no match for programs coded
   in C or high-performance Scheme systems. That is the price to be
   payed for a simple system.

   Note that I/O is fully unbuffered, the `read(2)' and `write(2)'
   library function or system calls are directly invoked for every
   input or output operation. If you have a lot of operations on
   ports, consider using intermediate string-ports or strings for
   accumulating data before reading or writing it.

   Some tips for obtaining fast code:

   - Use `define-inline' or syntax-definitions for small procedures
     where the call-overhead might be higher than the cost of
     executing the procedure.

   - Dispatching among the cases of a `case-lambda' is very efficient
     and is preferrable to using rest-argument lists and destructuring
     them manually.

   - If you are doing a lot of numerical calculations, consider using
     `fastmath.scm'.

   - There are a number of low-level "intrinsic" procedures, mostly
     intended for internal use and often implemented as macros or
     inline-procedures. Check out the sources of the runtime library
     for more information.

   - Try to reduce the amount of live-data to speed up garbage
     collection.

8.6 Hacking and Extending the Compiler 
=======================================

   The compiler has a simple structure and basically consists of
   the following files:

   `alexpand.scm': The syntax-expander. It takes a single
       unexpanded toplevel expression and expands all macros in a
       single pass.

   `bones.scm': A program-specification file referencing all
       modules of the compiler.

   `cc.scm': The closure-conversion pass.

   `cp.scm': Implements simple constant- and copy-propagation.
       This pass also indentifies some direct calls to known
       procedures.

   `cmplr.scm': The core compiler, that runs the different stages
       and control the translation of the simplified program into
       assembly code.

   `cps.scm': The CPS-transformation pass.

   `main.scm': The /driver/, parsing command-line arguments and
       invoking the translation process.

   `megalet.scm', `match.scm': Convenience macros with more
       concise binding forms for multiple values and for pattern
       matching.

   `pp.scm': The pretty-printer, used for dumping source code, for
       example as the result of the `-expand' option.

   `program.scm': Parses program specifications, returning a
       single toplevel-expression.

   `mangle.scm': Does name-mangling of string-constants and
       variable-names.

   `ra.scm': Register assignment. This is used for reducing spills
       when setting up argument registers for procedure calls,
       `$inline' and `$allocate' forms.

   `source.scm': Some operations on source expressions.

   `support.scm': Miscellaneous non-standard helper procedures for
       all sorts of things.

   `tsort.scm': An implementation of topological sort, used in
       `ra.scm'.

   `uv.scm': Contains code to detect unused local and global
       variables by recursively walking the program and collecting 
       usage information.

   `version.scm': Contains a definition for the current compiler
       version.

   `x86_64.scm': The /backend/ for generating x86_64 code. This
       file is the only part of the compiler that is architecture
       specific.

   Building the compiler is straightforward:

  bones bones.scm -o bones.s


   Building the Windows version requires enabling the `windows' feature:

  bones bones.scm -o bones.s -feature windows


   Similar for Mac OS:

  bones bones.scm -o bones.s -feature mac


   The compiler does not use an abstract syntax tree as an internal
   code representation - /S-expressions/ representing code are used
   throughout all stages of the compilation process, with added
   internal special forms to reflect information that can not be
   expression with standard Scheme forms. Additional forms may be
   defined, but care must be taken to extend all passes that may see
   these new forms with appropriate code to handle these extensions.

   Internal special forms are named with a `$' (dollar) prefix. Note
   that the syntax-expander doesn't know about these, so if you add
   special forms that may appear in source-programs, then parts of the
   form that are to be unevaluated need to be quoted or wrapped inside
   a `lambda' forms. For an example of this, check out the
   implementation of `case-lambda' in `r5rs.scm'.

   The compiler passes done are program-expansions, macro-expansion,
   canonicalization, unused-variable detection, CPS-conversion,
   closure-conversion and translation to assembly code. The
   canonicalization phase also performs alpha-conversion, so all local
   variables are uniquely named.

   Since the compiler is used to compile itself, the usual
   bootstrap-issues arise: it must always be possible to compile the
   compiler with the previous version of it. This is kind of obvious,
   but it is easy to code oneself into a corner when this is not done
   with care.

   The compiler assumes a closed-world model of the source program -
   once compiled, no external code is assumed to exist, which allows
   fairly sophisticated optimizations to be made. Still, simplicity in
   a language implementations makes it easier for the user to reason
   about the behaviour of the program, so it is advisable to think
   twice before adding more complexity to the compiler.

   Also because the program, and specifically, the global environment
   will not change at run-time, global variables are implemented as
   simple static variables. Literal constants are directly stored in
   the `.data' section and are not subject to garbage collection (and
   immutable).

9 Data Representation 
~~~~~~~~~~~~~~~~~~~~~~

  Values used in the system are of two kinds: 64-bit immediate exact
  numbers (/fixnums/) and 64-bit pointers to data. A fixnum is marked
  by having the lowest bit set to 1. Pointers are always aligned on an
  8-byte boundary and so automatically have their lowest 3 bits set
  to 0. This allows to distinguish between immediate and non-immediate
  data by just testing the lowest bit. This is necessary to implement
  type-predicates and to allow the garbage collector (described below)
  to handle them specifically.

  Fixnums represent an integer value shifted one bit to the left, 
  with the lowest bit set.

  Non-immediate data (/blocks/) are preceded by a 64-bit /header/,
  containing the size and the type of the block, in addition to one
  bit used for house-keeping during garbage collections. The size is
  encoded in the lowest 56 bits, the type in the upper 7 bits.

  Non-immediate data is again classified into normal blocks,
  containing other values (fixnums or pointers to blocks) and
  /byteblocks/, where the actual data-area after the header contains
  raw bytes. Currently this are strings and floating-point numbers.

  The last class of blocks are /special/ blocks, which are like normal
  blocks, but with the first /slot/ holding a native pointer
  value. This is used for procedures - the pointer in that case points
  to the actual machine code of the procedure (and is not traced
  during GC). The remaining slots hold free variables, in case the
  procedure is a closure, i.e.  needs to retain references to
  variables defined in a scope lexically outside of the procedure.

  BONES uses IEEE-754 floating-point numbers on all supported
  platforms.

  Here is a rough overview of the different objects used in the
  system:

    Type          Typecode   Immediate   Byteblock   Special   Remarks              
   -------------+----------+-----------+-----------+---------+---------------------
    Fixnum        -          X           -           -                              
    Null          #x00                                         The empty list       
    Symbol        #x01                                                              
    Pair          #x02                                                              
    Vector        #x03                                                              
    Char          #x04                                                              
    End-of-file   #x05                                                              
    Void          #x06                                         The undefined value  
    Boolean       #x07                                                              
    Port          #x08                                                              
    Promise       #x09                                                              
    Record        #x0a                                                              
    Flonum        #x10                   X                                          
    String        #x11                   X                                          
    Procedure     #x20                               X                              

  `#t', `#f' and the undefined (void) value are unique and never
  allocated.

  Strings use a single-byte encoding, [Unicode] is not
  supported. Characters-codes are not limited to 8-bit, though, but
  when assigned or extracted to/from strings, only the least
  significant 8 bits are used.

  For information about the use of the slots in port and record
  objects, you should study the source code, in particular `r5rs.scm'.

  Records are vector-like blocks where the two first slots contain a
  record /tag/ (symbol) and a fixnum /id/, designating the exact type
  of the record. Id 1 is used for error-objects.

  As argument types are not checked, primitives are only sensitive to
  the underlying representation, not the actual type. This can be used
  for interesting effects, for example:

  (string->copy <FLOAT>)  ==>  <a string containing the raw bytes of the IEEE double>
  
  (vector->list '(1 . 2)) ==>  (1 2)
  
  (define-values (make pred? data) (make-disjoint-type 'foo))
  (vector-copy (make 99))    ==>  #(foo 2 99)


  Operations that expect a normal block will have an undefined effect
  when invoked on objects that are byte-blocks and vice versa. Usually
  the program will crash.


  [Unicode]: http://unicode.org

10 Garbage Collection 
~~~~~~~~~~~~~~~~~~~~~~

  BONES uses a Cheney-style copying garbage collector. This sacrifices
  one half of the available heap space to gain faster allocation and
  collection and to make the time needed to collect garbage
  independent on the amount of available memory. Fixnums are ignored
  during GC, blocks are traversed in breadth-first manner to copy all
  live data in every collection into the second half of the
  heap-space.  Once all live data has been copied, the halves of the
  heap-space are /flipped/ and program execution continues. The time
  needed for a GC depends on the amount of live data. The fewer data
  is allocated, the lower is the impact of the GC on the total
  run-time of a program.

  Pointers that do not point into the garbage collected heap are
  ignored by GC, so it is allowed to store pointers to user-allocated
  blocks (in fact to any kind of data) into Scheme data-structures.

  Note that storing pointers to heap-allocated data in non-heap data
  is disallowed and optionally checked at runtime. Otherwise the
  garbage collector will not be able to detect that the stored value
  is still live, if no other pointer to that object exists. Since the
  GC moves data in the heap on every collection (because it is copied
  from the old heap-space to the new one), the old pointer will be
  invalid after the collection finishes. A check for this can be
  enabled by passing `-DENABLE_WRITE_BARRIER' to NASM, which may
  slightly decrease runtime performance.

  The heap can not be resized dynamically and has a fixed size.  Use
  the `TOTAL_HEAP_SIZE' macro to specify a heap-size different from
  the default size. 1% of the heap is used as a /reserve zone/ - once
  the allocation-pointer enters this zone, a garbage collection will
  be triggered. This allows to defer the limit-check when small blocks
  of data are allocated.

11 Interfacing to Foreign Code 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  There are basically four ways to invoke non-Scheme code from compiled
  programs:

  1. Use `system' to run shell commands. This is the easiest and most
     portable method, unless you need to have access the BONES
     runtime- system or want to modify or inspect Scheme data.

  2. Embed a compiled program inside other code that can interface to
     C. For more information on this see below. This is also quite
     straightforward and very flexible, without the need to use
     assembly code.

  3. Use the `$inline' special form to mix assembly code instructions
     directly with Scheme code, where `$inline' is defined as

     `($inline STRING ARGUMENT ...)': Executes the assembler
         instructions in `STRING', given the results of evaluating the
         arguments in `ARGUMENTS', specifically in the `rax', `r11'
         and `r15' registers. At most three arguments may be
         passed. Multiple instructions inside `STRING' can be given,
         when separated by `;' (semicolon). Note that changing other
         registers than the ones used for passing arguments may change
         local variables and thus should be avoided, unless you really
         know what you are doing. `STRING' is not evaluated.

  4. Use the `$primitive' special form to refer to a primitive
     procedure written in assembly code:

     `($primitive STRING)': Returns a procedure refering to the
         primitive procedure named by the label `STRING'. `STRING' is
         not evaluated.

     The procedure must be written in assembly code and takes its
     arguments in the argument registers described in the chapter
     describing the register usage. `rbx' holds the current procedure,
     `rcx' the current continuation (a procedure block), and `rdx' to
     `r12' contain the first seven arguments passed to the current
     procedure. Additional arguments are passed in a static array and
     are not accessible from separately compiled code, so your
     primitive may not accept more than seven arguments.

     To return, invoke the continuation procedure with a single result
     in `rcx', like this (NASM syntax, assuming the result is in `rax'):

  mov rbx, rcx            ; make continuation the current procedure
  mov rcx, rax            ; put result into the correct register
  mov rax, [rbx + 8]      ; get code-pointer to continuation
  jmp rax                 ; and jump to it


     During execution of your primitive you are free to modify all
     registers as you please, with the exception of `rbp', `r13' and
     `r14', as long as you follow the steps given to invoke the
     continuation. `rsp' may be modified, but must be restored when
     the primitive returns.

     No implicit argument-count checks are done. Returning multiple
     values from user-defined primitives is not supported. The stack
     is not used for procedure call arguments or return-addresses.

12 Embedding 
~~~~~~~~~~~~~

  Compiled programs can be linked to code written in other languages,
  provided they are able to interface to C. Normally an object file
  compiled with BONES exports the entry-point `main' (or `_start' when
  the C-libray is disabled). By compiling the Scheme program with the
  option `-feature embedded', the entry-point is renamed to `bones',
  having this signature:

  BONES_X scheme(BONES_X argument);


  where `BONES_X' is a generic type designating a Scheme value.  

  The function is not reentrant, and so can not be used by multiple
  threads, because it refers to static data buffers and variables.  It
  is possible to combine multiple programs by using the
  `-DPREFIX=<prefix>' option when assembling each compiled program. In
  this case the entry-point will be named `<prefix>_scheme'. Every
  entry-point refers to a self-contained Scheme environment and thus
  can be run in a separate thread, if desired.

  The file `bones.h' contains a number of macros and type-definitions
  (including the `BONES_X' type) that provides some basic operations
  to extract types and values from Scheme data objects. Refer to this
  file for more information.

  On the first invocation of the entry-point, the argument is ignored.
  Once the Scheme procedure `return-to-host' is invoked, the
  entry-point function returns the argument given to that
  primitive. This value will be located in the Scheme heap (unless it
  is a fixnum) and should not be retained across later invocations of
  the entry-point, because subsequent garbage collections will move
  the object and thus invalidate the retained pointer.

  The argument can be a fixnum, or a pointer to Scheme-data allocated
  in non-heap memory. In that case it will be ignored by the garbage
  collector, but may not be modified to hold pointer to heap-data
  (this will be detected and result in an error-message and
  termination of the process, when the program was assembled with the
  `-DENABLE_WRITE_BARRIER' argument.
  
  If you want to copy the argument given to the entry-point into the
  Scheme heap, take a look at the file `copy.scm' in the BONES
  distribution. This file provides a procedure to create a heap-copy
  of an arbitrary complex Scheme object.

  Note that `return-to-host' suspends the executing Scheme code and
  returns the argument given to the following invocation of the
  entry-point.

  When embedding code in a shared library on Linux or *BSD, you should
  compile your Scheme code with the `-feature pic' option to make sure
  the generated code is position independent. On Windows and Mac,
  `pic' is enabled by default.

13 Debugging 
~~~~~~~~~~~~~

  As already mentioned, there are practically no debugging facilities.
  It is possible to enable some checks for lowlevel data access and
  for exceeding argument-counts and -limits. To do so, use the
  `-feature check' option (or use `provide' for enabling this in your
  program specifications).

  Currently, the `check' feature enables the following:

  - Out-of-bounds slot-access of vector-like objects.

  - Out-of-bounds access of byte-vectors objects.

  - Checking whether a non-procedure was called.

  - Checking the correct number of arguments in a procedure call.

  - Checking the maximum number of arguments in `apply'.

  - Checking attempts to modify immutable data (literals).

  Failed checks invoke the exception handler, and thus can be caught.
  In certain fatal situations, for example, when the heap is full or
  corrupted, the error-processing machinery may not be able to exit
  with a meaningful message.

  Access to undefined ("unbound") variables will produce linker
  errors, where the variable name will be mangled, by prefixing it
  with "___" (three underscores) and replacing non-alphanumeric
  characters with "_XX". "XX" in this case is the hexadecimal
  code of the character.

14 Bugs and Limitations 
~~~~~~~~~~~~~~~~~~~~~~~~

  The heap has a fixed size and does not resize dynamically. You can
  change the default size of the heap setting the `TOTAL_HEAP_SIZE'
  configuration macro when assembling a compiled program.

  `inexact->exact' uses the `FISTTP' instruction, so a CPU with SSE3
  support is required.

  Complex numbers and other numeric types with the exception of small
  integers and floating-point numbers are not supported.

  `string->number' does not detect overflow when converting integers
  and returns -1 if the result does not fit into a fixnum.

  Library support for on Linux for "nolibc" mode is incomplete:

  - `number->string' and `string->number' do not support inexact
    numbers.

  The maximum number of arguments that can be passed to a procedure is
  currently 1024 + 9. You can change this by giving a different value
  for the `NUMBER_OF_NON_REGISTER_ARGUMENTS' in the `boneslib.s' file.

  Building shared libraries ("bundles") from code compiled with BONES
  does currently not work on Mac OS X.

15 Porting Guide 
~~~~~~~~~~~~~~~~~

  The system should be relatively easy to port. The only
  target-specific code is contained in the `x86_64.scm' backend, in
  the runtime library in `<ARCH>/boneslib.s' and in the files defining
  intrinsics and system-calls. The compiler makes very little
  assumptions about the used assembler-syntax and should be suitable
  for most assemblers, unless you want to generate code for a very
  unusual target.

  The code-generation scheme assumes that a certain number of
  registers is available, and the more registers are available for
  arguments and local variables, the better. In fact, this is the
  reason why BONES has not been ported to i386: the number of
  available registers is too low. On modern architectures this is not
  much of an issue anymore, though.

  The default configuration file `base.scm' selects the appropriate
  files to be included for defining intrinsics and system-specific
  code, so extending this will be required. If no target feature is
  given on the command-line (currently `linux', `bsd', `mac' or
  `windows'), then a /default-target/ is selected, which is the
  target, the compiler is currently running on, or more specifically:
  the target for which the currently executing compiler was
  configured. This feature will be named `default-linux',
  `default-bsd', `default-mac' or `default-windows', depending on the
  OS.  Don't forget to add a case for the `default-configuration'
  variable in `cmplr.scm'.

  If you intend to provide a `nolibc' mode, that is, a compilation
  mode that generates code for use without the C library, then this
  should also be reflected in `base.scm'.

  The code currently makes no assumptions about endianness.

  On 32-bit systems, care must be taken with alignment of
  floating-point numbers: the actual float value must be located on an
  8-byte boundary, as most architectures require this, and even if
  not, aligned memory-accesses are likely to be more
  efficient. Allocation of floating-point numbers and the code in the
  garbage collector copying data from one heap-space to the other need
  to take this into account.

  On x86_64 systems, the FPU provides machine-instructions for many
  transcendental functions. Other architectures will probably not have
  support for these, so the associated intrinsic operations in
  `<ARCH>/intrinsics.scm' need to be adapted to call C library
  functions.

  The actual implementation of library- and system-calls are located
  in `<ARCH>/<OS>/syscalls.scm' and `<ARCH>/<OS>/syscalls-nolibc.scm',
  respectively, depending on the compilation mode. The operations here
  use OS-dependent constants, defined in `<ARCH>/<OS>/constants.scm',
  which needs to be generated and added manually. To generate this
  file, compile and run the C program `constants.c', which produces
  the file when executed.

  All compile-time features, defined using the `-feature' option or
  the `provide' program-specification clause are also defined in
  compiled programs in the form of assembler macros (or symbols),
  prefixed with `FEATURE_', converted to uppercase and with `-'
  (hyphen) replaced by `_' (underscore) characters. The generated code
  should also perform an `include' of the `<ARCH>/boneslib.s' file
  appropriate for the target platform.

  To reduce code size in statically linked executables it may be
  worthwhile to use a replacement for the standard C library. `glibc'
  is relatively large and complex and there exist alternatives in the
  form of [MUSL] or [dietlibc], which are much smaller, simpler and more
  than sufficient in most cases. By linking these statically, the
  program is self-contained and has fast startup times. Using MUSL,
  for example is straightforward: instead of linking with `gcc', use
  `musl-gcc', like this:

  bones myprogram.scm -o myprogram.s
  nasm -f elf64 myprogram.s -o myprogram.o
  musl-gcc myprogram.o -o myprogram



  [MUSL]: http://www.musl.org
  [dietlibc]: http://www.fefe.de/dietlibc/

16 Suggestions for Projects 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you are interested in extending the compiler or runtime system,
  or if you'd like to experiment, here are a few suggestions:

  - Generating machine code and creating elf64/win64 object files
    directly. This would remove the need for an assembler and speed up
    the compilation process.

  - Compiling into memory directly, for later execution. This is
    complicated by the fact that memory-pages need to be made
    executable, but still be reclaimable by the garbage collector.

  - Extending the language, for example by adding support for [R7RS].
    Implementing the R7RS module system will be quite a challenge,
    though, as the syntax-expander used ("alexpander") does not
    support this in the moment.

  - Porting the system to other architectures and operating systems.

  - Generate code suitable for the GNU assembler (`as'). Since the GNU
    binutils are needed anyway on Linux, and because `as' is quite
    fast, this would remove the need for NASM. On the other hand, NASM
    is more featureful and supports a nicer and more consistent
    assembler syntax.

  - Other backends, say, asm.js or LLVM. This is a challenge as well,
    as these languages do not support jumps to arbitrary locations.

  - With a little work code compiled with BONES should be able to run
    without any library at all ("bare metal"), which would make it
    possible to write, say, an OS Kernel in Scheme.


    [R7RS]: http://www.scheme-reports.prg

17 Terms of Use 
~~~~~~~~~~~~~~~~

  Some parts of BONES are not by the author, but have been taken from
  other sources:

    The pretty-printer (`pp.scm') is

    Copyright (c) 1991, Marc Feeley,
    Author: Marc Feeley, Distribution restrictions: none

    The pattern-matching package was written by Alex Shinn and
    is in the public domain.

    The syntax-expander (`alexpand.scm') is

      Copyright 2002-2004 Al Petrofsky

      Redistribution and use in source and binary forms, with or without
      modification, are permitted provided that the following conditions
      are met:

       Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

       Redistributions in binary form must reproduce the above copyright
         notice, this list of conditions and the following disclaimer in
         the documentation and/or other materials provided with the
         distribution.

       Neither the name of the author nor the names of its contributors
         may be used to endorse or promote products derived from this
         software without specific prior written permission.

      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
      HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
      INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
      BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
      OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
      AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
      LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
      WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
      POSSIBILITY OF SUCH DAMAGE.

    The topological sorting routine in `tsort.scm' was written by
    Moritz Heidkamp and is in the public domain.

  Everything else was written by Felix Winkelmann and is put into the
  public domain.

18 Further Reading 
~~~~~~~~~~~~~~~~~~~

  [Cheney's Algorithm] describes the garbage collection method used in
  BONES.

  [Compiling with Continuations] by Andrew Appel covers CPS-based
  compilation of high-level languages in great detail.

  [Essentials of Programming Languages] describes the CPS-conversion
  algorithm used in BONES and contains a lot of interesting
  information about implementing interpreters.

  [Agner Fog's website] contains a wealth of information about x86 CPUs
  and optimization tips.

  A list of of Linux/x86_64 system calls can be found [here].

  [Simply FPU] contains a lot of information on the x86 FPU.

  Check out [schemers.org] for basic information about Scheme and the
  different standards.

  The [Intel manuals] exhaustively describe the x86 architecture.

  Also check out the  [Linux ABI] for x86_64 architectures.


  [Cheney's Algorithm]: https://en.wikipedia.org/wiki/Cheney%27s_algorithm
  [Compiling with Continuations]: http://www.amazon.com/Compiling-Continuations-Andrew-W-Appel/dp/052103311X
  [Essentials of Programming Languages]: http://www.amazon.com/Essentials-Programming-Languages-Daniel-Friedman/dp/0262062798/ref=sr_1_1?s=books&ie=UTF8&qid=1403309494&sr=1-1&keywords=essentials+of+programming+languages
  [Agner Fog's website]: http://agner.org/optimize/
  [here]: http://blog.rchapman.org/post/36801038863/linux-system-call-table-for-x86-64
  [Simply FPU]: http://www.ray.masmcode.com/tutorial/index.html
  [schemers.org]: http://www.schemers.org
  [Intel manuals]: http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
  [Linux ABI]: http://www.x86-64.org/documentation_folder/abi-0.99.pdf

19 Contact 
~~~~~~~~~~~

  If you have questions, bug-reports or suggestions for improvements,
  please contact me at felix <at> call-with-current-continuation <dot>
  org.


