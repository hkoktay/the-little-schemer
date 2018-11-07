# Code for The Little Schemer


## Background

The concise book 'The Little Schemer' explains fundamental concepts of computer
science with a simple but powerful programming language: scheme.[1][2] Even so you
don't need a computer to work through the book - pen and paper is sufficient - it is
helpful to have an interactive environment while working through the book.

## Summary

This is the code for the book 'The Little Schemer'. The code is split into 10 files;
one scheme source file for every chapter. This code could be useful for programming
beginners while reading the book, because it contains tests which are also usage
examples of the defined functions. If you plan to use this code while reading
The Little Schemer, I recommend to load the code function by function; not the whole
file. The reason is that functions in a chapter will be redefined
and thus in the source files will be redefined. So if you work through the book don't
load the chapter file in the repl, instead evaluate only the procedures of a chapter you
want to work with.

## Additional Information

Nearly every function or procedure has a signature which shows the data types
the function needs. Non build-in types are shown in square brackets. For example
the data definition for an atom is in square brackets because it is not a
built-in data structure in scheme. As an example here is the "atom" data
definition:

    An [atom] is
    - Number or
    - String or
    - Symbol

Here is the definition of the 'atom?' function with the function signature as a
comment:

    ;; atom?: any -> boolean
    (define (atom? e)
       (and (not (list? e)) (not (pair? e))))

This means that the function ```atom?``` can take a scheme expression of any type as
an argument and returns a value of type boolean. Another example with the
function ```member?``` shows the compound data type ```[listof atom]```:

    ;; member?: [atom] [listof atom] -> boolean
    (define member?
      (lambda (a lat)
        (cond
         ((null? lat) #f)
         (else (or (eq? (car lat) a)
                   (member? a (cdr lat)))))))

The function ```member?``` takes an atom and a list as arguments. The elements of
the list are atoms. The result type is a boolean. Note that scheme has strong
dynamic types. Types are checked at run-time not at compile-time. Thus types are
checked when you run the program or call a function. If you use the wrong type,
a scheme implementation should return an error. A list of data type definitions
used in The Little Schemer can be found in the file NOTES. If the definition
of a function changes during the chapter, it is annotated with "1.revision",
"2.revision" etc.

Almost every function in the code has tests. Loading the scheme file of the
chapter evaluates the corresponding tests. If a test fails it prints a message
to output port showing the expected value and the computed value of the scheme
expression. For example the test ```(test "atom?"  (atom? 'a) #f)``` would print
these lines:

    Testing "atom?"
    Failed: (atom? 'a)
    Expected: #f
    Computed: #t

The tests have two goals. They ensure that the code is correct and they are
examples showing how to apply the code. This is also the reason why the tests
are close to the defined function and not in a separate file.

The recommended scheme implementation for the code is Chez Scheme, because of
its debugging features, good documentation and build-in expression editor
[2][3][4]. You can test the code by loading the chapter source file you want to
work with. If you have Chez Scheme installed, type

    scheme

or 

    petite

into your terminal, which will start a repl. Now you can run the tests by
loading the scheme file:

    (load "chapter01.scm")

If you want to run all tests in all files just execute the scheme script
'run-tests.scm' in the code directory. This needs an installation of Chez
Scheme. Another option is to use this bash one liner in the code directory:

    for f in chapter* ; do echo '(exit)' | scheme $f -q --; done

If you already tested the chapter code restart your scheme implementation to
clear the top-level environment. This is also recommended when you have finished
a chapter and want to start with another chapter. Chez Scheme comes with a
scheme expression editior to edit scheme code. For more information about the
editor look at the man file of Chez Scheme with

    man scheme

or the online documentation.[5]


References:

[1] https://mitpress.mit.edu/books/little-schemer

[2] https://github.com/cisco/ChezScheme

[3] http://www.cs.indiana.edu/chezscheme/debug/

[4] https://cisco.github.io/ChezScheme/csug9.4/

[5] https://cisco.github.io/ChezScheme/csug9.5/expeditor.html#./expeditor:h0
