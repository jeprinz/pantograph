This is the source code for Pantograph, a structure editor.
It was written by Jacob Prinz and Henry Blanchette.

Overall, the source code is designed to support a variety of editors, although only one particular editor is implemented.
the "src/Language/Pantograph/Generic" folder has things that would be relevant to any editor, while the
"src/Language/Pantograph/Generic" folder has things relevant to particular languages. The only particular language that
we ever finished implementing is the one used in Pantograph, at "src/Language/Pantograph/Generic/Currying.purs".


The general paradigm behind how languages are implemented is described in Section 5 of the paper.
A language consists of a set of "sorts" (which we just refer to as trees in the paper), which are essentially
the possible judgements, while a program is a derivation of a given sort.
Both sorts and derivations are ultimately just trees, where each node has a label and some children.

A definition of a language consists of several pieces.
First, you need two types which implement the typeclasses IsExprLabel and IsRulelabel.
These are the labels for sorts and the labels for types respectively.

Then, you need to implement an instance of editorSpec, which defines various further functions involving these types.
