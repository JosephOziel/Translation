USING: arrays kernel math math.functions multiline peg peg.ebnf random ranges sequences splitting ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

EBNF: process [=[

rule = ("U" | "A" | "C" | "G" | (.)~)+ => [[ [ ignore = ] reject dup length 0 swap 3 <range> >array over length 3 mod zero? [ rest but-last ] [ rest ] if split-indices ]]

]=]

: mutation ( symbols -- mutated ) dup drop ; ! TODO

: code->string ( code-list -- code-string ) dup drop ; ! after mutation is applied.the string output is what will be fed into the second parser

! PRIVATE>