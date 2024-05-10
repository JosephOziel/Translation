USING: arrays grouping kernel math math.functions multiline peg peg.ebnf random ranges sequences sets ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

: process ( code -- processed ) "UACG" within ; ! 3 group

: mutation ( symbols -- mutated ) 
!    "" join
    dup length mut-rate * over length 4 - randoms [ dup 3 < [ 3 + ] when ] map
    [
        { 
          [ "UABG" random spin insert-nth ] ! insertion
          [ swap remove-nth ] ! deletion
          [ "UABG" random spin [ clone set-nth ] keep ] ! substitution
        } random call( x y -- z )
    ] each 3 group " " join
; 

: code->string ( code-list -- code-string ) " " join ; ! after mutation is applied.the string output is what will be fed into the second parser

! PRIVATE>