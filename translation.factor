USING: arrays grouping kernel math math.functions multiline peg peg.ebnf random ranges sequences sets strings ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

! DOESNT WORK!! FIXX!!
: check ( code -- verified_code ) 
    dup
    [ 3 0 spin <slice> >string "AUG" = ]
    [ dup length 1 - over length 4 - <slice> >string { "UGA" "UAA" "UAG" } member? ] ! maybe also add the two letter one
    bi and swap '[ _ ] [ "Program should start and end with the proper codons." throw ] if
;

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
! "AUG" is the Met amino acid
EBNF: parse-translation [=[
    start = "AUG" 
    end = ("U" | "A" | "G" | "C")+ ?[ length 2 <= ]? 
    code = "a"
]=]

! PRIVATE>