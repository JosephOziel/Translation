USING: arrays grouping io kernel math math.functions multiline peg peg.ebnf random ranges sequences sets strings vector ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

ERROR: incorrect-start-end-codons program ; ! "Program should start and end with the proper codons."

: check ( code -- verified_code ) 
    dup
    [ "AUG" head? ]
    [ 3 tail* { "UGA" "UAA" "UAG" } member? ] ! maybe also add the two letter one
    bi and [ incorrect-start-end-codons ] unless ;

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
    ] each 3 group " " join ; 

: compose-all ( seq -- quot )
    [ ] [ compose ] reduce ;

: get-2 ( seq -- seq second first )
    >vector [ pop ] keep [ pop ] keep spin ;

: get-1 ( seq -- seq first )
    >vector [ pop ] keep swap ;

! No structure for the stack, since there will be no environment I think

: phe ( stack -- stack ) [ read1 suffix ] ; ! read a character

: leu ( stack -- stack ) 1 suffix ; ! push 1 

: ile ( stack -- stack ) [ get-1 write1 ] ; ! print character and pop it

: met ( stack -- stack ) [ get-2 mod suffix ] ; ! mod a b ;

: val ( stack -- stack ) ; ! dip 

: ser ( stack -- stack ) ; ! repn

: pro ( stack -- stack ) ; ! compose

: thr ( stack -- stack ) ; ! zap

: ala ( stack -- stack ) ; ! dup

: tyr ( stack -- stack ) ; ! unit

: his ( stack -- stack ) [ ] suffix ; ! push empty quote

: gln ( stack -- stack ) ; ! cons

: asn ( stack -- stack ) [ get-2 + suffix ] ; ! +

: lys ( stack -- stack ) [ get-2 - suffix ] ; ! -

: asp ( stack -- stack ) [ get-2 * suffix ] ; ! *

: glu ( stack -- stack ) [ get-2 / suffix ] ; ! /

: cys ( stack -- stack ) [ get-2 ^ suffix ] ; ! ^

: trp ( stack -- stack ) [ get-2 logn suffix ] ; ! logn x n (maybe change)

: arg ( stack -- stack ) get-1 call( stack -- stack ); ! i/call

: gly ( stack -- stack ) get-2 swap 2array append ; ! swap

EBNF: parse-translation [=[
    start = "AUG"
    phe = "UUU" | "UUC"                                 => [[ [ phe ] ]]
    leu = "UUA" | "UUG" | "CUU" | "CUC" | "CUA" | "CUG" => [[ [ leu ] ]]
    ile = "AUU" | "AUC" | "AUA"                         => [[ [ ile ] ]]
    met = "AUG"                                         => [[ [ met ] ]]
    val = "GUU" | "GUC" | "GUA" | "GUG"                 => [[ [ val ] ]]
    ser = "UCU" | "UCC" | "UCA" | "UCG" | "AGU" | "AGC" => [[ [ ser ] ]]
    pro = "CCU" | "CCC" | "CCA" | "CCG"                 => [[ [ pro ] ]]
    thr = "ACU" | "ACC" | "ACA" | "ACG"                 => [[ [ thr ] ]]
    ala = "GCU" | "GCC" | "GCA" | "GCG"                 => [[ [ ala ] ]]
    tyr = "UAU" | "UAC"                                 => [[ [ tyr ] ]]
    his = "CAU" | "CAC"                                 => [[ [ his ] ]]
    gln = "CAA" | "CAG"                                 => [[ [ gln ] ]]
    asn = "AAU" | "AAC"                                 => [[ [ asn ] ]]
    lys = "AAA" | "AAG"                                 => [[ [ lys ] ]]
    asp = "GAU" | "GAC"                                 => [[ [ asp ] ]]
    glu = "GAA" | "GAG"                                 => [[ [ glu ] ]]
    cys = "UGU" | "UGC"                                 => [[ [ cys ] ]]
    trp = "UGG"                                         => [[ [ trp ] ]]
    arg = "CGU" | "CGC" | "CGA" | "CGG" | "AGA" | "AGG" => [[ [ arg ] ]]
    gly = "GGU" | "GGC" | "GGA" | "GGG"                 => [[ [ gly ] ]]
    end = ("U" | "A" | "G" | "C")+ ?[ length 2 <= ]? 

    amino-acid = phe|leu|ile|met|val|ser|pro|thr|ala|tyr|his|gln|asn|lys|asp|glu|cys|trp|arg|gly
    space = " "
    code = start~ space~ (amino-acid space~)+ end~      => [[ compose-all ]]
]=]

! PRIVATE>