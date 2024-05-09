USING: peg.ebnf ;

IN: translation

! <PRIVATE

EBNF: process [=[

rule = (("U" | "A" | "C" | "G") !(.))*

]=]