
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token BOOL
%token NAT

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token EOEXPRESION
%token LETREC
%token CONCAT
%token CIRCUNFLEJO
%token LTUPLA
%token RTUPLA
%token ASTERISCO
%token COMA
%token FIRST
%token SECOND



%token <int> INTV
%token <string> STRINGV
%token <string> TSTRING

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ term EOEXPRESION
      { Bind ($1, $3) }
 | term EOEXPRESION
      { Eval $1 }
  


term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

	

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | FIRST atomicTerm
      { TmFirst $2 }
  | SECOND atomicTerm
      { TmSecond $2 }
  | CONCAT atomicTerm atomicTerm
	  { TmConcat($2, $3)}
  | atomicTerm CIRCUNFLEJO atomicTerm
	  { TmConcat($1, $3)} 
  | appTerm atomicTerm
      { TmApp ($1, $2) }




atomicTerm :
	LTUPLA term COMA term RTUPLA
      { TmTupla ($2,$4) }
  | LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | TSTRING
	  { TmString $1} 
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | TSTRING
      { TyString }
  | LTUPLA ty ASTERISCO ty RTUPLA
	  { TyTupla ($2, $4) }
  

