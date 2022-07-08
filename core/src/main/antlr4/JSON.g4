grammar JSON;

expr : expr '+' expr EOF
    | Number;

Number : [0-9]+;


