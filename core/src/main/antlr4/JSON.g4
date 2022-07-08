grammar JSON;

expr : expr '+' expr

    | Number;

Number : [0-9]+;

