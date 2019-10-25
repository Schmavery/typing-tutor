type t;
let peek: t => option(char);
let pop: t => option(char);
let junk: t => unit;
let match: t => string => bool;
let create: string => t;
let locString: t => string;