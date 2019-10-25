open Printf;

module Stream = CStream;

type expr =
 | App(expr, list(expr))
 | Bool(bool)
 | Var(string)
 | Str(string)
//  | Fun(list(string), expr)
//  | Let(string, expr, expr)
//  | Block(list(stmt))
//  | Cond(expr, expr, expr)
 ;
// type program = list(expr);

let rec exprToString = expr => switch (expr) {
  | Str(s) => "\"" ++ s ++ "\"";
  | Bool(b) => b ? "True" : "False";
  | Var(s) => s;
  | App(fn, args) =>
    let fnStr = switch (fn) { 
      | Var(s) => s
      | e => "(" ++ exprToString(e) ++ ")"
    };
    sprintf(
      "%s(%s)",
      fnStr,
      String.concat(", ", List.map(exprToString, args)));
};

let fail = (stream, err) => {
  let red = "\027[31m";
  let endcolor = "\027[0m";
  let error = sprintf("Syntax Error %s: %s", Stream.locString(stream), err);
  prerr_endline(red ++ error ++ endcolor);
  raise(Failure(error));
};

let unexpected = (stream) => {
  let chStr = switch (Stream.peek(stream)) {
    | Some(c) => String.make(1, c);
    | None => "EOF"
  };
  fail(stream, "Unexpected character '" ++ chStr ++ "'");
};

let junkRestOfLine = (s) => {
  while ({
      let c = Stream.peek(s);
      c != None && c != Some('\n')
  }) {
      Stream.junk(s);
  }
  if (Stream.peek(s) == Some('\n')) {Stream.junk(s)}
};

let junkSpaces = (s) => {
  while (Stream.peek(s) == Some(' ')) {
      Stream.junk(s);
  }
};

let rec parseIdent = (stream, acc) => {
  switch (Stream.peek(stream)) {
  | Some('A'..'Z' as c) | Some('a'..'z' as c) | Some('0'..'9' as c) => 
    Stream.junk(stream);
    parseIdent(stream, acc ++ String.make(1, c))
  | _ => Var(acc)
  }
};

let rec parseString = (stream, acc: string) =>
  switch (Stream.pop(stream)) {
  | Some('"') => Str(acc)
  | Some(c) => parseString(stream, acc ++ (String.make(1, c)))
  | None => fail(stream, "Unterminated string literal")
};

// TODO refactor to hold some parse context kinda thing
let rec parseExpr = (stream) => {
  switch (Stream.peek(stream)) {
  | Some('a'..'z') =>
    let ident = parseIdent(stream, "");
    if (Stream.peek(stream) == Some('(')) {
      Stream.junk(stream);
      let args = parseArgs(stream, []);
      App(ident, args);
    } else {
      // Just a var... eh not sure
      ident
    }
  | Some('"') =>
    Stream.junk(stream);
    parseString(stream, "");
  | _ => unexpected(stream)
  }
} and parseArgs = (stream, args) =>
  switch (Stream.peek(stream)) {
  | Some(')') =>
    Stream.junk(stream);
    List.rev(args);
  | Some(_) =>
    let e = parseExpr(stream);
    switch (Stream.peek(stream)){
      | Some(',') =>
        Stream.junk(stream);
        parseArgs(stream, [e, ...args])
      | Some(')') =>
        Stream.junk(stream);
        List.rev([e, ...args])
      | None => fail(stream, "Unclosed function call (1)")
      | _ => unexpected(stream)
    }
  | None => fail(stream, "Unclosed function call (2)")
}

let rec parseExprs = (stream, exprs) => 
  {
    switch (Stream.peek(stream)) {
  | Some(' ') | Some('\n') | Some(';') =>
    Stream.junk(stream);
    parseExprs(stream, exprs);
  | Some('#') =>
    junkRestOfLine(stream);
    parseExprs(stream, exprs);
  | Some(_) =>
    let expr = parseExpr(stream);
    parseExprs(stream, [expr, ...exprs])
  | None =>
    List.rev(exprs)
  }}

let parse = (text) => {
  parseExprs(Stream.create(text))
}

// parse("Move.forward(); say(\"Hello\")");
// let exprs = parse("forward()\nsay(\"Hello\")", []);

// List.iter(expr => print_endline(exprToString(expr)), exprs);