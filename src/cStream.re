type t = {
  content: string,
  mutable lineNum: int,
  mutable colNum: int,
  mutable charNum: int,
};

let peek = (stream: t) :option(char) =>
  switch (String.get(stream.content, stream.charNum)) {
  | c => Some(c)
  | exception Invalid_argument(_) => None
  };

let match = (s, match) => {
  try (String.sub(s.content, s.charNum, String.length(match)) == match) {
  |  Invalid_argument(_) => false 
  }
};

let junk = (stream: t) =>
  switch (String.get(stream.content, stream.charNum)) {
  | '\n' =>
    stream.lineNum = stream.lineNum + 1;
    stream.colNum = 0;
    stream.charNum = stream.charNum + 1;
  | _ =>
    stream.charNum = stream.charNum + 1;
    stream.colNum = stream.colNum + 1;
  | exception Invalid_argument(_) => raise(Failure("End of stream"))
};

let pop = (stream: t) =>
  switch (peek(stream)) {
  | Some(_) as c => junk(stream); c
  | None as c => c
};

let locString = stream =>
  Printf.sprintf("(line %d, col %d)", stream.lineNum + 1, stream.colNum + 1);

let create = (s: string) :t => {
  content: s,
  lineNum: 0,
  colNum: 0,
  charNum: 0,
};