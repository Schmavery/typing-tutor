/*
open Reprocessing;
open Printf;
open Lang;

type state = {
  time: float,
  lines: array(string),
  cursor: (int, int),
  font: fontT,
};

let setup = (env) => {
  Env.size(~width=500, ~height=300, env);
  let font = Draw.loadFont(~filename="./monogram.fnt", ~isPixel=true, env);
  {lines: [|""|], time: 0., cursor: (0, 0), font}
};

module Option = {
  type t('a) = option('a);
  let iter = (t, ~f) => switch (t) {
    | None => ()
    | Some(a) => f(a)
  }
};

let constrainCursor = (lines, (row, col)) => {
  let row = max(0, min(Array.length(lines) - 1, row));
  let rowContents = Array.get(lines, row);
  let col = max(0, min(String.length(rowContents), col));
  (row, col)
};

let updateLines = (~lines, ~cursor, f) => {
  let linesLength = Array.length(lines);
  let (row, col) = constrainCursor(lines, cursor);
  let (before, after) = {
    let rowContents = try (Array.get(lines, row)) {
      | Invalid_argument(_) => failwith("Invalid row somehow " ++ string_of_int(row));
    };
    let rLen = String.length(rowContents);
    let before = String.sub(rowContents, 0, col);
    let after = String.sub(rowContents, col, rLen - col);
    (before, after)
  };
  switch (f(before, after)) {
    | `Delete(addToPreviousLine) =>
      // remove row
      let newLines = if (linesLength > 1 && row > 0) {
        let newLines = Array.make(linesLength - 1, "");
        Array.blit(lines, 0, newLines, 0, row);
        Array.blit(lines, row + 1, newLines, row, linesLength - row - 1);
        newLines
      } else {
        lines
      };
      let pos = if (row > 0) {
        let prevRow = max(0, row - 1);
        let prevRowContents = Array.get(newLines, prevRow);
        Array.set(newLines, prevRow, prevRowContents ++ addToPreviousLine);
        (prevRow, String.length(prevRowContents));
      } else {
        (0, 0)
      };

      (newLines, pos)
    
    | `Edit(before, after) =>
      // update row
      let newString = before ++ after;
      Array.set(lines, row, newString);
      (lines, (row, String.length(before)))
    
    | `Split(changedRow, newRow, indent) =>
      // Edit and add row
      let indentStr = String.make(indent, ' ');
      let newLines = Array.make(linesLength + 1, "");
      Array.blit(lines, 0, newLines, 0, row);
      Array.set(newLines, row, changedRow);
      Array.set(newLines, row + 1, indentStr ++ newRow);
      Array.blit(lines, row + 1, newLines, row + 2, linesLength - row - 1);
      (newLines, (row + 1, indent))
  }
};

let rec countLeadingSpaces = (s, i) => {
  if (i < String.length(s) - 1 && String.get(s, i) == ' ') {
    countLeadingSpaces(s, i + 1)
  } else {
    i
  }
};

// TODO: select mode
// addChar -> delete selection, add char
// delete/backspace -> delete selection
// enter -> delete selection, split
// arrow -> de-select (different end location per arrow!)
// arrow + shift -> select more or less...
// 
// Either:
// - cursor manipulation
// - delete and then do stuff
// OR TAB/Shift-tab

module Location = {
  type t = {
    row: int,
    col: int
  };

  let constrain = (lines, {row, col}) => {
    let row = max(0, min(Array.length(lines) - 1, row));
    let rowContents = Array.get(lines, row);
    let col = max(0, min(String.length(rowContents), col));
    {row, col}
  };

  let (<) = (a, b) =>
    a.col < b.col || (a.col == b.col && a.row < b.row);
    
}

// type cursor =
//   [ `Normal(location)
//   | `Select(location, location)];

// let deleteSelection = (lines, `Select(firstLoc, lastLoc)) => {
//   let firstLoc = constrainLocation(lines, firstLoc);
//   let lastLoc = constrainLocation(lines, lastLoc);
//   let lengthLines
//   let newLines = Array.make(linesLength - 1, "");
//   Array.blit(lines, 0, newLines, 0, row);
//   Array.blit(lines, row + 1, newLines, row, linesLength - row - 1);
//   newLines
  // TODO delete rows between
  // TODO delete after leftloc
  // TODO delete before rightloc
// };

// let handleSelect = (key, lines, selectDir, leftLoc, rightLoc) => {
//   switch (key) {
//     | `Letter(c) => ()
//     | _ => ()
//   }
// };

// TODO: round tabs, shift-tab
let getTypedChar = (key, shift) => switch (key) {
  | `Letter(c) when shift => Char.uppercase(c)
  | `Letter(c) => c
  | `Num(n) when shift => 
      let shiftNumbers = [|')', '!', '@', '#', '$', '%', '^', '&', '*', '('|];
      Array.get(shiftNumbers, n)
  | `Num(n) => String.get(string_of_int(n), 0)
  | `Space => ' '
  | `Quote as k
  | `Comma as k
  | `Minus as k
  | `Period as k
  | `Slash as k
  | `Semicolon as k
  | `Equals as k
  | `OpenBracket as k
  | `CloseBracket as k
  | `Backslash as k
  | `Backtick as k =>
    let (normal, shifted) = Key.symbols(k);
    shift ? shifted : normal
};

let keyTyped = ({lines, cursor} as state, env) => {
  let updateLines = updateLines(~lines, ~cursor);
  let addChar = (c) => updateLines((before, after) => `Edit(before ++ String.make(1, c), after));
  let key = Key.kind(Env.keyCode(env));
  let (lines, cursor) = switch (key) {
    | `Letter(_) as k | `Num(_) as k | `Space as k 
    | `Quote as k | `Comma as k | `Minus as k | `Period as k
    | `Slash as k | `Semicolon as k | `Equals as k
    | `OpenBracket as k | `CloseBracket as k | `Backslash as k
    | `Backtick as k => addChar(getTypedChar(k, Key.modifier(`Shift, env)));

    | `Tab => updateLines((before, after) => `Edit(before ++ "  ", after));
    | `Enter =>
      updateLines((before, after) => {
        let leading = countLeadingSpaces(before, 0);
        `Split(before, after, leading)
        });
      
    | `Backspace =>
      updateLines(
        (before, after) => before == "" ? 
          `Delete(after) : 
          `Edit(String.sub(before, 0, String.length(before) - 1), after));
    | `Delete => 
      updateLines(
        (before, after) =>
          `Edit(before, after == "" ? "" :
            String.sub(after, 1, String.length(after) - 1)))
    | `Arrow(dir) => 
      let (row, col) = cursor;
      let (rowOffset, colOffset) = switch (dir) {
        | `Left => (0, -1)
        | `Right => (0, 1)
        | `Up => (-1, 0)
        | `Down => (1, 0)
      };
      (lines, constrainCursor(lines, (row + rowOffset, col + colOffset)))
    | _ => 
      print_endline("Unknown key");
      (lines, cursor)
  };

  {...state, lines, cursor}
};

let draw = (state, env) => {
  Draw.background(Constants.white, env);
  let font = state.font;
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=100), env);
  let row = fst(state.cursor);
  let beforeText = String.sub(Array.get(state.lines, row), 0, snd(state.cursor));
  let w = Draw.textWidth(~font, ~body=beforeText, env);
  Draw.rect(~pos=(w + 10, 20 * row), ~width=12, ~height=15, env);
  Draw.tint(Constants.black, env);
  Array.iteri((i, str) => Draw.text(~font, ~body=str, ~pos=(10, 20 * i), env),
    state.lines);

  state
};

run(~setup, ~draw, ~keyTyped, ());
*/