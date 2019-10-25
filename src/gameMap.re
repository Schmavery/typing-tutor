let mapString = {|
000000000122223000000000000
000012222255552222223000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000045555555555555556000000
000078888455556888889000000
000000000788889000000000000
000000000000000000000000000
|};

type t = array(array([ | `Nothing | `Floor | `Wall(string) | `Unknown]));

let createGrid = s => {
  let s = String.trim(s);
  let strs = Reprocessing.Utils.split(s, ~sep='\n');
  let width = String.length(List.nth(strs, 0));
  let height = List.length(strs);
  let m = Array.make_matrix(width, height, `Nothing);
  List.iteri(
    (y, s) =>
      String.iteri(
        (x, c) =>
          m[x][y] = (
            switch (c) {
            | '0' => `Nothing
            | '1' => `Wall("topleft")
            | '2' => `Wall("top")
            | '3' => `Wall("topright")
            | '4' => `Wall("left")
            | '5' => `Floor
            | '6' => `Wall("right")
            | '7' => `Wall("bottomleft")
            | '8' => `Wall("bottom")
            | '9' => `Wall("bottomright")
            | _ => `Unknown
            }
          ),
        s,
      ),
    strs,
  );
  m;
};