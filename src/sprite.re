open Reprocessing;
open Std;

type spriteData = {
  x: int,
  y: int,
  w: int,
  h: int,
};

let spritesData = [
  ("floor", (4, 2)),
  ("wall_topleft", (0, 0)),
  ("wall_top", (1, 0)),
  ("wall_topright", (2, 0)),
  ("wall_left", (0, 1)),
  ("wall_right", (2, 1)),
  ("wall_bottomleft", (0, 2)),
  ("wall_bottom", (1, 2)),
  ("wall_bottomright", (2, 2)),
  ("player", (0, 4)),
  ("robot", (3, 4)),
  // ("foo", {x: 0, y: 0, w: 8, h: 8}),
  // ("foo", {x: 0, y: 0, w: 8, h: 8}),
  // ("foo", {x: 0, y: 0, w: 8, h: 8}),
];

type t = {
  sheet: imageT,
  map: StringMap.t(spriteData),
};

let load = (spritesPath, spritesData, env) => {
  let sheet = Draw.loadImage(~filename=spritesPath, ~isPixel=true, env);
  let map = List.fold_left(
    (map, (name, (x, y))) =>
      StringMap.add(name, {x:x*8, y:y*8, w:8, h:8}, map),
    StringMap.empty,
    spritesData);
  {sheet, map}
};

let placeholderUncentered = (name, ~pos, ~width, ~height, env) => {
  Draw.fill(Constants.red, env);
  Draw.noStroke(env);
  Draw.rectf(~pos=Point.(toPair(pos)), ~width, ~height, env);
  Draw.text(
    ~body=name,
    ~pos=(int_of_float(pos.x), int_of_float(pos.y)),
    env,
  );
};

let draw = (t, ~name, ~pos, ~flipped=false, ~scale=4.0, env) => {
  switch (StringMap.find(name, t.map)) {
  | {x, y, w, h} =>
    let width = float_of_int(w) *. scale *. (flipped ? (-1.) : 1.);
    let height = float_of_int(h) *. scale;
    Draw.subImagef(
      t.sheet,
      ~pos=Point.Float.(toPair(pos - create(width /. 2., height /. 2.))),
      ~width,
      ~height,
      ~texPos=(x, y),
      ~texWidth=w,
      ~texHeight=h,
      env,
    );
  | exception Not_found =>
    placeholderUncentered(
      name,
      ~pos=Point.Float.(pos - create(50., 50.)),
      ~width=100.,
      ~height=100.,
      env,
    )
  };
};