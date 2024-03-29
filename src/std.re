module Option = {
  type t('a) = option('a);
  let iter = (t, ~f) => switch (t) {
    | None => ()
    | Some(a) => f(a)
  };
  let map = (t, ~f) => switch (t) {
    | None => None
    | Some(a) => Some(f(a))
  };
  let get = (t, ~default) => switch (t) {
    | None => default
    | Some(a) => a
  }
};

module Point = {
  type t('a) = {
    x: 'a,
    y: 'a,
  };
  module Common = {
    let create = (x, y) => {x, y};
    let toPair = ({x, y}) => (x, y);
    let fromPair = ((x, y)) => {x, y};
    let map = (~f, {x, y}) => {x:f(x), y:f(y)}; 
  };
  include Common;

  module Int = {
    include Common;
    type nonrec t = t(int);
    let zero = {x: 0, y: 0};
    let add = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 + x2, y: y1 + y2};
    let sub = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 - x2, y: y1 - y2};
    let addScalar = ({x, y}, s) => {x: x + s, y: y + s};
    let divScalar = ({x, y}, s) => {x: x / s, y: y / s};
    let neg = x => sub(zero, x);
    let mag = ({x, y}) => sqrt(float_of_int(x*x + y*y));
    let print = t => Printf.printf("{x:%d,y:%d}\n%!", t.x, t.y);
    let ofFloatPt = t => map(t, ~f=int_of_float);
    let (+) = add;
    let (-) = sub;
    let (/@) = divScalar
    let (+@) = addScalar;
  };

  module Float = {
    include Common;
    type nonrec t = t(float);
    let zero = {x: 0., y: 0.};
    let add = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 +. x2, y: y1 +. y2};
    let sub = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 -. x2, y: y1 -. y2};
    let addScalar = ({x, y}, s) => {x: x +. s, y: y +. s};
    let divScalar = ({x, y}, s) => {x: x /. s, y: y /. s};
    let neg = x => sub(zero, x);
    let mag = ({x, y}) => sqrt(x *. x +.  y *. y);
    let print = t => Printf.printf("{x:%f,y:%f}\n%!", t.x, t.y);
    let ofIntPt = t => map(t, ~f=float_of_int);
    let (+) = add;
    let (-) = sub;
    let (/@) = divScalar;
    let (+@) = addScalar;
  };

  //   let fromIntPair = ((x, y)) => {x: float_of_int(x), y: float_of_int(y)};
  // let moveTo = (src, ~dest, ~speed) => {
  //   let dist = Reprocessing.Utils.distf(~p1=toPair(src), ~p2=toPair(dest));
  //   if (dist <= speed) {
  //     dest;
  //   } else {
  //     let {x: srcX, y: srcY} = src;
  //     let {x: destX, y: destY} = dest;
  //     let mult = speed /. dist;
  //     let d = create((destX -. srcX) *. mult, (destY -. srcY) *. mult);
  //     addf(src, d);
  //   };
  // };
};

module StringMap = Map.Make(String);