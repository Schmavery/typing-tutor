open Reprocessing;
open Printf;
open Std;

let tileSize = 32;

module Robot = {
  type t = {
    id: int,
    pos: Point.t(float),
    code: Editor.t,
    say: option(string),
  };

  let create = {
    let counter = ref(0);
    (x, y) => {
      let id = counter^;
      incr(counter);
      {id, say: None, code: Editor.create(), pos: Point.Float.ofIntPt(Point.create(x * tileSize, y * tileSize))}
    };
  }
};

type state = {
  time: float,
  font: fontT,
  sprites: Sprite.t,
  map: GameMap.t,
  player: Point.t(float),
  editing: option(Robot.t),
  robots: list(Robot.t),
};


let setup = (env) => {
  Env.size(~width=800, ~height=600, env);
  let font = Draw.loadFont(~filename="./monogram.fnt", ~isPixel=true, env);
  let sprites = Sprite.load("./sprites.png", Sprite.spritesData, env);
  let map = GameMap.createGrid(GameMap.mapString);
  let robots = [Robot.create(7, 4), Robot.create(9, 6)];
  {time: 0., font, sprites, map, robots, editing: None, player: Point.Float.ofIntPt(Point.create(7 * tileSize, 4 * tileSize))}
};

let keyTyped = ({editing} as state, env) => {
  {...state, editing: Option.map(editing, ~f=(e => {...e, code: Editor.keyTyped(e.code, env)}))}
};

let drawBg = (offset: Point.t(float), grid, sprites, env) => {
  Array.iteri(
    (x, col) =>
      Array.iteri(
        (y, tile) =>
          {
            let tileOffset = Point.Int.(create(tileSize * x, tileSize * y)) |> Point.map(~f=float_of_int);
            let pos = Point.Float.(tileOffset + offset);
            switch (tile) {
          | `Unknown => Sprite.draw(sprites, ~name="unknown", ~pos, env);
          | `Floor => Sprite.draw(sprites, ~name="floor", ~pos, env);
          | `Wall(kind) => Sprite.draw(sprites, ~name="wall_" ++ kind, ~pos, env);
          | `Nothing => ()
          }},
        col,
      ),
    grid,
  );
};

let pointCollides = (pt, grid) => {
  let tilePt : Point.t(int) = Point.Int.(ofFloatPt(pt) /@ tileSize);
  switch (Array.get(grid, tilePt.x)) {
    | col => switch (Array.get(col, tilePt.y)) {
      | `Wall(_) => true
      | _ => false
      | exception Invalid_argument(_) => true
    }
    | exception Invalid_argument(_) => true
  }
};

let calcPosition = (player, grid, env) : Point.t(float) => {
  let speed = 3.;

  let xOffset = (Env.key(Right, env) || Env.key(D, env) ? speed : 0.);
  let yOffset = (Env.key(Up, env) || Env.key(W, env) ? -. speed : 0.);
  let xOffset = xOffset +. (Env.key(Left, env) || Env.key(A, env) ? -. speed : 0.);
  let yOffset = yOffset +. (Env.key(Down, env) || Env.key(S, env) ? speed : 0.);

  let newPos = Point.Float.(player + create(xOffset, yOffset));
  let newXPos = Point.Float.(player + create(xOffset, 0.));
  let newYPos = Point.Float.(player + create(0., yOffset));

  let noCollision = p => {
    let tsf = float_of_int(tileSize);
    !pointCollides(p, grid) &&
    !pointCollides(Point.Float.(p + create(tsf, 0.)), grid) &&    
    !pointCollides(Point.Float.(p + create(0., tsf)), grid) &&    
    !pointCollides(Point.Float.(p +@ tsf), grid)
  };

  if (noCollision(newPos)) {
    newPos
  } else if (noCollision(newXPos)) {
    newXPos
  } else if (noCollision(newYPos)) {
    newYPos
  } else {
    player
  }
};

let getCloseRobot({player, robots}, closeDist) = {
  let dist = (r: Robot.t) => Point.Float.(mag(player - r.pos));
  let rec getClosestRobot = (l, acc) => {
    switch (l, acc) {
      | ([hd, ...tl], None) =>
        getClosestRobot(tl, Some((hd, dist(hd))))
      | ([hd, ...tl], Some((_, crd))) when dist(hd) < crd =>
        getClosestRobot(tl, Some((hd, dist(hd))))
      | ([_, ...tl], acc) =>
        getClosestRobot(tl, acc)
      | ([], Some((robot, _))) => Some(robot)
      | ([], None) => None
    }
  };
  switch (getClosestRobot(robots, None)) {
    | Some(robot) as r when dist(robot) < closeDist => r
    | _ => None
  };
};


let draw = (state, env) => {
  let center = Point.create(Env.width(env) / 2, Env.height(env) / 2) |> Point.map(~f=float_of_int);  
  let closeRobot = getCloseRobot(state, float_of_int(tileSize));

  switch (state.editing) {
    | Some(robot) =>
      let pad = 20;
      let rect : Editor.rect = {
        x: Env.width(env) / 3 * 2,
        y: pad,
        w: Env.width(env) / 3,
        h: Env.height(env) - pad - pad
      };
      Editor.draw(rect, Constants.white, robot.code, state.font, env);
    | None =>
      Draw.background(Constants.black, env);
      Draw.tint(Constants.white, env);
      drawBg(Point.Float.(center - state.player), state.map, state.sprites, env);
      List.iter((robot : Robot.t) => 
        {
          let isClose = closeRobot
            |> Option.map(~f=(close : Robot.t) => close.id == robot.id)
            |> Option.get(~default=false);

          Draw.tint(isClose ? Utils.color(~r=150, ~g=150, ~b=150, ~a=255) : Constants.white, env);
          Sprite.draw(state.sprites, ~name="robot", ~pos=Point.Float.(center - state.player + robot.pos), env)
        },
        state.robots);
      Draw.tint(Constants.white, env);
      Sprite.draw(state.sprites, ~name="player", ~pos=center, env);
  };

  let (editing, robots) = switch (state.editing){
    | Some(curr : Robot.t) when Env.keyReleased(Escape, env) =>
      (None, List.map((r : Robot.t) => r.id == curr.id ? curr : r, state.robots))
    | None when Env.keyReleased(Enter, env) => (closeRobot, state.robots)
    | _ => (state.editing, state.robots)
  };

  // let robots = List.map(
  //   robot => {
  //     // Check if collision
  //   },
  //   state.robots
  // );

  let player = switch (state.editing) {
    | Some(_) => state.player
    | None => calcPosition(state.player, state.map, env)
  };


  {...state, editing, time: state.time +. Env.deltaTime(env), player, robots}
};

run(~setup, ~draw, ~keyTyped, ());
