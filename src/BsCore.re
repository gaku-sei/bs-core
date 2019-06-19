include InnerResult;

let xor: (bool, bool) => bool =
  (x, y) =>
    switch (x, y) {
    | (true, true) => false
    | (false, false) => false
    | (true, false) => true
    | (false, true) => true
    };

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, x, y) => f(y, x);

let identity: 'a => 'a = x => x;

let const: ('a, 'b) => 'a = (x, _) => x;

let (<|): ('a => 'b, 'a) => 'b = (f, x) => f(x);

let (|>): ('a, 'a => 'b) => 'b = (x, f) => f(x);

let (<<): ('b => 'c, 'a => 'b, 'a) => 'c = (f, g, x) => f(g(x));

let (>>): ('a => 'b, 'b => 'c, 'a) => 'c = (f, g, x) => g(f(x));

module Option = {
  let some: 'a => option('a) = x => Some(x);

  let map: ('a => 'b, option('a)) => option('b) =
    f =>
      fun
      | None => None
      | Some(x) => Some(f(x));

  let (<$>) = map;

  let apply: (option('a => 'b), option('a)) => option('b) =
    (f, r) =>
      switch (f, r) {
      | (Some(f), _) => r |> map(f)
      | (None, _) => None
      };

  let (<*>) = apply;

  let bind: ('a => option('b), option('a)) => option('b) =
    f =>
      fun
      | None => None
      | Some(x) => f(x);

  let (>>=) = (f, o) => bind(o, f);

  let withDefault: ('a, option('a)) => 'a =
    x =>
      fun
      | None => x
      | Some(x) => x;
};

module Result = {
  let ok: 'a => result('a, 'error) = ok => Ok(ok);

  let error: 'a => result('a, 'error) = error => Error(error);

  let map: ('a => 'b, result('a, 'error)) => result('b, 'error) =
    f =>
      fun
      | Error(error) => Error(error)
      | Ok(x) => Ok(f(x));

  let (<$>) = map;

  let apply:
    (result('a => 'b, 'error), result('a, 'error)) => result('b, 'error) =
    (f, r) => {
      switch (f, r) {
      | (Error(error), _) => Error(error)
      | (Ok(f), _) => r |> map(f)
      };
    };

  let (<*>) = apply;

  let bind:
    ('a => result('b, 'error), result('a, 'error)) => result('b, 'error) =
    f =>
      fun
      | Error(error) => Error(error)
      | Ok(x) => f(x);

  let (>>=) = (f, r) => bind(r, f);

  let withDefault: ('a, result('a, 'error)) => 'a =
    x =>
      fun
      | Error(_) => x
      | Ok(x) => x;

  let hush: result('a, 'error) => option('a) =
    fun
    | Error(_) => None
    | Ok(x) => Some(x);

  let note: ('error, option('a)) => result('a, 'error) =
    e =>
      fun
      | None => Error(e)
      | Some(x) => Ok(x);

  let mapError:
    ('errorA => 'errorB, result('a, 'errorA)) => result('a, 'errorB) =
    f =>
      fun
      | Error(error) => Error(f(error))
      | Ok(x) => Ok(x);
};