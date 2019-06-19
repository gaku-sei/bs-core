open Jest;
open Expect;
open! Expect.Operators;
open! BsCore;

describe("Option", () => {
  open! Option;

  test("map some", () =>
    expect((+)(20) <$> Some(22)) == Some(42)
  );

  test("map none", () =>
    expect((+)(20) <$> None) == None
  );

  test("apply some", () =>
    expect(Some((+)(20)) <*> Some(22)) == Some(42)
  );

  test("apply none", () =>
    expect(Some((+)(20)) <*> None) == None
  );

  test("bind some", () =>
    expect(Some(22) >>= ((+)(20) >> some)) == Some(42)
  );

  test("bind none", () =>
    expect(Some(22) >>= const(None)) == None
  );

  test("withDefault some", () =>
    expect(Some(42) |> withDefault(0)) == 42
  );

  test("withDefault none", () =>
    expect(None |> withDefault(42)) == 42
  );
});

describe("Result", () => {
  open! Result;

  test("map ok", () =>
    expect((+)(20) <$> Ok(22)) == Ok(42)
  );

  test("map error", () =>
    expect((+)(20) <$> Error("error")) == Error("error")
  );

  test("apply ok", () =>
    expect(Ok((+)(20)) <*> Ok(22)) == Ok(42)
  );

  test("apply error", () =>
    expect(Ok((+)(20)) <*> Error("error")) == Error("error")
  );

  test("bind ok", () =>
    expect(Ok(22) >>= ((+)(20) >> ok)) == Ok(42)
  );

  test("bind error", () =>
    expect(Ok(22) >>= const(Error("error"))) == Error("error")
  );

  test("withDefault ok", () =>
    expect(Ok(42) |> withDefault(0)) == 42
  );

  test("withDefault error", () =>
    expect(Error("error") |> withDefault(42)) == 42
  );

  test("hush ok", () =>
    expect(Ok(42) |> hush) == Some(42)
  );

  test("hush error", () =>
    expect(Error("error") |> hush) == None
  );

  test("note ok", () =>
    expect(Some(42) |> note("error")) == Ok(42)
  );

  test("note error", () =>
    expect(None |> note("error")) == Error("error")
  );

  test("mapError ok", () =>
    expect(Ok(42) |> mapError(const("error"))) == Ok(42)
  );

  test("mapError error", () =>
    expect(Error(42) |> mapError(const("error"))) == Error("error")
  );
});