open Fretboard.FretNumbers
open Jest

describe("FretNumbers.buildNumbers", () => {
  open Expect;

  test("zfills list of more than 1 element properly starting from 0", () =>
    expect(list{1, 5, 7}->buildNumbers(0, list{}))
    ->toEqual(list{0, 1, 0, 0, 0, 5, 0, 7}))

  test("zfills empty list properly", () =>
    expect(list{}->buildNumbers(0, list{}))
    ->toEqual(list{}))

  test("fills list of 0 element properly starting from 1", () =>
    expect(list{1, 3}->buildNumbers(1, list{}))
    ->toEqual(list{1, 0, 3}))
});
