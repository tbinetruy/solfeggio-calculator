open Theory
open Theory.Interval
open Theory.Note
open Jest
open Belt

describe("relative_intervals_of_notes", () => {
  open Expect;

  test("major triad", () =>
    expect(list{C(Natural), E(Natural), G(Natural)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Major->Third, Minor->Third}))

  test("minor triad", () =>
    expect(list{C(Natural), E(Flat), G(Natural)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Minor->Third, Major->Third}))

  test("diminished triad", () =>
    expect(list{C(Natural), E(Flat), G(Flat)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Minor->Third, Minor->Third}))

  test("major seventh", () =>
    expect(list{C(Natural), E(Natural), G(Natural), B(Natural)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Major->Third, Minor->Third, Major->Third}))

  test("minor seventh", () =>
    expect(list{C(Natural), E(Flat), G(Natural), B(Flat)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Minor->Third, Major->Third, Minor->Third}))

  test("dominant seventh", () =>
    expect(list{C(Natural), E(Natural), G(Natural), B(Flat)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Major->Third, Minor->Third, Minor->Third}))

  test("diminished interval", () =>
    expect(list{E(Flat), G(DoubleFlat)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Diminished->Third}))

  test("half diminished seventh", () =>
    expect(list{C(Natural), E(Flat), G(Flat), B(Flat)}->relativeIntervals_of_notes(list{}))
    ->toEqual(list{Minor->Third, Minor->Third, Major->Third}))
})

describe("chord_of_intervals", () => {
  open Expect;

  test("minor triad", () =>
    expect(list{Minor->Third, Major->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(MinorTriad)))

  test("major triad", () =>
    expect(list{Major->Third, Minor->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(MajorTriad)))

  test("diminished triad", () =>
    expect(list{Minor->Third, Minor->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(DiminishedTriad)))

  test("minor seventh", () =>
    expect(list{Minor->Third, Major->Third, Minor->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(MinorSeventh)))

  test("major seventh", () =>
    expect(list{Major->Third, Minor->Third, Major->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(MajorSeventh)))

  test("half diminished seventh", () =>
    expect(list{Minor->Third, Minor->Third, Major->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(HalfDiminishedSeventh)))

  test("dominant seventh", () =>
    expect(list{Major->Third, Minor->Third, Minor->Third}->chord_of_relativeIntervals)
    ->toEqual(Result.Ok(DominanteSeventh)))
});


describe("harmonize scale", () => {
  open Expect;

  test("zip", () => {
    let l1 = list{C(Natural), C(Natural)}
    let l2 = list{D(Natural), D(Natural)}
    let l3 = list{E(Natural), E(Natural)}
    let expected = list{
      list{C(Natural), D(Natural), E(Natural)},
      list{C(Natural), D(Natural), E(Natural)},
    }
    expect(list{l1, l2, l3}->transpose)
    ->toEqual(expected)
  })
});

describe("harmonize scale", () => {
  open Expect;

  test("transpose", () => {
    let major_scale = C(Natural)->buildScale(MajorScale)
    let phrygian_mode = E(Natural)->buildScale(PhrygianMode)
    let mixolidian_mode = G(Natural)->buildScale(MixolydianMode)
    let expected = list{
      list{C(Natural), E(Natural), G(Natural)},
      list{D(Natural), F(Natural), A(Natural)},
      list{E(Natural), G(Natural), B(Natural)},
      list{F(Natural), A(Natural), C(Natural)},
      list{G(Natural), B(Natural), D(Natural)},
      list{A(Natural), C(Natural), E(Natural)},
      list{B(Natural), D(Natural), F(Natural)},
      list{C(Natural), E(Natural), G(Natural)},
    }
    expect(list{major_scale, phrygian_mode, mixolidian_mode}->transpose)
    ->toEqual(expected)
  })

  test("harmonization_matrix", () => {
    let harmonization_matrix = C(Natural)->buildScale(MajorScale)->get_harmonization_matrix
    let expected = G(Natural)->stackIntervalsRelatively(MajorScale->relativeIntervals_of_scale->get_nth_mode(4))

    expect(harmonization_matrix->List.getExn(4))
    ->toEqual(expected)
  })

  test("harmonize C major scale with triads", () => {
    let scale = C(Natural)->buildScale(MajorScale)
    let scale_harmonization = scale->harmonize_scale_with_triads

    let expected = list{
      MajorTriad,
      MinorTriad,
      MinorTriad,
      MajorTriad,
      MajorTriad,
      MinorTriad,
      DiminishedTriad,
      MajorTriad,
    }->Result.Ok

    expect(scale_harmonization)
    ->toEqual(expected)
  })

  test("harmonize C major scale with tetrads", () => {
    let scale = C(Natural)->buildScale(MajorScale)
    let scale_harmonization = scale->harmonize_scale_with_tetrades

    let expected = list{
      MajorSeventh,
      MinorSeventh,
      MinorSeventh,
      MajorSeventh,
      DominanteSeventh,
      MinorSeventh,
      HalfDiminishedSeventh,
      MajorSeventh,
    }->Result.Ok

    expect(scale_harmonization)
    ->toEqual(expected)
  })
});
