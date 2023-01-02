open Theory
open Theory.Harmonization
open Theory.Interval
open Theory.Intervals
open Theory.Note
open Theory.Scale
open Theory.Chord
open Jest
open Belt

describe("interval_of_semitones", () => {
  open Expect
  let from_semitones = Interval.from_semitones

  test("1 notes and 0 semitones", () => expect(from_semitones(1, 0))->toEqual(Result.Ok(Unison)))

  test("1 notes and 20 semitones", () =>
    expect(from_semitones(1, 20))->toEqual(Interval.Errors.unison_semitones)
  )

  test("2 notes and 10 semitones", () =>
    expect(from_semitones(2, 10))->toEqual(ThirdQualifier.Errors.semitones)
  )

  test("2 notes and 1 semitones", () =>
    expect(from_semitones(2, 1))->toEqual(Result.Ok(Minor->Second))
  )

  test("2 notes and 2 semitones", () =>
    expect(from_semitones(2, 2))->toEqual(Result.Ok(Major->Second))
  )

  test("3 notes and 2 semitones", () =>
    expect(from_semitones(3, 2))->toEqual(Result.Ok(Diminished->Third))
  )

  test("3 notes and 3 semitones", () =>
    expect(from_semitones(3, 3))->toEqual(Result.Ok(Minor->Third))
  )

  test("3 notes and 4 semitones", () =>
    expect(from_semitones(3, 4))->toEqual(Result.Ok(Major->Third))
  )

  test("3 notes and 5 semitones", () =>
    expect(from_semitones(3, 5))->toEqual(Result.Ok(Augmented->Third))
  )

  test("4 notes and 4 semitones", () =>
    expect(from_semitones(4, 4))->toEqual(Result.Ok(Diminished->Fourth))
  )

  test("4 notes and 5 semitones", () =>
    expect(from_semitones(4, 5))->toEqual(Result.Ok(Perfect->Fourth))
  )

  test("4 notes and 6 semitones", () =>
    expect(from_semitones(4, 6))->toEqual(Result.Ok(Augmented->Fourth))
  )

  test("5 notes and 6 semitones", () =>
    expect(from_semitones(5, 6))->toEqual(Result.Ok(Diminished->Fifth))
  )

  test("5 notes and 7 semitones", () =>
    expect(from_semitones(5, 7))->toEqual(Result.Ok(Perfect->Fifth))
  )

  test("5 notes and 8 semitones", () =>
    expect(from_semitones(5, 8))->toEqual(Result.Ok(Augmented->Fifth))
  )

  test("6 notes and 7 semitones", () =>
    expect(from_semitones(6, 7))->toEqual(Result.Ok(Diminished->Sixth))
  )

  test("6 notes and 8 semitones", () =>
    expect(from_semitones(6, 8))->toEqual(Result.Ok(Minor->Sixth))
  )

  test("6 notes and 9 semitones", () =>
    expect(from_semitones(6, 9))->toEqual(Result.Ok(Major->Sixth))
  )

  test("6 notes and 10 semitones", () =>
    expect(from_semitones(6, 10))->toEqual(Result.Ok(Augmented->Sixth))
  )

  test("7 notes and 9 semitones", () =>
    expect(from_semitones(7, 9))->toEqual(Result.Ok(Diminished->Seventh))
  )

  test("7 notes and 10 semitones", () =>
    expect(from_semitones(7, 10))->toEqual(Result.Ok(Minor->Seventh))
  )

  test("7 notes and 11 semitones", () =>
    expect(from_semitones(7, 11))->toEqual(Result.Ok(Major->Seventh))
  )

  test("7 notes and 12 semitones", () =>
    expect(from_semitones(7, 12))->toEqual(Result.Ok(Augmented->Seventh))
  )

  test("8 notes and 12 semitones", () => expect(from_semitones(8, 12))->toEqual(Result.Ok(Octave)))

  test("9 notes and 12 semitones", () =>
    expect(from_semitones(9, 12))->toEqual(Interval.Errors.nNotes_too_large)
  )

  test("8 notes and 20 semitones", () =>
    expect(from_semitones(7, 20))->toEqual(Interval.ThirdQualifier.Errors.semitones)
  )
})

describe("addIntervals", () => {
  open Expect

  test("Minor->Second + Minor->Second", () =>
    expect(addIntervals(Minor->Second, Minor->Second))->toEqual(Result.Ok(Diminished->Third))
  )

  test("Minor->Second + Major->Second", () =>
    expect(addIntervals(Minor->Second, Major->Second))->toEqual(Result.Ok(Minor->Third))
  )

  test("Major->Second + Minor->Second", () =>
    expect(addIntervals(Major->Second, Minor->Second))->toEqual(Result.Ok(Minor->Third))
  )

  test("Perfect->Fourth + Perfect->Fifth", () =>
    expect(addIntervals(Perfect->Fourth, Perfect->Fifth))->toEqual(Result.Ok(Octave))
  )

  test("Minor->Third + Major->Third", () =>
    expect(addIntervals(Minor->Third, Major->Third))->toEqual(Result.Ok(Perfect->Fifth))
  )

  test("Perfect->Fifth + Major->Third", () =>
    expect(addIntervals(Perfect->Fifth, Major->Third))->toEqual(Result.Ok(Major->Seventh))
  )

  test("Major->Seventh + Major->Seventh", () =>
    expect(addIntervals(Major->Seventh, Major->Seventh))->toEqual(Interval.Errors.nNotes_too_large)
  )
})

describe("absolute_intervals_of_notes", () => {
  open Expect

  test("major triad", () =>
    expect(list{C(Natural), E(Natural), G(Natural)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Major->Third, Minor->Third}),
    )
  )

  test("minor triad", () =>
    expect(list{C(Natural), E(Flat), G(Natural)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Minor->Third, Major->Third}),
    )
  )

  test("diminished triad", () =>
    expect(list{C(Natural), E(Flat), G(Flat)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Minor->Third, Minor->Third}),
    )
  )

  test("major seventh", () =>
    expect(
      list{C(Natural), E(Natural), G(Natural), B(Natural)}->relativeIntervals_of_notes,
    )->toEqual(Relative(list{Major->Third, Minor->Third, Major->Third}))
  )

  test("minor seventh", () =>
    expect(list{C(Natural), E(Flat), G(Natural), B(Flat)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Minor->Third, Major->Third, Minor->Third}),
    )
  )

  test("dominant seventh", () =>
    expect(list{C(Natural), E(Natural), G(Natural), B(Flat)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Major->Third, Minor->Third, Minor->Third}),
    )
  )

  test("diminished interval", () =>
    expect(list{E(Flat), G(DoubleFlat)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Diminished->Third}),
    )
  )

  test("half diminished seventh", () =>
    expect(list{C(Natural), E(Flat), G(Flat), B(Flat)}->relativeIntervals_of_notes)->toEqual(
      Relative(list{Minor->Third, Minor->Third, Major->Third}),
    )
  )
})

describe("chord_of_intervals", () => {
  open Expect
  open Intervals

  test("minor triad", () =>
    expect(Absolute(list{Minor->Third, Perfect->Fifth})->to_chord)->toEqual(Result.Ok(MinorTriad))
  )

  test("major triad", () =>
    expect(Absolute(list{Major->Third, Perfect->Fifth})->to_chord)->toEqual(Result.Ok(MajorTriad))
  )

  test("diminished triad", () =>
    expect(Absolute(list{Minor->Third, Diminished->Fifth})->to_chord)->toEqual(
      Result.Ok(DiminishedTriad),
    )
  )

  test("minor seventh", () =>
    expect(Absolute(list{Minor->Third, Perfect->Fifth, Minor->Seventh})->to_chord)->toEqual(
      Result.Ok(MinorSeventh),
    )
  )

  test("major seventh", () =>
    expect(Absolute(list{Major->Third, Perfect->Fifth, Major->Seventh})->to_chord)->toEqual(
      Result.Ok(MajorSeventh),
    )
  )

  test("half diminished seventh", () =>
    expect(Absolute(list{Minor->Third, Diminished->Fifth, Minor->Seventh})->to_chord)->toEqual(
      Result.Ok(HalfDiminishedSeventh),
    )
  )

  test("diminished seventh", () =>
    expect(Absolute(list{Minor->Third, Diminished->Fifth, Diminished->Seventh})->to_chord)->toEqual(
      Result.Ok(DiminishedSeventh),
    )
  )

  test("dominant seventh", () =>
    expect(Absolute(list{Major->Third, Perfect->Fifth, Minor->Seventh})->to_chord)->toEqual(
      Result.Ok(DominanteSeventh),
    )
  )
})

describe("harmonize scale", () => {
  open Expect

  test("zip", () => {
    let l1 = list{C(Natural), C(Natural)}
    let l2 = list{D(Natural), D(Natural)}
    let l3 = list{E(Natural), E(Natural)}
    let expected = list{
      list{C(Natural), D(Natural), E(Natural)},
      list{C(Natural), D(Natural), E(Natural)},
    }
    expect(list{l1, l2, l3}->transpose)->toEqual(expected)
  })
})

describe("harmonize scale", () => {
  open Expect

  test("transpose", () => {
    let major_scale = C(Natural)->Scale.to_notes(MajorScale)
    let phrygian_mode = E(Natural)->Scale.to_notes(PhrygianMode)
    let mixolidian_mode = G(Natural)->Scale.to_notes(MixolydianMode)
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
    expect(list{major_scale, phrygian_mode, mixolidian_mode}->transpose)->toEqual(expected)
  })

  test("absoluteIntervals_of_relativeIntervals", () => {
    let relativeIntervals = MajorScale->Scale.to_intervals
    let absoluteIntervals = relativeIntervals->to_absolute
    let expected = Absolute(list{
      Major->Second,
      Major->Third,
      Perfect->Fourth,
      Perfect->Fifth,
      Major->Sixth,
      Major->Seventh,
      Octave,
    })
    expect(absoluteIntervals)->toEqual(Result.Ok(expected))
  })

  test("harmonization_matrix", () => {
    let harmonization_matrix = MajorScale->get_harmonization_matrix
    let expected = MajorScale->Scale.to_intervals->get_nth_mode(4)

    expect(harmonization_matrix->List.getExn(4))->toEqual(expected)
  })

  test("harmonize major scale with triads", () => {
    let scale_harmonization = MajorScale->Harmonization.to_triads

    let expected =
      list{
        MajorTriad,
        MinorTriad,
        MinorTriad,
        MajorTriad,
        MajorTriad,
        MinorTriad,
        DiminishedTriad,
      }->Result.Ok

    expect(scale_harmonization)->toEqual(expected)
  })

  test("harmonize major scale with tetrads", () => {
    let scale_harmonization = MajorScale->Harmonization.to_tetrads

    let expected =
      list{
        MajorSeventh,
        MinorSeventh,
        MinorSeventh,
        MajorSeventh,
        DominanteSeventh,
        MinorSeventh,
        HalfDiminishedSeventh,
      }->Result.Ok

    expect(scale_harmonization)->toEqual(expected)
  })

  test("harmonize C major scale with triads", () => {
    let scale_harmonization = MajorScale->Harmonization.to_triad_progression(C(Natural))

    let expected =
      list{
        C(Natural)->Chord.to_notes(MajorTriad),
        D(Natural)->Chord.to_notes(MinorTriad),
        E(Natural)->Chord.to_notes(MinorTriad),
        F(Natural)->Chord.to_notes(MajorTriad),
        G(Natural)->Chord.to_notes(MajorTriad),
        A(Natural)->Chord.to_notes(MinorTriad),
        B(Natural)->Chord.to_notes(DiminishedTriad),
      }->Result.Ok

    expect(scale_harmonization)->toEqual(expected)
  })

  test("harmonize C major scale with tetrads", () => {
    let scale_harmonization = MajorScale->Harmonization.to_tetrad_progression(C(Natural))

    let expected =
      list{
        C(Natural)->Chord.to_notes(MajorSeventh),
        D(Natural)->Chord.to_notes(MinorSeventh),
        E(Natural)->Chord.to_notes(MinorSeventh),
        F(Natural)->Chord.to_notes(MajorSeventh),
        G(Natural)->Chord.to_notes(DominanteSeventh),
        A(Natural)->Chord.to_notes(MinorSeventh),
        B(Natural)->Chord.to_notes(HalfDiminishedSeventh),
      }->Result.Ok

    expect(scale_harmonization)->toEqual(expected)
  })

  test("harmonize dorian mode with tetrads", () => {
    let scale_harmonization = DorianMode->to_tetrads

    let expected =
      list{
        MinorSeventh,
        MinorSeventh,
        MajorSeventh,
        DominanteSeventh,
        MinorSeventh,
        HalfDiminishedSeventh,
        MajorSeventh,
      }->Result.Ok

    expect(scale_harmonization)->toEqual(expected)
  })
})
