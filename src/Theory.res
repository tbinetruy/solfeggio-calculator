open Belt

let semitones_in_octave = 12

module Accidental = {
  type accidental =
    | DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp

  let to_string = accidental => {
    switch accidental {
    | Flat => "b"
    | DoubleFlat => "bb"
    | Natural => ""
    | DoubleSharp => "##"
    | Sharp => "#"
    }
  }

  let to_semitones = accidental => {
    switch accidental {
    | DoubleFlat => -2
    | Flat => -1
    | Natural => 0
    | Sharp => 1
    | DoubleSharp => 2
    }
  }
}

module Note = {
  open Accidental

  type note =
    | C(accidental)
    | D(accidental)
    | E(accidental)
    | F(accidental)
    | G(accidental)
    | A(accidental)
    | B(accidental)

  let setAccidental = (note, accidental) =>
    switch note {
    | C(_) => C(accidental)
    | D(_) => D(accidental)
    | E(_) => E(accidental)
    | F(_) => F(accidental)
    | G(_) => G(accidental)
    | A(_) => A(accidental)
    | B(_) => B(accidental)
    }

  let is_same_note_familly = (noteA, noteB) =>
    switch (noteA, noteB) {
    | (C(_), C(_))
    | (D(_), D(_))
    | (E(_), E(_))
    | (F(_), F(_))
    | (G(_), G(_))
    | (A(_), A(_))
    | (B(_), B(_)) => true
    | (_, _) => false
    }

  let to_string = note =>
    switch note {
    | C(accidental) => "C" ++ accidental->to_string
    | D(accidental) => "D" ++ accidental->to_string
    | E(accidental) => "E" ++ accidental->to_string
    | F(accidental) => "F" ++ accidental->to_string
    | G(accidental) => "G" ++ accidental->to_string
    | A(accidental) => "A" ++ accidental->to_string
    | B(accidental) => "B" ++ accidental->to_string
    }

  let getNextNote = note =>
    switch note {
    | C(accidental) => D(accidental)
    | D(accidental) => E(accidental)
    | E(accidental) => F(accidental)
    | F(accidental) => G(accidental)
    | G(accidental) => A(accidental)
    | A(accidental) => B(accidental)
    | B(accidental) => C(accidental)
    }

  let to_semitones = note =>
    switch note {
    | C(accidental) => 0 + accidental->to_semitones
    | D(accidental) => 2 + accidental->to_semitones
    | E(accidental) => 4 + accidental->to_semitones
    | F(accidental) => 5 + accidental->to_semitones
    | G(accidental) => 7 + accidental->to_semitones
    | A(accidental) => 9 + accidental->to_semitones
    | B(accidental) => 11 + accidental->to_semitones
    }

  let rec getNthNote = (rootNote, n) =>
    switch n {
    | 0 => rootNote
    | _ => getNthNote(rootNote->getNextNote, n - 1)
    }

  let semitonesBetweenNotes = (noteA, noteB) => {
    let delta = noteB->to_semitones - noteA->to_semitones
    delta < 0 ? semitones_in_octave + delta : delta
  }
}

open Note

type semitone = int

/*
   for second, third, sixth and seventh:
     quality =
               ...
               -3 => doubly diminished
               -2 => diminished
               -1 => minor
               0 => major
               1 => augmented
               2 => doubly augmented
               ...

   for fourth and fifth:
     quality =
                ...
                -2 => doubly diminished
                -1 => diminished
                0 => perfect
                1 => augmented
                2 => doubly augmented
                ...
   quality: 0 is either major or perfect
 */

module Interval = {
  module FifthQualifier = {
    type t =
      | Diminished
      | Perfect
      | Augmented

    module Errors = {
      let semitones = Result.Error("Semitones greater that 1 or smaller that -1 are not supported.")
    }

    let to_semitones = qualifier =>
      switch qualifier {
      | Diminished => -1
      | Perfect => 0
      | Augmented => 1
      }

    let qualifier_of_semitones = semitones =>
      switch semitones {
      | -1 => Result.Ok(Diminished)
      | 0 => Result.Ok(Perfect)
      | 1 => Result.Ok(Augmented)
      | _ => Errors.semitones
      }

    let to_string = qualifier =>
      switch qualifier {
      | Diminished => "diminished"
      | Perfect => "perfect"
      | Augmented => "augmented"
      }
  }

  module ThirdQualifier = {
    type t =
      | Diminished
      | Minor
      | Major
      | Augmented

    module Errors = {
      let semitones = Result.Error("Semitones greater that 1 or smaller that -2 are not supported.")
    }

    let to_semitones = qualifier =>
      switch qualifier {
      | Diminished => -2
      | Minor => -1
      | Major => 0
      | Augmented => 1
      }

    let qualifier_of_semitones = semitones =>
      switch semitones {
      | -2 => Result.Ok(Diminished)
      | -1 => Result.Ok(Minor)
      | 0 => Result.Ok(Major)
      | 1 => Result.Ok(Augmented)
      | _ => Errors.semitones
      }

    let to_string = qualifier =>
      switch qualifier {
      | Diminished => "diminished"
      | Minor => "minor"
      | Major => "major"
      | Augmented => "augmented"
      }
  }

  type interval =
    | Unison
    | Second(ThirdQualifier.t)
    | Third(ThirdQualifier.t)
    | Fourth(FifthQualifier.t)
    | Fifth(FifthQualifier.t)
    | Sixth(ThirdQualifier.t)
    | Seventh(ThirdQualifier.t)
    | Octave

  module Errors = {
    let unison_semitones = Result.Error("semitones != 1 are not supported with Unison")
    let octave_semitones = Result.Error("semitones != 12 are not supported with Octave")
    let nNotes_too_large = Result.Error("nNotes > 8 (Octave) are not supported")
  }

  let to_semitones = interval =>
    switch interval {
    | Unison => C(Natural)->to_semitones
    | Second(qualifier) => D(Natural)->to_semitones + qualifier->ThirdQualifier.to_semitones
    | Third(qualifier) => E(Natural)->to_semitones + qualifier->ThirdQualifier.to_semitones
    | Fourth(qualifier) => F(Natural)->to_semitones + qualifier->FifthQualifier.to_semitones
    | Fifth(qualifier) => G(Natural)->to_semitones + qualifier->FifthQualifier.to_semitones
    | Sixth(qualifier) => A(Natural)->to_semitones + qualifier->ThirdQualifier.to_semitones
    | Seventh(qualifier) => B(Natural)->to_semitones + qualifier->ThirdQualifier.to_semitones
    | Octave => semitones_in_octave
    }

  let nNotes_of_interval = interval =>
    switch interval {
    | Unison => 1
    | Second(_) => 2
    | Third(_) => 3
    | Fourth(_) => 4
    | Fifth(_) => 5
    | Sixth(_) => 6
    | Seventh(_) => 7
    | Octave => 8
    }

  let interval_of_semitones = (nNotes, nSemitones) =>
    switch nNotes {
    | 1 =>
      if nSemitones == 0 {
        Result.Ok(Unison)
      } else {
        Errors.unison_semitones
      }
    | 2 =>
      let qualifier =
        (nSemitones - Major->Second->to_semitones)->ThirdQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Second(qualifier))
    | 3 =>
      let qualifier =
        (nSemitones - Major->Third->to_semitones)->ThirdQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Third(qualifier))
    | 4 =>
      let qualifier =
        (nSemitones - Perfect->Fourth->to_semitones)->FifthQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Fourth(qualifier))
    | 5 =>
      let qualifier =
        (nSemitones - Perfect->Fifth->to_semitones)->FifthQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Fifth(qualifier))
    | 6 =>
      let qualifier =
        (nSemitones - Major->Sixth->to_semitones)->ThirdQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Sixth(qualifier))
    | 7 =>
      let qualifier =
        (nSemitones - Major->Seventh->to_semitones)->ThirdQualifier.qualifier_of_semitones
      qualifier->Result.map(qualifier => Seventh(qualifier))
    | 8 =>
      if nSemitones == 12 {
        Result.Ok(Octave)
      } else {
        Errors.octave_semitones
      }
    | _ => Errors.nNotes_too_large
    }

  let addIntervals = (intervalA, intervalB) => {
    let nNotes = intervalA->nNotes_of_interval + intervalB->nNotes_of_interval - 1
    let nSemitones = intervalA->to_semitones + intervalB->to_semitones
    interval_of_semitones(nNotes, nSemitones)
  }

  let to_string = interval =>
    switch interval {
    | Unison => "unison"
    | Second(qualifier) => qualifier->ThirdQualifier.to_string ++ " second"
    | Third(qualifier) => qualifier->ThirdQualifier.to_string ++ " third"
    | Fourth(qualifier) => qualifier->FifthQualifier.to_string ++ " fourth"
    | Fifth(qualifier) => qualifier->FifthQualifier.to_string ++ " fifth"
    | Sixth(qualifier) => qualifier->ThirdQualifier.to_string ++ " sixth"
    | Seventh(qualifier) => qualifier->ThirdQualifier.to_string ++ " seventh"
    | Octave => "octave"
    }

  let rec intervalNumber_of_notes = (noteA, noteB, distanceAccumulator) =>
    if noteA->is_same_note_familly(noteB) {
      distanceAccumulator
    } else {
      let nextNote = noteA->getNextNote
      intervalNumber_of_notes(nextNote, noteB, distanceAccumulator + 1)
    }

  let interval_of_notes = (noteA, noteB) => {
    let deltaSemitones = semitonesBetweenNotes(noteA, noteB)
    switch intervalNumber_of_notes(noteA, noteB, 0) {
    | 0 => Result.Ok(Unison)
    | 1 =>
      (deltaSemitones - Second(Major)->to_semitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Second(qualifier))
    | 2 =>
      (deltaSemitones - Third(Major)->to_semitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Third(qualifier))
    | 3 =>
      (deltaSemitones - Fourth(Perfect)->to_semitones)
      ->FifthQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Fourth(qualifier))
    | 4 =>
      (deltaSemitones - Fifth(Perfect)->to_semitones)
      ->FifthQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Fifth(qualifier))
    | 5 =>
      (deltaSemitones - Sixth(Major)->to_semitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Sixth(qualifier))
    | 6 =>
      (deltaSemitones - Seventh(Major)->to_semitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Result.map(qualifier => Seventh(qualifier))
    | 7 => Result.Ok(Octave)
    | _ => Errors.nNotes_too_large
    }
  }

  let note_of_cannonical_interval = (rootNote, interval) =>
    switch interval {
    | Unison => rootNote
    | Second(_) => rootNote->getNthNote(1)
    | Third(_) => rootNote->getNthNote(2)
    | Fourth(_) => rootNote->getNthNote(3)
    | Fifth(_) => rootNote->getNthNote(4)
    | Sixth(_) => rootNote->getNthNote(5)
    | Seventh(_) => rootNote->getNthNote(6)
    | Octave => rootNote->getNthNote(7)
    }

  let note_of_interval = (rootNote, interval) => {
    let newNote = rootNote->note_of_cannonical_interval(interval)->setAccidental(Natural)
    let targetSemitoneDifference = interval->to_semitones
    let actualSemitoneDifference = semitonesBetweenNotes(rootNote, newNote)
    let accidental = switch targetSemitoneDifference - actualSemitoneDifference {
    | -2 => Accidental.DoubleFlat
    | -1 => Accidental.Flat
    | 1 => Accidental.Sharp
    | 2 => Accidental.DoubleSharp
    | 0
    | _ =>
      Accidental.Natural
    }
    newNote->setAccidental(accidental)
  }

  let stackIntervalsRelatively = (root, intervals) => {
    let rec stackClassIntervalsRelatively = (root, class_intervals) => {
      switch class_intervals {
      | list{} => list{root}
      | list{interval} => list{root, note_of_interval(root, interval)}
      | list{interval, ...rest} =>
        let nextNote = note_of_interval(root, interval)
        let subChord = switch stackClassIntervalsRelatively(nextNote, rest) {
        | list{}
        | list{_} =>
          list{}
        | list{_, ...rest} => rest
        }
        list{root, nextNote}->List.concat(subChord)
      }
    }
    stackClassIntervalsRelatively(root, intervals)
  }

  let stackIntervalsAbsolutely = (root, intervals) =>
    intervals->List.reduce(list{root}, (acc, interval) =>
      acc->List.concat(list{note_of_interval(root, interval)})
    )

  let buildInterval = (root, named_interval) => {
    switch named_interval {
    | Second(qualifier) => root->stackIntervalsRelatively(list{Second(qualifier)})
    | Third(qualifier) => root->stackIntervalsRelatively(list{Third(qualifier)})
    | Fourth(qualifier) => root->stackIntervalsRelatively(list{Fourth(qualifier)})
    | Fifth(qualifier) => root->stackIntervalsRelatively(list{Fifth(qualifier)})
    | Sixth(qualifier) => root->stackIntervalsRelatively(list{Sixth(qualifier)})
    | Seventh(qualifier) => root->stackIntervalsRelatively(list{Seventh(qualifier)})
    | Unison
    | Octave =>
      list{root}
    }
  }
}

let getTonic = notes =>
  switch notes {
  | list{} => None
  | list{head}
  | list{head, ..._} =>
    Some(head)
  }

open Interval

module Notes = {
  type t = list<note>

  let string_of_notes = notes =>
    notes
    ->List.reduce("", (acc, note) => acc ++ note->Note.to_string ++ "  ")
    ->Js.String2.slice(~from=0, ~to_=-2)

  let rec relativeIntervals_of_notes = (notes, acc) => {
    switch notes {
    | list{root, next_note, ...rest} =>
      root
      ->interval_of_notes(next_note)
      ->Result.mapWithDefault(list{}, interval => list{interval})
      ->List.concat(acc)
      ->List.concat(rest->List.add(next_note)->relativeIntervals_of_notes(list{}))
    | list{_}
    | list{} =>
      list{}
    }
  }

  let rec absoluteIntervals_of_notes = (notes, acc) => {
    switch notes {
    | list{root, next_note, ...rest} =>
      root
      ->interval_of_notes(next_note)
      ->Result.mapWithDefault(list{}, interval => list{interval})
      ->List.concat(acc)
      ->List.concat(rest->List.add(root)->absoluteIntervals_of_notes(list{}))
    | list{_}
    | list{} =>
      list{}
    }
  }
}

module Chord = {
  type chord =
    | MajorTriad
    | MinorTriad
    | AugmentedTriad
    | DiminishedTriad
    | SuspendedTriad
    | PowerChord
    | DiminishedPowerChord
    | AugmentedPowerChord
    | MajorSeventh
    | DominanteSeventh
    | MinorSeventhMajor
    | MinorSeventh
    | AugmentedMajorSeventh
    | HalfDiminishedSeventh
    | DiminishedSeventh
    | SuspendedSeventh
    | SeventhAugmentedFifth
    | SeventhDiminishedFifth
    | MajorSixth
    | MinorSixth


  let string_of_chord = chord =>
    switch chord {
    | MajorTriad => "majorTriad"
    | MinorTriad => "minorTriad"
    | AugmentedTriad => "augmentedTriad"
    | DiminishedTriad => "diminishedTriad"
    | SuspendedTriad => "suspendedTriad"
    | PowerChord => "powerChord"
    | DiminishedPowerChord => "diminishedPowerChord"
    | AugmentedPowerChord => "augmentedPowerChord"
    | MajorSeventh => "majorSeventh"
    | DominanteSeventh => "dominantSeventh"
    | MinorSeventhMajor => "minorSeventhMajor"
    | MinorSeventh => "minorSeventh"
    | AugmentedMajorSeventh => "augmentedMajorSeventh"
    | HalfDiminishedSeventh => "halfDiminishedSeventh"
    | DiminishedSeventh => "diminishedSeventh"
    | SuspendedSeventh => "suspendedSeventh"
    | SeventhAugmentedFifth => "seventhAugmentedFifth"
    | SeventhDiminishedFifth => "seventhDiminishedFifth"
    | MajorSixth => "majorSixth"
    | MinorSixth => "minorSixth"
    }

  let buildChord = (root, chord) =>
    switch chord {
    | MajorTriad => root->stackIntervalsRelatively(list{Major->Third, Minor->Third})
    | MinorTriad => root->stackIntervalsRelatively(list{Minor->Third, Major->Third})
    | AugmentedTriad => root->stackIntervalsRelatively(list{Major->Third, Major->Third})
    | DiminishedTriad => root->stackIntervalsRelatively(list{Minor->Third, Minor->Third})
    | SuspendedTriad => root->stackIntervalsRelatively(list{Perfect->Fourth, Major->Second})
    | PowerChord => root->stackIntervalsRelatively(list{Perfect->Fifth})
    | AugmentedPowerChord => root->stackIntervalsRelatively(list{Augmented->Fifth})
    | DiminishedPowerChord => root->stackIntervalsRelatively(list{Diminished->Fifth})
    | MajorSeventh => root->stackIntervalsRelatively(list{Major->Third, Minor->Third, Major->Third})
    | DominanteSeventh =>
      root->stackIntervalsRelatively(list{Major->Third, Minor->Third, Minor->Third})
    | MinorSeventhMajor =>
      root->stackIntervalsRelatively(list{Minor->Third, Major->Third, Major->Third})
    | MinorSeventh => root->stackIntervalsRelatively(list{Minor->Third, Major->Third, Minor->Third})
    | AugmentedMajorSeventh =>
      root->stackIntervalsRelatively(list{Major->Third, Major->Third, Minor->Third})
    | HalfDiminishedSeventh =>
      root->stackIntervalsAbsolutely(list{Minor->Third, Diminished->Fifth, Minor->Seventh})
    | DiminishedSeventh =>
      root->stackIntervalsAbsolutely(list{Minor->Third, Diminished->Fifth, Diminished->Seventh})
    | SuspendedSeventh =>
      root->stackIntervalsAbsolutely(list{Perfect->Fourth, Perfect->Fifth, Minor->Seventh})
    | SeventhAugmentedFifth =>
      root->stackIntervalsAbsolutely(list{Major->Third, Augmented->Fifth, Minor->Seventh})
    | SeventhDiminishedFifth =>
      root->stackIntervalsAbsolutely(list{Major->Third, Diminished->Fifth, Minor->Seventh})
    | MajorSixth => root->stackIntervalsAbsolutely(list{Major->Third, Perfect->Fifth, Major->Sixth})
    | MinorSixth => root->stackIntervalsAbsolutely(list{Minor->Third, Perfect->Fifth, Major->Sixth})
  }

  let string_of_chords = chords =>
    chords
    ->List.reduce("", (acc, chord) => acc ++ chord->string_of_chord ++ " | ")
    ->Js.String2.slice(~from=0, ~to_=-3)
}

open Chord

module Scale = {
  type t =
    | MajorScale
    | NaturalMinorScale
    | HarmonicMinorScale
    | IonianMode
    | DorianMode
    | PhrygianMode
    | LydianMode
    | MixolydianMode
    | AeolianMode
    | IocrianMode

  let string_of_scale = scale =>
    switch scale {
    | MajorScale => "Major Scale"
    | NaturalMinorScale => "Natural Minor"
    | HarmonicMinorScale => "Harmonic Minor"
    | IonianMode => "Ionial Mode"
    | DorianMode => "Dorian Mode"
    | PhrygianMode => "Phrygian Mode"
    | LydianMode => "Lydian Mode"
    | MixolydianMode => "Mixolydian Mode"
    | AeolianMode => "Aeolian Mode"
    | IocrianMode => "Iocrian Mode"
    }

  let rec get_nth_mode = (intervals, n) => {
    switch n {
    | 0 => intervals
    | _ =>
      switch intervals {
      | list{} => list{}
      | list{head, ...tail} => get_nth_mode(tail->List.concat(list{head}), n - 1)
      }
    }
  }

  let rec relativeIntervals_of_scale = scale =>
    switch scale {
    | MajorScale =>
      list{
        Major->Second,
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
      }
    | IonianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(0)
    | DorianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(1)
    | PhrygianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(2)
    | LydianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(3)
    | MixolydianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(4)
    | AeolianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(5)
    | IocrianMode => MajorScale->relativeIntervals_of_scale->get_nth_mode(6)
    | NaturalMinorScale =>
      list{
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
      }
    | HarmonicMinorScale =>
      list{
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
        Augmented->Second,
        Minor->Second,
      }
    }

  let buildScale = (root, scale) => root->stackIntervalsRelatively(scale->relativeIntervals_of_scale)

  let string_of_intervals = intervals => {
    "root" ++
    intervals->List.reduce("", (acc, interval) => acc ++ " -> " ++ interval->Interval.to_string)
  }
}


let chord_of_absoluteIntervals = intervals =>
  switch intervals {
  | list{Third(Minor), Fifth(Perfect)} => Result.Ok(MinorTriad)
  | list{Third(Major), Fifth(Perfect)} => Result.Ok(MajorTriad)
  | list{Third(Minor), Fifth(Diminished)} => Result.Ok(DiminishedTriad)
  | list{Third(Major), Fifth(Augmented)} => Result.Ok(AugmentedTriad)
  | list{Third(Minor), Fifth(Perfect), Seventh(Minor)} => Result.Ok(MinorSeventh)
  | list{Third(Major), Fifth(Perfect), Seventh(Major)} => Result.Ok(MajorSeventh)
  | list{Third(Minor), Fifth(Diminished), Seventh(Minor)} => Result.Ok(HalfDiminishedSeventh)
  | list{Third(Major), Fifth(Perfect), Seventh(Minor)} => Result.Ok(DominanteSeventh)
  | list{Third(Minor), Fifth(Diminished), Seventh(Diminished)} => Result.Ok(DiminishedSeventh)
  | list{Third(Major), Fifth(Augmented), Seventh(Major)} => Result.Ok(AugmentedMajorSeventh)
  | list{Third(Minor), Fifth(Perfect), Seventh(Major)} => Result.Ok(MinorSeventhMajor)
  | _ =>
    Result.Error(
      "Could not find the matching chord for intervals" ++
      ": " ++
      intervals
      ->List.reduce("", (acc, interval) => acc ++ interval->Interval.to_string ++ " > ")
      ->Js.String2.slice(~from=0, ~to_=-3),
    )
  }

module Harmonization = {
  open Scale

  let rec transpose = l =>
    switch l {
    | list{} => list{}
    | list{list{}, ...xss} => transpose(xss)
    | list{list{x, ...xs}, ...xss} =>
      let head = list{x, ...xss->List.map(List.headExn)}
      let tail = transpose(list{xs, ...xss->List.map(List.tailExn)})
      list{head, ...tail}
    }

  // see https://www.bluesguitarinstitute.com/how-to-harmonize-a-scale/
  let get_harmonization_matrix = scale => {
    let scale_intervals = scale->relativeIntervals_of_scale
    scale_intervals->List.mapWithIndex((i, _) => scale_intervals->get_nth_mode(i))
  }

  let print_matrix = matrix => {
    let string_of_row = row => row->List.reduce("", (acc, note) => acc ++ note->Note.to_string ++ " ")

    matrix->List.reduce("", (acc, row) => acc ++ row->string_of_row ++ "\n")
  }

  let filter_list_by_indexes = (notes, indexes) =>
    notes->List.reduceWithIndex(list{}, (acc, note, i) => {
      switch indexes->Set.Int.has(i) {
      | true => acc->List.concat(list{note})
      | false => acc
      }
    })

  let absoluteIntervals_of_relativeIntervals = relativeIntervals => {
    let rec f = relativeIntervals =>
      switch relativeIntervals {
      | list{} => Result.Ok(list{})
      | list{_} => Result.Ok(list{})
      | list{first, second, ...tail} =>
        addIntervals(first, second)->Result.flatMap(absoluteInterval => {
          f(list{absoluteInterval, ...tail})->Result.map(absoluteIntervals => list{
            absoluteInterval,
            ...absoluteIntervals,
          })
        })
      }
    switch relativeIntervals {
    | list{} => Result.Ok(list{})
    | list{interval} => Result.Ok(list{interval})
    | list{first, ..._} => f(relativeIntervals)->Result.map(intervals => list{first, ...intervals})
    }
  }

  let chords_of_scale = (scale, spec) => {
    scale
    ->get_harmonization_matrix
    ->transpose
    ->List.reduce(Result.Ok(list{}), (acc, scale_intervals) => {
      let intervals =
        scale_intervals
        ->absoluteIntervals_of_relativeIntervals
        ->Result.map(intervals => intervals->filter_list_by_indexes(spec))

      switch intervals->Result.flatMap(intervals => intervals->chord_of_absoluteIntervals) {
      | Result.Ok(chord) => acc->Result.map(acc => list{chord, ...acc})
      | Result.Error(msg) => Result.Error(msg)
      }
    })
    ->Result.map(List.reverse)
  }

  let triads_of_scale = scale => scale->chords_of_scale([1, 3]->Set.Int.fromArray)

  let tetrads_of_scale = scale => scale->chords_of_scale([1, 3, 5]->Set.Int.fromArray)

  let harmonize_scale = (scale, root, spec) => {
    let scale_notes = root->buildScale(scale)
    let scale_chords = scale->chords_of_scale(spec->Set.Int.fromArray)
    let rec f = arg =>
      switch arg {
      | list{(root, chord), ...tail} => list{root->buildChord(chord), ...f(tail)}
      | list{} => list{}
      }
    scale_chords->Result.map(scale_chords => f(List.zip(scale_notes, scale_chords)))
  }

  let harmonize_scale_with_triads = (scale, root) => harmonize_scale(scale, root, [1, 3])

  let harmonize_scale_with_tetrads = (scale, root) => harmonize_scale(scale, root, [1, 3, 5])
}



let string_of_progression = p => p->List.reduce("", (acc, h) => acc ++ h->Notes.string_of_notes ++ " | ")
