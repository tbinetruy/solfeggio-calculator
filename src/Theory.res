open Belt

let semitones_in_octave = 12

module Accidental = {
  type t =
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

  type t =
    | C(Accidental.t)
    | D(Accidental.t)
    | E(Accidental.t)
    | F(Accidental.t)
    | G(Accidental.t)
    | A(Accidental.t)
    | B(Accidental.t)

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

module Notes = {
  type t = list<Note.t>

  let string_of_notes = notes =>
    notes
    ->List.reduce("", (acc, note) => acc ++ note->Note.to_string ++ "  ")
    ->Js.String2.slice(~from=0, ~to_=-2)
}

module Progression = {
  type t = list<list<Note.t>>

  let to_string = p => p->List.reduce("", (acc, h) => acc ++ h->Notes.string_of_notes ++ " | ")
}

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
  open Note

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

  let from_semitones = (nNotes, nSemitones) =>
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

  let from_notes = (noteA, noteB) => {
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

  let next_note = (rootNote, interval) => {
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

  let to_notes = (root_note, interval) => list{root_note, root_note->next_note(interval)}

  let addIntervals = (intervalA, intervalB) => {
    let nNotes = intervalA->nNotes_of_interval + intervalB->nNotes_of_interval - 1
    let nSemitones = intervalA->to_semitones + intervalB->to_semitones
    from_semitones(nNotes, nSemitones)
  }
}

let getTonic = notes =>
  switch notes {
  | list{} => None
  | list{head}
  | list{head, ..._} =>
    Some(head)
  }

module Intervals = {
  open Interval

  type t =
    | Relative(list<Interval.interval>)
    | Absolute(list<Interval.interval>)

  let to_notes = (root, intervals) =>
    switch intervals {
    | Relative(intervals) =>
      let rec stackClassIntervalsRelatively = (root, class_intervals) => {
        switch class_intervals {
        | list{} => list{root}
        | list{interval} => list{root, next_note(root, interval)}
        | list{interval, ...rest} =>
          let nextNote = next_note(root, interval)
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
    | Absolute(intervals) =>
      intervals->List.reduce(list{root}, (acc, interval) =>
        acc->List.concat(list{next_note(root, interval)})
      )
    }

  let relativeIntervals_of_notes = notes => {
    let rec relativeIntervals_of_notes = (notes, acc) => {
      switch notes {
      | list{root, next_note, ...rest} =>
        root
        ->from_notes(next_note)
        ->Result.mapWithDefault(list{}, interval => list{interval})
        ->List.concat(acc)
        ->List.concat(rest->List.add(next_note)->relativeIntervals_of_notes(list{}))
      | list{_}
      | list{} =>
        list{}
      }
    }
    relativeIntervals_of_notes(notes, list{})->Relative
  }

  let absoluteIntervals_of_notes = notes => {
    let rec absoluteIntervals_of_notes = (notes, acc) => {
      switch notes {
      | list{root, next_note, ...rest} =>
        root
        ->from_notes(next_note)
        ->Result.mapWithDefault(list{}, interval => list{interval})
        ->List.concat(acc)
        ->List.concat(rest->List.add(root)->absoluteIntervals_of_notes(list{}))
      | list{_}
      | list{} =>
        list{}
      }
    }
    absoluteIntervals_of_notes(notes, list{})->Absolute
  }

  let to_list = intervals =>
    switch intervals {
    | Absolute(intervals)
    | Relative(intervals) => intervals
    }

  let map = (intervals, f) =>
    switch intervals {
    | Absolute(intervals) => Absolute(f(intervals))
    | Relative(intervals) => Relative(f(intervals))
    }

  let to_string = intervals =>
    intervals
    ->to_list
    ->List.reduce("", (acc, interval) => acc ++ interval->Interval.to_string ++ " > ")
    ->Js.String2.slice(~from=0, ~to_=-3)

  let to_absolute = relativeIntervals => {
    let rec f = relativeIntervals =>
      switch relativeIntervals {
      | list{} => Result.Ok(list{})
      | list{_} => Result.Ok(list{})
      | list{first, second, ...tail} =>
        Interval.addIntervals(first, second)->Result.flatMap(absoluteInterval => {
          f(list{absoluteInterval, ...tail})->Result.map(absoluteIntervals => list{
            absoluteInterval,
            ...absoluteIntervals,
          })
        })
      }
    switch relativeIntervals {
    | Relative(relativeIntervals) =>
      let absoluteIntervals = switch relativeIntervals {
      | list{} => Result.Ok(list{})
      | list{interval} => Result.Ok(list{interval})
      | list{first, ..._} =>
        f(relativeIntervals)->Result.map(intervals => list{first, ...intervals})
      }
      absoluteIntervals->Result.map(intervals => Absolute(intervals))
    | Absolute(intervals) => Result.Ok(Absolute(intervals))
    }
  }
}

module Chord = {
  open Interval

  type t =
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

  let to_string = chord =>
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

  let to_intervals = chord =>
    switch chord {
    | MajorTriad => Intervals.Relative(list{Major->Third, Minor->Third})
    | MinorTriad => Intervals.Relative(list{Minor->Third, Major->Third})
    | AugmentedTriad => Intervals.Relative(list{Major->Third, Major->Third})
    | DiminishedTriad => Intervals.Relative(list{Minor->Third, Minor->Third})
    | SuspendedTriad => Intervals.Relative(list{Perfect->Fourth, Major->Second})
    | PowerChord => Intervals.Relative(list{Perfect->Fifth})
    | AugmentedPowerChord => Intervals.Relative(list{Augmented->Fifth})
    | DiminishedPowerChord => Intervals.Relative(list{Diminished->Fifth})
    | MajorSeventh => Intervals.Relative(list{Major->Third, Minor->Third, Major->Third})
    | DominanteSeventh => Intervals.Relative(list{Major->Third, Minor->Third, Minor->Third})
    | MinorSeventhMajor => Intervals.Relative(list{Minor->Third, Major->Third, Major->Third})
    | MinorSeventh => Intervals.Relative(list{Minor->Third, Major->Third, Minor->Third})
    | AugmentedMajorSeventh => Intervals.Relative(list{Major->Third, Major->Third, Minor->Third})
    | HalfDiminishedSeventh =>
      Intervals.Absolute(list{Minor->Third, Diminished->Fifth, Minor->Seventh})
    | DiminishedSeventh =>
      Intervals.Absolute(list{Minor->Third, Diminished->Fifth, Diminished->Seventh})
    | SuspendedSeventh => Intervals.Absolute(list{Perfect->Fourth, Perfect->Fifth, Minor->Seventh})
    | SeventhAugmentedFifth =>
      Intervals.Absolute(list{Major->Third, Augmented->Fifth, Minor->Seventh})
    | SeventhDiminishedFifth =>
      Intervals.Absolute(list{Major->Third, Diminished->Fifth, Minor->Seventh})
    | MajorSixth => Intervals.Absolute(list{Major->Third, Perfect->Fifth, Major->Sixth})
    | MinorSixth => Absolute(list{Minor->Third, Perfect->Fifth, Major->Sixth})
    }

  let to_notes = (root, chord) => root->Intervals.to_notes(chord->to_intervals)

  let from_intervals = intervals =>
    switch intervals {
    | Intervals.Absolute(intervals) =>
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
    | Relative(intervals) =>
      switch intervals {
      | list{Third(Major), Third(Minor)} => Result.Ok(MajorTriad)
      | list{Third(Minor), Third(Major)} => Result.Ok(MinorTriad)
      | _ =>
        Result.Error(
          "Could not find the matching chord for intervals" ++
          ": " ++
          intervals
          ->List.reduce("", (acc, interval) => acc ++ interval->Interval.to_string ++ " > ")
          ->Js.String2.slice(~from=0, ~to_=-3),
        )
      }
    }
}

module Scale = {
  open Interval

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
      | Intervals.Relative(list{}) => Relative(list{})
      | Relative(list{head, ...tail}) =>
        get_nth_mode(Relative(tail->List.concat(list{head})), n - 1)
      | _ => Relative(list{})
      }
    }
  }

  let rec to_intervals = scale =>
    switch scale {
    | MajorScale =>
      Intervals.Relative(list{
        Major->Second,
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
      })
    | IonianMode => MajorScale->to_intervals->get_nth_mode(0)
    | DorianMode => MajorScale->to_intervals->get_nth_mode(1)
    | PhrygianMode => MajorScale->to_intervals->get_nth_mode(2)
    | LydianMode => MajorScale->to_intervals->get_nth_mode(3)
    | MixolydianMode => MajorScale->to_intervals->get_nth_mode(4)
    | AeolianMode => MajorScale->to_intervals->get_nth_mode(5)
    | IocrianMode => MajorScale->to_intervals->get_nth_mode(6)
    | NaturalMinorScale =>
      Relative(list{
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
      })
    | HarmonicMinorScale =>
      Relative(list{
        Major->Second,
        Minor->Second,
        Major->Second,
        Major->Second,
        Minor->Second,
        Augmented->Second,
        Minor->Second,
      })
    }

  let to_notes = (root, scale) => root->Intervals.to_notes(scale->to_intervals)

  let string_of_intervals = intervals => {
    "root" ++
    intervals->List.reduce("", (acc, interval) => acc ++ " -> " ++ interval->Interval.to_string)
  }
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
    let intervals = scale->to_intervals
    intervals->Intervals.to_list->List.mapWithIndex((i, _) => intervals->get_nth_mode(i))
  }

  let transpose_harmonization_matrix = matrix => {
    matrix
    ->List.reduceReverse(Result.Ok(list{}), (acc, intervals) => {
      switch intervals {
      | Intervals.Absolute(_) => Result.Error("Only relative intervals are supported")
      | Relative(intervals) => acc->Result.map(acc => list{intervals, ...acc})
      }
    })
    ->Result.map(matrix => matrix->transpose->List.map(intervals => Intervals.Relative(intervals)))
  }

  let print_matrix = matrix => {
    let string_of_row = row =>
      row->List.reduce("", (acc, note) => acc ++ note->Note.to_string ++ " ")

    matrix->List.reduce("", (acc, row) => acc ++ row->string_of_row ++ "\n")
  }

  let filter_list_by_indexes = (notes, indexes) =>
    notes->List.keepWithIndex((_, i) => indexes->Set.Int.has(i))

  let filter_and_maintain_order = (l, indexes) =>
    indexes->Array.reduceReverse(Result.Ok(list{}), (acc, scale_degree) =>
      acc->Result.flatMap(acc =>
        switch l->List.get(scale_degree) {
        | Some(chord) => Result.Ok(list{chord, ...acc})
        | None => Result.Error("scale degree not found")
        }
      )
    )

  let to_chords = (scale, chord_degrees, scale_degrees: array<int>) => {
    scale
    ->get_harmonization_matrix
    ->transpose_harmonization_matrix
    ->Result.flatMap(matrix =>
      matrix
      ->filter_and_maintain_order(scale_degrees)
      ->Result.flatMap(chords =>
        chords->List.reduceReverse(
          Result.Ok(list{}),
          (acc, scale_intervals) => {
            let intervals =
              scale_intervals
              ->Intervals.to_absolute
              ->Result.map(
                intervals =>
                  intervals->Intervals.map(
                    intervals => intervals->filter_list_by_indexes(chord_degrees),
                  ),
              )

            switch intervals->Result.flatMap(intervals => intervals->Chord.from_intervals) {
            | Result.Ok(chord) => acc->Result.map(acc => list{chord, ...acc})
            | Result.Error(msg) => Result.Error(msg)
            }
          },
        )
      )
    )
  }

  let to_progression = (scale, root, chord_degrees, scale_degrees) => {
    let scale_notes = root->Scale.to_notes(scale)->filter_and_maintain_order(scale_degrees)
    let scale_chords = scale->to_chords(chord_degrees, scale_degrees)
    let to_notes = ((root, chord)) => root->Chord.to_notes(chord)
    switch (scale_notes, scale_chords) {
    | (Result.Ok(scale_notes), Result.Ok(scale_chords)) =>
      List.zip(scale_notes, scale_chords)->List.map(to_notes)->Result.Ok
    | (Result.Error(msg), _) => Result.Error(msg)
    | (_, Result.Error(msg)) => Result.Error(msg)
    }
  }

  let triad_degrees = [1, 3]->Set.Int.fromArray
  let tetrad_degrees = [1, 3, 5]->Set.Int.fromArray
  let scale_harmonization_degrees = [0, 1, 2, 3, 4, 5, 6]

  let to_triads = scale => scale->to_chords(triad_degrees, scale_harmonization_degrees)
  let to_tetrads = scale => scale->to_chords(tetrad_degrees, scale_harmonization_degrees)

  let to_triad_progression = (scale, root) =>
    to_progression(scale, root, triad_degrees, scale_harmonization_degrees)
  let to_tetrad_progression = (scale, root) =>
    to_progression(scale, root, tetrad_degrees, scale_harmonization_degrees)
}

module Chords = {
  type t = list<Chord.t>

  let to_string = chords =>
    chords
    ->List.reduce("", (acc, chord) => acc ++ chord->Chord.to_string ++ " | ")
    ->Js.String2.slice(~from=0, ~to_=-3)
}
