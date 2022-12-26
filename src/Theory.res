open Belt;



module Accidental = {
  type accidental =
    | DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp;

  let to_string = accidental => {
    switch (accidental) {
    | Flat => "b"
    | DoubleFlat => "bb"
    | Natural => ""
    | DoubleSharp => "##"
    | Sharp => "#"
    };
  };

  let to_semitones = accidental => {
    switch (accidental) {
    | DoubleFlat => -2
    | Flat => -1
    | Natural => 0
    | Sharp => 1
    | DoubleSharp => 2
    };
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
    | B(accidental);

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
    switch (note) {
    | C(accidental) => "C" ++ accidental->to_string
    | D(accidental) => "D" ++ accidental->to_string
    | E(accidental) => "E" ++ accidental->to_string
    | F(accidental) => "F" ++ accidental->to_string
    | G(accidental) => "G" ++ accidental->to_string
    | A(accidental) => "A" ++ accidental->to_string
    | B(accidental) => "B" ++ accidental->to_string
    };

  let getNextNote = note =>
    switch note {
    | C(accidental) => D(accidental)
    | D(accidental) => E(accidental)
    | E(accidental) => F(accidental)
    | F(accidental) => G(accidental)
    | G(accidental) => A(accidental)
    | A(accidental) => B(accidental)
    | B(accidental) => C(accidental)
    };

  let to_semitones = note =>
    switch (note) {
    | C(accidental) => 0 + accidental->to_semitones
    | D(accidental) => 2 + accidental->to_semitones
    | E(accidental) => 4 + accidental->to_semitones
    | F(accidental) => 5 + accidental->to_semitones
    | G(accidental) => 7 + accidental->to_semitones
    | A(accidental) => 9 + accidental->to_semitones
    | B(accidental) => 11 + accidental->to_semitones
    };

  let rec getNthNote = (rootNote, n) =>
    switch (n) {
    | 0 => rootNote
    | _ => getNthNote(rootNote->getNextNote, n - 1)
    };

  let semitonesBetweenNotes = (noteA, noteB) => {
    let delta = (noteB->to_semitones) - (noteA->to_semitones);
    delta < 0 ? 12 + delta : delta;
  };
}

open Note

type semitone = int;

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

    let to_semitones = qualifier =>
      switch qualifier {
        | Diminished => -1
        | Perfect => 0
        | Augmented => 1
      }

    let qualifier_of_semitones = semitones =>
      switch semitones {
        | -1 => Some(Diminished)
        | 0 => Some(Perfect)
        | 1 => Some(Augmented)
        | _ => None
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

    let to_semitones = qualifier =>
      switch qualifier {
        | Diminished => -2
        | Minor => -1
        | Major => 0
        | Augmented => 1
      }

    let qualifier_of_semitones = semitones =>
      switch semitones {
        | -2 => Some(Diminished)
        | -1 => Some(Minor)
        | 0 => Some(Major)
        | 1 => Some(Augmented)
        | _ => None
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

  let to_cannonical_interval = interval =>
      switch interval {
        | Unison => Unison
        | Second(_) => Second(Major)
        | Third(_) => Third(Major)
        | Fourth(_) => Fourth(Perfect)
        | Fifth(_) => Fifth(Perfect)
        | Sixth(_) => Sixth(Major)
        | Seventh(_) => Seventh(Major)
        | Octave => Octave
      }

  let to_cannonical_semitones = interval =>
      switch interval {
        | Unison => 0
        | Second(_) => 2
        | Third(_) => 4
        | Fourth(_) => 5
        | Fifth(_) => 7
        | Sixth(_) => 9
        | Seventh(_) => 11
        | Octave => 12
      }

  let to_semitones = interval => {
    let deltaSemitones =
      switch interval {
      | Unison => 0
      | Second(qualifier) => qualifier->ThirdQualifier.to_semitones
      | Third(qualifier) => qualifier->ThirdQualifier.to_semitones
      | Fourth(qualifier) => qualifier->FifthQualifier.to_semitones
      | Fifth(qualifier) => qualifier->FifthQualifier.to_semitones
      | Sixth(qualifier) => qualifier->ThirdQualifier.to_semitones
      | Seventh(qualifier) => qualifier->ThirdQualifier.to_semitones
      | Octave => 0
    }
    interval->to_cannonical_semitones + deltaSemitones
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
    if (noteA->is_same_note_familly(noteB)) {
      distanceAccumulator;
    } else {
      let nextNote = noteA->getNextNote
      intervalNumber_of_notes(nextNote, noteB, distanceAccumulator + 1);
    };

  let interval_of_notes = (noteA, noteB) => {
    let deltaSemitones = semitonesBetweenNotes(noteA, noteB);
    switch (intervalNumber_of_notes(noteA, noteB, 0)) {
    | 0 => Some(Unison)
    | 1 =>
      (Second(Major)->to_semitones - deltaSemitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Second(qualifier))
    | 2 =>
      (Third(Major)->to_semitones - deltaSemitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Third(qualifier))
    | 3 =>
      (Fourth(Perfect)->to_semitones - deltaSemitones)
      ->FifthQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Fourth(qualifier))
    | 4 =>
      (Fifth(Perfect)->to_semitones - deltaSemitones)
      ->FifthQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Fifth(qualifier))
    | 5 =>
      (Sixth(Major)->to_semitones - deltaSemitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Sixth(qualifier))
    | 6 =>
      (Seventh(Major)->to_semitones - deltaSemitones)
      ->ThirdQualifier.qualifier_of_semitones
      ->Option.map(qualifier => Seventh(qualifier))
    | 7 => Some(Octave)
    | _ => None
    };
  };

  let note_of_cannonical_interval = (rootNote, interval) =>
    switch (interval) {
    | Unison => rootNote
    | Second(_) => rootNote->getNthNote(1)
    | Third(_) => rootNote->getNthNote(2)
    | Fourth(_) => rootNote->getNthNote(3)
    | Fifth(_) => rootNote->getNthNote(4)
    | Sixth(_) => rootNote->getNthNote(5)
    | Seventh(_) => rootNote->getNthNote(6)
    | Octave => rootNote->getNthNote(7)
    };

  let note_of_interval = (rootNote, interval) => {
    let newNote= rootNote->note_of_cannonical_interval(interval)->setAccidental(Natural);
    let targetSemitoneDifference = interval->to_semitones;
    let actualSemitoneDifference = semitonesBetweenNotes(rootNote, newNote);
    let accidental =
      switch (targetSemitoneDifference - actualSemitoneDifference) {
      | (-2) => Accidental.DoubleFlat
      | (-1) => Accidental.Flat
      | 1 => Accidental.Sharp
      | 2 => Accidental.DoubleSharp
      | 0
      | _ => Accidental.Natural
      };
    newNote->setAccidental(accidental)
  };

  module NamedInterval = {
    type interval =
      | MinorSecond
      | MajorSecond
      | MinorThird
      | MajorThird
      | DiminishedFourth
      | PerfectFourth
      | AugmentedFourth
      | DiminishedFifth
      | PerfectFifth
      | AugmentedFifth
      | MinorSixth
      | MajorSixth
      | DiminishedSeventh
      | MinorSeventh
      | MajorSeventh

    let to_interval = named_interval =>
      switch named_interval {
        | MinorSecond => Second(Minor)
        | MajorSecond => Second(Major)
        | MinorThird => Third(Minor)
        | MajorThird => Third(Major)
        | DiminishedFourth => Fourth(Diminished)
        | PerfectFourth => Fourth(Perfect)
        | AugmentedFourth => Fourth(Augmented)
        | DiminishedFifth => Fifth(Diminished)
        | PerfectFifth => Fifth(Perfect)
        | AugmentedFifth => Fifth(Perfect)
        | MinorSixth => Sixth(Minor)
        | MajorSixth => Sixth(Major)
        | DiminishedSeventh => Seventh(Diminished)
        | MinorSeventh => Seventh(Minor)
        | MajorSeventh => Seventh(Major)
      }

    let to_string = named_interval =>
      named_interval->to_interval->to_string
  }

  let stackIntervalsRelatively = (root, named_intervals) => {
    let intervals = named_intervals
      ->List.map(el => el->NamedInterval.to_interval)

    let rec stackClassIntervalsRelatively = (root, class_intervals) => {
      switch (class_intervals) {
      | list{} => list{root}
      | list{interval} => list{root, note_of_interval(root, interval)}
      | list{interval, ...rest} =>
        let nextNote = note_of_interval(root, interval);
        let subChord =
          switch (stackClassIntervalsRelatively(nextNote, rest)) {
          | list{}
          | list{_} => list{}
          | list{_, ...rest} => rest
          };
        list{root, nextNote}->List.concat(subChord);
      };
    }
    stackClassIntervalsRelatively(root, intervals)
  };

  let stackIntervalsAbsolutely = (root, named_intervals) =>
    named_intervals
      ->List.map(el => el->NamedInterval.to_interval)
      ->List.reduce(list{root}, (acc, interval) => {
      acc->List.concat(list{note_of_interval(root, interval)})
    });
}

let getTonic = notes =>
  switch (notes) {
  | list{} => None
  | list{head}
  | list{head, ..._} => Some(head)
  };

open Interval

let buildInterval = (root, named_interval) => {
  switch named_interval {
    | NamedInterval.MinorSecond => root->stackIntervalsRelatively(list{NamedInterval.MinorSecond})
    | NamedInterval.MajorSecond => root->stackIntervalsRelatively(list{NamedInterval.MajorSecond})
    | NamedInterval.MinorThird => root->stackIntervalsRelatively(list{NamedInterval.MinorThird})
    | NamedInterval.MajorThird => root->stackIntervalsRelatively(list{NamedInterval.MajorThird})
    | NamedInterval.DiminishedFourth => root->stackIntervalsRelatively(list{NamedInterval.DiminishedFourth})
    | NamedInterval.PerfectFourth => root->stackIntervalsRelatively(list{NamedInterval.PerfectFourth})
    | NamedInterval.AugmentedFourth => root->stackIntervalsRelatively(list{NamedInterval.AugmentedFourth})
    | NamedInterval.DiminishedFifth => root->stackIntervalsRelatively(list{NamedInterval.DiminishedFifth})
    | NamedInterval.PerfectFifth => root->stackIntervalsRelatively(list{NamedInterval.PerfectFifth})
    | NamedInterval.AugmentedFifth => root->stackIntervalsRelatively(list{NamedInterval.AugmentedFifth})
    | NamedInterval.MinorSixth => root->stackIntervalsRelatively(list{NamedInterval.MinorSixth})
    | NamedInterval.MajorSixth => root->stackIntervalsRelatively(list{NamedInterval.MajorSixth})
    | NamedInterval.DiminishedSeventh => root->stackIntervalsRelatively(list{NamedInterval.DiminishedSeventh})
    | NamedInterval.MinorSeventh => root->stackIntervalsRelatively(list{NamedInterval.MinorSeventh})
    | NamedInterval.MajorSeventh => root->stackIntervalsRelatively(list{NamedInterval.MajorSeventh})
  }
}

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
  | MinorSixth;

let string_of_chord = chord =>
  switch (chord) {
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
  };

let buildChord = (root, chord) =>
  switch (chord) {
  | MajorTriad => root->stackIntervalsRelatively(list{MajorThird, MinorThird})
  | MinorTriad => root->stackIntervalsRelatively(list{MinorThird, MajorThird})
  | AugmentedTriad => root->stackIntervalsRelatively(list{MajorThird, MajorThird})
  | DiminishedTriad =>
    root->stackIntervalsRelatively(list{MinorThird, MinorThird})
  | SuspendedTriad =>
    root->stackIntervalsRelatively(list{PerfectFourth, MajorSecond})
  | PowerChord => root->stackIntervalsRelatively(list{PerfectFifth})
  | AugmentedPowerChord => root->stackIntervalsRelatively(list{AugmentedFifth})
  | DiminishedPowerChord => root->stackIntervalsRelatively(list{DiminishedFifth})
  | MajorSeventh =>
    root->stackIntervalsRelatively(list{MajorThird, MinorThird, MajorThird})
  | DominanteSeventh =>
    root->stackIntervalsRelatively(list{MajorThird, MinorThird, MinorThird})
  | MinorSeventhMajor =>
    root->stackIntervalsRelatively(list{MinorThird, MajorThird, MajorThird})
  | MinorSeventh =>
    root->stackIntervalsRelatively(list{MinorThird, MajorThird, MinorThird})
  | AugmentedMajorSeventh =>
    root->stackIntervalsRelatively(list{MajorThird, MajorThird, MinorThird})
  | HalfDiminishedSeventh =>
    root->stackIntervalsAbsolutely(list{
      MinorThird,
      DiminishedFifth,
      MinorSeventh,
    })
  | DiminishedSeventh =>
    root->stackIntervalsAbsolutely(list{
      MinorThird,
      DiminishedFifth,
      DiminishedSeventh,
    })
  | SuspendedSeventh =>
    root->stackIntervalsAbsolutely(list{
      PerfectFourth,
      PerfectFifth,
      MinorSeventh,
    })
  | SeventhAugmentedFifth =>
    root->stackIntervalsAbsolutely(list{MajorThird, AugmentedFifth, MinorSeventh})
  | SeventhDiminishedFifth =>
    root->stackIntervalsAbsolutely(list{
      MajorThird,
      DiminishedFifth,
      MinorSeventh,
    })
  | MajorSixth =>
    root->stackIntervalsAbsolutely(list{MajorThird, PerfectFifth, MajorSixth})
  | MinorSixth =>
    root->stackIntervalsAbsolutely(list{MinorThird, PerfectFifth, MajorSixth})
  };

type scale =
  | MajorScale

let string_of_scale = scale =>
  switch (scale) {
    | MajorScale => "Major Scale"
  }

let buildScale = (root, scale) =>
  switch (scale) {
  | MajorScale => root->stackIntervalsRelatively(list{
      MajorSecond,
      MajorSecond,
      MinorSecond,
      MajorSecond,
      MajorSecond,
      MajorSecond,
      MinorSecond,
    });
  }

let string_of_notes = notes =>
  notes->List.reduce("", (acc, note) => acc ++ (note->Note.to_string));

let rec relativeIntervals_of_notes = (notes, acc) => {
  switch notes {
    | list{root, next_note, ...rest} =>
        root
        ->interval_of_notes(next_note)
        ->Option.mapWithDefault(list{}, interval => list{interval})
        ->List.concat(acc)
        ->List.concat(rest->List.add(next_note)->relativeIntervals_of_notes(list{}))
    | list{_}
    | list{} => list{}
  }
}

/*
let a = {
  pitchClass: C,
  accidental: Natural,
}
let b = {
  pitchClass: B,
  accidental: Natural,
}
let c = {
  pitchClass: C,
  accidental: Natural,
}
let intervalClass_of_string = interval =>
    switch interval {
    | Unison => "unison"
    | Second(semitone) => "second(" ++ semitone->Belt.Int.toString ++ ")"
    | Third(semitone) => "third(" ++ semitone->Belt.Int.toString ++ ")"
    | Fourth(semitone) => "fourth(" ++ semitone->Belt.Int.toString ++ ")"
    | Fifth(semitone) => "fifth(" ++ semitone->Belt.Int.toString ++ ")"
    | Sixth(semitone) => "sixth(" ++ semitone->Belt.Int.toString ++ ")"
    | Seventh(semitone) => "seventh(" ++ semitone->Belt.Int.toString ++ ")"
    | Octave => "octave"
    }
let relatives = relativeIntervals_of_notes(list{a, b, c}, list{})
let str_relatives =
  relatives->List.reduce("", (acc, el) => acc ++ "," ++ el->intervalClass_of_string)
Js.Console.log(str_relatives)
*/
