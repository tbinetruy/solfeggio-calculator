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
type interval_class =
  | Unison
  | Second(semitone)
  | Third(semitone)
  | Fourth(semitone)
  | Fifth(semitone)
  | Sixth(semitone)
  | Seventh(semitone)
  | Octave;

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

let string_of_interval = interval =>
  switch interval {
    | MinorSecond => "minorSecond"
    | MajorSecond => "majorSecond"
    | MinorThird => "minorThird"
    | MajorThird => "majorThird"
    | DiminishedFourth => "diminishedFourth"
    | PerfectFourth => "perfectFourth"
    | AugmentedFourth => "augmentedFourth"
    | DiminishedFifth => "diminishedFifth"
    | PerfectFifth => "perfectFifth"
    | AugmentedFifth => "augmentedFifth"
    | MinorSixth => "minorSixth"
    | MajorSixth => "majorSixth"
    | DiminishedSeventh => "diminishedSeventh"
    | MinorSeventh => "minorSeventh"
    | MajorSeventh => "majorSeventh"
  }


let interval_class_of_interval = interval =>
  switch interval {
    | MinorSecond => Second(-1)
    | MajorSecond => Second(0)
    | MinorThird => Third(-1)
    | MajorThird => Third(0)
    | DiminishedFourth => Fourth(-1)
    | PerfectFourth => Fourth(0)
    | AugmentedFourth => Fourth(1)
    | DiminishedFifth => Fifth(-1)
    | PerfectFifth => Fifth(0)
    | AugmentedFifth => Fifth(1)
    | MinorSixth => Sixth(-1)
    | MajorSixth => Sixth(0)
    | DiminishedSeventh => Seventh(-2)
    | MinorSeventh => Seventh(-1)
    | MajorSeventh => Seventh(0)
  }

let semitones_of_interval = interval =>
  switch (interval) {
  | Unison => 0
  | Second(quality) => 2 + quality
  | Third(quality) => 4 + quality
  | Fourth(quality) => 5 + quality
  | Fifth(quality) => 7 + quality
  | Sixth(quality) => 9 + quality
  | Seventh(quality) => 11 + quality
  | Octave => 12
  };

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
  | 1 => Some(Second(2 - deltaSemitones))
  | 2 => Some(Third(4 - deltaSemitones))
  | 3 => Some(Fourth(5 - deltaSemitones))
  | 4 => Some(Fifth(7 - deltaSemitones))
  | 5 => Some(Sixth(9 - deltaSemitones))
  | 6 => Some(Seventh(11 - deltaSemitones))
  | 7 => Some(Octave)
  | _ => None
  };
};

let note_of_interval = (rootNote, interval) =>
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
  let newNote= rootNote->note_of_interval(interval)->setAccidental(Natural);
  let targetSemitoneDifference = interval->semitones_of_interval;
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

let stackIntervalsRelatively = (root, intervals) => {
  let intervals = intervals
    ->List.map(el => el->interval_class_of_interval)

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

let stackIntervalsAbsolutely = (root, intervals) =>
  intervals
    ->List.map(el => el->interval_class_of_interval)
    ->List.reduce(list{root}, (acc, interval) => {
    acc->List.concat(list{note_of_interval(root, interval)})
  });

let getTonic = notes =>
  switch (notes) {
  | list{} => None
  | list{head}
  | list{head, ..._} => Some(head)
  };

let buildInterval = (root, interval) =>
  switch interval {
    | MinorSecond => root->stackIntervalsRelatively(list{MinorSecond})
    | MajorSecond => root->stackIntervalsRelatively(list{MajorSecond})
    | MinorThird => root->stackIntervalsRelatively(list{MinorThird})
    | MajorThird => root->stackIntervalsRelatively(list{MajorThird})
    | DiminishedFourth => root->stackIntervalsRelatively(list{DiminishedFourth})
    | PerfectFourth => root->stackIntervalsRelatively(list{PerfectFourth})
    | AugmentedFourth => root->stackIntervalsRelatively(list{AugmentedFourth})
    | DiminishedFifth => root->stackIntervalsRelatively(list{DiminishedFifth})
    | PerfectFifth => root->stackIntervalsRelatively(list{PerfectFifth})
    | AugmentedFifth => root->stackIntervalsRelatively(list{AugmentedFifth})
    | MinorSixth => root->stackIntervalsRelatively(list{MinorSixth})
    | MajorSixth => root->stackIntervalsRelatively(list{MajorSixth})
    | DiminishedSeventh => root->stackIntervalsRelatively(list{DiminishedSeventh})
    | MinorSeventh => root->stackIntervalsRelatively(list{MinorSeventh})
    | MajorSeventh => root->stackIntervalsRelatively(list{MajorSeventh})
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
  notes->List.reduce("", (acc, note) => acc ++ (note |> to_string));

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
