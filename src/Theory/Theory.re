type pitchClass =
  | C
  | D
  | E
  | F
  | G
  | A
  | B;

type accidental =
  | DoubleFlat
  | Flat
  | Natural
  | Sharp
  | DoubleSharp;

type note = {
  pitchClass,
  accidental,
};

let string_of_pitchClass = pitchClass =>
  switch (pitchClass) {
  | C => "C"
  | D => "D"
  | E => "E"
  | F => "F"
  | G => "G"
  | A => "A"
  | B => "B"
  };

let string_of_accidental = accidental => {
  switch (accidental) {
  | Flat => "b"
  | DoubleFlat => "bb"
  | Natural => ""
  | DoubleSharp => "##"
  | Sharp => "#"
  };
};

let string_of_note = note =>
  (note.pitchClass |> string_of_pitchClass)
  ++ (note.accidental |> string_of_accidental);

let getNextPitchClass = pitchClass =>
  switch (pitchClass) {
  | C => D
  | D => E
  | E => F
  | F => G
  | G => A
  | A => B
  | B => C
  };

let rec getNthPitchClass = (rootPitchClass, n) =>
  switch (n) {
  | 0 => rootPitchClass
  | _ => getNthPitchClass(rootPitchClass |> getNextPitchClass, n - 1)
  };

let semitones_of_pitchClass = pitchClass =>
  switch (pitchClass) {
  | C => 0
  | D => 2
  | E => 4
  | F => 5
  | G => 7
  | A => 9
  | B => 11
  };

let semitones_of_note = note =>
  (note.pitchClass |> semitones_of_pitchClass)
  + (
    switch (note.accidental) {
    | DoubleFlat => (-2)
    | Flat => (-1)
    | Natural => 0
    | Sharp => 1
    | DoubleSharp => 2
    }
  );

let semitonesBetweenNotes = (noteA, noteB) => {
  let delta = (noteB |> semitones_of_note) - (noteA |> semitones_of_note);
  delta < 0 ? 12 + delta : delta;
};

type semitone = int;
type interval =
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

let minorSecond = Second(-1);
let majorSecond = Second(0);
let minorThird = Third(-1);
let majorThird = Third(0);
let diminishedFourth = Fourth(-1);
let perfectFourth = Fourth(0);
let augmentedFourth = Fourth(1);
let diminishedFifth = Fifth(-1);
let perfectFifth = Fifth(0);
let augmentedFifth = Fifth(1);
let minorSixth = Sixth(-1);
let majorSixth = Sixth(0);
let diminishedSeventh = Seventh(-2);
let minorSeventh = Seventh(-1);
let majorSeventh = Seventh(0);

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
  if (noteA.pitchClass == noteB.pitchClass) {
    distanceAccumulator;
  } else {
    let nextNote = {
      pitchClass: noteA.pitchClass |> getNextPitchClass,
      accidental: noteA.accidental,
    };
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

let pitchClass_of_interval = (rootPitchClass, interval) =>
  switch (interval) {
  | Unison => rootPitchClass
  | Second(_) => rootPitchClass->getNthPitchClass(1)
  | Third(_) => rootPitchClass->getNthPitchClass(2)
  | Fourth(_) => rootPitchClass->getNthPitchClass(3)
  | Fifth(_) => rootPitchClass->getNthPitchClass(4)
  | Sixth(_) => rootPitchClass->getNthPitchClass(5)
  | Seventh(_) => rootPitchClass->getNthPitchClass(6)
  | Octave => rootPitchClass->getNthPitchClass(7)
  };

let note_of_interval = (rootNote, interval) => {
  let newPitchClass = rootNote.pitchClass->pitchClass_of_interval(interval);
  let newNote = {pitchClass: newPitchClass, accidental: Natural};
  let targetSemitoneDifference = interval->semitones_of_interval;
  let actualSemitoneDifference = semitonesBetweenNotes(rootNote, newNote);
  let accidental =
    switch (targetSemitoneDifference - actualSemitoneDifference) {
    | (-2) => DoubleFlat
    | (-1) => Flat
    | 1 => Sharp
    | 2 => DoubleSharp
    | 0
    | _ => Natural
    };
  {pitchClass: newPitchClass, accidental};
};

let rec stackIntervalsRelatively = (root, intervals) => {
  switch (intervals) {
  | [] => [root]
  | [interval] => [root, note_of_interval(root, interval)]
  | [interval, ...rest] =>
    let nextNote = note_of_interval(root, interval);
    let subChord =
      switch (stackIntervalsRelatively(nextNote, rest)) {
      | []
      | [_] => []
      | [_, ...rest] => rest
      };
    [root, nextNote]->Belt.List.concat(subChord);
  };
};

let rec stackIntervalsAbsolutely = (root, intervals) =>
  intervals->Belt.List.reduce([root], (acc, interval) => {
    acc->Belt.List.concat([note_of_interval(root, interval)])
  });

let getTonic = notes =>
  switch (notes) {
  | [] => None
  | [head]
  | [head, ..._] => Some(head)
  };

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
  | MajorTriad => root->stackIntervalsRelatively([majorThird, minorThird])
  | MinorTriad => root->stackIntervalsRelatively([minorThird, majorThird])
  | AugmentedTriad => root->stackIntervalsRelatively([majorThird, majorThird])
  | DiminishedTriad =>
    root->stackIntervalsRelatively([minorThird, minorThird])
  | SuspendedTriad =>
    root->stackIntervalsRelatively([perfectFourth, majorSecond])
  | PowerChord => root->stackIntervalsRelatively([perfectFifth])
  | AugmentedPowerChord => root->stackIntervalsRelatively([augmentedFifth])
  | DiminishedPowerChord => root->stackIntervalsRelatively([diminishedFifth])
  | MajorSeventh =>
    root->stackIntervalsRelatively([majorThird, minorThird, majorThird])
  | DominanteSeventh =>
    root->stackIntervalsRelatively([majorThird, minorThird, minorThird])
  | MinorSeventhMajor =>
    root->stackIntervalsRelatively([minorThird, majorThird, majorThird])
  | MinorSeventh =>
    root->stackIntervalsRelatively([minorThird, majorThird, minorThird])
  | AugmentedMajorSeventh =>
    root->stackIntervalsRelatively([majorThird, majorThird, minorThird])
  | HalfDiminishedSeventh =>
    root->stackIntervalsAbsolutely([
      minorThird,
      diminishedFifth,
      minorSeventh,
    ])
  | DiminishedSeventh =>
    root->stackIntervalsAbsolutely([
      minorThird,
      diminishedFifth,
      diminishedSeventh,
    ])
  | SuspendedSeventh =>
    root->stackIntervalsAbsolutely([
      perfectFourth,
      perfectFifth,
      minorSeventh,
    ])
  | SeventhAugmentedFifth =>
    root->stackIntervalsAbsolutely([majorThird, augmentedFifth, minorSeventh])
  | SeventhDiminishedFifth =>
    root->stackIntervalsAbsolutely([
      majorThird,
      diminishedFifth,
      minorSeventh,
    ])
  | MajorSixth =>
    root->stackIntervalsAbsolutely([majorThird, perfectFifth, majorSixth])
  | MinorSixth =>
    root->stackIntervalsAbsolutely([minorThird, perfectFifth, majorSixth])
  };

let buildMajorScale = root =>
  root->stackIntervalsRelatively([
    majorSecond,
    majorSecond,
    minorSecond,
    majorSecond,
    majorSecond,
    majorSecond,
    minorSecond,
  ]);

let string_of_notes = notes =>
  notes->Belt.List.reduce("", (acc, note) => acc ++ (note |> string_of_note));
