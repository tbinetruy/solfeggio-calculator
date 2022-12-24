open Belt;


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
  notes->List.reduce("", (acc, note) => acc ++ (note |> string_of_note));
