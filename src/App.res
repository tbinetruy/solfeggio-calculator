open Theory
open Theory.Note
open Theory.Chord
open Theory.Scale
open Theory.Harmonization
open Theory.Notes
open Fretboard.Tunning

module StringMap = Map.Make({
  type t = string
  let compare = compare
})

open Belt

module Select = {
  let getOptions = spec =>
    ([] |> StringMap.fold(
      (value, _, acc) =>
        acc->Array.concat([<option key=value value> {React.string(value)} </option>]),
      spec,
    ))->React.array

  let onChange = (spec, event) => {
    let _ =
      ReactEvent.Form.target(event)["value"]
      ->StringMap.find_opt(spec)
      ->Option.map(callback => callback())
  }

  @react.component
  let make = (~spec, ~value) => {
    <div>
      <select value onChange={onChange(spec)}> {spec->getOptions} </select>
    </div>
  }
}

module AnnotatedFretboard = {
  @react.component
  let make = (~notes, ~tunning) => {
    let chord_name =
      notes
      ->Intervals.absoluteIntervals_of_notes
      ->Chord.from_intervals
      ->Result.mapWithDefault("", chord => " (" ++ chord->Chord.to_string ++ ")")

    <div>
      <div> {React.string(notes->string_of_notes ++ chord_name)} </div>
      <div> {React.string(notes->Intervals.relativeIntervals_of_notes->Intervals.to_string)} </div>
      <div> {React.string(notes->Intervals.absoluteIntervals_of_notes->Intervals.to_string)} </div>
      <Fretboard notes tunning />
    </div>
  }
}

@react.component
let make = () => {
  let (rootPitchClass, setRootPitchClass) = React.useState(() => C(Accidental.Natural))
  let (accidental, setAccidental) = React.useState(() => Accidental.Natural)
  let (chordType, _setChordType) = React.useState(() => None)
  let (scaleType, _setScaleType) = React.useState(() => Some(HarmonicMinorScale))
  let (intervalType, _setIntervalType) = React.useState(() => None)
  let (progressionType, _setProgressionType) = React.useState(() => Some([1, 4, 0]))
  let (tunning, setTunning) = React.useState(() => Standard)
  let root = rootPitchClass->Note.setAccidental(accidental)

  let buildInterval = Interval.to_notes
  let notes = switch (intervalType, chordType, scaleType) {
  | (Some(intervalType), Some(_), Some(_)) => root->buildInterval(intervalType)
  | (Some(intervalType), None, Some(_)) => root->buildInterval(intervalType)
  | (Some(intervalType), Some(_), None) => root->buildInterval(intervalType)
  | (Some(intervalType), None, None) => root->buildInterval(intervalType)
  | (None, Some(chordType), Some(_)) => root->Chord.to_notes(chordType)
  | (None, Some(chordType), None) => root->Chord.to_notes(chordType)
  | (None, None, Some(scaleType)) => root->Scale.to_notes(scaleType)
  | (None, None, None) => list{}
  }

  let harmonization_triad_chords = scaleType->Option.mapWithDefault("", scale =>
    switch scale->Harmonization.to_triads {
    | Result.Ok(chords) => chords->Chords.to_string
    | Result.Error(msg) => msg
    }
  )

  let harmonization_triads = scaleType->Option.mapWithDefault("", scale =>
    switch scale->to_triad_progression(root) {
    | Result.Ok(harmonization) => harmonization->Progression.to_string
    | Result.Error(msg) => msg
    }
  )

  let harmonization_tetrad_chords = scaleType->Option.mapWithDefault("", scale =>
    switch scale->Harmonization.to_tetrads {
    | Result.Ok(chords) => chords->Chords.to_string
    | Result.Error(msg) => msg
    }
  )

  let harmonization_tetrads = scaleType->Option.mapWithDefault("", scale =>
    switch scale->Harmonization.to_tetrad_progression(root) {
    | Result.Ok(harmonization) => harmonization->Progression.to_string
    | Result.Error(msg) => msg
    }
  )

  let setChordType = f => {
    _setChordType(f)
    _setIntervalType(_ => None)
    _setScaleType(_ => None)
  }

  let setScaleType = f => {
    _setScaleType(f)
    _setIntervalType(_ => None)
    _setChordType(_ => None)
  }

  let setIntervalType = f => {
    _setIntervalType(f)
    _setScaleType(_ => None)
    _setChordType(_ => None)
  }

  let tunningSpec =
    [Standard, Ukulele]->Array.reduce(StringMap.empty, (acc, el) =>
      acc |> StringMap.add(el->string_of_tunning, () => setTunning(_ => el))
    )

  let rootPitchSpec =
    [
      C(Natural),
      D(Natural),
      E(Natural),
      F(Natural),
      G(Natural),
      A(Natural),
      B(Natural),
    ]->Array.reduce(StringMap.empty, (acc, el) =>
      acc |> StringMap.add(el->Note.to_string, () => setRootPitchClass(_ => el))
    )

  let accidentalSpec =
    [Accidental.Flat, Accidental.Natural, Accidental.Sharp]->Array.reduce(StringMap.empty, (
      acc,
      accidental,
    ) =>
      acc |> StringMap.add(accidental->Accidental.to_string, () => setAccidental(_ => accidental))
    )

  open Interval
  let intervalTypeSpec =
    [
      Minor->Second,
      Major->Second,
      Diminished->Third,
      Minor->Third,
      Major->Third,
      Diminished->Fourth,
      Perfect->Fourth,
      Augmented->Fourth,
      Diminished->Fifth,
      Perfect->Fifth,
      Augmented->Fifth,
      Minor->Sixth,
      Major->Sixth,
      Diminished->Seventh,
      Minor->Seventh,
      Major->Seventh,
    ]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
      acc |> StringMap.add(el->Option.mapWithDefault("None", Interval.to_string), () =>
        setIntervalType(_ => el)
      )
    )

  let chordTypeSpec =
    [
      MajorTriad,
      MinorTriad,
      AugmentedTriad,
      DiminishedTriad,
      SuspendedTriad,
      PowerChord,
      DiminishedPowerChord,
      AugmentedPowerChord,
      MajorSeventh,
      DominanteSeventh,
      MinorSeventhMajor,
      MinorSeventh,
      AugmentedMajorSeventh,
      HalfDiminishedSeventh,
      DiminishedSeventh,
      SuspendedSeventh,
      SeventhAugmentedFifth,
      SeventhDiminishedFifth,
      MajorSixth,
      MinorSixth,
    ]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
      acc |> StringMap.add(el->Option.mapWithDefault("None", Chord.to_string), () =>
        setChordType(_ => el)
      )
    )

  let scaleTypeSpec =
    [
      MajorScale,
      NaturalMinorScale,
      HarmonicMinorScale,
      IonianMode,
      DorianMode,
      PhrygianMode,
      LydianMode,
      MixolydianMode,
      AeolianMode,
      IocrianMode,
    ]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
      acc |> StringMap.add(el->Option.mapWithDefault("None", string_of_scale), () =>
        setScaleType(_ => el)
      )
    )

  let string_of_progressionType = degreeNumbers =>
    degreeNumbers->Array.reduce("", (acc, degreeNumber) =>
      acc ++ (degreeNumber + 1)->Int.toString ++ " "
    )

  let progressionTypeSpec =
    [[1, 4, 0]]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
      acc |> StringMap.add(
        el->Option.mapWithDefault(
          "None",
          degreeNumbers => degreeNumbers->string_of_progressionType,
        ),
        () => _setProgressionType(_ => el),
      )
    )

  let progression =
    switch (scaleType, progressionType) {
    | (Some(scale), Some(progression_degrees)) =>
      to_progression(
        scale,
        root,
        tetrad_degrees,
        progression_degrees,
      )->Result.getWithDefault(list{})
    | _ => list{}
    }
    ->List.mapWithIndex((i, notes) => <AnnotatedFretboard key={i->Int.toString} notes tunning />)
    ->List.toArray
    ->React.array

  <div>
    <Select spec=tunningSpec value={tunning->string_of_tunning} />
    <Select spec=rootPitchSpec value={rootPitchClass->Note.to_string} />
    <Select spec=accidentalSpec value={accidental->Accidental.to_string} />
    <Select
      spec=intervalTypeSpec value={intervalType->Option.mapWithDefault("None", Interval.to_string)}
    />
    <Select spec=chordTypeSpec value={chordType->Option.mapWithDefault("None", Chord.to_string)} />
    <Select spec=scaleTypeSpec value={scaleType->Option.mapWithDefault("None", string_of_scale)} />
    <Select
      spec=progressionTypeSpec
      value={progressionType->Option.mapWithDefault("None", el => el->string_of_progressionType)}
    />
    <div> {React.string(harmonization_triad_chords)} </div>
    <div> {React.string(harmonization_triads)} </div>
    <div> {React.string("----")} </div>
    <div> {React.string(harmonization_tetrad_chords)} </div>
    <div> {React.string(harmonization_tetrads)} </div>
    <div> {React.string("----")} </div>
    <AnnotatedFretboard notes tunning />
    {progression}
  </div>
}
