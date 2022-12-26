open Theory;
open Note;
open Fretboard;

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

open Belt;

module Select = {
  let getOptions = spec =>
    (
      []
      |> StringMap.fold(
           (value, _, acc) =>
             acc->Array.concat([
               <option key=value value> {React.string(value)} </option>,
             ]),
           spec,
         )
    )
    ->React.array;

  let onChange = (spec, event) => {
    let _ =
      ReactEvent.Form.target(event)["value"]
      ->StringMap.find_opt(spec)
      ->Option.map(callback => callback());
    ();
  };

  @react.component
  let make = (~spec, ~value) => {
    <div>
      <select value onChange={onChange(spec)}> {spec->getOptions} </select>
    </div>;
  };
};

@react.component
let make = () => {
  let (rootPitchClass, setRootPitchClass) = React.useState(() => C(Accidental.Natural));
  let (accidental, setAccidental) = React.useState(() => Accidental.Flat);
  let (chordType, _setChordType) = React.useState(() => Some(MajorSeventh));
  let (scaleType, _setScaleType) = React.useState(() => None);
  let (intervalType, _setIntervalType) = React.useState(() => None);
  let (tunning, setTunning) = React.useState(() => Standard);
  let root = rootPitchClass->Note.setAccidental(accidental)

  let notes = switch (intervalType, chordType, scaleType) {
    | (Some(intervalType), Some(_), Some(_)) => root->buildInterval(intervalType)
    | (Some(intervalType), None, Some(_)) => root->buildInterval(intervalType)
    | (Some(intervalType), Some(_), None) => root->buildInterval(intervalType)
    | (Some(intervalType), None, None) => root->buildInterval(intervalType)
    | (None, Some(chordType), Some(_)) => root->buildChord(chordType)
    | (None, Some(chordType), None) => root->buildChord(chordType)
    | (None, None, Some(scaleType)) => root->buildScale(scaleType)
    | (None, None, None) => list{}
  }

  let setChordType = (f) => {
    _setChordType(f)
    _setIntervalType(_ => None)
    _setScaleType(_ => None)
  }

  let setScaleType = (f) => {
    _setScaleType(f)
    _setIntervalType(_ => None)
    _setChordType(_ => None)
  }

  let setIntervalType = (f) => {
    _setIntervalType(f)
    _setScaleType(_ => None)
    _setChordType(_ => None)
  }

  let tunningSpec =
    [Standard, Ukulele]
    ->Array.reduce(StringMap.empty, (acc, el) =>
        acc |> StringMap.add(el->string_of_tunning, () => setTunning(_ => el))
      );

  let rootPitchSpec =
    [C(Natural), D(Natural), E(Natural), F(Natural), G(Natural), A(Natural), B(Natural)]
    ->Array.reduce(StringMap.empty, (acc, el) =>
        acc
        |> StringMap.add(el->to_string, () =>
             setRootPitchClass(_ => el)
           )
      );

  let accidentalSpec =
    [Accidental.Flat, Accidental.Natural, Accidental.Sharp]
    ->Array.reduce(StringMap.empty, (acc, accidental) =>
        acc
        |> StringMap.add(accidental->Accidental.to_string, () =>
             setAccidental(_ => accidental)
           )
      );

  open Interval
  let intervalTypeSpec =
    [
      Minor->Second,
      Major->Second,
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
        acc |> StringMap.add(el->Option.mapWithDefault("None", Interval.to_string), () => setIntervalType(_ => el))
      );

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
        acc |> StringMap.add(el->Option.mapWithDefault("None", string_of_chord), () => setChordType(_ => el))
      );

  let scaleTypeSpec =
    [
      MajorScale,
    ]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
        acc |> StringMap.add(el->Option.mapWithDefault("None", string_of_scale), () => setScaleType(_ => el))
      );

  <div>
    <Select spec=tunningSpec value={tunning->string_of_tunning} />
    <Select spec=rootPitchSpec value={rootPitchClass->Note.to_string} />
    <Select spec=accidentalSpec value={accidental->Accidental.to_string} />
    <Select spec=intervalTypeSpec value={intervalType->Option.mapWithDefault("None", Interval.to_string)} />
    <Select spec=chordTypeSpec value={chordType->Option.mapWithDefault("None", string_of_chord)} />
    <Select spec=scaleTypeSpec value={scaleType->Option.mapWithDefault("None", string_of_scale)} />
    <div> {React.string(notes->string_of_notes)} </div>
    <div> {React.string(notes->relativeFormula_of_notes)} </div>
    <Fretboard notes tunning />
  </div>;
};
