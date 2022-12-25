open Theory;
open Accidental;
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
  let (rootPitchClass, setRootPitchClass) = React.useState(() => C(Natural));
  let (accidental, setAccidental) = React.useState(() => Flat);
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
        |> StringMap.add(el->string_of_note, () =>
             setRootPitchClass(_ => el)
           )
      );

  let accidentalSpec =
    [Flat, Natural, Sharp]
    ->Array.reduce(StringMap.empty, (acc, accidental) =>
        acc
        |> StringMap.add(accidental->to_string, () =>
             setAccidental(_ => accidental)
           )
      );

  let intervalTypeSpec =
    [
      MinorSecond,
      MajorSecond,
      MinorThird,
      MajorThird,
      DiminishedFourth,
      PerfectFourth,
      AugmentedFourth,
      DiminishedFifth,
      PerfectFifth,
      AugmentedFifth,
      MinorSixth,
      MajorSixth,
      DiminishedSeventh,
      MinorSeventh,
      MajorSeventh,
    ]
    ->Array.map(el => Some(el))
    ->Array.concat([None])
    ->Array.reduceU(StringMap.empty, (. acc, el) =>
        acc |> StringMap.add(el->Option.mapWithDefault("None", string_of_interval), () => setIntervalType(_ => el))
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
    <Select spec=rootPitchSpec value={rootPitchClass->string_of_note} />
    <Select spec=accidentalSpec value={accidental->to_string} />
    <Select spec=intervalTypeSpec value={intervalType->Option.mapWithDefault("None", string_of_interval)} />
    <Select spec=chordTypeSpec value={chordType->Option.mapWithDefault("None", string_of_chord)} />
    <Select spec=scaleTypeSpec value={scaleType->Option.mapWithDefault("None", string_of_scale)} />
    <div> {React.string(notes |> string_of_notes)} </div>
    <Fretboard notes tunning />
  </div>;
};
