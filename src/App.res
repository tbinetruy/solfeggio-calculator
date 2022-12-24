open Theory;
open Fretboard;

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

module Select = {
  let getOptions = spec =>
    (
      []
      |> StringMap.fold(
           (value, _, acc) =>
             acc->Belt.Array.concat([
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
      ->Belt.Option.map(callback => callback());
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
  let (rootPitchClass, setRootPitchClass) = React.useState(() => C);
  let (accidental, setAccidental) = React.useState(() => Flat);
  let (chordType, _setChordType) = React.useState(() => None);
  let (scaleType, _setScaleType) = React.useState(() => None);
  let (tunning, setTunning) = React.useState(() => Standard);
  let root = {pitchClass: rootPitchClass, accidental};

  let notes = switch (chordType, scaleType) {
    | (Some(chordType), Some(_)) => root->buildChord(chordType)
    | (Some(chordType), None) => root->buildChord(chordType)
    | (None, Some(scaleType)) => root->buildScale(scaleType)
    | (None, None) => list{}
  }

  let setChordType = (f) => {
    _setChordType(f)
    _setScaleType(_ => None)
  }

  let setScaleType = (f) => {
    _setScaleType(f)
    _setChordType(_ => None)
  }

  let tunningSpec =
    [Standard, Ukulele]
    ->Belt.Array.reduce(StringMap.empty, (acc, el) =>
        acc |> StringMap.add(el->string_of_tunning, () => setTunning(_ => el))
      );

  let rootPitchSpec =
    [C, D, E, F, G, A, B]
    ->Belt.Array.reduce(StringMap.empty, (acc, el) =>
        acc
        |> StringMap.add(el->string_of_pitchClass, () =>
             setRootPitchClass(_ => el)
           )
      );

  let accidentalSpec =
    [Flat, Natural, Sharp]
    ->Belt.Array.reduce(StringMap.empty, (acc, el) =>
        acc
        |> StringMap.add(el->string_of_accidental, () =>
             setAccidental(_ => el)
           )
      );

  let chordTypeSpec =
    list{
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
    }
    ->Belt.List.map(el => Some(el))
    ->Belt.List.add(None)
    ->Belt.List.reduce(StringMap.empty, (acc, el) =>
        acc |> StringMap.add(el->Belt.Option.mapWithDefault("None", string_of_chord), () => setChordType(_ => el))
      );

  let scaleTypeSpec =
    list{
      MajorScale,
    }
    ->Belt.List.map(el => Some(el))
    ->Belt.List.add(None)
    ->Belt.List.reduce(StringMap.empty, (acc, el) =>
        acc |> StringMap.add(el->Belt.Option.mapWithDefault("None", string_of_scale), () => setScaleType(_ => el))
      );

  <div>
    <Select spec=tunningSpec value={tunning->string_of_tunning} />
    <Select spec=rootPitchSpec value={rootPitchClass->string_of_pitchClass} />
    <Select spec=accidentalSpec value={accidental->string_of_accidental} />
    <Select spec=chordTypeSpec value={chordType->Belt.Option.mapWithDefault("None", string_of_chord)} />
    <Select spec=scaleTypeSpec value={scaleType->Belt.Option.mapWithDefault("None", string_of_scale)} />
    <div> {React.string(notes |> string_of_notes)} </div>
    <Fretboard notes tunning />
  </div>;
};
