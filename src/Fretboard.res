open Theory;
open Belt;


type tunning =
  | Standard
  | Ukulele;

let string_of_tunning = tunning =>
  switch (tunning) {
  | Standard => "standard"
  | Ukulele => "ukulele"
  };

let build_tunning = tunning =>
  switch (tunning) {
  | Standard => list{
      {pitchClass: E, accidental: Natural}, // high E
      {pitchClass: B, accidental: Natural},
      {pitchClass: G, accidental: Natural},
      {pitchClass: D, accidental: Natural},
      {pitchClass: A, accidental: Natural},
      {pitchClass: E, accidental: Natural},
    }
  | Ukulele => list{
      {pitchClass: A, accidental: Natural},
      {pitchClass: E, accidental: Natural},
      {pitchClass: C, accidental: Natural},
      {pitchClass: G, accidental: Natural},
    }
  };

let rec createString = (startNoteHeight, length, acc) => {
  switch (length) {
  | 0 => acc
  | n =>
    createString(
      startNoteHeight + 1,
      n - 1,
      acc->List.concat(list{mod(startNoteHeight, 12)}),
    )
  };
};

let rootNote = {pitchClass: E, accidental: Natural};

let drawNotesOnString = (chord, fretZeroNote) => {
  let fretZeroHight = fretZeroNote |> semitones_of_note;
  fretZeroHight
  ->createString(13, list{})
  ->List.map(currentSemitone => {
      chord->List.reduce(None, (acc, note) =>
        switch (acc) {
        | None =>
          let noteSemitone =
            note->semitones_of_note < 0
              ? mod((12 + note->semitones_of_note), 12)
              : mod(note->semitones_of_note, 12);
          noteSemitone == currentSemitone ? Some(note) : None;
        | Some(_) => acc
        }
      )
    });
};

let drawString = (notes, stringRootNote) => {
  let wrapperStyle =
    ReactDOM.Style.make(
      ~width="2rem",
      ~height="2rem",
      ~borderRight="1px solid black",
      ~borderLeft="1px solid black",
      ~position="relative",
      (),
    );
  let getNoteStyle = interval =>
    ReactDOM.Style.make(
      ~width="1.5rem",
      ~height="1.5rem",
      ~borderRadius="50%",
      ~background="red",
      ~position="absolute",
      ~top="50%",
      ~left="50%",
      ~transform="translate(-50%, -50%)",
      ~display="flex",
      ~justifyContent="center",
      ~alignItems="center",
      ~fontSize="0.8rem",
      ~border="1px solid black",
      ~backgroundColor=
        switch (interval) {
        | Unison
        | Octave => "#416ab0"
        | Second(_) => "#6290bf"
        | Third(_) => "#80b0cc"
        | Fourth(_) => "#9bccd5"
        | Fifth(_) => "#b6e2dc"
        | Sixth(_) => "#cff2e0"
        | Seventh(_) => "#e8fce1"
        },
      (),
    );
  let stringStyle =
    ReactDOM.Style.make(
      ~width="2rem",
      ~height="1rem",
      ~borderBottom="1px solid black",
      (),
    );

  let tonic = getTonic(notes);
  tonic->Option.mapWithDefault(
    <div> {React.string("Error")} </div>, tonic => {
    notes
    ->drawNotesOnString(stringRootNote)
    ->List.mapWithIndex((i, note) =>
        <div style=wrapperStyle key={i->string_of_int}>
          <div style=stringStyle />
          {switch (note) {
           | Some(n) =>
             tonic
             ->interval_of_notes(n)
             ->Option.mapWithDefault(
                 <div> {React.string("error")} </div>, i => {
                 <div style={getNoteStyle(i)}>
                   {React.string(n->string_of_note)}
                 </div>
               })
           | None => <div />
           }}
        </div>
      )
    ->List.toArray
    ->React.array
  });
};

let drawFretboard = (notes, tunning) => {
  tunning
  ->List.mapWithIndex((i, fretZeroNote) =>
      <div
        style={ReactDOM.Style.make(~display="flex", ())}
        key={i->string_of_int}>
        {notes->drawString(fretZeroNote)}
      </div>
    )
  ->List.toArray
  ->React.array;
};

@react.component
let make = (~notes, ~tunning) => {
  <div> {notes->drawFretboard(tunning->build_tunning)} </div>;
};
