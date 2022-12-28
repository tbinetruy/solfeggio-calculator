open Theory;
open Note;
open Belt;


module Tunning = {
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
        E(Natural),
        B(Natural),
        G(Natural),
        D(Natural),
        A(Natural),
        E(Natural),
      }
    | Ukulele => list{
        B(Natural),
        E(Natural),
        C(Natural),
        G(Natural),
      }
    };
}

module String = {
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

  let drawNotesOnString = (chord, fretZeroNote) => {
    let fretZeroHight = fretZeroNote->to_semitones;
    fretZeroHight
    ->createString(13, list{})
    ->List.map(currentSemitone => {
        chord->List.reduce(None, (acc, note) =>
          switch (acc) {
          | None =>
            let noteSemitone =
              note->to_semitones < 0
                ? mod((12 + note->to_semitones), 12)
                : mod(note->to_semitones, 12);
            noteSemitone == currentSemitone ? Some(note) : None;
          | Some(_) => acc
          }
        )
      });
  };

  @react.component
  let make = (~notes, ~rootNote) => {
    let wrapperStyle =
      ReactDOM.Style.make(
        ~width="2rem",
        ~height="2rem",
        ~borderRight="1px solid black",
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
          | Interval.Unison
          | Interval.Octave => "#416ab0"
          | Interval.Second(_) => "#6290bf"
          | Interval.Third(_) => "#80b0cc"
          | Interval.Fourth(_) => "#9bccd5"
          | Interval.Fifth(_) => "#b6e2dc"
          | Interval.Sixth(_) => "#cff2e0"
          | Interval.Seventh(_) => "#e8fce1"
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

    notes
    ->getTonic
    ->Option.mapWithDefault(
      <div> {React.string("Error")} </div>, tonic => {
      notes
      ->drawNotesOnString(rootNote)
      ->List.mapWithIndex((i, note) =>
          <div style=wrapperStyle key={i->string_of_int}>
            <div style=stringStyle />
            {switch (note) {
            | Some(n) =>
              tonic
              ->Interval.interval_of_notes(n)
              ->Option.mapWithDefault(
                  <div> {React.string("error")} </div>, i => {
                  <div style={getNoteStyle(i)}>
                    {React.string(n->to_string)}
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
}

module FretNumbers = {
  // turns list{1, 3, 5} into list{0, 1, 0, 0, 3, 0, 5}
  let rec buildNumbers = (numbers, counter, acc) =>
    switch numbers {
      | list{head, ...tail} =>
          if (counter < head) {
            numbers->buildNumbers(counter + 1, acc->List.add(0))
          } else {
            tail->buildNumbers(counter + 1, acc->List.add(head))
          }
      | _ => acc->List.reverse
    }

  @react.component
  let make = (~numbers) => {
    let wrapperStyle =
      ReactDOM.Style.make(
        ~display="flex",
        (),
      );

    let numberStyle =
      ReactDOM.Style.make(
        ~display="flex",
        ~alignItems="center",
        ~justifyContent="center",
        ~width="2rem",
        ~height="2rem",
        ~borderRight="1px solid white",
        (),
      );


    let numbersElement =
      numbers
      ->buildNumbers(0, list{})
      ->List.toArray
      ->Array.map(number => switch number {
        | 0 => <div style=numberStyle></div>
        | number => <div style=numberStyle> {React.string(number->Int.toString)} </div>
      })
      ->React.array

    <div style=wrapperStyle>
      { numbersElement }
    </div>
  }
}

let drawFretboard = (notes, tunning) => {
  tunning
  ->List.mapWithIndex((i, fretZeroNote) =>
      <div
        style={ReactDOM.Style.make(~display="flex", ())}
        key={i->string_of_int}>
        <String notes={notes} rootNote={fretZeroNote} />
      </div>
    )
  ->List.toArray
  ->React.array;
};

@react.component
let make = (~notes, ~tunning) => {
  <div>
    <div> {notes->drawFretboard(tunning->Tunning.build_tunning)} </div>
    <FretNumbers numbers={list{1, 3, 5, 7, 9, 12}}/>
  </div>
};
