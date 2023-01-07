open Jest
open Expect
open Webapi.Dom
open Belt


// https://github.com/rescriptbr/react-testing-library/blob/main/src/ReactTestingLibrary.res
type renderResult

@module("@testing-library/react") external act: (unit => unit) => unit = "act"

@module("@testing-library/react")
external render: React.element => renderResult = "render"
@get external container: renderResult => Dom.element = "container"
@get external baseElement: renderResult => Dom.element = "baseElement"

type fireEvent
@module("@testing-library/react")
external fireEvent: fireEvent = "fireEvent"
@send external click: (fireEvent, Dom.element) => unit = "click"


module Test = {
  @react.component
  let make = () => {
    let (counter, setCounter) = React.useState(() => 0)
    let f = _ => setCounter(counter => counter + 1)

    <div id="foo" onClick={f}>{React.string(counter->Int.toString)}</div>
  }
}

describe("Test Component", () => {
  test("Component renders", () => {
    let el =
      <Test />
      ->render
      ->container
      ->Element.querySelector("#foo")
      ->Option.getExn

    let _ = fireEvent->click(el)

    el
      ->Element.textContent
      ->expect
      ->toEqual("1")
  })
})
