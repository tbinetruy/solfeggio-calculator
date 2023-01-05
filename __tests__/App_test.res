open Jest
open Expect
open Webapi.Dom


type renderResult

@module("@testing-library/react") external act: (unit => unit) => unit = "act"

@module("@testing-library/react")
external render: React.element => renderResult = "render"
@get external container: renderResult => Dom.element = "container"
@get external baseElement: renderResult => Dom.element = "baseElement"


describe("Test Component", () => {
  test("Component renders", () => {
    let el = <div id="foo">{React.string("foo")}</div>
    el
      ->render
      ->baseElement
      ->Element.textContent
      ->expect
      ->toEqual("foo")
  })
})
