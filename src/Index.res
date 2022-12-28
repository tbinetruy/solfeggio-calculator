switch ReactDOM.querySelector("body") {
  | None => ()
  | Some(body) =>
      let root = ReactDOM.Client.createRoot(body)
      ReactDOM.Client.Root.render(root, <App />)
}
