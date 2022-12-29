switch ReactDOM.querySelector("#app") {
| None => ()
| Some(body) =>
  let root = ReactDOM.Client.createRoot(body)
  ReactDOM.Client.Root.render(root, <App />)
}
