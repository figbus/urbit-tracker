import { Elm } from './src/Live.elm'


const app = Elm.Live.init({
  node: document.getElementById('root'),
  flags: Date.now()
})


app.ports.setupUrbitEventSource.subscribe((url) => {
  const eventSource = new EventSource(url, { withCredentials: true });

  eventSource.onmessage = function (event) {
    app.ports.onUrbitMessage.send({ message: event });
  };

  eventSource.onerror = function (event) {
    app.ports.onUrbitMessage.send({ error: event });
  };
});