// @ts-ignore
import { Elm } from './Main.elm';

import { createInterface, Interface } from 'readline';

const main = async () => {
  const i = createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const e = Elm.Main.init({ flags: '"hello"' });
  e.ports.output.subscribe(console.log);

  for (;;) {
    const code = await prompt(i);
    if (code === 'exit') {
      break;
    }
    e.ports.input.send(code);
  }

  i.close();
};

const prompt = (i: Interface) => {
  return new Promise((res) => {
    i.question('> ', (ans) => res(ans));
  });
};

main();
