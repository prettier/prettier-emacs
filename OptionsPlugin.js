/*
 * A plugin that simply outputs all Prettier options serialized as
 * JSON. It ignores all input, except that it's never called when
 * input is empty.
 */

const parserName = 'optionsPrinter';
const astFormat = `${parserName}-ast`;
const languageName = parserName;

module.exports = {
  languages: [{
    name: languageName,
    parsers: [parserName]
  }],
  parsers: {
    [parserName]: {
      parse: () => ({}),
      astFormat,
      locStart: () => 0,
      locEnd: () => 0
    }
  },
  options: {},
  defaultOptions: {},
  printers: {
    [astFormat]: {
      print: (path, options, print) => JSON.stringify(options)
    }
  }
}
