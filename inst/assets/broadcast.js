/**
* Return the name of the function arguments.
*
* @param {function} func A function.
*
* @returns A character array.
*
* @example
* function max(a, b) {
*     return a > b ? a : b;
* }
* // returns ['a', 'b']
* formalArgs(max)
*
* @exports
*/
const formalArgs = function(func) {
  let funStr = func.toString();
  let args = funStr.slice(funStr.indexOf('(') + 1, funStr.indexOf(')'));
  return args.split(",").map(x => x.split("=")[0].trim());
};

const maxLen = function(args) {
  let lens = args
    .filter(x => Array.isArray(x))
    .map(x => x.length);

  if (lens.length === 0) return 1;
  return lens.reduce((a, b) => Math.max(a, b));
};

// Broadcasting (handles different shape of data)
// Note that broadcast only works for function that has at most 10 arguments.
// The limitation is due to `Ramda.curry` implementation.
const broadcast = function(f) {
    let argNames = formalArgs(f);
    let g = R.curry(f);
    function h(fs, maybe_b, id) {
        let b_is_Arr = Array.isArray(maybe_b);
        if (b_is_Arr && (fs.length != maybe_b.length)) {
            throw new Error(`Length of '${argNames[id + 1]}' do not match '${argNames[0]}'.`);
        }
        return b_is_Arr ?
            R.zipWith((f,x) => f(x), fs, maybe_b) :
            fs.map(f => f(maybe_b));
    }
    return function() {
        let  args = [...arguments]
        , fstArgs = R.head(args)
        ,    init = Array.isArray(fstArgs) ? fstArgs : Array(maxLen(args)).fill(fstArgs);

        let result = R.reduce(h, init.map(x => g(x)), R.tail(args));
        return result;
    };
};
