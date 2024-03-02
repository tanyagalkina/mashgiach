(() => {
  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Main/index.js
  var main = /* @__PURE__ */ log("Hello, Purescript \u{1F35D} \u{1F602}");

  // <stdin>
  main();
})();
