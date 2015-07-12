/* global exports */
"use strict";

// module Test.Utils

exports.exit = function (exitCode) {
  return function () {
    process.exit(exitCode);
  };
};
